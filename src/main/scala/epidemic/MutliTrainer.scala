package epidemic

import org.apache.spark.sql.{DataFrame, SparkSession}
import org.apache.spark.sql.functions._
import io.github.pashashiz.spark_encoders.TypedEncoder.*
import io.github.pashashiz.spark_encoders.TypedEncoder.given
import epidemic.Snapshots

final case class RowToy(
                         epoch: Int, step: Int, country: String,
                         reward: Double, s: Double, i: Double, r: Double, d: Double, v: Double, eps: Double
                       )

final class MultiTrainerToy(
                             spark: SparkSession,
                             hp: HyperParams,
                             cfg: TrainConfig,
                             world: WorldToy,
                             logger: Logger
                           ) {
  private val ss = spark
  import ss.implicits.given

  private val logEvery = math.max(200, hp.logInterval)
  private val learnEvery = hp.learnEvery

  def run(): DataFrame = {
    val rows = scala.collection.mutable.ArrayBuffer[RowToy]()
    var step = 0
    
    val ema = scala.collection.mutable.Map.empty[String, Double].withDefaultValue(0.0)
    val emaAlpha = 0.2
    
    WandB.log(Map("training_started" -> 1.0, "initial_step" -> 0.0))
    WandB.flush(0L)

    for (ep <- 1 to cfg.epochs) {
      var t = 0
      while (t < cfg.stepsPerEpoch) {
        val batch = world.stepOne(cfg.stepsPerEpoch)
        
        val avgLoss = world.nodes.map(_.agent.emaLossNow).sum / world.nodes.size.toDouble.max(1.0)

        // collect for offline df
        batch.foreach { case (name, s2, r) =>
          val idx = world.nodes.indexWhere(_.name == name)
          val node = world.nodes(idx)
          val epsNow = node.agent.epsilonNow

          rows += RowToy(ep, step, name, r, s2.s, s2.i, s2.r, s2.d, s2.v, epsNow)
        }
        
        if (step % 50 == 0) {
          batch.foreach { case (c, _, r) =>
            val prev = ema(c)
            ema.update(c, emaAlpha * r + (1.0 - emaAlpha) * prev)
          }
          
          val epsMean = world.nodes.map(_.agent.epsilonNow).sum / world.nodes.size.toDouble
          val lossEma = world.nodes.headOption.map(_.agent.emaLossNow).getOrElse(0.0)
          
          val currentMetrics = scala.collection.mutable.Map[String, Double]()
          
          ema.foreach { case (c, v) => currentMetrics(s"toy_rew_$c") = v }
          
          currentMetrics("toy_eps_mean") = epsMean
          currentMetrics("loss_ema") = lossEma
          currentMetrics("step") = step.toDouble
          currentMetrics("epoch") = ep.toDouble
          
          val rewardValues = ema.values.toSeq
          if (rewardValues.nonEmpty) {
            val mean = rewardValues.sum / rewardValues.length
            val variance = rewardValues.map(r => math.pow(r - mean, 2)).sum / rewardValues.length
            val cv = if (mean != 0.0) math.sqrt(variance) / math.abs(mean) else 0.0
            val range = rewardValues.max - rewardValues.min

            currentMetrics("reward_heterogeneity_variance") = variance
            currentMetrics("reward_heterogeneity_cv") = cv
            currentMetrics("reward_heterogeneity_range") = range
            currentMetrics("reward_mean") = mean
            currentMetrics("reward_std") = math.sqrt(variance)
          }
          
          world.nodes.foreach { node =>
            try {
              val profile = Geo.CountryProfiles.getProfile(node.name)
              currentMetrics(s"${node.name}_infection_rate") = node.s.i.toDouble / node.s.s.max(1.0)
              currentMetrics(s"${node.name}_healthcare_load") = node.s.i.toDouble / (profile.hospPer1k * profile.pop / 1000.0)
              currentMetrics(s"${node.name}_per_capita_infections") = node.s.i.toDouble / profile.pop.toDouble
              currentMetrics(s"${node.name}_policy_stringency") = node.lastA.toDouble / 7.0  // Normalize action to 0-1
            } catch {
              case _: Exception => // Skip if profile not found
            }
          }

          // Force immediate logging
          println(s"Step $step: Logging ${currentMetrics.size} metrics to WANDB")
          WandB.log(currentMetrics.toMap)
          WandB.flush(step.toLong)

          // Debug print some key metrics
          if (step % 200 == 0) {
            println(s"=== Step $step Debug ===")
            ema.foreach { case (country, reward) =>
              println(f"$country: $reward%.4f")
            }
            println(f"EpsMean: $epsMean%.4f, LossEma: $lossEma%.4f")
            println("=====================")
          }

          logger.logScalar("toy_eps_mean", epsMean, step)
          Snapshots.writeEpochSnapshot(spark, world, step, avgLoss, "stream_in/snapshots")
        }

        t += 1
        step += 1
      }

      // End of epoch logging
      WandB.log(Map("epoch_completed" -> ep.toDouble, "total_steps" -> step.toDouble))
      WandB.flush(step.toLong)
      
      val finalAvgLoss = world.nodes.map(_.agent.emaLossNow).sum / world.nodes.size.toDouble.max(1.0)
      Snapshots.writeEpochSnapshot(spark, world, step, finalAvgLoss, "stream_in/snapshots")

      // periodic greedy evaluation
      if (cfg.evalEvery > 0 && ep % cfg.evalEvery == 0) {
        import Validation._
        val saved = world.snapshotStates()
        world.resetToInitial()

        val series = scala.collection.mutable.Map.empty[String, Vector[Double]].withDefaultValue(Vector.empty)
        var k = 0
        val maxEvalSteps = 800

        while (k < maxEvalSteps) {
          val batch = world.stepOneGreedy(maxEvalSteps)
          batch.foreach { case (name, s2, _) => series.update(name, series(name) :+ s2.i) }
          k += 1
        }

        val pops = world.nodes.map(n => n.name -> n.pop.toDouble).toMap
        val evalMetrics = scala.collection.mutable.Map[String, Double]()

        series.foreach { case (c, is) =>
          val isN = is.map(_ / math.max(1.0, pops(c)))
          evalMetrics(s"eval_arrival_$c") = arrivalTime(is, 10.0).toDouble
          evalMetrics(s"eval_peakI_$c") = peakI(is)
          evalMetrics(s"eval/arrivalN_$c") = arrivalTime(isN, 1e-4).toDouble
          evalMetrics(s"eval/peakI_norm_$c") = peakI(isN)
        }

        evalMetrics("eval_epoch") = ep.toDouble

        println(s"Evaluation epoch $ep: Logging ${evalMetrics.size} eval metrics")
        WandB.log(evalMetrics.toMap)
        WandB.flush(step.toLong)

        world.restoreStates(saved)
      }
    }
    
    WandB.log(Map(
      "training_completed" -> 1.0,
      "final_step" -> step.toDouble,
      "total_epochs" -> cfg.epochs.toDouble
    ))
    WandB.flush(step.toLong)

    val df = spark.createDataset(rows.toSeq).toDF
    df.groupBy("epoch", "country")
      .agg(
        avg(col("reward")).as("avg_reward"),
        last(col("s")).as("final_s"),
        last(col("i")).as("final_i"),
        last(col("d")).as("final_d"),
        last(col("eps")).as("epsilon")
      )
  }
}