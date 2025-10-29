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

  private val logEvery   = math.max(200, hp.logInterval)
  private val learnEvery = hp.learnEvery

  def run(): DataFrame = {
    val rows = scala.collection.mutable.ArrayBuffer[RowToy]()
    var step = 0

    // EMA buffer for smoother reward logs
    val ema = scala.collection.mutable.Map.empty[String, Double].withDefaultValue(0.0)
    val emaAlpha = 0.2

    for (ep <- 1 to cfg.epochs) {
      var t = 0
      while (t < cfg.stepsPerEpoch) {
        val batch = world.stepOne(cfg.stepsPerEpoch)
        val lossSum = batch.map(_._3).sum

        // collect for offline df
        batch.foreach { case (name, s2, r) =>
          val idx = world.nodes.indexWhere(_.name == name)
          val epsNow = world.nodes(idx).agent.epsilonNow
          rows += RowToy(ep, step, name, r, s2.s, s2.i, s2.r, s2.d, s2.v, epsNow)
        }

        if (step % logEvery == 0) {
          // update EMA
          batch.foreach { case (c, _, r) =>
            val prev = ema(c)
            ema.update(c, emaAlpha * r + (1.0 - emaAlpha) * prev)
          }
          // epsilon mean and agent loss ema
          val epsMean = world.nodes.map(_.agent.epsilonNow).sum / world.nodes.size.toDouble
          val lossEma = world.nodes.headOption.map(_.agent.emaLossNow).getOrElse(0.0)

          // single logging point; no explicit "step" key to avoid out-of-order warnings
          WandB.log(ema.map { case (c, v) => s"toy_rew_$c" -> v }.toMap ++
            Map("toy_eps_mean" -> epsMean, "loss_ema" -> lossEma))
          logger.logScalar("toy_eps_mean", epsMean, step)
          WandB.flush(step.toLong)
          
          Snapshots.writeEpochSnapshot(spark, world, step, lossSum, "stream_in/snapshots")
        }

        t += 1;
        step += 1
      }

      Snapshots.writeEpochSnapshot(spark, world, step, 0.0, "stream_in/snapshots")

      // periodic greedy evaluation (no replay writes, no learning)
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
        val metrics = series.flatMap { case (c, is) =>
          val isN = is.map(_ / math.max(1.0, pops(c)))
          Seq(
            s"eval_arrival_$c" -> arrivalTime(is, 10.0).toDouble,
            s"eval_peakI_$c" -> peakI(is),
            s"eval/arrivalN_$c" -> arrivalTime(isN, 1e-4).toDouble,
            s"eval/peakI_norm_$c" -> peakI(isN)
          )
        }.toMap
        WandB.log(metrics ++ Map("eval_epoch" -> ep.toDouble))
        world.restoreStates(saved)


      }
    }
      
    
    val df = spark.createDataset(rows.toSeq).toDF
    df.groupBy("epoch","country")
      .agg(
        avg(col("reward")).as("avg_reward"),
        last(col("s")).as("final_s"),
        last(col("i")).as("final_i"),
        last(col("d")).as("final_d"),
        last(col("eps")).as("epsilon")
      )
  }
}
