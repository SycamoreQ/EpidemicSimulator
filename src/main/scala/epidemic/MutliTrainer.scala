// src/main/scala/epidemic/MultiTrainerToy.scala
package epidemic

import org.apache.spark.sql.{DataFrame, SparkSession}
import org.apache.spark.sql.functions._
import io.github.pashashiz.spark_encoders.TypedEncoder.*
import io.github.pashashiz.spark_encoders.TypedEncoder.given

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

  // Throttles
  private val logEvery   = math.max(50, hp.logInterval) // batch metrics every 50 steps
  private val learnEvery = 4                             // (learn cadence is enforced in WorldToy)

  def run(): DataFrame = {
    val rows = scala.collection.mutable.ArrayBuffer[RowToy]()
    var step = 0

    for (ep <- 1 to cfg.epochs) {
      var t = 0
      while (t < cfg.stepsPerEpoch) {
        val batch = world.stepOne(cfg.stepsPerEpoch) // WorldToy handles act/observe/learn cadence

        // Collect rows for Spark table
        batch.foreach { case (name, s2, r) =>
          val idx = world.nodes.indexWhere(_.name == name)
          val eps = world.nodes(idx).agent.epsilonNow
          rows += RowToy(ep, step, name, r, s2.s, s2.i, s2.r, s2.d, s2.v, eps)
        }

        // Batch W&B metrics every N steps (single flush)
        if (step % logEvery == 0) {
          val byC: Map[String, Double] =
            batch.groupBy(_._1).view.mapValues(_.map(_._3).sum).toMap
          // Use WandB.log to send one dict; avoid per-metric cross-language calls
          WandB.log(byC.map { case (c, v) => s"toy_rew_$c" -> v })
          logger.logScalar("toy_eps_mean", world.nodes.map(_.agent.epsilonNow).sum / world.nodes.size, step)
          WandB.flush(step.toLong)
        }

        t += 1; step += 1
      }
    }

    val ds = spark.createDataset(rows.toSeq)
    val df = ds.toDF()
    df.groupBy("epoch", "country")
      .agg(
        avg($"reward").as("avg_reward"),
        last($"s").as("final_s"),
        last($"i").as("final_i"),
        last($"d").as("final_d"),
        last($"eps").as("epsilon")
      )
  }
}
