package epidemic

import org.apache.spark.sql.{DataFrame, SparkSession}
import org.apache.spark.sql.functions._
import spark.implicits.given
import io.github.pashashiz.spark_encoders.TypedEncoder.*
import io.github.pashashiz.spark_encoders.TypedEncoder.given

final case class CountryRow(
                             epoch: Int, step: Int, country: String,
                             reward: Double, s: Double, i: Double, r: Double, d: Double, v: Double, eps: Double
                           ) derives TypedEncoder

final class MultiTrainer(
                          spark: SparkSession,
                          hp: HyperParams,
                          cfg: TrainConfig,
                          world: World,
                          logger: Logger
                        ) {
  def run(): DataFrame = {
    val rows = scala.collection.mutable.ArrayBuffer[CountryRow]()
    var globalStep = 0

    for (ep <- 1 to cfg.epochs) {
      var t = 0
      while (t < cfg.stepsPerEpoch) {
        val batch = world.stepOne(t, cfg.stepsPerEpoch)
        batch.foreach { case (name, s2, r) =>
          val idx = world.nodes.indexWhere(_.cfg.name == name)
          val eps = world.nodes(idx).agent.epsilonNow
          rows += CountryRow(ep, globalStep, name, r, s2.s, s2.i, s2.r, s2.d, s2.v, eps)
          logger.logScalar(s"rew_$name", r, globalStep)
        }
        logger.logScalar("eps_mean", world.nodes.map(_.agent.epsilonNow).sum / world.nodes.size, globalStep)
        WandB.flush(globalStep.toLong) // optional streaming
        t += 1; globalStep += 1
      }
    }

    val ds = spark.createDataset(rows.toSeq)
    val df = ds.toDF()
    df.groupBy($"epoch", $"country")
      .agg(
        avg($"reward").as("avg_reward"),
        last($"s").as("final_s"),
        last($"i").as("final_i"),
        last($"d").as("final_d"),
        last($"eps").as("epsilon")
      )
  }
}
