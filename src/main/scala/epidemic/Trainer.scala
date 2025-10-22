package epidemic


import io.github.pashashiz.spark_encoders.TypedEncoder.*   // API (derive, etc.)
import io.github.pashashiz.spark_encoders.TypedEncoder.given
import org.apache.spark.sql.{DataFrame, SparkSession}
import org.apache.spark.sql.functions.*

final case class TrainRow(
                           epoch: Int,
                           totalReward: Double,
                           avgLoss: Double,
                           epsilon: Double,
                           finalI: Double,
                           finalD: Double
                         )

final class Trainer(spark: SparkSession, hp: HyperParams, cfg: TrainConfig, logger: Logger):

  // Scala 3: bring Spark SQL implicits (toDS/toDF) into scope for this SparkSession

  import spark.implicits.given
  import org.apache.spark.sql.Encoder
  import io.github.pashashiz.spark_encoders.TypedEncoder.typedEncoderToEncoder


  private val env = new EpidemicEnv(rewardClip = hp.rewardClip)
  private val agent = new DDQNAgent(hp, stateSize = 7, actionSize = Action.values.length)

  private def initial(seed: Int, tMax: Int): State =
    State(
      s = 1_000_000,
      i = 2_000,
      r = 500,
      d = 50,
      v = 30_000,
      hospCap = 10_000,
      t = 0,
      tMax = tMax,
      seed = seed
    )

  def run(): DataFrame =
    val rows = scala.collection.mutable.ArrayBuffer[TrainRow]()
    var ep = 1
    while ep <= cfg.epochs do
      var s = env.reset(initial(seed = ep, tMax = cfg.stepsPerEpoch))
      var totalR = 0.0
      var avgLoss = 0.0
      var t = 0
      while t < cfg.stepsPerEpoch do
        val a = agent.act(s)
        val (s2, r, done) = env.step(s, Action.fromId(a))
        agent.observe(Transition(s, a, r, s2, done))
        avgLoss += agent.learn(hp.batchSize , gradAccumSteps = 4)
        totalR += r
        s = s2

        if done then
          // reset to keep fixed-horizon training stable
          s = env.reset(initial(seed = ep * 991 + t, tMax = cfg.stepsPerEpoch))

        if t % 100 == 0 then
          logger.log(
            Map(
              "step" -> (t + (ep - 1) * cfg.stepsPerEpoch).toDouble,
              "loss" -> avgLoss / math.max(1, t + 1),
              "eps"  -> agent.epsilonNow
            )
          )
        t += 1

      val row = TrainRow(
        epoch = ep,
        totalReward = totalR,
        avgLoss = avgLoss / math.max(1, cfg.stepsPerEpoch),
        epsilon = agent.epsilonNow,
        finalI = s.i,
        finalD = s.d
      )
      rows += row
      logger.log(
        Map(
          "epoch"   -> ep.toDouble,
          "reward"  -> row.totalReward,
          "avg_loss"-> row.avgLoss,
          "epsilon" -> row.epsilon
        )
      )

      if ep % cfg.evalEvery == 0 then evaluate(ep)
      ep += 1

    // Use Scala 3 encoders to build a typed Dataset, then toDF for aggregation
    val ds = spark.createDataset(rows.toSeq)
    val df = ds.toDF()

    df.groupBy($"epoch")
      .agg(
        avg($"totalReward").as("avg_reward"),
        avg($"avgLoss").as("avg_loss"),
        last($"epsilon").as("epsilon"),
        last($"finalI").as("finalI"),
        last($"finalD").as("finalD")
      )

  private def evaluate(epoch: Int): Unit =
    val scenarios = Seq(
      "Early"     -> State(990_000, 10_000, 0, 0, 5_000, 10_000, 0, 200, 0),
      "Peak"      -> State(800_000, 150_000, 30_000, 10_000, 50_000, 10_000, 0, 200, 0),
      "Control"   -> State(300_000, 1_000, 650_000, 10_000, 800_000, 10_000, 0, 200, 0),
      "Overflow"  -> State(600_000, 250_000, 100_000, 20_000, 100_000, 10_000, 0, 200, 0)
    )
    scenarios.foreach { case (name, s0) =>
      val q = agent.qValues(s0)
      val a = q.zipWithIndex.maxBy(_._1)._2
      logger.log(Map(s"eval_${name}_qmax" -> q.max, s"eval_${name}_action" -> a.toDouble))
    }
