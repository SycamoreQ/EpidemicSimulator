package epidemic

import org.apache.spark.sql.SparkSession
import Geo.Country

@main def main(): Unit = {
  val spark = SparkSession.builder()
    .appName("Epidemic-DDQN-Scala3-ToyMeta")
    .master("local[*]")
    .config("spark.sql.shuffle.partitions","8")
    .config("spark.serializer","org.apache.spark.serializer.KryoSerializer")
    .config("spark.network.timeout","600s")
    .config("spark.dynamicAllocation.enabled","false")
    .config("spark.ui.showConsoleProgress","false")
    .getOrCreate()
  spark.sparkContext.setLogLevel("WARN")

  val hp  = HyperParams()
  val cfg = TrainConfig()

  val base = State(
    s = 1_000_000, i = 0, r = 0, d = 0, v = 0,
    hospCap = 10_000, t = 0, tMax = cfg.stepsPerEpoch, seed = 1
  )

  // Optional: quick calibration (homogeneous) to get a baseline beta
  import Calibrate.*
  val betaGrid     = (24 to 27).map(_ / 100.0)
  val travelScales = (5 to 9).map(_ / 10.0)
  val targets = Targets(
    tArrive = Map("Japan" -> 40.0, "India" -> 55.0, "Italy" -> 60.0, "Germany" -> 58.0, "USA" -> 52.0, "China" -> 0.0),
    peakI   = Map("Japan" -> 10000.0, "India" -> 18000.0, "Italy" -> 8000.0, "Germany" -> 9000.0, "USA" -> 12000.0, "China" -> 35000.0)
  )
  val cfgShort = cfg.copy(stepsPerEpoch = 300, epochs = 1)
  val (_, (bestLoss, (bestBeta, _))) =
    gridSurface(hp, cfgShort, base.copy(tMax = 300), seedIdx = 0, targets, betaGrid, travelScales, validate = true)
  println(f"[surface] best loss=$bestLoss%.4f beta=$bestBeta%.3f")

  // Per-country env factory (Geo.Country)
  val makeEnvFor: Country => EpidemicEnv = c =>
    new EpidemicEnv(
      baseBeta   = bestBeta * c.betaMul,
      baseGamma  = 0.12,
      baseIFR    = 0.008 * c.ifrMul,
      noise      = c.noise,
      rewardClip = hp.rewardClip
    )

  // Connectivity
  val W2 = ToyConn.W2   

  // Init W&B
  WandB.init(
    entity  = sys.env.getOrElse("WANDB_ENTITY","kaushik-80405-amrita-vishwa-vidyapeetham"),
    project = sys.env.getOrElse("WANDB_PROJECT","epidemic-ddqn"),
    name    = s"toy-${System.currentTimeMillis()}",
    config  = Map(
      "gamma" -> hp.gamma, "lr" -> hp.lr, "epsilonStart" -> hp.epsilonStart,
      "epsilonEnd" -> hp.epsilonEnd, "epsilonDecaySteps" -> hp.epsilonDecaySteps,
      "targetUpdateEvery" -> hp.targetUpdateEvery, "batchSize" -> hp.batchSize,
      "replayCapacity" -> hp.replayCapacity, "hidden" -> hp.hidden,
      "learnEvery" -> hp.learnEvery, "stepsPerEpoch" -> cfg.stepsPerEpoch, "epochs" -> cfg.epochs
    )
  )

  val world = new WorldToy(
    hp         = hp,
    base       = base,
    seedIdx    = 0,
    rng        = new scala.util.Random(7),
    conn       = W2,
    makeEnv    = () => new EpidemicEnv(baseBeta = bestBeta, baseGamma = 0.12, baseIFR = 0.008, noise = 0.05, rewardClip = hp.rewardClip),
    makeEnvFor = Some(makeEnvFor),     // pass heterogeneous factory (Geo.Country)
    validate   = false
  )

  val trainer = new MultiTrainerToy(spark, hp, cfg, world, WandB)
  val df = trainer.run()
  df.write.mode("overwrite").parquet("toy_results.parquet")
  df.coalesce(1).write.mode("overwrite").option("header","true").csv("toy_results_csv")
  println("Saved: toy_results.parquet and toy_results_csv/")

  WandB.finish()
  spark.stop()
}
