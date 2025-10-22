package epidemic

import org.apache.spark.sql.SparkSession

@main def main(): Unit =
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
  val cfg = TrainConfig(epochs = 10, stepsPerEpoch = 400, evalEvery = 5)

  val base = State(
    s = 1_000_000, i = 0, r = 0, d = 0, v = 0,
    hospCap = 10_000, t = 0, tMax = cfg.stepsPerEpoch, seed = 1
  )

  // W&B optional
  WandB.init(
    entity = sys.env.getOrElse("WANDB_ENTITY","kaushik-80405-amrita-vishwa-vidyapeetham"),
    project = sys.env.getOrElse("WANDB_PROJECT","epidemic-ddqn"),
    name = s"toy-${System.currentTimeMillis()}",
    config = Map("countries" -> ToyConn.C.size)
  )



  // Seed at China (index 0)
  val world = new WorldToy(hp, base, seedIdx = 0)
  val trainer = new MultiTrainerToy(spark, hp, cfg, world, WandB)

  // inside main(), before training:
  val chinaIdx = 0
  val japanIdx = 1

  // 1) One-hop check
  val W1 = Scenarios.oneHop(ToyConn.W, from = chinaIdx, to = japanIdx)
  val world1 = new WorldToy(hp, base.copy(tMax = 300), seedIdx = chinaIdx, conn = W1, validate = true)
  (0 until 300).foreach(_ => world1.stepOne(300))

  // 2) Grid calibration (short)
  import Calibrate.*
  val targets = Targets(
    tArrive = Map("Japan" -> 40, "India" -> 55), // toy values you expect/observed
    peakI = Map("Japan" -> 900, "India" -> 800)
  )
  val (bestLoss, (bestBeta, bestTravel), bestMetrics) =
    gridSearch(hp, cfg.copy(stepsPerEpoch = 300, epochs = 1), base.copy(tMax = 300), seedIdx = chinaIdx,
      targets, betaGrid = Seq(0.25, 0.30, 0.35), travelScales = Seq(0.5, 1.0, 2.0), validate = true)
  println(s"Calib best loss=$bestLoss beta=$bestBeta travel=$bestTravel")
  bestMetrics.foreach { case (c, m) => println(s"$c -> arrival=${m.arrival}, peakI=${m.peakI}, finalI=${m.finalI}") }

  val df = trainer.run()

  df.write.mode("overwrite").parquet("toy_results.parquet")
  df.coalesce(1).write.mode("overwrite").option("header","true").csv("toy_results_csv")
  println("Saved: toy_results.parquet and toy_results_csv/")

  WandB.finish()
  spark.stop()
