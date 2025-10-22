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
  val cfg = TrainConfig(epochs = 25, stepsPerEpoch = 800, evalEvery = 5)

  val ts = bestTravel
  val Wscaled = Scenarios.scaleRow(ToyConn.W, row = 0, factor = ts)
  

  val base = State(
    s = 1_000_000, i = 0, r = 0, d = 0, v = 0,
    hospCap = 10_000, t = 0, tMax = cfg.stepsPerEpoch, seed = 1
  )

  // Expanded grids
  val betaGrid       = (24 to 27).map(_ / 100.0)   
  val travelScales   = (5 to 9).map(_ / 10.0)     

  import Calibrate.*

  // Single targets definition
  val targets = Targets(
    tArrive = Map(
      "Japan" -> 40.0, "India" -> 55.0, "Italy" -> 60.0,
      "Germany" -> 58.0, "USA" -> 52.0, "China" -> 0.0
    ),
    peakI   = Map(
      "Japan" -> 10000.0, "India" -> 18000.0, "Italy" -> 8000.0,
      "Germany" -> 9000.0, "USA" -> 12000.0, "China" -> 35000.0
    )
  )

  // Surface (fast)
  val cfgShort = cfg.copy(stepsPerEpoch = 300, epochs = 1)
  val (rows, (bestLoss, (bestBeta, bestTravel))) =
    gridSurface(hp, cfgShort, base.copy(tMax = 300), seedIdx = 0, targets,
      betaGrid, travelScales, validate = true)

  println(f"[surface] best loss=$bestLoss%.4f beta=$bestBeta%.3f travel=$bestTravel%.2f")

  // Write CSV for the heatmap
  val out = new java.io.PrintWriter("calib_surface.csv")
  try {
    out.println("beta,travel,loss")
    rows.foreach { case (b, t, l) => out.println(f"$b%.3f,$t%.2f,$l%.6f") }
  } finally out.close()

  // Optional W&B session for training
  WandB.init(
    entity = sys.env.getOrElse("WANDB_ENTITY","kaushik-80405-amrita-vishwa-vidyapeetham"),
    project = sys.env.getOrElse("WANDB_PROJECT","epidemic-ddqn"),
    name = s"toy-${System.currentTimeMillis()}",
    config = Map("countries" -> ToyConn.C.size, "best_beta" -> bestBeta, "best_travel" -> bestTravel)
  )

  // Train (you can pass bestBeta/bestTravel via makeEnv/conn if desired)
  val world = new WorldToy(
    hp, base, seedIdx = 0,
    conn = Wscaled,
    makeEnv = () => new EpidemicEnv(baseBeta = bestBeta, rewardClip = hp.rewardClip)
  )
  val trainer = new MultiTrainerToy(spark, hp, cfg, world, WandB)
  val df = trainer.run()

  df.write.mode("overwrite").parquet("toy_results.parquet")
  df.coalesce(1).write.mode("overwrite").option("header","true").csv("toy_results_csv")
  println("Saved: toy_results.parquet and toy_results_csv/")

  WandB.finish()
  spark.stop()
