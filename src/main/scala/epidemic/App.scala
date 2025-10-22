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
  val df = trainer.run()

  df.write.mode("overwrite").parquet("toy_results.parquet")
  df.coalesce(1).write.mode("overwrite").option("header","true").csv("toy_results_csv")
  println("Saved: toy_results.parquet and toy_results_csv/")

  WandB.finish()
  spark.stop()
