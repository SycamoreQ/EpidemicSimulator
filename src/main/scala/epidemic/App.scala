package epidemic

import org.apache.spark.sql.SparkSession
import Mobility._

@main def main(): Unit =
  val spark = SparkSession.builder()
    .appName("Epidemic-DDQN-Scala3-Meta")
    .master("local[*]")
    .config("spark.sql.shuffle.partitions", "8")
    .config("spark.serializer", "org.apache.spark.serializer.KryoSerializer")
    .config("spark.network.timeout", "600s")
    .config("spark.dynamicAllocation.enabled", "false")
    .getOrCreate()
  spark.sparkContext.setLogLevel("WARN")

  val hp  = HyperParams()
  val cfg = TrainConfig(epochs = 20, stepsPerEpoch = 500, evalEvery = 5)

  val countries = Vector(
    CountryConfig("China",   39.9, 116.4, 1_410_000_000, 50, 1.5),
    CountryConfig("India",   28.6,  77.2, 1_400_000_000, 40, 1.2),
    CountryConfig("Japan",   35.7, 139.7,   125_000_000, 20, 1.3),
    CountryConfig("Germany", 52.5,  13.4,    84_000_000, 20, 1.3),
    CountryConfig("Italy",   41.9,  12.5,    59_000_000, 15, 1.1),
    CountryConfig("France",  48.9,   2.3,    68_000_000, 20, 1.2),
    CountryConfig("USA",     38.9, -77.0,   333_000_000, 60, 1.6)
  )

  val base = State( s = 1_000_000, i = 0, r = 0, d = 0, v = 0, hospCap = 10_000, t = 0, tMax = cfg.stepsPerEpoch, seed = 1 )

  // W&B run (optional)
  WandB.init(
    entity = sys.env.getOrElse("WANDB_ENTITY","your_user"),
    project = sys.env.getOrElse("WANDB_PROJECT","epidemic-ddqn"),
    name = s"meta-${System.currentTimeMillis()}",
    config = Map("countries" -> countries.size, "dailyTravelRate" -> 2e-3)
  )

  val world  = new World(hp, countries, base, dailyTravelRate = 2e-3, seedCountry = "China")
  val trainer = new MultiTrainer(spark, hp, cfg, world, WandB)
  val df = trainer.run()

  df.write.mode("overwrite").parquet("meta_results.parquet")
  df.coalesce(1).write.mode("overwrite").option("header","true").csv("meta_results_csv")
  println("Saved: meta_results.parquet and meta_results_csv/")

  WandB.finish()
  spark.stop()
