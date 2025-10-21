// src/main/scala/epidemic/App.scala
package epidemic

import org.apache.spark.sql.SparkSession

@main def main(): Unit =
  val spark = SparkSession.builder()
    .appName("Epidemic-DDQN-Scala3")
    .master("local[*]")
    .config("spark.sql.shuffle.partitions", "8")
    .config("spark.serializer", "org.apache.spark.serializer.KryoSerializer")
    // Heartbeats and network timeouts (driver/executor)
    .config("spark.network.timeout", "600s")
    // driver waits this long for heartbeats [web:308]
    .config("spark.executor.heartbeatInterval", "30s") // executor sends heartbeats every 30s (<< network.timeout) [web:268]
    .config("spark.executor.heartbeat.maxRetries", "120") // tolerate many missed heartbeats (Spark 4 style; ignored if unsupported) [web:319]
    // Keep single local driver/executor predictable
    .config("spark.dynamicAllocation.enabled", "false")
    // avoid endpoint churn in local mode [web:307]
    .config("spark.ui.retainedJobs", "100")
    .getOrCreate()
  spark.sparkContext.setLogLevel("WARN")

  val hp  = HyperParams()
  val cfg = TrainConfig()
  val wb  = new WandB()

  val trainer = new Trainer(spark, hp, cfg, wb)
  val df = trainer.run()
  df.write.mode("overwrite").parquet("results.parquet")
  df.coalesce(1).write.mode("overwrite").option("header","true").csv("results_csv")

  println("Saved: results.parquet and results_csv/")
  spark.stop()
