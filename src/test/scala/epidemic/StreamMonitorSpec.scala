package epidemic
import org.apache.spark.sql.{SparkSession, Row}
import org.apache.spark.sql.execution.streaming.MemoryStream
import org.apache.spark.sql.functions._
import munit.FunSuite

class StreamMonitorSpec extends FunSuite {
  val spark = SparkSession.builder().master("local[2]").appName("test").getOrCreate()
  import spark.implicits._

  test("aggregation correctness") {
    implicit val sql = spark.sqlContext
    val ms = new MemoryStream[(String, Int, Double, Int, Double, Double)](1, spark.sqlContext)
    val sdf = ms.toDS().toDF("country","t","i","arrival","peakI","loss")
    var out: Array[Row] = Array.empty
    val q = sdf.writeStream.outputMode("append").foreachBatch { (batch, _) =>
      val summary = batch.groupBy("country").agg(
        max(col("i")).as("current_i"),
        max(col("peakI")).as("rolling_peak"),
        min(col("arrival")).as("min_arrival"),
        avg(col("loss")).as("avg_loss"),
        max(col("t")).as("latest_step")
      )
      out = summary.orderBy("country").collect()
    }.start()
    ms.addData(("USA",1,0.1,10,0.2,1.0), ("USA",2,0.3,9,0.4,2.0), ("Japan",1,0.2,8,0.3,3.0))
    q.processAllAvailable(); q.stop()
    val usa = out.find(_.getAs[String]("country")=="USA").get
    assertEquals(usa.getAs[Double]("current_i"), 0.3, 1e-9)
    assertEquals(usa.getAs[Int]("min_arrival"), 9)
  }
}

