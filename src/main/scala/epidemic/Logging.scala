// src/main/scala/epidemic/Logging.scala
package epidemic
import java.io.{FileWriter, BufferedWriter}
import java.time.Instant


final class WandB(project: String = "epidemic-ddqn") {
  private val enabled = sys.env.get("WANDB_API_KEY").exists(_.nonEmpty)
  private val csv = new BufferedWriter(new FileWriter("metrics.csv", true))

  def log(kv: Map[String, Double]): Unit = {
    val ts = Instant.now().toEpochMilli
    // Always write CSV for reliability
    val line = (Seq(ts.toDouble) ++ kv.toSeq.sortBy(_._1).map(_._2)).mkString(",")
    csv.write(line); csv.newLine(); csv.flush()

    if (!enabled) {
      // explicit notice when WANDB is not active
      if (kv.contains("epoch")) println(f"[wandb:disabled] epoch=${kv("epoch")}%.0f reward=${kv.getOrElse("reward",0.0)}%.3f ema_loss=${kv.getOrElse("ema_loss",0.0)}%.5f")
    } else {
      // Keep as console for now; switch to real HTTP client or official integration if available
      println(kv.map{ case (k,v) => s"$k=${"%.4f".format(v)}" }.mkString("[wandb] ", ", ", ""))
    }
  }

  def close(): Unit = csv.close()
}
