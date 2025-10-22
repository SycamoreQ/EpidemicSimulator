package epidemic

import me.shadaj.scalapy.py

trait Logger {
  def logScalar(tag: String, value: Double, tick: Int): Unit

  def logAny(tag: String, value: py.Dynamic, tick: Int): Unit

  def log(kv: Map[String, Double]): Unit = {
    // tick is not used by W&B; pass 0 or a monotonic counter if needed
    kv.foreach { case (k, v) => logScalar(k, v, 0) }
  }
}
