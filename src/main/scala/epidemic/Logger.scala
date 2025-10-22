package epidemic

import me.shadaj.scalapy.py

trait Logger {
  def logScalar(tag: String, value: Double, tick: Int): Unit

  def logAny(tag: String, value: py.Dynamic, tick: Int): Unit

  def log(kv: Map[String, Double]): Unit = {
    kv.foreach { case (k, v) => logScalar(k, v, 0) }
  }
}
