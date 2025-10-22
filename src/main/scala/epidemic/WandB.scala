package epidemic

import me.shadaj.scalapy.py
import me.shadaj.scalapy.py.{Any => PyAny}
import me.shadaj.scalapy.py.Dynamic

object WandB extends Logger {
  private val wandb = py.module("wandb")
  private var run: Option[py.Dynamic] = None
  private var buf: py.Dynamic = Dynamic.global.dict()

  // Convert Scala -> Python safely; no AnyRef fallback
  private def toPy(x: Any): py.Any = x match {
    case v: py.Any      => v
    case v: py.Dynamic  => v.asInstanceOf[py.Any]
    case None           => Dynamic.global.None.asInstanceOf[py.Any]
    case Some(v)        => toPy(v)
    case d: Double      => PyAny.from(d)
    case f: Float       => PyAny.from(f.toDouble)
    case i: Int         => PyAny.from(i)
    case l: Long        => PyAny.from(l.toDouble)   // W&B treats numbers as floats
    case b: Boolean     => PyAny.from(b)
    case s: String      => PyAny.from(s)
    case m: Map[?, ?] =>
      val dct = Dynamic.global.dict()
      m.asInstanceOf[Map[String, Any]].foreach { case (k, v) => dct.bracketUpdate(k, toPy(v)) }
      dct.asInstanceOf[py.Any]
    case seq: Seq[?] =>
      val lst = Dynamic.global.list()
      seq.asInstanceOf[Seq[Any]].foreach(v => lst.append(toPy(v)))
      lst.asInstanceOf[py.Any]
    case other =>
      // last-resort: stringify or throw; avoid Any.from(Object)
      PyAny.from(other.toString)
  }

  def init(entity: String, project: String, name: String, config: Map[String, Any] = Map.empty): Unit = {
    val cfg = Dynamic.global.dict()
    config.foreach { case (k, v) => cfg.bracketUpdate(k, toPy(v)) }
    run = Some(wandb.init(entity = entity, project = project, name = name, config = cfg)) // W&B init API [web:323]
  }

  override def logScalar(tag: String, value: Double, tick: Int): Unit = {
    buf.bracketUpdate(tag, PyAny.from(value))
  }

  override def logAny(tag: String, value: py.Dynamic, tick: Int): Unit = {
    buf.bracketUpdate(tag, value)
  }

  def flush(step: Long): Unit = {
    run.foreach { r =>
      buf.bracketUpdate("step", PyAny.from(step.toDouble))
      r.log(buf, step = step.toDouble) // explicit step is recommended for stable x-axis [web:321]
      buf = Dynamic.global.dict()       // clear buffer after sending [web:347]
    }
  }

  def summary(kv: Map[String, Any]): Unit = {
    run.foreach { r =>
      val s = r.selectDynamic("summary")
      kv.foreach { case (k, v) => s.bracketUpdate(k, toPy(v)) }
    }
  }

  def finish(): Unit = run.foreach(_.finish()) // close the run [web:345]
}
