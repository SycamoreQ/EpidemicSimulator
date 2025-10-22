package epidemic

import me.shadaj.scalapy.py
import me.shadaj.scalapy.py.{Any => PyAny}
import me.shadaj.scalapy.py.Dynamic

object WandB extends Logger {
  private val wandb = py.module("wandb")
  private var run: Option[py.Dynamic] = None
  private var buf: py.Dynamic = Dynamic.global.dict()
  
  private def toPy(x: Any): py.Any = x match {
    case v: py.Any      => v
    case v: py.Dynamic  => v.asInstanceOf[py.Any]
    case None           => Dynamic.global.None.asInstanceOf[py.Any]
    case Some(v)        => toPy(v)
    case d: Double      => PyAny.from(d)
    case f: Float       => PyAny.from(f.toDouble)
    case i: Int         => PyAny.from(i)
    case l: Long        => PyAny.from(l.toDouble)   
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

  override def log(kv: Map[String, Double]): Unit = {
    kv.foreach { case (k, v) => buf.bracketUpdate(k, PyAny.from(v)) }
  }

  def flush(step: Long): Unit = {
    run.foreach { r =>
      val pyStep = PyAny.from(step) 
      r.log(buf, step = pyStep) 
      buf = Dynamic.global.dict() 
    }
  }


  def summary(kv: Map[String, Any]): Unit = {
    run.foreach { r =>
      val s = r.selectDynamic("summary")
      kv.foreach { case (k, v) => s.bracketUpdate(k, toPy(v)) }
    }
  }

  def finish(): Unit = run.foreach(_.finish()) 
}
