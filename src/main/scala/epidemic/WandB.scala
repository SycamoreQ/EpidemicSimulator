package epidemic

import me.shadaj.scalapy.py
import me.shadaj.scalapy.py.{Any => PyAny}
import me.shadaj.scalapy.py.Dynamic

object WandB extends Logger {
  private val wandb = py.module("wandb")
  private var run: Option[py.Dynamic] = None
  private var buf: py.Dynamic = Dynamic.global.dict()

  private def toPy(x: Any): py.Any = x match {
    case v: py.Any => v
    case v: py.Dynamic => v.asInstanceOf[py.Any]
    case None => Dynamic.global.None.asInstanceOf[py.Any]
    case Some(v) => toPy(v)
    case d: Double => PyAny.from(d)
    case f: Float => PyAny.from(f.toDouble)
    case i: Int => PyAny.from(i)
    case l: Long => PyAny.from(l.toDouble)
    case b: Boolean => PyAny.from(b)
    case s: String => PyAny.from(s)
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
    try {
      val cfg = Dynamic.global.dict()
      config.foreach { case (k, v) => cfg.bracketUpdate(k, toPy(v)) }

      // Enhanced initialization with more settings
      run = Some(wandb.init(
        entity = entity,
        project = project,
        name = name,
        config = cfg,
        mode = "online",  // Force online mode
        save_code = true,
        resume = "allow"
      ))

      println(s"WANDB initialized successfully: ${name}")

      // Test logging immediately
      log(Map("initialization_test" -> 1.0, "timestamp" -> System.currentTimeMillis().toDouble))
      flush(0L)

    } catch {
      case e: Exception =>
        println(s"WANDB initialization failed: ${e.getMessage}")
        e.printStackTrace()
    }
  }

  override def logScalar(tag: String, value: Double, tick: Int): Unit = {
    try {
      buf.bracketUpdate(tag, PyAny.from(value))
      println(f"Logged scalar: $tag = $value%.4f")
    } catch {
      case e: Exception =>
        println(s"Error logging scalar $tag: ${e.getMessage}")
    }
  }

  override def logAny(tag: String, value: py.Dynamic, tick: Int): Unit = {
    try {
      buf.bracketUpdate(tag, value)
    } catch {
      case e: Exception =>
        println(s"Error logging any $tag: ${e.getMessage}")
    }
  }

  override def log(kv: Map[String, Double]): Unit = {
    try {
      kv.foreach { case (k, v) =>
        buf.bracketUpdate(k, PyAny.from(v))
      }
      if (kv.nonEmpty) {
        println(s"Logged ${kv.size} metrics: ${kv.keys.take(3).mkString(", ")}...")
      }
    } catch {
      case e: Exception =>
        println(s"Error in log: ${e.getMessage}")
        e.printStackTrace()
    }
  }

  def flush(step: Long): Unit = {
    try {
      run match {
        case Some(r) =>
          val pyStep = PyAny.from(step)
          r.log(buf, step = pyStep)
          println(s"Flushed to WANDB at step $step")
          buf = Dynamic.global.dict()
        case None =>
          println("WARNING: WANDB not initialized, cannot flush")
      }
    } catch {
      case e: Exception =>
        println(s"Error flushing to WANDB: ${e.getMessage}")
        e.printStackTrace()
    }
  }

  def summary(kv: Map[String, Any]): Unit = {
    try {
      run.foreach { r =>
        val s = r.selectDynamic("summary")
        kv.foreach { case (k, v) => s.bracketUpdate(k, toPy(v)) }
      }
    } catch {
      case e: Exception =>
        println(s"Error in summary: ${e.getMessage}")
    }
  }

  def finish(): Unit = {
    try {
      run.foreach { r =>
        r.finish()
        println("WANDB run finished successfully")
      }
    } catch {
      case e: Exception =>
        println(s"Error finishing WANDB: ${e.getMessage}")
    }
  }
}
