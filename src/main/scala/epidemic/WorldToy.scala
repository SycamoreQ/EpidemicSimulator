package epidemic

import scala.util.Random
import scala.collection.parallel.CollectionConverters._
import epidemic.Validation.*
import epidemic.{Action, State, EpidemicEnv, DDQNAgent}
// Shared integer allocator exposed from EpidemicEnv companion
import epidemic.EpidemicEnv.allocateInt
import Geo.*

final class WorldToy(
                      hp: HyperParams,
                      base: State,
                      seedIdx: Int = 0,
                      rng: Random = new Random(7),
                      conn: Array[Array[Double]] = ToyConn.W,
                      makeEnv: () => EpidemicEnv = () => new EpidemicEnv(),
                      makeEnvFor: Option[Country => EpidemicEnv] = None,
                      validate: Boolean = false,
                      parallelism: Int = Runtime.getRuntime.availableProcessors()
                    ) {

  private var gStep: Long = 0L
  private val learnEvery = hp.learnEvery
  private val worldPop0: Long = ToyConn.C.map(_.pop.toLong).sum

  private val initStates: Vector[State] = ToyConn.C.zipWithIndex.map { case (cfg, i) =>
    val hospCap = (base.hospCap * (cfg.pop.toDouble / 1_000_000.0)).max(1.0)
    if (i == seedIdx) base.copy(i = (0.001 * cfg.pop).toInt, s = cfg.pop - (0.001 * cfg.pop).toInt, hospCap = hospCap)
    else base.copy(i = 0, s = cfg.pop, hospCap = hospCap)
  }.toVector

  final case class Node(
                         idx: Int,
                         name: String,
                         pop: Int,
                         env: EpidemicEnv,
                         agent: DDQNAgent,
                         var s: State,
                         var lastA: Int = 0
                       )

  val nodes: Vector[Node] = ToyConn.C.zipWithIndex.map { case (cfg, i) =>
    val env = makeEnvFor.map(f => f(cfg)).getOrElse(makeEnv())
    val agent = new DDQNAgent(hp, stateSize = 7, actionSize = Action.values.length)
    val s0 = initStates(i)
    if (validate) assertState(s0, Invariants(cfg.pop, base.tMax))
    Node(i, cfg.name, cfg.pop, env, agent, s0, 0)
  }.toVector

  private lazy val parNodes = {
    val p = nodes.par
    p.tasksupport = new scala.collection.parallel.ForkJoinTaskSupport(
      new java.util.concurrent.ForkJoinPool(parallelism)
    )
    p
  }

  def snapshotStates(): Vector[State] = nodes.map(_.s)

  def restoreStates(ss: Vector[State]): Unit = nodes.zip(ss).foreach { case (n, s) => n.s = s }

  def resetToInitial(): Unit = nodes.indices.foreach(i => nodes(i).s = initStates(i))


  private inline def sumState(st: State): Long =
    math.round(st.s + st.i + st.r + st.d + st.v).toLong

  // Training step: uses epsilon-greedy, writes replay, and learns on cadence
  def stepOne(tMax: Int): Vector[(String, State, Double)] = {
    val local = Array.ofDim[(State, Double, Boolean)](nodes.size)
    val work = if (parallelism > 1) parNodes else nodes

    work.map { n =>
      val a = n.agent.act(n.s) // epsilon-greedy with schedule
      val tb = sumState(n.s)
      val (s2, r, done) = n.env.step(n.s, Action.fromId(a))
      n.lastA = a
      if (validate) {
        val ta = sumState(s2)
        require(ta == tb, s"mass not conserved (local): $ta != $tb for ${n.name} at t=${n.s.t}")
      }
      n.s -> ((s2, r, done, a))
    }.toVector.zipWithIndex.foreach { case ((_, tup), i) =>
      val (s2, r, done, a) = tup
      nodes(i).agent.observe(Transition(nodes(i).s, a, r, s2, done))
      local(i) = (s2, r, done)
    }

    val totalAfterLocal: Long =
      if (validate) local.view.map(_._1).map(sumState).sum else 0L

    // Mobility with integer-conserving exports along conn weights
    val movedS = Array.fill(nodes.size)(0)
    val movedI = Array.fill(nodes.size)(0)
    var i = 0
    while (i < nodes.size) {
      val s2 = local(i)._1
      val Si = math.max(0, math.round(s2.s).toInt)
      val Ii = math.max(0, math.round(s2.i).toInt)
      val wRow = Array.tabulate(nodes.size)(j => if (j == i) 0.0 else conn(i)(j))
      val sumW = math.max(0.0, wRow.sum)
      val exportS = math.min(Si.toLong, math.floor(Si * sumW + 1e-9).toLong).toInt
      val exportI = math.min(Ii.toLong, math.floor(Ii * sumW + 1e-9).toLong).toInt
      val allocS = allocateInt(exportS, wRow)
      val allocI = allocateInt(exportI, wRow)
      var j = 0
      while (j < nodes.size) {
        if (j != i) { movedS(j) += allocS(j); movedI(j) += allocI(j) }
        j += 1
      }
      movedS(i) -= exportS; movedI(i) -= exportI
      i += 1
    }

    val out = Vector.newBuilder[(String, State, Double)]
    i = 0
    while (i < nodes.size) {
      val (s2, r, _) = local(i)
      val s3 = s2.copy(
        s = math.max(0, s2.s + movedS(i)),
        i = math.max(0, s2.i + movedI(i)),
        t = math.min(s2.t + 1, tMax)
      )
      nodes(i).s = s3
      out += ((nodes(i).name, s3, r))
      i += 1
    }

    if (validate) {
      val totalAfterMove: Long = nodes.view.map(_.s).map(sumState).sum
      require(totalAfterMove == totalAfterLocal, s"global mass changed during mobility: $totalAfterMove != $totalAfterLocal")
    }

    gStep += 1
    if (gStep % learnEvery == 0) {
      val bs = hp.batchSize
      // Warm-up: learn only after some replay is filled
      nodes.foreach { n =>
        if (n.agent.replaySize >= bs * 4) n.agent.learn(bs, 1)
      }
    }
    out.result()
  }

  // Greedy evaluation step: no replay writes and no learning
  def stepOneGreedy(tMax: Int): Vector[(String, State, Double)] = {
    val local = Array.ofDim[(State, Double, Boolean)](nodes.size)
    val work = if (parallelism > 1) parNodes else nodes

    work.map { n =>
      val aGreedy = n.agent.actGreedy(n.s)
      val tb = sumState(n.s)
      val (s2, r, done) = n.env.step(n.s, Action.fromId(aGreedy))
      n.lastA = aGreedy
      if (validate) {
        val ta = sumState(s2)
        require(ta == tb, s"mass not conserved (local): $ta != $tb for ${n.name} at t=${n.s.t}")
      }
      n.s -> ((s2, r, done))
    }.toVector.zipWithIndex.foreach { case ((_, tup), i) =>
      val (s2, r, done) = tup
      local(i) = (s2, r, done) // no observe during eval
    }

    val totalAfterLocal: Long =
      if (validate) local.view.map(_._1).map(sumState).sum else 0L

    val movedS = Array.fill(nodes.size)(0)
    val movedI = Array.fill(nodes.size)(0)
    var i = 0
    while (i < nodes.size) {
      val s2 = local(i)._1
      val Si = math.max(0, math.round(s2.s).toInt)
      val Ii = math.max(0, math.round(s2.i).toInt)
      val wRow = Array.tabulate(nodes.size)(j => if (j == i) 0.0 else conn(i)(j))
      val sumW = math.max(0.0, wRow.sum)
      val exportS = math.min(Si.toLong, math.floor(Si * sumW + 1e-9).toLong).toInt
      val exportI = math.min(Ii.toLong, math.floor(Ii * sumW + 1e-9).toLong).toInt
      val allocS = allocateInt(exportS, wRow)
      val allocI = allocateInt(exportI, wRow)
      var j = 0
      while (j < nodes.size) {
        if (j != i) { movedS(j) += allocS(j); movedI(j) += allocI(j) }
        j += 1
      }
      movedS(i) -= exportS; movedI(i) -= exportI
      i += 1
    }

    val out = Vector.newBuilder[(String, State, Double)]
    i = 0
    while (i < nodes.size) {
      val (s2, r, _) = local(i)
      val s3 = s2.copy(
        s = math.max(0, s2.s + movedS(i)),
        i = math.max(0, s2.i + movedI(i)),
        t = math.min(s2.t + 1, tMax)
      )
      nodes(i).s = s3
      out += ((nodes(i).name, s3, r))
      i += 1
    }

    if (validate) {
      val totalAfterMove: Long = nodes.view.map(_.s).map(sumState).sum
      require(totalAfterMove == totalAfterLocal, s"global mass changed during mobility: $totalAfterMove != $totalAfterLocal")
    }
    out.result()
  }
}
