// src/main/scala/epidemic/WorldToy.scala
package epidemic

import scala.util.Random

final case class Node(
                       idx: Int,
                       name: String,
                       pop: Int,
                       env: EpidemicEnv,
                       agent: DDQNAgent,
                       var s: State
                     )

final class WorldToy(
                      hp: HyperParams,
                      base: State,
                      seedIdx: Int = 0,
                      rng: Random = new Random(7),
                      conn: Array[Array[Double]] = ToyConn.W,                 // pluggable matrix
                      makeEnv: () => EpidemicEnv = () => new EpidemicEnv(),  // pluggable env
                      validate: Boolean = false                               // enable invariants
                    ) {
  import Validation.*

  private var gStep: Long = 0L
  private val learnEvery = 4

  // Initial global population for drift checks
  private val worldPop0: Long = ToyConn.C.map(_.pop.toLong).sum

  val nodes: Vector[Node] = ToyConn.C.zipWithIndex.map { case (cfg, i) =>
    val env = makeEnv()
    val agent = new DDQNAgent(hp, stateSize = 7, actionSize = Action.values.length)
    val s0 =
      if (i == seedIdx) base.copy(i = (0.001 * cfg.pop).toInt, s = cfg.pop - (0.001 * cfg.pop).toInt)
      else base.copy(i = 0, s = cfg.pop)
    if (validate) assertState(s0, Invariants(cfg.pop, base.tMax))
    Node(i, cfg.name, cfg.pop, env, agent, s0)
  }.toVector

  // Largest-remainder proportional allocator for exact integer totals
  private def allocateInt(total: Int, weights: Array[Double]): Array[Int] = {
    val sumW = weights.sum
    if (total <= 0 || sumW <= 0.0) return Array.fill(weights.length)(0)
    val raw  = weights.map(w => (w / sumW) * total)
    val base = raw.map(math.floor(_).toInt)
    var rem  = total - base.sum
    val frac = raw.zipWithIndex.map { case (x, j) => (x - math.floor(x), j) }
    scala.util.Sorting.stableSort(frac)(Ordering.by[(Double, Int), Double](_._1).reverse)
    var k = 0
    while (rem > 0 && k < frac.length) { base(frac(k)._2) += 1; rem -= 1; k += 1 }
    base
  }

  private inline def sumState(st: State): Long =
    math.round(st.s + st.i + st.r + st.d + st.v).toLong

  def stepOne(tMax: Int): Vector[(String, State, Double)] = {
    val local = Array.ofDim[(State, Double, Boolean)](nodes.size)

    // Local (within-country) step with per-country invariants
    var i = 0
    while (i < nodes.size) {
      val n = nodes(i)
      val a = n.agent.act(n.s)
      val (s2, r, done) = n.env.step(n.s, Action.fromId(a))
      if (validate) assertState(s2, Invariants(n.pop, tMax)) // per-country check before mobility [web:534]
      n.agent.observe(Transition(n.s, a, r, s2, done))
      if (gStep % learnEvery == 0) n.agent.learn(hp.batchSize, gradAccumSteps = 4)
      local(i) = (s2, r, done)
      i += 1
    }

    // Global total after local
    val totalAfterLocal: Long =
      if (validate) local.view.map(_._1).map(sumState).sum else 0L

    // Integer-conserving mobility
    val movedS = Array.fill(nodes.size)(0)
    val movedI = Array.fill(nodes.size)(0)

    i = 0
    while (i < nodes.size) {
      val s2 = local(i)._1
      val Si = math.max(0, math.round(s2.s).toInt)
      val Ii = math.max(0, math.round(s2.i).toInt)

      val wRow = Array.tabulate(nodes.size)(j => if (j == i) 0.0 else conn(i)(j))
      val sumW = math.max(0.0, wRow.sum)

      // Avoid float underflow on tiny sums; +1e-9 guards borderline cases
      val exportS = math.min(Si.toLong, math.floor(Si * sumW + 1e-9).toLong).toInt
      val exportI = math.min(Ii.toLong, math.floor(Ii * sumW + 1e-9).toLong).toInt

      val allocS = allocateInt(exportS, wRow)
      val allocI = allocateInt(exportI, wRow)

      var j = 0
      while (j < nodes.size) {
        if (j != i) {
          movedS(j) += allocS(j)
          movedI(j) += allocI(j)
        }
        j += 1
      }
      movedS(i) -= exportS
      movedI(i) -= exportI
      i += 1
    }

    // Apply mobility deltas; do not assert per-country here (totals change by design)
    val out = Vector.newBuilder[(String, State, Double)]
    i = 0
    while (i < nodes.size) {
      val (s2, r, done) = local(i)
      val s3 = s2.copy(
        s = math.max(0, s2.s + movedS(i)),
        i = math.max(0, s2.i + movedI(i)),
        t = math.min(s2.t + 1, tMax)
      )
      nodes(i).s = s3
      out += ((nodes(i).name, s3, r))
      i += 1
    }

    // Global conservation check after mobility
    if (validate) {
      val totalAfterMove: Long = nodes.view.map(_.s).map(sumState).sum
      require(totalAfterMove == totalAfterLocal,
        s"global mass changed during mobility: $totalAfterMove != $totalAfterLocal")
      require(totalAfterMove == worldPop0,
        s"global mass drift vs initial: $totalAfterMove != $worldPop0")
    }

    gStep += 1
    out.result()
  }
}
