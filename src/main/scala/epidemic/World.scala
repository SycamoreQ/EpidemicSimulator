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

final class WorldToy(hp: HyperParams, base: State, seedIdx: Int = 0, rng: Random = new Random(7)) {
  import ToyConn._

  // NEW: global step counter + cadence
  private var gStep: Long = 0L
  private val learnEvery = 4

  val nodes: Vector[Node] = C.zipWithIndex.map { case (cfg, i) =>
    val env = new EpidemicEnv(rewardClip = hp.rewardClip)
    val agent = new DDQNAgent(hp, stateSize = 7, actionSize = Action.values.length)
    val s0 =
      if (i == seedIdx) base.copy(i = (0.001 * cfg.pop).toInt, s = cfg.pop - (0.001 * cfg.pop).toInt)
      else base.copy(i = 0, s = cfg.pop)
    Node(i, cfg.name, cfg.pop, env, agent, s0)
  }.toVector

  // one synchronous step for all countries + airport moves
  def stepOne(tMax: Int): Vector[(String, State, Double)] = {
    val local = Array.ofDim[(State, Double, Boolean)](nodes.size)

    var i = 0
    while (i < nodes.size) {
      val n = nodes(i)
      val a = n.agent.act(n.s)
      val (s2, r, done) = n.env.step(n.s, Action.fromId(a))
      n.agent.observe(Transition(n.s, a, r, s2, done))
      // NEW: learn throttling
      if (gStep % learnEvery == 0) {
        n.agent.learn(hp.batchSize, gradAccumSteps = 4)
      }
      local(i) = (s2, r, done)
      i += 1
    }

    // Apply simple exports of S and I according to W
    val movedS = Array.fill(nodes.size)(0.0)
    val movedI = Array.fill(nodes.size)(0.0)

    i = 0
    while (i < nodes.size) {
      val s2 = local(i)._1
      val Si = s2.s.toDouble
      val Ii = s2.i.toDouble
      var j = 0
      while (j < nodes.size) {
        val rate = W(i)(j)
        if (rate > 0) {
          movedS(j) += rate * Si
          movedI(j) += rate * Ii
          movedS(i) -= rate * Si
          movedI(i) -= rate * Ii
        }
        j += 1
      }
      i += 1
    }

    val out = Vector.newBuilder[(String, State, Double)]
    i = 0
    while (i < nodes.size) {
      val (s2, r, done) = local(i)
      val sMoved = math.round(movedS(i)).toInt
      val iMoved = math.round(movedI(i)).toInt
      val s3 = s2.copy(
        s = math.max(0, s2.s + sMoved),
        i = math.max(0, s2.i + iMoved),
        t = math.min(s2.t + 1, tMax)
      )
      nodes(i).s = s3
      out += ((nodes(i).name, s3, r))
      i += 1
    }

    // NEW: advance global step once per world tick
    gStep += 1
    out.result()
  }
}
