package epidemic

import scala.util.Random

import Geo._ 
import Mobility._

final case class CountryState(
                               idx: Int,
                               cfg: CountryConfig,
                               env: EpidemicEnv,
                               agent: DDQNAgent,
                               var s: State,
                               risk: Double
                             )

final class World(
                   hp: HyperParams,
                   countries: Vector[CountryConfig],
                   baseState: State,
                   dailyTravelRate: Double = 2e-3,
                   seedCountry: String = "China",
                   rng: Random = new Random(42)
                 ) {

  private val n = countries.length
  private val indexByName = countries.zipWithIndex.map{ case (c,i) => c.name -> i }.toMap
  private val dist = Array.tabulate(n, n){ (i,j) =>
    if (i==j) 0.0 else haversine(countries(i).lat, countries(i).lon, countries(j).lat, countries(j).lon)
  }
  private val rawW = Array.tabulate(n, n){ (i,j) =>
    if (i==j) 0.0 else gravityAir(countries(i), countries(j), dist(i)(j))
  }
  private val outbound = Array.tabulate(n){ i => normalizeOutbound(rawW(i), dailyTravelRate) }

  // Proximity/air risk scalar in [0,1] per country (normalize inbound)
  private val inboundSum = Array.tabulate(n)(j => (0 until n).map(i => rawW(i)(j)).sum)
  private val maxInbound = inboundSum.maxOption.getOrElse(1.0)
  private val riskVec = inboundSum.map(x => if (maxInbound <= 0) 0.0 else math.min(1.0, x / maxInbound))

  val nodes: Vector[CountryState] = {
    val seedIdx = indexByName.getOrElse(seedCountry, 0)
    countries.zipWithIndex.map { case (cfg, i) =>
      val env = new EpidemicEnv(rewardClip = hp.rewardClip, riskFactor = riskVec(i))
      val agent = new DDQNAgent(hp, stateSize = 7, actionSize = Action.values.length)
      val s0 =
        if (i == seedIdx) baseState.copy(i = (0.001 * cfg.pop).toInt, s = (cfg.pop - 0.001 * cfg.pop).toInt)
        else baseState.copy(i = 0, s = cfg.pop.toInt)
      CountryState(i, cfg, env, agent, s0, riskVec(i))
    }.toVector
  }

  def stepOne(tick: Int, tMax: Int): Vector[(String, State, Double)] = {
    // 1) Local steps
    val local = Array.ofDim[(State, Double, Boolean)](n)
    var i = 0
    while (i < n) {
      val cs = nodes(i)
      val a = cs.agent.act(cs.s)
      val (s2, r, done) = cs.env.step(cs.s, Action.fromId(a))
      cs.agent.observe(Transition(cs.s, a, r, s2, done))
      cs.agent.learn(hp.batchSize, gradAccumSteps = 4)
      local(i) = (s2, r, done)
      i += 1
    }

    // 2) Mobility coupling on S and I
    val movedS = Array.fill(n)(0.0); val movedI = Array.fill(n)(0.0)
    i = 0
    while (i < n) {
      val s2 = local(i)._1
      val Si = s2.s.toDouble; val Ii = s2.i.toDouble
      var j = 0
      while (j < n) {
        val rate = outbound(i)(j)
        if (rate > 0 && i != j) {
          movedS(j) += rate * Si; movedI(j) += rate * Ii
          movedS(i) -= rate * Si; movedI(i) -= rate * Ii
        }
        j += 1
      }
      i += 1
    }

    // 3) Apply moves and update
    i = 0
    val out = Vector.newBuilder[(String, State, Double)]
    while (i < n) {
      val (s2, r, done) = local(i)
      val sMoved = math.round(movedS(i)).toInt
      val iMoved = math.round(movedI(i)).toInt
      val s3 = s2.copy(
        s = math.max(0, s2.s + sMoved),
        i = math.max(0, s2.i + iMoved),
        t = math.min(s2.t + 1, tMax)
      )
      nodes(i).s = s3
      out += ((nodes(i).cfg.name, s3, r))
      i += 1
    }
    out.result()
  }
}
