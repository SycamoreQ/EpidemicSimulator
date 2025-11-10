package epidemic

import scala.util.Random
import Geo.CountryProfiles

enum Action(val id: Int, val label: String):
  case NoneA extends Action(0, "None")
  case Distancing extends Action(1, "Distancing")
  case TravelBan extends Action(2, "TravelBan")
  case Lockdown extends Action(3, "Lockdown")
  case NormalCare extends Action(4, "NormalCare")
  case SurgeCare extends Action(5, "SurgeCare")
  case TargetedVax extends Action(6, "TargetedVax")
  case MassVax extends Action(7, "MassVax")

object Action:
  val all: Array[Action] = Action.values
  def fromId(i: Int): Action =
    val idx = math.max(0, math.min(all.length - 1, i))
    all(idx)

final case class State(
                        s: Double, i: Double, r: Double, d: Double, v: Double,
                        hospCap: Double, t: Int, tMax: Int, seed: Int
                      ):
  def n: Double = s + i + r + d
  def util: Double = i / math.max(1.0, hospCap)
  def features: Array[Double] =
    val N = math.max(1.0, n)
    Array(s / N, i / N, r / N, d / N, v / N, util, t.toDouble / math.max(1, tMax))

final class EpidemicEnv(
                         baseBeta: Double = 0.35,
                         baseGamma: Double = 0.12,
                         baseIFR: Double = 0.008,
                         noise: Double = 0.05,
                         rewardClip: Double = 5.0,
                         riskFactor: Double = 0.0,
                         countryName: String = "Unknown",
                         seed: Int = 42
                       ) {
  private val rnd = new Random(seed)

  private val countryProfile = try {
    CountryProfiles.getProfile(countryName)
  } catch {
    case _: Exception => Geo.Country(countryName, 50000000, 0.0, 0.0)
  }

  
  def reset(s0: State): State = s0

  private def clamp01(x: Double): Double = math.max(0.0, math.min(1.0, x))

  @inline private def allocateInt(total: Int, weights: Array[Double]): Array[Int] =
    EpidemicEnv.allocateInt(total, weights)

  def step(st: State, act: Action): (State, Double, Boolean) = {
    var beta = baseBeta * countryProfile.betaMul
    var gamma = baseGamma
    var ifr = baseIFR * countryProfile.ifrMul
    var cap = st.hospCap

    act match {
      case Action.Distancing =>
        val effectiveness = countryProfile.economicResilience * 0.75 + 0.5  // 0.5 to 1.25
        beta *= effectiveness
      case Action.TravelBan =>
        val effectiveness = (1.0 - countryProfile.connectivityStrength) * 0.4 + 0.4  // 0.4 to 0.8
        beta *= effectiveness
      case Action.Lockdown =>
        val effectiveness = countryProfile.economicResilience * 0.5 + 0.2  // 0.2 to 0.7
        beta *= effectiveness
      case Action.SurgeCare =>
        val capMultiplier = 1.0 + countryProfile.healthcareCapacity  // 1.3 to 1.95
        cap *= capMultiplier
        gamma *= 1.3
      case _ => ()
    }

    val s0 = math.max(0, math.round(st.s).toInt)
    val i0 = math.max(0, math.round(st.i).toInt)
    val r0 = math.max(0, math.round(st.r).toInt)
    val d0 = math.max(0, math.round(st.d).toInt)
    val v0 = math.max(0, math.round(st.v).toInt)
    val pop0 = s0 + i0 + r0 + d0 + v0
    val mix = math.max(1.0, s0 + i0 + r0)

    // Country-specific noise with extreme variations
    val countryNoise = noise * (0.2 + countryProfile.noise * 1.5)
    val stoch = 1.0 + countryNoise * (rnd.nextGaussian())

    val effBeta = clamp01(beta * stoch)
    val effGamma = clamp01(gamma * stoch)
    val effIFR = clamp01((if (i0 > cap) ifr * 6.0 else ifr) * stoch)  // Increased from 4.0

    val vaxRate = act match {
      case Action.TargetedVax => 0.02 * countryProfile.healthcareCapacity
      case Action.MassVax => 0.05 * countryProfile.healthcareCapacity
      case _ => 0.0
    }

    // Integer-conserving splits for S and I
    val pInf = clamp01(effBeta * (if (mix > 0) i0.toDouble / mix else 0.0))
    val pVax = clamp01(vaxRate)
    val wS0 = math.max(0.0, 1.0 - pInf - pVax)
    val sAlloc = allocateInt(s0, Array(wS0, pInf, pVax))
    val sStay = sAlloc(0); val sInf = sAlloc(1); val sVax = sAlloc(2)

    val pRec = clamp01(effGamma)
    val pDie = clamp01(effIFR)
    val wI0 = math.max(0.0, 1.0 - pRec - pDie)
    val iAlloc = allocateInt(i0, Array(wI0, pRec, pDie))
    val iStay = iAlloc(0); val rec = iAlloc(1); val die = iAlloc(2)

    // Provisional next counts (integers)
    var sN = s0 - sInf - sVax
    var iN = iStay + sInf
    var rN = r0 + rec
    var dN = d0 + die
    var vN = v0 + sVax

    // Residual correction to enforce exact conservation per country
    val sumN = sN + iN + rN + dN + vN
    val resid = pop0 - sumN
    if (resid != 0) {
      sN = math.max(0, sN + resid)
    }

    val next = st.copy(
      s = sN.toDouble, i = iN.toDouble, r = rN.toDouble,
      d = dN.toDouble, v = vN.toDouble, hospCap = cap, t = st.t + 1
    )

    // extreme heterogeneous reward calculation
    val reward = calculateExtremeHeterogeneousReward(st, next, act)
    val done = (next.t >= next.tMax) || (next.i / math.max(1.0, next.n) < 1e-5) || (next.d / math.max(1.0, next.n) > 0.02)

    (next, reward, done)
  }

  private def calculateExtremeHeterogeneousReward(state: State, nextState: State, action: Action): Double = {
    val profile = countryProfile

    // Basic metrics
    val Nnext = math.max(1.0, nextState.n)
    val di = (nextState.i - state.i) / Nnext
    val dd = (nextState.d - state.d) / Nnext
    val overflow = math.max(0.0, nextState.util - 1.0)

    val healthWeight = profile.healthcareCapacity match {
      case x if x > 0.8 => 0.2   // Advanced systems care less about health metrics
      case x if x > 0.5 => 0.6   // Moderate systems balance
      case _ => 0.9              // Poor systems prioritize health heavily
    }
    val economicWeight = 1.0 - healthWeight

    // extreme scaling factors
    val extremeGDPFactor = profile.gdpPerCapita match {
      case x if x > 50000 => 0.3    // Rich countries: much lower penalties
      case x if x > 20000 => 1.0    // Middle income: normal
      case x if x > 5000  => 2.5    // Lower middle: high penalties
      case _ => 5.0                 // Poor countries: extreme penalties
    }

    val extremeHealthcareFactor = profile.healthcareCapacity match {
      case x if x > 0.8 => 0.2      // Excellent: very low penalties
      case x if x > 0.5 => 1.0      // Good: normal
      case _ => 8.0                 // Poor: extreme penalties
    }

    val extremeDensityFactor = profile.populationDensity match {
      case x if x > 400 => 3.0      // Very dense: high penalty
      case x if x > 200 => 2.0      // Dense: moderate penalty
      case x if x > 100 => 1.0      // Normal: baseline
      case _ => 0.5                 // Low density: lower penalty
    }

    val extremeAgeFactor = 1.0 + profile.ageDistributionElderly * 8.0  // Much stronger age effect

    val infectionPenalty = di * 200.0 * extremeDensityFactor * healthWeight * extremeGDPFactor
    val mortalityPenalty = dd * 2000.0 * extremeAgeFactor * extremeHealthcareFactor * healthWeight
    val healthcarePenalty = overflow * 1000.0 * healthWeight * extremeHealthcareFactor

    val extremeActionCost = calculateExtremeActionCost(action, profile) * economicWeight * extremeGDPFactor


    val baseReward = -(infectionPenalty + mortalityPenalty + healthcarePenalty + extremeActionCost)


    val shaped = baseReward * (1.0 + 0.75 * riskFactor) + (riskFactor * npiBonus(action))
    
    math.max(-rewardClip * 3.0, math.min(rewardClip, shaped))
  }

  private def calculateExtremeActionCost(action: Action, profile: Geo.Country): Double = {
    val baseActionCost = action match {
      case Action.NoneA => 0.0
      case Action.Distancing => 0.05
      case Action.TravelBan => 0.08
      case Action.Lockdown => 0.15   
      case Action.NormalCare => 0.01
      case Action.SurgeCare => 0.06
      case Action.TargetedVax => 0.03
      case Action.MassVax => 0.05
    }


    val extremeEconomicMultiplier = profile.gdpPerCapita match {
      case x if x > 50000 => 0.3      // Rich countries handle costs easily
      case x if x > 20000 => 1.0      // Middle income: normal costs
      case x if x > 5000  => 3.0      // Lower middle: high costs
      case _ => 8.0                   // Poor countries: extreme costs
    }


    val extremeMobilityMultiplier = action match {
      case Action.Lockdown | Action.TravelBan =>
        1.0 + (1.0 - profile.economicResilience) * 2.0  // Much stronger economic impact
      case _ => 1.0
    }

    baseActionCost * extremeEconomicMultiplier * extremeMobilityMultiplier * 30.0  
  }
  

  private def npiBonus(action: Action): Double = action match {
    case Action.Lockdown => 0.05 * countryProfile.economicResilience
    case Action.TravelBan => 0.03 * countryProfile.connectivityStrength
    case Action.Distancing => 0.02
    case Action.TargetedVax => 0.02 * countryProfile.healthcareCapacity
    case Action.MassVax => 0.04 * countryProfile.healthcareCapacity
    case _ => 0.0
  }

  final case class EnvParams(beta: Double, gamma: Double, ifr: Double, noise: Double)
  def params: EnvParams = EnvParams(baseBeta, baseGamma, baseIFR, noise)
}

object EpidemicEnv {
  def allocateInt(total: Int, weights: Array[Double]): Array[Int] = {
    val sumW = weights.sum
    if (total <= 0 || sumW <= 0.0) return Array.fill(weights.length)(0)
    val raw = weights.map(w => (w / sumW) * total)
    val base = raw.map(math.floor(_).toInt)
    var rem = total - base.sum
    val frac = raw.zipWithIndex.map { case (x, j) => (x - math.floor(x), j) }
    scala.util.Sorting.stableSort(frac)(Ordering.by[(Double, Int), Double](_._1).reverse)
    var k = 0
    while (rem > 0 && k < frac.length) {
      base(frac(k)._2) += 1; rem -= 1; k += 1
    }
    base
  }
}