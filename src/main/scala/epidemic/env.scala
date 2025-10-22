// src/main/scala/epidemic/Env.scala
package epidemic

import scala.util.Random

enum Action(val id: Int, val label: String):
  case NoneA        extends Action(0, "None")
  case Distancing   extends Action(1, "Distancing")
  case TravelBan    extends Action(2, "TravelBan")
  case Lockdown     extends Action(3, "Lockdown")
  case NormalCare   extends Action(4, "NormalCare")
  case SurgeCare    extends Action(5, "SurgeCare")
  case TargetedVax  extends Action(6, "TargetedVax")
  case MassVax      extends Action(7, "MassVax")

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
                         riskFactor: Double = 0.0
                       ) {
  private val rnd = new Random(42)
  def reset(s0: State): State = s0

  // Helpers for integer-conserving allocations
  private def clamp01(x: Double): Double = math.max(0.0, math.min(1.0, x)) // keeps probabilities valid [web:534]
  private def allocateInt(total: Int, weights: Array[Double]): Array[Int] = { // largest-remainder apportionment [web:547]
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

  def step(st: State, act: Action): (State, Double, Boolean) = {
    // Copy state and parameters
    var beta = baseBeta; var gamma = baseGamma; var ifr = baseIFR; var cap = st.hospCap
    var s = st.s; var i = st.i; var r = st.r; var d = st.d; var v = st.v

    // Action effects (same structure as before)
    act match {
      case Action.Distancing  => beta *= 0.75
      case Action.TravelBan   => beta *= 0.65
      case Action.Lockdown    => beta *= 0.35
      case Action.SurgeCare   => cap  *= 1.6; gamma *= 1.3
      case Action.TargetedVax =>
      // Vaccination will be handled via integer allocation below; keep as rate only
      case Action.MassVax     =>
      // Vaccination handled via rate below as well
      case _ =>
    }

    // Rates and stochasticity
    val Npop = math.max(1.0, s + i + r) // exclude d from mixing pool as before [web:534]
    val stoch = 1.0 + noise * (rnd.nextGaussian())

    // Effective rates with simple NPI-dependent multipliers
    val effBeta = clamp01(beta * stoch)                             // effective infection contact rate [web:534]
    val effGamma = clamp01(gamma * stoch)                           // effective recovery rate [web:534]
    val effIFR = if (i > cap) clamp01(ifr * 4.0 * stoch) else clamp01(ifr * stoch) // overflow raises IFR [web:534]

    // Vaccination rates from actions as probabilities per step
    val vaxRate = act match {
      case Action.TargetedVax => 0.02
      case Action.MassVax     => 0.05
      case _                  => 0.0
    }

    // Integer-conserving splits

    // S split: stay vs infect vs vaccinate; weights must sum to <= 1, remainder is "stay" [web:534]
    val pInf = clamp01(effBeta * (if (Npop > 0) i / Npop else 0.0))
    val pVax = clamp01(vaxRate)
    val wS0  = math.max(0.0, 1.0 - pInf - pVax) // stay share
    val sInt = math.max(0, math.round(s).toInt) // integer source from S
    val sAlloc = allocateInt(sInt, Array(wS0, pInf, pVax)) // [stay, infect, vax] [web:547]
    val sStay = sAlloc(0); val sInf = sAlloc(1); val sVax = sAlloc(2)

    // I split: stay vs recover vs die [web:534]
    val pRec = clamp01(effGamma)
    val pDie = clamp01(effIFR)
    val wI0  = math.max(0.0, 1.0 - pRec - pDie) // stay share
    val iInt = math.max(0, math.round(i).toInt)
    val iAlloc = allocateInt(iInt, Array(wI0, pRec, pDie)) // [stay, recover, die] [web:547]
    val iStay = iAlloc(0); val rec = iAlloc(1); val die = iAlloc(2)

    // Apply exact integer updates; keep state as Double but values are integers
    val sNext = sInt - sInf - sVax
    val iNext = iStay + sInf
    val rNext = math.round(r).toInt + rec
    val dNext = math.round(d).toInt + die
    val vNext = math.round(v).toInt + sVax

    val next = st.copy(
      s = sNext.toDouble, i = iNext.toDouble, r = rNext.toDouble,
      d = dNext.toDouble, v = vNext.toDouble, hospCap = cap, t = st.t + 1
    )

    // Reward and termination (unchanged structure)
    val Nnext = math.max(1.0, next.n)
    val di = (next.i - st.i) / Nnext
    val dd = (next.d - st.d) / Nnext
    val overflow = math.max(0.0, next.util - 1.0)

    val actionCost = act match {
      case Action.NoneA        => 0.0
      case Action.Distancing   => 0.02
      case Action.TravelBan    => 0.03
      case Action.Lockdown     => 0.06
      case Action.NormalCare   => 0.005
      case Action.SurgeCare    => 0.02
      case Action.TargetedVax  => 0.01
      case Action.MassVax      => 0.02
    }

    def npiBonus(a: Action): Double = a match {
      case Action.Lockdown    => 0.03
      case Action.TravelBan   => 0.02
      case Action.Distancing  => 0.01
      case Action.TargetedVax => 0.01
      case Action.MassVax     => 0.02
      case _ => 0.0
    }

    val baseReward = -50.0 * di - 400.0 * dd - 200.0 * overflow - 10.0 * actionCost
    val shaped = baseReward * (1.0 + 0.75 * riskFactor) + (riskFactor * npiBonus(act))
    val reward = math.max(-rewardClip, math.min(rewardClip, shaped))
    val done = (next.t >= next.tMax) || (next.i / Nnext < 1e-5) || (next.d / Nnext > 0.02)
    (next, reward, done)
  }
}
