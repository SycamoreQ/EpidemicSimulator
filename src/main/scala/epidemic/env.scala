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
  
  private def clamp01(x: Double): Double = math.max(0.0, math.min(1.0, x))

  @inline private def allocateInt(total: Int, weights: Array[Double]): Array[Int] =
    EpidemicEnv.allocateInt(total, weights)

  def step(st: State, act: Action): (State, Double, Boolean) = {
    var beta = baseBeta; var gamma = baseGamma; var ifr = baseIFR; var cap = st.hospCap
    act match {
      case Action.Distancing  => beta *= 0.75
      case Action.TravelBan   => beta *= 0.65
      case Action.Lockdown    => beta *= 0.35
      case Action.SurgeCare   => cap  *= 1.6; gamma *= 1.3
      case _ => ()
    }

    val s0 = math.max(0, math.round(st.s).toInt)
    val i0 = math.max(0, math.round(st.i).toInt)
    val r0 = math.max(0, math.round(st.r).toInt)
    val d0 = math.max(0, math.round(st.d).toInt)
    val v0 = math.max(0, math.round(st.v).toInt)
    val pop0 = s0 + i0 + r0 + d0 + v0
    
    val mix = math.max(1.0, s0 + i0 + r0) 
    val stoch = 1.0 + noise * (rnd.nextGaussian())
    val effBeta  = clamp01(beta  * stoch)
    val effGamma = clamp01(gamma * stoch)
    val effIFR   = clamp01((if (i0 > cap) ifr * 4.0 else ifr) * stoch)

    val vaxRate = act match {
      case Action.TargetedVax => 0.02
      case Action.MassVax     => 0.05
      case _                  => 0.0
    }

    // Integer-conserving splits for S and I
    val pInf = clamp01(effBeta * (if (mix > 0) i0.toDouble / mix else 0.0))
    val pVax = clamp01(vaxRate)
    val wS0  = math.max(0.0, 1.0 - pInf - pVax)
    val sAlloc = allocateInt(s0, Array(wS0, pInf, pVax)) // [stay, infect, vax]
    val sStay = sAlloc(0); val sInf = sAlloc(1); val sVax = sAlloc(2)

    val pRec = clamp01(effGamma)
    val pDie = clamp01(effIFR)
    val wI0  = math.max(0.0, 1.0 - pRec - pDie)
    val iAlloc = allocateInt(i0, Array(wI0, pRec, pDie)) // [stay, recover, die]
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
