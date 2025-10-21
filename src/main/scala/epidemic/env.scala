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
                         rewardClip: Double = 5.0         // NEW
                       ) {
  private val rnd = new Random(42)
  def reset(s0: State): State = s0

  def step(st: State, act: Action): (State, Double, Boolean) = {
    var beta = baseBeta; var gamma = baseGamma; var ifr = baseIFR; var cap = st.hospCap
    var s = st.s; var i = st.i; var r = st.r; var d = st.d; var v = st.v

    act match {
      case Action.Distancing => beta *= 0.75
      case Action.TravelBan  => beta *= 0.65
      case Action.Lockdown   => beta *= 0.35
      case Action.SurgeCare  => cap  *= 1.6; gamma *= 1.3
      case Action.TargetedVax => val amt = math.min(0.02 * s, s); s -= amt; v += amt
      case Action.MassVax     => val amt = math.min(0.05 * s, s); s -= amt; v += amt
      case _ =>
    }

    val N = math.max(1.0, s + i + r)
    val stoch = 1.0 + noise * (rnd.nextGaussian())
    val newInf = math.max(0.0, beta * s * i / N * stoch)
    val rec    = math.max(0.0, gamma * i * stoch)
    val ifrEff = if (i > cap) ifr * 4.0 else ifr
    val deaths = math.max(0.0, ifrEff * i * stoch)

    s = math.max(0.0, s - newInf)
    i = math.max(0.0, i + newInf - rec - deaths)
    r = r + rec
    d = d + deaths

    val next = st.copy(s = s, i = i, r = r, d = d, v = v, hospCap = cap, t = st.t + 1)

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

    val rewardRaw = -50.0 * di - 400.0 * dd - 200.0 * overflow - 10.0 * actionCost
    val clipped = math.max(-rewardClip, math.min(rewardClip, rewardRaw))
    val done = (next.t >= next.tMax) || (next.i / Nnext < 1e-5) || (next.d / Nnext > 0.02)
    (next, clipped, done)
  }
}

