package epidemic

import org.scalatest.funsuite.AnyFunSuite
import Geo.Country

final class HeterogeneitySpec extends AnyFunSuite {

  // Local country-aware factory that mirrors your App.scala setup
  private def makeEnvFor(c: Country): EpidemicEnv =
    new EpidemicEnv(
      baseBeta   = 0.26 * c.betaMul,   // transmissibility multiplier
      baseGamma  = 0.12,
      baseIFR    = 0.008 * c.ifrMul,   // fatality multiplier
      noise      = c.noise,
      rewardClip = 5.0
    )

  test("country multipliers change epidemic dynamics over one step") {
    val cn = ToyConn.C.find(_.name == "China").get
    val jp = ToyConn.C.find(_.name == "Japan").get

    val s0 = State(s = 1_000_000, i = 1_000, r = 0, d = 0, v = 0, hospCap = 10_000, t = 0, tMax = 10, seed = 1)

    val (sCn1, _, _) = makeEnvFor(cn).step(s0, Action.fromId(0))
    val (sJp1, _, _) = makeEnvFor(jp).step(s0, Action.fromId(0))

    // Behavior-based check: different multipliers should give different next I
    assert(sCn1.i != sJp1.i)
  }
}
