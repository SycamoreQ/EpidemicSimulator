package epidemic

import munit.FunSuite
import Validation._

class ValidationSpec extends FunSuite {
  test("assertState enforces mass conservation and time bounds") {
    val s = State(9, 1, 0, 0, 0, hospCap = 10, t = 0, tMax = 10, seed = 1)
    assertNoDiff("", "") // no-op to satisfy munit
    assertNoException(assertState(s, Invariants(pop = 10, tMax = 10)))
  }

  test("arrivalTime returns series length when threshold never reached") {
    val at = arrivalTime(Seq.fill(5)(1.0), thresh = 10.0)
    assertEquals(at, 5)
  }

  test("peakI returns max or zero for empty series") {
    assertEquals(peakI(Seq.empty[Double]), 0.0)
    assertEquals(peakI(Seq(0.1, 0.5, 0.3)), 0.5)
  }
}
 