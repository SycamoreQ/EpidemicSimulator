package epidemic

import org.scalatest.funsuite.AnyFunSuite

final class ValidationSpec extends AnyFunSuite {

  test("assertState enforces mass conservation and time bounds") {
    val s = State(s = 9, i = 1, r = 0, d = 0, v = 0, hospCap = 10, t = 0, tMax = 10, seed = 1)
    // Should run without throwing for valid invariants
    Validation.assertState(s, Validation.Invariants(pop = 10, tMax = 10))
    assert(true)
  }

  test("arrivalTime returns series length when threshold never reached") {
    val at = Validation.arrivalTime(Seq(1.0, 2.0, 3.0, 4.0, 5.0), thresh = 10.0)
    assert(at == 5)
  }

  test("peakI returns max or zero for empty series") {
    assert(Validation.peakI(Seq.empty[Double]) == 0.0)
    assert(math.abs(Validation.peakI(Seq(0.1, 0.5, 0.3)) - 0.5) <= 1e-12)
  }
}
