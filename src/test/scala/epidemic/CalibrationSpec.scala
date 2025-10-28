package epidemic

import org.scalatest.funsuite.AnyFunSuite

final class CalibrationSpec extends AnyFunSuite {
  test("gridSurface runs and returns finite best loss") {
    val hp   = HyperParams()
    val cfg  = TrainConfig(epochs = 1, stepsPerEpoch = 60)
    val base = State(s = 1_000_000, i = 0, r = 0, d = 0, v = 0, hospCap = 10_000, t = 0, tMax = 60, seed = 3)
    val targets = Calibrate.Targets(Map.empty, Map.empty)
    val (rows, (bestLoss, _)) =
      Calibrate.gridSurface(hp, cfg, base, seedIdx = 0, targets,
        betaGrid = Seq(0.24, 0.26), travelScales = Seq(0.8, 1.0), validate = false, heterogeneous = false)
    assert(rows.nonEmpty)
    assert(bestLoss.isFinite)
  }
}
