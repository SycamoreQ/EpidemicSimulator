package epidemic

import org.scalatest.funsuite.AnyFunSuite

final class MobilitySpec extends AnyFunSuite {

  test("W rows obey cap and diagonals are zero") {
    val W = ToyConn.W
    for (i <- W.indices) {
      assert(math.abs(W(i)(i)) <= 1e-12)
      val row = (0 until W.length).filter(_ != i).map(j => W(i)(j)).sum
      assert(row <= 0.20000001, s"row $i exceeds cap: $row")
      assert(row >= 0.0)
    }
  }

  test("scaleRow preserves zero diagonal and rescales off-diagonals") {
    val W  = ToyConn.W
    val i0 = 0
    val A  = ToyConn.scaleRow(W, row = i0, factor = 1.5)
    assert(A(i0)(i0) == 0.0)
    val sW = (0 until W.length).filter(_ != i0).map(W(i0)(_)).sum
    val sA = (0 until A.length).filter(_ != i0).map(A(i0)(_)).sum
    assert(sA <= math.min(0.2, 1.5 * sW) + 1e-9)
  }

  test("mobility export equals sum of imports (S+I) within tolerance for one step") {
    val hp   = HyperParams()
    val cfg  = TrainConfig(epochs = 1, stepsPerEpoch = 1)
    val base = State(s = 1_000_000, i = 0, r = 0, d = 0, v = 0, hospCap = 10_000, t = 0, tMax = 100, seed = 1)
    val W    = ToyConn.W
    val world = new WorldToy(hp, base, seedIdx = 0, conn = W, validate = false)

    // snapshot pre-step S+I
    val preSI = world.nodes.map(n => n.name -> (n.s.s + n.s.i).toDouble).toMap

    // advance one learning step (uses mobility)
    world.stepOne(cfg.stepsPerEpoch)

    val postSI = world.nodes.map(n => n.name -> (n.s.s + n.s.i).toDouble).toMap

    val totalPre  = preSI.values.sum
    val totalPost = postSI.values.sum
    assert(math.abs(totalPre - totalPost) <= 1e-6 * totalPre, s"mass not conserved: $totalPre vs $totalPost")
  }
}
