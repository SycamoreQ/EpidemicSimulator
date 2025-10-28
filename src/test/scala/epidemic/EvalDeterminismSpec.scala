package epidemic

import org.scalatest.funsuite.AnyFunSuite

final class EvalDeterminismSpec extends AnyFunSuite {

  test("greedy eval is deterministic after resetToInitial with fixed RNG") {
    val hp   = HyperParams()
    val base = State(s = 1_000_000, i = 0, r = 0, d = 0, v = 0, hospCap = 10_000, t = 0, tMax = 120, seed = 42)

    val world = new WorldToy(
      hp         = hp,
      base       = base,
      seedIdx    = 0,
      rng        = new scala.util.Random(7),
      conn       = ToyConn.W,
      makeEnv    = () => new EpidemicEnv(baseBeta = 0.26, baseGamma = 0.12, baseIFR = 0.008, noise = 0.02, rewardClip = hp.rewardClip),
      validate   = false
    )

    def runEvalSeries(): Vector[Double] = {
      world.resetToInitial()
      val out = scala.collection.mutable.ArrayBuffer.empty[Double]
      var k = 0
      while (k < 100) {
        val batch = world.stepOneGreedy(100)
        val usaI = batch.find(_._1 == "USA").get._2.i
        out += usaI
        k += 1
      }
      out.toVector
    }

    val a = runEvalSeries()
    val b = runEvalSeries()
    assert(a == b)
  }
}
