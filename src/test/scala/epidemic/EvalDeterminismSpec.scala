package epidemic

import org.scalatest.funsuite.AnyFunSuite

final class EvalDeterminismSpec extends AnyFunSuite {

  private def makeWorld(seed: Int): WorldToy = {
    val hp0 = HyperParams().copy(learnEvery = Int.MaxValue) // disable learning in eval
    val base = State(
      s = 0, i = 0, r = 0, d = 0, v = 0,
      hospCap = 10_000, t = 0, tMax = 120, seed = 42
    )
    new WorldToy(
      hp         = hp0,
      base       = base,
      seedIdx    = 0,
      rng        = new scala.util.Random(seed),
      conn       = ToyConn.W,
      makeEnv    = () => new EpidemicEnv(baseBeta = 0.26, baseGamma = 0.12, baseIFR = 0.008, noise = 0.02, rewardClip = hp0.rewardClip),
      validate   = false,
      parallelism = 1 // avoid nondeterministic scheduling
    )
  }

  private def runEvalSeries(world: WorldToy, steps: Int): Vector[Double] = {
    val out = scala.collection.mutable.ArrayBuffer.empty[Double]
    var k = 0
    while (k < steps) {
      val batch = world.stepOneGreedy(steps)
      val usaI = batch.find(_._1 == "USA").get._2.i
      out += usaI
      k += 1
    }
    out.toVector
  }

  test("greedy eval is deterministic with fixed seeds and no learning") {
    val w1 = makeWorld(seed = 7)
    val w2 = makeWorld(seed = 7)

    val a = runEvalSeries(w1, 100)
    val b = runEvalSeries(w2, 100)

    assert(a == b)
  }
}
