package epidemic

import org.scalatest.funsuite.AnyFunSuite

final class ReplayBufferSpec extends AnyFunSuite {

  private def mkState(i: Int): State =
    State(s = 1000 - i, i = i.toDouble, r = 0.0, d = 0.0, v = 0.0, hospCap = 100.0, t = i, tMax = 999, seed = 1)

  private def tr(i: Int): Transition =
    Transition(s = mkState(i), a = i % 3, r = i.toDouble, s2 = mkState(i + 1), done = false)

  test("deterministic sample across identical buffers (same RNG seed, same contents)") {
    val cap = 16
    val n   = 12
    val a = new ReplayBuffer(capacity = cap)
    val b = new ReplayBuffer(capacity = cap)
    (0 until cap).foreach { i => a.push(tr(i)); b.push(tr(i)) }

    val sampA = a.sample(n).map(t => (t.a, t.s.i)).toVector
    val sampB = b.sample(n).map(t => (t.a, t.s.i)).toVector

    assert(sampA == sampB)  // same Random(123) and same buffer contents
  }

  test("ring buffer overwrites older entries and size stays at capacity") {
    val cap = 4
    val rb  = new ReplayBuffer(capacity = cap)
    (0 until 8).foreach(i => rb.push(tr(i)))      // push twice the capacity
    assert(rb.size == cap)

    val got = rb.sample(cap).map(_.s.i.toInt).toSet // sampling cap elements returns all stored entries
    val expect = Set(4, 5, 6, 7)                    // last cap items should remain after overwrite
    assert(got == expect)
  }
}
