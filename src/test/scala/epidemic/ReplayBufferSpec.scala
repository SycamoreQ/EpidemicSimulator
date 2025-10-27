package epidemic
import munit.FunSuite

class ReplayBufferSpec extends FunSuite {
  test("ring overwrite and size") {
    val rb = new ReplayBuffer(4)
    val s = State(10,0,0,0,0,10,0,10,1)
    (0 until 6).foreach { k => rb.push(Transition(s.copy(t=k), 0, k, s.copy(t=k+1), false)) }
    assertEquals(rb.size, 4)
  }
  test("deterministic sample with fixed seed") {
    val rb = new ReplayBuffer(10)
    val s = State(10,0,0,0,0,10,0,10,1)
    (0 until 10).foreach { k => rb.push(Transition(s.copy(t=k), 0, 0.0, s.copy(t=k+1), false)) }
    val a = rb.sample(5).map(_.t).toSeq
    val b = rb.sample(5).map(_.t).toSeq
    assertEquals(a, b)
  }
}
