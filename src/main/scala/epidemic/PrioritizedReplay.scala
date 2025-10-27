// src/main/scala/epidemic/PrioritizedReplay.scala
package epidemic

import scala.util.Random

// Reuse epidemic.Transition defined in ReplayBuffer.scala; DO NOT redefine it here.

final class PrioritizedReplay(
                               capacity: Int,
                               alpha: Double = 0.6,     // priority exponent
                               betaStart: Double = 0.4  // initial importance-sampling exponent
                             ) {
  private val rnd = new Random(123)
  private val data = new Array[Transition](capacity)
  private val prio = new Array[Double](capacity)
  private var size = 0
  private var idx  = 0
  private var sumP = 0.0
  private var beta = betaStart

  def push(tr: Transition, tdErr: Double = 1.0): Unit = {
    val p = math.pow(math.abs(tdErr) + 1e-6, alpha)
    if (size < capacity) size += 1 else sumP -= prio(idx)
    data(idx) = tr
    prio(idx) = p
    sumP += p
    idx = (idx + 1) % capacity
  }

  // Returns (transition, weight, index) where weight is the importance-sampling weight
  def sample(n: Int): Array[(Transition, Double, Int)] = {
    val k = math.min(n, size)
    val out = new Array[(Transition, Double, Int)](k)
    var t = 0
    while (t < k) {
      val r = rnd.nextDouble() * math.max(1e-12, sumP)
      var acc = 0.0
      var j = 0
      var picked = 0
      while (j < size) {
        acc += prio(j)
        if (acc >= r) { picked = j; j = size } else j += 1
      }
      val p = prio(picked) / math.max(1e-12, sumP)
      val w = math.pow(size * p, -beta)
      out(t) = (data(picked), w, picked)
      t += 1
    }
    out
  }

  // Update priorities after a learn() call using fresh TD errors
  def updatePriorities(idxs: Array[Int], tdErrs: Array[Double]): Unit = {
    var k = 0
    while (k < idxs.length) {
      val i = idxs(k)
      sumP -= prio(i)
      prio(i) = math.pow(math.abs(tdErrs(k)) + 1e-6, alpha)
      sumP += prio(i)
      k += 1
    }
  }

  def annealBeta(to: Double = 1.0, rate: Double = 1e-4): Unit =
    beta = math.min(to, beta + rate)

  def currentSize: Int = size
}
