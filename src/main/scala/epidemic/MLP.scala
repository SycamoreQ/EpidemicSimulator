// src/main/scala/epidemic/MLP.scala
package epidemic
import scala.util.Random

final class MLP(val in: Int, val h: Int, val out: Int, val lr: Double) {
  private val rnd = new Random(7)
  private def rand(r: Int, c: Int) = Array.fill(r, c)(rnd.nextGaussian() * 0.05)
  var W1 = rand(h, in); var b1 = Array.fill(h)(0.0)
  var W2 = rand(h, h);  var b2 = Array.fill(h)(0.0)
  var W3 = rand(out, h);var b3 = Array.fill(out)(0.0)

  private def relu(x: Double) = if (x > 0) x else 0.0
  private def drelu(x: Double) = if (x > 0) 1.0 else 0.0
  private def matVec(W: Array[Array[Double]], x: Array[Double]) =
    W.map(row => row.view.zip(x).map { case (w, xi) => w * xi }.sum)

  def predict(x: Array[Double], clamp: Double = Double.PositiveInfinity): Array[Double] = {
    val z1 = matVec(W1, x).zip(b1).map { case (s, b) => s + b }
    val h1 = z1.map(relu)
    val z2 = matVec(W2, h1).zip(b2).map { case (s, b) => s + b }
    val h2 = z2.map(relu)
    val y  = matVec(W3, h2).zip(b3).map { case (s, b) => s + b }
    if (clamp.isFinite) y.map(v => math.max(-clamp, math.min(clamp, v))) else y
  }

  private def clipInPlace(a: Array[Array[Double]], c: Double): Unit =
    if (c.isFinite) for (i <- a.indices; j <- a(i).indices) a(i)(j) = math.max(-c, math.min(c, a(i)(j)))
  private def clipInPlace(a: Array[Double], c: Double): Unit =
    if (c.isFinite) for (i <- a.indices) a(i) = math.max(-c, math.min(c, a(i)))

  // Accumulate grads over a micro-batch, then apply one update
  def trainBatch(xs: Array[Array[Double]], tgts: Array[Array[Double]], gradClip: Double, clamp: Double): Double = {
    val n = xs.length
    val gW1 = Array.fill(h, in)(0.0); val gb1 = Array.fill(h)(0.0)
    val gW2 = Array.fill(h, h)(0.0);  val gb2 = Array.fill(h)(0.0)
    val gW3 = Array.fill(out, h)(0.0);val gb3 = Array.fill(out)(0.0)
    var lossSum = 0.0

    var k = 0
    while (k < n) {
      val x0 = xs(k)
      val z1 = matVec(W1, x0).zip(b1).map { case (s, b) => s + b }; val h1 = z1.map(relu)
      val z2 = matVec(W2, h1).zip(b2).map { case (s, b) => s + b }; val h2 = z2.map(relu)
      val y  = matVec(W3, h2).zip(b3).map { case (s, b) => s + b }
      val yC = if (clamp.isFinite) y.map(v => math.max(-clamp, math.min(clamp, v))) else y

      val tgt = tgts(k)
      val err = yC.zip(tgt).map { case (p, t) => p - t }
      lossSum += err.map(e => 0.5 * e * e).sum

      // backprop
      val dW3 = Array.tabulate(out, h)((i,j) => err(i) * h2(j))
      val db3 = err.clone()
      val dh2 = Array.tabulate(h)(j => (0 until out).map(i => W3(i)(j) * err(i)).sum)
      val dz2 = dh2.zip(z2).map { case (g, z) => g * drelu(z) }
      val dW2 = Array.tabulate(h, h)((i,j) => dz2(i) * h1(j))
      val db2 = dz2.clone()
      val dh1 = Array.tabulate(h)(j => (0 until h).map(i => W2(i)(j) * dz2(i)).sum)
      val dz1 = dh1.zip(z1).map { case (g, z) => g * drelu(z) }
      val dW1 = Array.tabulate(h, in)((i,j) => dz1(i) * x0(j))
      val db1 = dz1.clone()

      // accumulate
      var i = 0
      while (i < out) { var j = 0; while (j < h) { gW3(i)(j) += dW3(i)(j); j += 1 }; i += 1 }
      var i2 = 0
      while (i2 < out) { gb3(i2) += db3(i2); i2 += 1 }
      var i3 = 0
      while (i3 < h) { var j2 = 0; while (j2 < h) { gW2(i3)(j2) += dW2(i3)(j2); j2 += 1 }; i3 += 1 }
      var i4 = 0
      while (i4 < h) { gb2(i4) += db2(i4); i4 += 1 }
      var i5 = 0
      while (i5 < h) { var j3 = 0; while (j3 < in) { gW1(i5)(j3) += dW1(i5)(j3); j3 += 1 }; i5 += 1 }
      var i6 = 0
      while (i6 < h) { gb1(i6) += db1(i6); i6 += 1 }

      k += 1
    }

    // average + clip
    val scale = 1.0 / math.max(1, n)
    for (i <- 0 until h; j <- 0 until in) gW1(i)(j) *= scale
    for (i <- 0 until h) gb1(i) *= scale
    for (i <- 0 until h; j <- 0 until h) gW2(i)(j) *= scale
    for (i <- 0 until h) gb2(i) *= scale
    for (i <- 0 until out; j <- 0 until h) gW3(i)(j) *= scale
    for (i <- 0 until out) gb3(i) *= scale

    clipInPlace(gW3, gradClip); clipInPlace(gb3, gradClip)
    clipInPlace(gW2, gradClip); clipInPlace(gb2, gradClip)
    clipInPlace(gW1, gradClip); clipInPlace(gb1, gradClip)

    // SGD step
    for (i <- W3.indices; j <- W3(i).indices) W3(i)(j) -= lr * gW3(i)(j)
    for (i <- b3.indices) b3(i) -= lr * gb3(i)
    for (i <- W2.indices; j <- W2(i).indices) W2(i)(j) -= lr * gW2(i)(j)
    for (i <- b2.indices) b2(i) -= lr * gb2(i)
    for (i <- W1.indices; j <- W1(i).indices) W1(i)(j) -= lr * gW1(i)(j)
    for (i <- b1.indices) b1(i) -= lr * gb1(i)

    lossSum / math.max(1, n)
  }

  def copyFrom(src: MLP): Unit = {
    def copy2(dst: Array[Array[Double]], s: Array[Array[Double]]): Unit =
      for (i <- dst.indices) Array.copy(s(i), 0, dst(i), 0, dst(i).length)
    def copy1(dst: Array[Double], s: Array[Double]): Unit =
      Array.copy(s, 0, dst, 0, dst.length)
    copy2(W1, src.W1); copy1(b1, src.b1)
    copy2(W2, src.W2); copy1(b2, src.b2)
    copy2(W3, src.W3); copy1(b3, src.b3)
  }
}
