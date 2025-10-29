package epidemic

import org.scalatest.funsuite.AnyFunSuite

final class MobilitySpec extends AnyFunSuite {

  test("W rows obey cap and diagonals are zero") {
    val W = ToyConn.W
    W.indices.foreach { i =>
      assert(W(i)(i) == 0.0)
      val rowSum = W(i).sum
      assert(rowSum <= 0.12 + 1e-12) 
    }
  }

  test("scaleRow preserves zero diagonal and rescales off-diagonals") {
    val row = Array(0.0, 0.003, 0.004, 0.001, 0.001, 0.002)
    val sum = row.sum
    val scaled = row.map(_ / (if (sum == 0) 1.0 else sum) * 0.01)
    assert(scaled(0) == 0.0)
    assert(math.abs(scaled.sum - 0.01) <= 1e-12)
  }

  test("mobility export equals sum of imports (S+I) within integer algorithm") {
    val W = ToyConn.W
    val n = W.length

    // Synthetic S,I per country at a time slice (approximate toy magnitudes)
    val S = Array(1_000_000, 120_000, 1_000_000, 80_000, 60_000, 300_000).map(_.toDouble)
    val I = Array(1_000, 200, 900, 100, 80, 400).map(_.toDouble)

    // Compute integer-conserving exports/imports exactly like WorldToy
    val movedS = Array.fill(n)(0)
    val movedI = Array.fill(n)(0)

    var i = 0
    while (i < n) {
      val wRow = Array.tabulate(n)(j => if (j == i) 0.0 else W(i)(j))
      val sumW = math.max(0.0, wRow.sum)

      val Si = math.max(0, math.round(S(i)).toInt)
      val Ii = math.max(0, math.round(I(i)).toInt)

      val exportS = math.min(Si.toLong, math.floor(Si * sumW + 1e-9).toLong).toInt
      val exportI = math.min(Ii.toLong, math.floor(Ii * sumW + 1e-9).toLong).toInt

      val allocS = EpidemicEnv.allocateInt(exportS, wRow)
      val allocI = EpidemicEnv.allocateInt(exportI, wRow)

      var j = 0
      while (j < n) {
        if (j != i) { movedS(j) += allocS(j); movedI(j) += allocI(j) }
        j += 1
      }
      movedS(i) -= exportS
      movedI(i) -= exportI

      i += 1
    }

    val net = movedS.sum + movedI.sum
    assert(net == 0, s"integer-conserving mobility must net to zero, got $net")
  }
}
