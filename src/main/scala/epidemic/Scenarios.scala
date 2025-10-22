package epidemic


object Scenarios {
  private def clone2D(a: Array[Array[Double]]) =
    a.map(_.clone())
  
  def oneHop(base: Array[Array[Double]], from: Int, to: Int): Array[Array[Double]] = {
    val W = clone2D(base)
    for (i <- W.indices; j <- W(i).indices) W(i)(j) = 0.0
    if (from != to) W(from)(to) = 0.004 // small toy rate
    W
  }

  // Scale a single origin row (e.g., China) by factor
  def scaleRow(base: Array[Array[Double]], row: Int, factor: Double): Array[Array[Double]] = {
    val W = clone2D(base)
    for (j <- W(row).indices) if (row != j) W(row)(j) *= factor
    W
  }
}

