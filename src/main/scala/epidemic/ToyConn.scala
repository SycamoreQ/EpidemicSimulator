package epidemic


object ToyConn {
  import Geo.*

  val C: Vector[Country] = Vector(
    Country("USA", 330_000_000, 38.0, -97.0, 2.9, 1.00, 1.00, 0.05),
    Country("Japan", 125_000_000, 36.0, 138.0, 7.8, 0.92, 0.90, 0.04),
    Country("Italy", 60_000_000, 42.5, 12.5, 3.2, 0.98, 1.05, 0.05),
    Country("India", 1_420_000_000, 22.0, 79.0, 0.7, 1.05, 0.95, 0.06),
    Country("Germany", 83_000_000, 51.0, 10.0, 8.0, 0.96, 0.95, 0.04),
    Country("China", 1_410_000_000, 35.0, 103.0, 4.3, 1.03, 1.00, 0.05)
  )

  val flightMul: Map[(String,String), Double] = Map(
    ("China","India") -> 1.8, ("India","China") -> 1.2,
    ("USA","Japan")   -> 1.6, ("Japan","USA")   -> 1.6,
    ("Germany","Italy")-> 1.3, ("Italy","Germany")->1.2
  ).withDefaultValue(1.0)

  def gravityMatrix(
                     countries: Vector[Country],
                     a: Double = 0.2, b: Double = 1.0, g: Double = 1.2,
                     k: Double = 1e-6, capRow: Double = 0.12
                   ): Array[Array[Double]] = {
    val n = countries.size
    val W = Array.fill(n, n)(0.0)
    for i <- 0 until n do
      val ci = countries(i)
      var rowSum = 0.0
      for j <- 0 until n do
        if (i != j) {
          val cj = countries(j)
          val d  = math.max(300.0, Geo.haversineKm(ci.lat, ci.lon, cj.lat, cj.lon))
          val f  = flightMul((ci.name, cj.name))
          val w  = k * math.pow(ci.pop.toDouble, a) * math.pow(cj.pop.toDouble, b) * f / math.pow(d, g)
          W(i)(j) = w
          rowSum += w
        }
      if (rowSum > 0) {
        val scale = math.min(1.0, capRow / rowSum)
        for j <- 0 until n do if (i != j) W(i)(j) *= scale
      }
      W(i)(i) = 0.0
    W
  }

  // Define W first
  val W: Array[Array[Double]] = gravityMatrix(C)

  // Then derive W2
  def scaleEdge(W: Array[Array[Double]], src: Int, dst: Int, factor: Double): Array[Array[Double]] = {
    val A = W.map(_.clone())
    A(src)(dst) *= factor
    A
  }
  val W2: Array[Array[Double]] = scaleEdge(W, src = 5 /* China */, dst = 3 /* India idx depends on your ordering */, factor = 3.0)

  // Utility: scale a specific source row (e.g., to simulate travel policy at a hub)
  def scaleRow(W: Array[Array[Double]], row: Int, factor: Double): Array[Array[Double]] = {
    val A = W.map(_.clone())
    val n = A.length
    var s = 0.0
    for j <- 0 until n do if (j != row) { A(row)(j) *= factor; s += A(row)(j) }
    // re-cap if we exceed 1.0
    val cap = math.min(0.2, s)
    if (s > cap && s > 0) {
      val sc = cap / s
      for j <- 0 until n do if (j != row) A(row)(j) *= sc
    }
    A(row)(row) = 0.0
    A
  }
}
