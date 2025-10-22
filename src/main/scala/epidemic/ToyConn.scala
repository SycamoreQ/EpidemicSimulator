// src/main/scala/epidemic/ToyConn.scala
package epidemic

final case class CountryCfg(name: String, pop: Int)

object ToyConn {
  val C: Vector[CountryCfg] = Vector(
    CountryCfg("China",   1_000_000),
    CountryCfg("Japan",     120_000),
    CountryCfg("India",   1_000_000),
    CountryCfg("Germany",    80_000),
    CountryCfg("Italy",      60_000),
    CountryCfg("USA",       300_000)
  )

  // Row-normalized “airport” matrix: fraction of S and I exported per step
  // Small numbers: e.g., 0.002 means 0.2% of S and I move to that destination per step.
  // Rows sum <= 0.01 to keep flows tiny.
  val W: Array[Array[Double]] = Array(
    // from China -> [CN, JP, IN, DE, IT, US]
    Array(0.0, 0.003, 0.004, 0.001, 0.001, 0.002),
    // Japan
    Array(0.003, 0.0, 0.001, 0.001, 0.001, 0.002),
    // India
    Array(0.004, 0.001, 0.0, 0.001, 0.001, 0.002),
    // Germany
    Array(0.001, 0.001, 0.001, 0.0, 0.001, 0.002),
    // Italy
    Array(0.001, 0.001, 0.001, 0.001, 0.0, 0.002),
    // USA
    Array(0.002, 0.002, 0.002, 0.002, 0.002, 0.0)
  )
  
  for (i <- W.indices) {
    for (j <- W(i).indices) W(i)(j) = math.max(0.0, if (i==j) 0.0 else W(i)(j))
  }
}
