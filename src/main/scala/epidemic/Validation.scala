package epidemic

object Validation {
  final case class Invariants(pop: Int, tMax: Int)

  def assertState(s: State, inv: Invariants): Unit = {
    require(s.s >= 0 && s.i >= 0 && s.r >= 0 && s.d >= 0 && s.v >= 0, s"neg compartment: $s")
    val total = s.s + s.i + s.r + s.d + s.v
    require(math.round(total).toInt == inv.pop, s"mass not conserved: $total != ${inv.pop}")
    require(s.t >= 0 && s.t <= inv.tMax, s"t out of bounds: ${s.t} not in [0, ${inv.tMax}]")
  }

  final case class Metrics(arrival: Int, peakI: Double, finalI: Double)

  def arrivalTime(iSeries: Seq[Double], thresh: Double = 10.0): Int =
    iSeries.indexWhere(_ >= thresh) match {
      case -1 => iSeries.length
      case k  => k
    }

  def peakI(iSeries: Seq[Double]): Double =
    if (iSeries.isEmpty) 0.0 else iSeries.max
}
