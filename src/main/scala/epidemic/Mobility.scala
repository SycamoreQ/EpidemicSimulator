package epidemic

object Mobility {
  // Gravity + air proxy; tune exponents/gamma
  def gravityAir(
                  ci: CountryConfig,
                  cj: CountryConfig,
                  distKm: Double,
                  alpha: Double = 1.0,
                  beta: Double = 1.0,
                  gamma: Double = 1.6,
                  airBlend: Double = 0.5
                ): Double = {
    val eps = 1e-6
    val gravity = math.pow(ci.pop + eps, alpha) * math.pow(cj.pop + eps, beta) / math.pow(distKm + 1.0, gamma)
    val air     = math.sqrt((ci.airports + 1.0) * (cj.airports + 1.0)) * ci.airScale * cj.airScale
    airBlend * air + (1.0 - airBlend) * gravity
  }

  def normalizeOutbound(ws: Array[Double], dailyRate: Double): Array[Double] = {
    val s = ws.sum
    if (s <= 0) Array.fill(ws.length)(0.0) else ws.map(_ * (dailyRate / s))
  }
}

final case class CountryConfig(
                                name: String,
                                lat: Double,
                                lon: Double,
                                pop: Double,
                                airports: Int = 1,
                                airScale: Double = 1.0
                              )
