package epidemic

object Geo {
  final case class Country(
                            name: String,
                            pop: Int,
                            lat: Double,
                            lon: Double,
                            hospPer1k: Double = 3.0,   // hospital beds per 1k people (toy)
                            betaMul: Double = 1.0,     // per-country transmissibility multiplier
                            ifrMul: Double = 1.0,      // infection fatality multiplier
                            noise: Double = 0.05       // env stochasticity
                          )

  // Haversine distance in kilometers
  def haversineKm(lat1: Double, lon1: Double, lat2: Double, lon2: Double): Double = {
    val R = 6371.0
    val dLat = math.toRadians(lat2 - lat1)
    val dLon = math.toRadians(lon2 - lon1)
    val a = math.pow(math.sin(dLat/2),2) +
      math.cos(math.toRadians(lat1)) * math.cos(math.toRadians(lat2)) * math.pow(math.sin(dLon/2),2)
    val c = 2 * math.atan2(math.sqrt(a), math.sqrt(1 - a))
    R * c
  }
}
