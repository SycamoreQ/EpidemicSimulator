package epidemic

object Geo {
  private val R = 6371.0088 // km
  def haversine(lat1: Double, lon1: Double, lat2: Double, lon2: Double): Double = {
    val dLat = math.toRadians(lat2 - lat1)
    val dLon = math.toRadians(lon2 - lon1)
    val a = math.pow(math.sin(dLat / 2), 2) +
      math.cos(math.toRadians(lat1)) * math.cos(math.toRadians(lat2)) * math.pow(math.sin(dLon / 2), 2)
    2 * R * math.asin(math.sqrt(a))
  }
}
