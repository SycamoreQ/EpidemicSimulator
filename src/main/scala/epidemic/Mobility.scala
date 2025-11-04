package epidemic

object Mobility {
  
  def gravityAir(
                  ci: CountryConfig,
                  cj: CountryConfig,
                  distKm: Double,
                  alpha: Double = 1.0,
                  beta: Double = 1.0,
                  gamma: Double = 1.6,
                  airBlend: Double = 0.5,
                  step: Int = 0
                ): Double = {
    val eps = 1e-6
    
    val profileI = Geo.CountryProfiles.getProfile(ci.name)
    val profileJ = Geo.CountryProfiles.getProfile(cj.name)

    // Base gravity model
    val gravity = math.pow(ci.pop + eps, alpha) * math.pow(cj.pop + eps, beta) / math.pow(distKm + 1.0, gamma)
    val air = math.sqrt((ci.airports + 1.0) * (cj.airports + 1.0)) * ci.airScale * cj.airScale
    val baseMobility = airBlend * air + (1.0 - airBlend) * gravity
    
    val connectivityFactor = (profileI.connectivityStrength + profileJ.connectivityStrength) / 2.0
    val economicFactor = math.sqrt(profileI.gdpPerCapita * profileJ.gdpPerCapita) / 50000.0
    val mobilityFactor = (profileI.mobilityIndex + profileJ.mobilityIndex) / 2.0
    
    val seasonalFactor = 1.0 + 0.15 * math.sin(step * math.Pi / 180.0) *
      ((profileI.climateFactor + profileJ.climateFactor) / 2.0)

    baseMobility * connectivityFactor * economicFactor * mobilityFactor * seasonalFactor
  }

  def normalizeOutbound(ws: Array[Double], dailyRate: Double): Array[Double] = {
    val s = ws.sum
    if (s <= 0) Array.fill(ws.length)(0.0) else ws.map(_ * (dailyRate / s))
  }

  // Enhanced country config with heterogeneity support
  final case class CountryConfig(
                                  name: String,
                                  lat: Double,
                                  lon: Double,
                                  pop: Double,
                                  airports: Int = 1,
                                  airScale: Double = 1.0
                                ) {
    def profile: Geo.Country = Geo.CountryProfiles.getProfile(name)
    
    def getMobilityReduction(infectionRate: Double, policyResponse: Double, step: Int): Double = {
      val p = profile
      
      val economicCapacity = math.min(p.gdpPerCapita / 70000.0, 1.0)

      // Base mobility reduction
      val baseReduction = math.min(infectionRate * 1.5, 0.9)

      val countryAdjustment = economicCapacity * p.economicResilience * policyResponse

      baseReduction * countryAdjustment
    }
  }
}
