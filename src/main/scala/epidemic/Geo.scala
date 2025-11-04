package epidemic

object Geo {
  final case class Country(
                            name: String,
                            pop: Int,
                            lat: Double,
                            lon: Double,
                            hospPer1k: Double = 3.0, // hospital beds per 1k people
                            betaMul: Double = 1.0,   
                            ifrMul: Double = 1.0,    // infection fatality multiplier
                            noise: Double = 0.05,    // env stochasticity
                            populationDensity: Double = 100.0,
                            gdpPerCapita: Double = 30000.0,
                            healthcareCapacity: Double = 0.7,
                            mobilityIndex: Double = 0.8,
                            ageDistributionElderly: Double = 0.15,
                            climateFactor: Double = 0.6,
                            connectivityStrength: Double = 0.8,
                            economicResilience: Double = 0.7
                          )
  
  object CountryProfiles {
    def getProfile(name: String): Country = name match {
      case "USA" => Country(
        name = "USA", pop = 331000000, lat = 39.8, lon = -98.5,
        hospPer1k = 2.9, betaMul = 1.0, ifrMul = 1.0, noise = 0.05,
        populationDensity = 36.0, gdpPerCapita = 65000.0, healthcareCapacity = 0.85,
        mobilityIndex = 0.9, ageDistributionElderly = 0.20, climateFactor = 0.7,
        connectivityStrength = 0.95, economicResilience = 0.9
      )
      case "Japan" => Country(
        name = "Japan", pop = 125000000, lat = 36.2, lon = 138.2,
        hospPer1k = 13.1, betaMul = 0.8, ifrMul = 0.9, noise = 0.03,
        populationDensity = 347.0, gdpPerCapita = 42000.0, healthcareCapacity = 0.95,
        mobilityIndex = 0.6, ageDistributionElderly = 0.30, climateFactor = 0.5,
        connectivityStrength = 0.8, economicResilience = 0.8
      )
      case "India" => Country(
        name = "India", pop = 1380000000, lat = 20.6, lon = 78.9,
        hospPer1k = 0.5, betaMul = 1.3, ifrMul = 1.2, noise = 0.08,
        populationDensity = 464.0, gdpPerCapita = 2100.0, healthcareCapacity = 0.3,
        mobilityIndex = 0.4, ageDistributionElderly = 0.07, climateFactor = 0.8,
        connectivityStrength = 0.4, economicResilience = 0.3
      )
      case "Italy" => Country(
        name = "Italy", pop = 60000000, lat = 41.9, lon = 12.6,
        hospPer1k = 3.2, betaMul = 1.1, ifrMul = 1.4, noise = 0.04,
        populationDensity = 206.0, gdpPerCapita = 35000.0, healthcareCapacity = 0.8,
        mobilityIndex = 0.7, ageDistributionElderly = 0.28, climateFactor = 0.6,
        connectivityStrength = 0.85, economicResilience = 0.7
      )
      case "Germany" => Country(
        name = "Germany", pop = 83000000, lat = 51.2, lon = 10.5,
        hospPer1k = 8.0, betaMul = 0.9, ifrMul = 0.8, noise = 0.03,
        populationDensity = 240.0, gdpPerCapita = 46000.0, healthcareCapacity = 0.9,
        mobilityIndex = 0.8, ageDistributionElderly = 0.25, climateFactor = 0.4,
        connectivityStrength = 0.9, economicResilience = 0.85
      )
      case "China" => Country(
        name = "China", pop = 1400000000, lat = 35.9, lon = 104.2,
        hospPer1k = 4.3, betaMul = 1.0, ifrMul = 1.0, noise = 0.06,
        populationDensity = 153.0, gdpPerCapita = 10500.0, healthcareCapacity = 0.6,
        mobilityIndex = 0.5, ageDistributionElderly = 0.18, climateFactor = 0.7,
        connectivityStrength = 0.7, economicResilience = 0.6
      )
      case _ => Country(name, 50000000, 0.0, 0.0) // Default fallback
    }
  }

  // Haversine distance in kilometers
  def haversineKm(lat1: Double, lon1: Double, lat2: Double, lon2: Double): Double = {
    val R = 6371.0
    val dLat = math.toRadians(lat2 - lat1)
    val dLon = math.toRadians(lon2 - lon1)
    val a = math.pow(math.sin(dLat/2),2) + math.cos(math.toRadians(lat1)) * math.cos(math.toRadians(lat2)) * math.pow(math.sin(dLon/2),2)
    val c = 2 * math.atan2(math.sqrt(a), math.sqrt(1 - a))
    R * c
  }
}
