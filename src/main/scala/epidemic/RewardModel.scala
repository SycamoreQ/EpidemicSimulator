package epidemic

object RewardModel {

  case class CountryMetrics(
                             peakInfections: Double,
                             arrivalTime: Double,
                             economicLoss: Double,
                             healthcareOverload: Double,
                             policyStringency: Double
                           )
  
  def calculateCountryReward(
                              country: String,
                              state: State,
                              action: Action,
                              nextState: State,
                              step: Int
                            ): Double = {

    val profile = Geo.CountryProfiles.getProfile(country)
    
    val currentInfectionRate = state.i.toDouble / state.s.max(1.0)
    val peakCapacityRatio = state.i.toDouble / (profile.hospPer1k * profile.pop / 1000.0)
    
    val policyStringency = action match {
      case Action.Lockdown => 0.8      // Highest stringency
      case Action.TravelBan => 0.6     // High stringency
      case Action.Distancing => 0.4    // Medium stringency
      case Action.SurgeCare => 0.2     // Low stringency (healthcare response)
      case Action.NormalCare => 0.1    // Very low stringency
      case Action.TargetedVax => 0.1   // Very low stringency
      case Action.MassVax => 0.15      // Low stringency
      case Action.NoneA => 0.0         // No stringency
    }
    val economicImpact = policyStringency * (profile.gdpPerCapita / 50000.0)
    
    val healthWeight = profile.healthcareCapacity match {
      case x if x > 0.8 => 0.3  // Advanced systems focus less on peak reduction
      case x if x > 0.5 => 0.5  // Moderate systems balance health and economy  
      case _ => 0.7             // Poor systems prioritize health outcomes
    }
    val economicWeight = 1.0 - healthWeight
    val timeWeight = 0.2

    val healthcarePenalty = if (peakCapacityRatio > 1.0) {
      (peakCapacityRatio - 1.0) * 10.0 * healthWeight
    } else 0.0

    val economicPenalty = economicImpact * economicWeight
    val mortalityPenalty = (nextState.d - state.d) * profile.ifrMul * healthWeight

    // Age-adjusted mortality penalty (elderly populations are more vulnerable)
    val ageAdjustedPenalty = mortalityPenalty * (1.0 + profile.ageDistributionElderly)
    
    -(healthcarePenalty + economicPenalty + ageAdjustedPenalty)
  }
  
  def calculateHeterogeneityMetrics(countryRewards: Map[String, Double]): Map[String, Double] = {
    val rewards = countryRewards.values.toSeq
    val mean = rewards.sum / rewards.length
    val variance = rewards.map(r => math.pow(r - mean, 2)).sum / rewards.length
    val coefficientOfVariation = if (mean != 0.0) math.sqrt(variance) / math.abs(mean) else 0.0

    Map(
      "reward_heterogeneity_variance" -> variance,
      "reward_heterogeneity_cv" -> coefficientOfVariation,
    )
  }
}
