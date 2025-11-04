package epidemic

import scala.util.Random
import Validation._
import Geo.Country

object Calibrate {
  final case class Targets(
                            tArrive: Map[String, Double],
                            peakI:   Map[String, Double],
                            threshI: Double = 10.0
                          )

  private def rmse(x: Seq[Double]): Double = {
    val n = x.size.toDouble
    math.sqrt(x.map(v => v*v).sum / math.max(1.0, n))
  }
  
  private def rollout(
                       hp: HyperParams,
                       cfg: TrainConfig,
                       base: State,
                       seedIdx: Int,
                       beta: Double,
                       travelScale: Double,
                       validate: Boolean,
                       heterogeneous: Boolean
                     ): Map[String, Metrics] = {
    val Wscaled = ToyConn.scaleRow(ToyConn.W, row = seedIdx, factor = travelScale)
    
    val makeEnvHomogeneous: () => EpidemicEnv =
      () => new EpidemicEnv(baseBeta = beta, baseGamma = 0.12, baseIFR = 0.008, noise = 0.05, rewardClip = hp.rewardClip, seed = 7)
    
    val makeEnvForHeterogeneous: (String, Int) => EpidemicEnv = { (name, pop) =>
      val profile = Geo.CountryProfiles.getProfile(name)
      val countrySeed = ToyConn.C.indexWhere(_.name == name) + 42
      
      val finalBeta = beta * (1.0 + (profile.populationDensity / 1000.0) * 0.5) * (3.0 - profile.healthcareCapacity * 2.0)
      val finalIFR = 0.008 * (1.0 + profile.ageDistributionElderly * 2.0)
      val finalNoise = 0.05 * (1.0 + (1.0 - profile.economicResilience))

      new EpidemicEnv(
        baseBeta = finalBeta,
        baseGamma = 0.12,
        baseIFR = finalIFR,
        noise = finalNoise,
        rewardClip = hp.rewardClip,
        countryName = name,
        seed = countrySeed 
      )
    }
    
    val makeEnvForOpt: Option[(String, Int) => EpidemicEnv] =
      if (heterogeneous) Some(makeEnvForHeterogeneous) else None

    val world = new WorldToy(
      hp = hp,
      base = base,
      seedIdx = seedIdx,
      rng = new scala.util.Random(7),
      conn = Wscaled,
      makeEnv = makeEnvHomogeneous, 
      makeEnvFor = makeEnvForOpt,  
      validate = true
    )

    val series = ToyConn.C.map(_.name).map(_ -> scala.collection.mutable.ArrayBuffer.empty[Double]).toMap
    var t = 0
    while (t < cfg.stepsPerEpoch) {
      val batch = world.stepOne(cfg.stepsPerEpoch)
      batch.foreach { case (name, s2, _) => series(name) += s2.i }
      t += 1
    }

    series.map { case (name, buf) =>
      val is: Seq[Double] = buf.toSeq
      val arr = arrivalTime(is, thresh = 10.0)
      val pk = peakI(is)
      val fin = if (is.nonEmpty) is.last else 0.0
      name -> Metrics(arrival = arr, peakI = pk, finalI = fin)
    }
  }
  
  def gridSurface(
                   hp: HyperParams,
                   cfg: TrainConfig,
                   base: State,
                   seedIdx: Int,
                   targets: Targets,
                   betaGrid: Seq[Double],
                   travelScales: Seq[Double],
                   validate: Boolean = true,
                   heterogeneous: Boolean = true
                 ): (Seq[(Double, Double, Double)], (Double, (Double, Double))) = {
    val countries = ToyConn.C.map(_.name)
    var bestLoss = Double.MaxValue
    var bestPair = (betaGrid.headOption.getOrElse(0.25), travelScales.headOption.getOrElse(1.0))
    val rows = scala.collection.mutable.ArrayBuffer.empty[(Double, Double, Double)]

    for (beta <- betaGrid; ts <- travelScales) {
      val m  = rollout(hp, cfg, base, seedIdx, beta, ts, validate, heterogeneous)

      val arrLoss = rmse(countries.map { c =>
        m(c).arrival.toDouble - targets.tArrive.getOrElse(c, m(c).arrival.toDouble)
      })
      val pkLoss  = rmse(countries.map { c =>
        m(c).peakI - targets.peakI.getOrElse(c, m(c).peakI)
      })
      val loss = 0.5 * arrLoss + 0.5 * pkLoss

      rows += ((beta, ts, loss))
      if (loss < bestLoss) { bestLoss = loss; bestPair = (beta, ts) }
    }

    (rows.toSeq, (bestLoss, bestPair))
  }
}