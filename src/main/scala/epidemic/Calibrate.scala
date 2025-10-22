// Calibrate.scala
package epidemic

import scala.util.Random
import Validation.*

object Calibrate {
  final case class Targets(
                            tArrive: Map[String, Double], 
                            peakI:   Map[String, Double],
                            threshI: Double = 10.0
                          )

  private def runDeterministic(
                                hp: HyperParams,
                                cfg: TrainConfig,
                                base: State,
                                seedIdx: Int,
                                conn: Array[Array[Double]],
                                baseBeta: Double,
                                validate: Boolean
                              ): Map[String, Metrics] = {
    val makeEnv = () => new EpidemicEnv(baseBeta = baseBeta, rewardClip = hp.rewardClip)
    val world = new WorldToy(hp, base, seedIdx, new Random(123), conn, makeEnv, validate)

    val series = ToyConn.C.map(_.name).map(_ -> scala.collection.mutable.ArrayBuffer.empty[Double]).toMap
    var t = 0
    while (t < cfg.stepsPerEpoch) {
      val batch = world.stepOne(cfg.stepsPerEpoch)
      batch.foreach { case (name, s2, _) => series(name) += s2.i }
      t += 1
    }
    series.map { case (name, is) =>
      val arr = arrivalTime(is.toSeq, thresh = 10.0)
      val pk  = peakI(is.toSeq)
      val fin = is.lastOption.getOrElse(0.0)
      name -> Metrics(arrival = arr, peakI = pk, finalI = fin)
    }
  }

  def rmse(x: Seq[Double]): Double = {
    val n = x.size.toDouble
    math.sqrt(x.map(v => v*v).sum / math.max(1.0, n))
  }

  def gridSearch(
                  hp: HyperParams,
                  cfg: TrainConfig,
                  base: State,
                  seedIdx: Int,
                  targets: Targets,
                  betaGrid: Seq[Double],
                  travelScales: Seq[Double],
                  validate: Boolean = true
                ): (Double, (Double, Double), Map[String, Metrics]) = {
    var best: (Double, (Double, Double), Map[String, Metrics]) = (Double.MaxValue, (0.0, 0.0), Map.empty)
    for (beta <- betaGrid; ts <- travelScales) {
      val W = Scenarios.scaleRow(ToyConn.W, row = seedIdx, factor = ts)
      val m  = runDeterministic(hp, cfg, base, seedIdx, W, baseBeta = beta, validate = validate)

      val countries = ToyConn.C.map(_.name)
      val arrLoss = rmse(countries.map { c =>
        m(c).arrival.toDouble - targets.tArrive.getOrElse(c, m(c).arrival.toDouble)
      })
      val pkLoss  = rmse(countries.map { c =>
        m(c).peakI - targets.peakI.getOrElse(c, m(c).peakI)
      })

      val loss = 0.5 * arrLoss + 0.5 * pkLoss
      if (loss < best._1) best = (loss, (beta, ts), m)
    }
    best
  }
}
