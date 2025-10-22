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
      val seq = is.toSeq
      val arr = arrivalTime(seq, thresh = 10.0)
      val pk  = peakI(seq)
      val fin = seq.lastOption.getOrElse(0.0)
      name -> Metrics(arrival = arr, peakI = pk, finalI = fin)
    }
  }

  def rmse(x: Seq[Double]): Double = {
    val n = x.size.toDouble
    math.sqrt(x.map(v => v*v).sum / math.max(1.0, n))
  }

  // NEW: evaluate full loss surface and return all rows
  def gridSurface(
                   hp: HyperParams,
                   cfg: TrainConfig,
                   base: State,
                   seedIdx: Int,
                   targets: Targets,
                   betaGrid: Seq[Double],
                   travelScales: Seq[Double],
                   validate: Boolean = true
                 ): (Seq[(Double, Double, Double)], (Double, (Double, Double))) = {
    val countries = ToyConn.C.map(_.name)
    var bestLoss = Double.MaxValue
    var bestPair = (0.0, 0.0)
    val rows = scala.collection.mutable.ArrayBuffer.empty[(Double, Double, Double)]

    for (beta <- betaGrid; ts <- travelScales) {
      val W = Scenarios.scaleRow(ToyConn.W, row = seedIdx, factor = ts)
      val m  = runDeterministic(hp, cfg, base, seedIdx, W, baseBeta = beta, validate = validate)

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
