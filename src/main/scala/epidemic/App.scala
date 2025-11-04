package epidemic

import org.apache.spark.sql.{Row, SparkSession}
import org.apache.spark.sql.functions._
import scala.util.Try

@main def main(): Unit = {
  val spark = SparkSession.builder()
    .appName("Epidemic-DDQN-Scala3-Clean")
    .master("local[*]")
    .config("spark.sql.shuffle.partitions", "8")
    .config("spark.serializer", "org.apache.spark.serializer.KryoSerializer")
    .config("spark.network.timeout", "600s")
    .config("spark.dynamicAllocation.enabled", "false")
    .config("spark.ui.showConsoleProgress", "false")
    .getOrCreate()

  spark.sparkContext.setLogLevel("WARN")

  val hp = HyperParams()
  val cfg = TrainConfig()
  val base = State(
    s = 1_000_000, i = 0, r = 0, d = 0, v = 0,
    hospCap = 10_000, t = 0, tMax = cfg.stepsPerEpoch, seed = 1
  )
  
  val owidCsv = sys.env.getOrElse("OWID_CSV", "data/owid-covid-data.csv")
  val toy2owid = Map("China" -> "China", "Japan" -> "Japan", "India" -> "India", "Germany" -> "Germany", "Italy" -> "Italy", "USA" -> "United States")
  val evalHorizon = cfg.stepsPerEpoch

  def buildISeriesFromOWID(path: String, tMax: Int): Map[String, Seq[Double]] = {
    import spark.implicits._
    val df = spark.read.option("header", "true").option("inferSchema", "true").csv(path)
      .select($"location", $"date", $"new_cases".cast("double").as("new_cases"))
      .withColumn("new_cases", coalesce(when(col("new_cases") < 0.0, 0.0).otherwise(col("new_cases")), lit(0.0)))
      .filter(col("location").isin(toy2owid.values.toSeq: _*))

    val rows = df.groupBy("location").agg(sort_array(collect_list(struct(col("date"), col("new_cases")))).as("xs")).collect()
    rows.map { r =>
      val owidName = r.getAs[String]("location")
      val xs = r.getAs[Seq[Row]]("xs")
      val ser = xs.map(rr => rr.getAs[Double]("new_cases")).take(tMax)
      val padded = if (ser.size < tMax) ser ++ Seq.fill(tMax - ser.size)(0.0) else ser
      val toyName = toy2owid.find(_._2 == owidName).map(_._1).getOrElse(owidName)
      toyName -> padded
    }.toMap
  }

  import Calibrate.*
  import Validation.*

  val iByCountryOpt = Try(buildISeriesFromOWID(owidCsv, evalHorizon)).toOption
  val observedTargetsOpt: Option[Targets] = iByCountryOpt.map { m =>
    Targets(
      tArrive = m.view.mapValues(ser => arrivalTime(ser, 10.0).toDouble).toMap,
      peakI = m.view.mapValues(ser => peakI(ser)).toMap
    )
  }

  val betaGrid = (24 to 27).map(_ / 100.0)
  val travelScales = (5 to 9).map(_ / 10.0)
  val fallbackTargets = Targets(
    tArrive = Map("Japan" -> 40.0, "India" -> 55.0, "Italy" -> 60.0, "Germany" -> 58.0, "USA" -> 52.0, "China" -> 0.0),
    peakI = Map("Japan" -> 10000.0, "India" -> 18000.0, "Italy" -> 8000.0, "Germany" -> 9000.0, "USA" -> 12000.0, "China" -> 35000.0)
  )

  val tgt = observedTargetsOpt.getOrElse(fallbackTargets)
  val cfgShort = cfg.copy(stepsPerEpoch = 300, epochs = 1)
  val (_, (bestLoss, (bestBeta, _))) =
    gridSurface(hp, cfgShort, base.copy(tMax = 300), seedIdx = 0, tgt, betaGrid, travelScales, validate = true)

  println(f"[surface] best loss=$bestLoss%.4f beta=$bestBeta%.3f")
  
  val betaMul = Map(
    "China" -> 1.00, "Japan" -> 0.75, "India" -> 1.50,
    "Germany" -> 0.80, "Italy" -> 1.20, "USA" -> 1.10
  )

  val ifrMul = Map(
    "China" -> 0.90, "Japan" -> 1.40, "India" -> 1.30,
    "Germany" -> 0.70, "Italy" -> 1.60, "USA" -> 1.00
  )

  val noiseMul = Map(
    "China" -> 0.8, "Japan" -> 0.9, "India" -> 1.3,
    "Germany" -> 0.7, "Italy" -> 1.1, "USA" -> 1.2
  )

  // Enhanced environment factory with moderate heterogeneity
  val makeEnvFor: (String, Int) => EpidemicEnv = { (name, pop) =>
    val profile = Geo.CountryProfiles.getProfile(name)

    val finalBeta = bestBeta * betaMul.getOrElse(name, 1.0) *
      (1.0 + (profile.populationDensity / 500.0) * 0.3) *
      (2.0 - profile.healthcareCapacity * 0.8)

    val finalIFR = 0.008 * ifrMul.getOrElse(name, 1.0) *
      (1.0 + profile.ageDistributionElderly * 2.0) *
      (2.5 - profile.healthcareCapacity * 1.5)

    val finalNoise = 0.05 * noiseMul.getOrElse(name, 1.0) *
      (1.0 + (1.0 - profile.economicResilience) * 0.8)

    new EpidemicEnv(
      baseBeta = finalBeta,
      baseGamma = 0.12,
      baseIFR = finalIFR,
      noise = finalNoise,
      rewardClip = hp.rewardClip * 1.5,  // Moderate reward range expansion
      countryName = name
    )
  }

  // ---------- MINIMAL W&B Configuration ----------
  WandB.init(
    entity = sys.env.getOrElse("WANDB_ENTITY", "kaushik-80405-amrita-vishwa-vidyapeetham"),
    project = sys.env.getOrElse("WANDB_PROJECT", "epidemic-ddqn"),
    name = s"clean-hetero-${System.currentTimeMillis()}",
    config = Map(
      "epochs" -> cfg.epochs,
      "steps_per_epoch" -> cfg.stepsPerEpoch,
      "heterogeneity" -> true,
    )
  )


  observedTargetsOpt.foreach { t =>
    t.tArrive.foreach { case (c, v) => WandB.log(Map(s"obs_arrival_$c" -> v)) }
    t.peakI.foreach { case (c, v) => WandB.log(Map(s"obs_peakI_$c" -> v)) }
  }

  val world = new WorldToy(
    hp = hp,
    base = base,
    seedIdx = 0,
    rng = new scala.util.Random(7),
    conn = ToyConn.W,
    makeEnv = () => new EpidemicEnv(baseBeta = bestBeta, baseGamma = 0.12, baseIFR = 0.008, noise = 0.05, rewardClip = hp.rewardClip),
    makeEnvFor = Some(makeEnvFor),
    validate = false,
    parallelism = Runtime.getRuntime.availableProcessors()
  )

  println("=== CLEAN HETEROGENEITY SETUP ===")
  world.nodes.foreach { node =>
    println(s"${node.name}: Environment configured with heterogeneity")
  }
  println("=================================")

  val trainer = new MultiTrainerToy(spark, hp, cfg, world, WandB)
  val df = trainer.run()

  df.write.mode("overwrite").parquet("toy_results_clean_heterogeneous.parquet")
  df.coalesce(1).write.mode("overwrite").option("header", "true").csv("toy_results_clean_heterogeneous_csv")

  WandB.finish()
  spark.stop()
}
