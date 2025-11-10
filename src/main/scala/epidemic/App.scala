package epidemic

import org.apache.spark.sql.{Row, SparkSession}
import org.apache.spark.sql.functions._
import scala.util.Try
import scala.collection.parallel.CollectionConverters._

@main def main(): Unit = {
  val spark = SparkSession.builder()
    .appName("Epidemic-DDQN-Scala3-ParallelData")
    .master("local[*]")
    .config("spark.sql.shuffle.partitions", "8")
    .config("spark.serializer", "org.apache.spark.serializer.KryoSerializer")
    .config("spark.network.timeout", "600s")
    .config("spark.dynamicAllocation.enabled", "false")
    .config("spark.ui.showConsoleProgress", "false")
    .config("spark.sql.adaptive.enabled", "true")
    .config("spark.sql.adaptive.coalescePartitions.enabled", "true")
    .getOrCreate()

  spark.sparkContext.setLogLevel("WARN")

  val hp = HyperParams()
  val cfg = TrainConfig()
  val base = State(
    s = 1_000_000, i = 0, r = 0, d = 0, v = 0,
    hospCap = 10_000, t = 0, tMax = cfg.stepsPerEpoch, seed = 1
  )

  // parallelized OWID dataloader
  val owidCsv = sys.env.getOrElse("OWID_CSV", "data/owid-covid-data.csv")
  val toy2owid = Map("China" -> "China", "Japan" -> "Japan", "India" -> "India",
    "Germany" -> "Germany", "Italy" -> "Italy", "USA" -> "United States")
  val evalHorizon = cfg.stepsPerEpoch

  def buildISeriesFromOWIDParallel(path: String, tMax: Int): Map[String, Seq[Double]] = {
    import spark.implicits._

    println("=== PARALLEL OWID DATA LOADING ===")
    val startTime = System.currentTimeMillis()

    // step 1: Load and partition CSV
    val rawDf = spark.read
      .option("header", "true")
      .option("inferSchema", "true")
      .csv(path)
      .repartition(6, col("location"))  // 6 partitions for 6 countries
      .cache()

    println(s"Loaded OWID CSV across ${rawDf.rdd.getNumPartitions} partitions")

    // step 2: Process each country in parallel - FIXED TYPE CONVERSION
    val countryResults = toy2owid.toSeq.par.map { case (toyCountry, owidCountry) =>
      println(s"Processing $toyCountry ($owidCountry) on parallel worker")

      val countryDf = rawDf
        .filter(col("location") === owidCountry)
        .select($"date", $"new_cases".cast("double").as("new_cases"))
        .withColumn("new_cases", coalesce(when(col("new_cases") < 0.0, 0.0).otherwise(col("new_cases")), lit(0.0)))
        .orderBy("date")

      val timeSeries = countryDf.select("new_cases").collect().map(_.getAs[Double]("new_cases")).toSeq.take(tMax)
      val paddedSeries = if (timeSeries.size < tMax) {
        timeSeries ++ Seq.fill(tMax - timeSeries.size)(0.0)
      } else {
        timeSeries
      }

      toyCountry -> paddedSeries
    }.seq.toMap

    rawDf.unpersist()
    val endTime = System.currentTimeMillis()
    println(f"Parallel OWID loading completed in ${(endTime - startTime) / 1000.0}%.2f seconds")
    println("=====================================")

    countryResults
  }

  import Calibrate.*
  import Validation.*

  // Use parallel data loading
  val iByCountryOpt = Try(buildISeriesFromOWIDParallel(owidCsv, evalHorizon)).toOption

  val observedTargetsOpt: Option[Targets] = iByCountryOpt.map { countryData =>
    val tArriveMap = countryData.map { case (country, series) =>
      country -> arrivalTime(series, 10.0).toDouble
    }
    val peakIMap = countryData.map { case (country, series) =>
      country -> peakI(series)
    }

    Targets(tArrive = tArriveMap, peakI = peakIMap)
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

  // enhanced heterogeneity
  val betaMul = Map("China" -> 1.00, "Japan" -> 0.75, "India" -> 1.50, "Germany" -> 0.80, "Italy" -> 1.20, "USA" -> 1.10)
  val ifrMul = Map("China" -> 0.90, "Japan" -> 1.40, "India" -> 1.30, "Germany" -> 0.70, "Italy" -> 1.60, "USA" -> 1.00)
  val noiseMul = Map("China" -> 0.8, "Japan" -> 0.9, "India" -> 1.3, "Germany" -> 0.7, "Italy" -> 1.1, "USA" -> 1.2)

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
      rewardClip = hp.rewardClip,
      countryName = name
    )
  }

  // wandb init
  WandB.init(
    entity = sys.env.getOrElse("WANDB_ENTITY", "kaushik-80405-amrita-vishwa-vidyapeetham"),
    project = sys.env.getOrElse("WANDB_PROJECT", "epidemic-ddqn"),
    name = s"parallel-data-${System.currentTimeMillis()}",
    config = Map(
      "epochs" -> cfg.epochs,
      "steps_per_epoch" -> cfg.stepsPerEpoch,
      "parallel_data_loading" -> true,
      "heterogeneity_enabled" -> true
    )
  )

  observedTargetsOpt.foreach { targets =>
    targets.tArrive.foreach { case (c, v) =>
      WandB.log(Map(s"obs_arrival_$c" -> v))
    }
    targets.peakI.foreach { case (c, v) =>
      WandB.log(Map(s"obs_peakI_$c" -> v))
    }
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

  println("=== PARALLEL PIPELINE VERIFICATION ===")
  println(f"Using ${Runtime.getRuntime.availableProcessors()} CPU cores for data processing")
  println(f"Spark partitions: ${spark.conf.get("spark.sql.shuffle.partitions")}")
  world.nodes.foreach { node =>
    println(s"${node.name}: Environment ready with parallel-loaded data")
  }
  println("======================================")

  val trainer = new MultiTrainerToy(spark, hp, cfg, world, WandB)
  val df = trainer.run()

  // writing results
  println("Writing results in parallel...")

  import scala.concurrent.Future
  import scala.concurrent.ExecutionContext.Implicits.global

  val parquetFuture = Future {
    df.write.mode("overwrite").parquet("toy_results_parallel.parquet")
    println("Parquet file written")
  }

  val csvFuture = Future {
    df.coalesce(1).write.mode("overwrite").option("header", "true").csv("toy_results_parallel_csv")
    println("CSV file written")
  }

  import scala.concurrent.duration._
  scala.concurrent.Await.ready(parquetFuture, 30.seconds)
  scala.concurrent.Await.ready(csvFuture, 30.seconds)

  println("All results saved in parallel!")
  WandB.finish()
  spark.stop()
}
