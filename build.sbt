ThisBuild / scalaVersion := "3.3.7"

lazy val sparkV = "3.5.2"

// Spark is consumed as Scala 2.13 artifacts from Scala 3 using for3Use2_13.
lazy val sparkModules: Seq[ModuleID] = Seq(
  "org.apache.spark" %% "spark-core" % sparkV % "provided",
  "org.apache.spark" %% "spark-sql"  % sparkV % "provided"
).map(_.cross(CrossVersion.for3Use2_13)) // maps Scala 3 to 2.13 libs [ref]

lazy val root = (project in file("."))
  .settings(
    name := "EpidemicSimulationNew",

    // Pin a single cross build for compat and xml (2.13) across the entire graph.
    dependencyOverrides ++= Seq(
      ("org.scala-lang.modules" %% "scala-collection-compat" % "2.13.0").cross(CrossVersion.for3Use2_13),
      ("org.scala-lang.modules" %% "scala-xml"               % "2.3.0" ).cross(CrossVersion.for3Use2_13)
    ),

    // Correct API for schemes uses % rather than ->.
    libraryDependencySchemes ++= Seq(
      "org.scala-lang.modules" %% "scala-collection-compat" % VersionScheme.Always,
      "org.scala-lang.modules" %% "scala-xml"               % VersionScheme.Always
    ),

    // Block accidental Scala 3 variants that reintroduce the clash.
    excludeDependencies ++= Seq(
      "org.scala-lang.modules" % "scala-collection-compat_3",
      "org.scala-lang.modules" % "scala-xml_3"
    ),

    libraryDependencies ++= sparkModules ++ Seq(
      // Scala 3 libs; exclude compat_3 to avoid re-adding it
      ("io.github.pashashiz" %% "spark-encoders" % "0.1.3")
        .exclude("org.scala-lang.modules","scala-collection-compat_3"),
      ("dev.scalapy" %% "scalapy-core" % "0.5.3")
        .exclude("org.scala-lang.modules","scala-collection-compat_3"),

      // Align compat and xml to Spark’s 2.13 line (single suffix across the graph)
      ("org.scala-lang.modules" %% "scala-collection-compat" % "2.13.0").cross(CrossVersion.for3Use2_13),
      ("org.scala-lang.modules" %% "scala-xml"               % "2.3.0" ).cross(CrossVersion.for3Use2_13),

      // Tests (defensively exclude compat_3 as well)
      ("org.scalatest" %% "scalatest" % "3.2.19" % Test)
        .exclude("org.scala-lang.modules","scala-collection-compat_3"),

      "org.scalameta" %% "munit"      % "1.0.0"  % Test
    ),


    // Put provided Spark on run classpath for local dev
    Compile / run :=
      Defaults.runTask(Compile / fullClasspath, Compile / run / mainClass, Compile / run / runner).evaluated,

    fork := true,
    Test / fork := true
  )

testFrameworks += new TestFramework("munit.Framework")

// JVM flags (unchanged)
javaOptions ++= Seq(
  "--add-opens=java.base/java.lang=ALL-UNNAMED",
  "--add-opens=java.base/java.lang.invoke=ALL-UNNAMED",
  "--add-opens=java.base/java.lang.reflect=ALL-UNNAMED",
  "--add-opens=java.base/java.io=ALL-UNNAMED",
  "--add-opens=java.base/java.net=ALL-UNNAMED",
  "--add-opens=java.base/java.nio=ALL-UNNAMED",
  "--add-opens=java.base/java.util=ALL-UNNAMED",
  "--add-opens=java.base/java.util.concurrent=ALL-UNNAMED",
  "--add-opens=java.base/java.util.concurrent.atomic=ALL-UNNAMED",
  "--add-opens=java.base/sun.nio.ch=ALL-UNNAMED",
  "--add-opens=java.base/sun.nio.cs=ALL-UNNAMED",
  "--add-opens=java.base/sun.security.action=ALL-UNNAMED",
  "--add-opens=java.base/sun.util.calendar=ALL-UNNAMED",
  "--add-opens=java.security.jgss/sun.security.krb5=ALL-UNNAMED",
  "-Dscalapy.python.library=/opt/homebrew/opt/python@3.9/Frameworks/Python.framework/Versions/3.9/lib/libpython3.9.dylib"
)
