ThisBuild / scalaVersion := "3.3.7"

lazy val sparkV = "3.5.2"

lazy val root = (project in file("."))
  .settings(
    name := "EpidemicSimulationNew",
    libraryDependencies ++= Seq(
      ("org.apache.spark" %% "spark-sql"  % sparkV % "provided").cross(CrossVersion.for3Use2_13),
      ("org.apache.spark" %% "spark-core" % sparkV % "provided").cross(CrossVersion.for3Use2_13),
      // Scala 3 encoders (published artifact on Maven Central)
      "io.github.pashashiz" %% "spark-encoders" % "0.1.3"
    ),
    // put provided deps on run classpath for local dev
    Compile / run := Defaults.runTask(Compile / fullClasspath, Compile / run / mainClass, Compile / run / runner).evaluated,
    fork := true,
    Test / fork := true
  )

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
  "--add-opens=java.security.jgss/sun.security.krb5=ALL-UNNAMED"
)

