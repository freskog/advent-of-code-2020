val dottyVersion = "3.0.0-M2"
val zioVersion = "1.0.3"

lazy val root = project
  .in(file("."))
  .settings(
    name := "advent-of-code-2020",
    version := "0.1.0",

    scalaVersion := dottyVersion,

    libraryDependencies ++= 
      Seq(
        "org.typelevel" %% "cats-parse" % "0.1.0",
        "dev.zio" %% "zio" % zioVersion,
        "dev.zio" %% "zio-streams" % zioVersion,
        "dev.zio" %% "zio-test" % zioVersion % "test"
      ).map(_.withDottyCompat(scalaVersion.value))
  )
