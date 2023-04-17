lazy val versions = new {
  val scalaVersion = "2.13.10"
  val zio2V = "2.0.10"

  val zio2 = Seq(
    "dev.zio" %% "zio" % zio2V,
    "dev.zio" %% "zio-streams" % zio2V
  )

  val zioTest = Seq(
    "dev.zio" %% "zio-test" % zio2V % Test,
    "dev.zio" %% "zio-test-sbt" % zio2V % Test
  )

  val zio2Prelude = Seq(
    "dev.zio" %% "zio-prelude" % "1.0.0-RC16"
  ) ++ zio2

  val zioDeps =
    zio2 ++
      zioTest ++
      zio2Prelude
}

lazy val root =
  project
    .in(file("."))
    .settings(settings)
    .settings(
      libraryDependencies ++= versions.zioDeps,
      testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")
    )

lazy val settings =
  commonSettings ++
    commandAliases

lazy val commonSettings =
  Seq(
    name := "zionomicon-exercises",
    scalaVersion := versions.scalaVersion,
    organization := "com.example"
  )

lazy val commandAliases =
  addCommandAlias("fmt", "all scalafmtSbt scalafmt test:scalafmt") ++
    addCommandAlias("check", "all scalafmtSbtCheck scalafmtCheck test:scalafmtCheck")

lazy val stdOptions = Seq(
  "-encoding",
  "UTF-8",
  "-explaintypes",
  "-Yrangepos",
  "-feature",
  "-language:higherKinds",
  "-language:existentials",
  "-Xlint:_,-type-parameter-shadow,-byname-implicit",
  "-Xsource:2.13",
  "-Ywarn-numeric-widen",
  "-Ywarn-value-discard",
  "-unchecked",
  "-deprecation",
  "-Xfatal-warnings"
)

lazy val stdOpts213 = Seq(
  "-Wunused:imports",
  "-Wvalue-discard",
  "-Wunused:patvars",
  "-Wunused:privates",
  "-Wunused:params"
)

scalacOptions := stdOptions ++ stdOpts213