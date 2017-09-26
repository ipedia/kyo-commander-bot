
lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.kyocommander",
      scalaVersion := "2.11.8",
      version      := "0.1.0-SNAPSHOT"
    )),
    resolvers += Resolver.sonatypeRepo("releases"),
    name := "kyo-commander-bot",
    libraryDependencies ++= Seq(
      "com.danielasfregola" %% "twitter4s" % "5.1",
      "org.scalatest" %% "scalatest" % "3.0.3" % Test
    )
  )
