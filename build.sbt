ThisBuild / scalaVersion     := "2.13.0"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "eu.cong"

lazy val spireVersion = "0.17.0-M1"
lazy val akkaVersion = "2.5.23"

lazy val root = (project in file("."))
  .settings(
    name := "Easy MPC",
    libraryDependencies ++= Seq(
      "org.bouncycastle" % "bcprov-jdk15on" % "1.61",
      "org.typelevel" %% "spire" % spireVersion,
      "com.typesafe.akka" %% "akka-actor" % akkaVersion,
      "com.typesafe.akka" %% "akka-testkit" % akkaVersion,
      "org.scalatest" %% "scalatest" % "3.0.8" % Test,
      "org.scalacheck" %% "scalacheck" % "1.14.0" % Test,
      "org.typelevel" %% "spire-laws" % spireVersion % Test,
    ),
  )
