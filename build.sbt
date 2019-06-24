ThisBuild / scalaVersion     := "2.13.0"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "eu.cong"

lazy val akkaVersion = "2.5.23"
lazy val spireVersion = "0.17.0-M1"

lazy val root = (project in file("."))
  .settings(
    name := "Easy MPC",
    libraryDependencies ++= Seq(
      // java
      "org.bouncycastle" % "bcprov-jdk15on" % "1.61",
      // scala
      "com.typesafe.akka" %% "akka-actor-typed" % akkaVersion,
      "com.typesafe.akka" %% "akka-actor-testkit-typed" % akkaVersion,
      "org.typelevel" %% "spire" % spireVersion,
      // test
      "org.scalatest" %% "scalatest" % "3.0.8" % Test,
      "org.scalacheck" %% "scalacheck" % "1.14.0" % Test,
      "org.typelevel" %% "spire-laws" % spireVersion % Test,
    ),
  )
