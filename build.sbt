ThisBuild / scalaVersion     := "2.12.8"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "eu.cong"

lazy val akkaVersion = "2.5.23"

lazy val root = (project in file("."))
  .settings(
    name := "Easy MPC",
    libraryDependencies ++= Seq(
      // java
      "org.bouncycastle" % "bcprov-jdk15on" % "1.61",
      // scala
      "com.typesafe.akka" %% "akka-actor" % akkaVersion,
      "com.typesafe.akka" %% "akka-testkit" % akkaVersion,
      // test
      "org.scalatest" %% "scalatest" % "3.0.5" % Test,
      "org.scalacheck" %% "scalacheck" % "1.14.0" % Test,
    ),
  )
