ThisBuild / scalaVersion     := "2.12.8"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.example"
ThisBuild / organizationName := "example"

lazy val root = (project in file("."))
  .settings(
    name := "Easy MPC",
    libraryDependencies += "org.bouncycastle" % "bcprov-jdk15on" % "1.61",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % Test,
  )

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
