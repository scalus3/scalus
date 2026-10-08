// Standalone build for scalus-crypto-jni. Run sbt from this directory. CI publishes with sbt ci-release.
ThisBuild / organization := "org.scalus"
ThisBuild / homepage := Some(url("https://github.com/scalus3/scalus"))
ThisBuild / licenses := List("Apache-2.0" -> url("https://www.apache.org/licenses/LICENSE-2.0"))
ThisBuild / developers := List(
  Developer("nau", "Alexander Nemish", "anemish@gmail.com", url("https://github.com/nau"))
)
ThisBuild / dynverTagPrefix := "crypto-jni-v"
// Untagged builds (pushes to master) publish a fixed snapshot that the main build depends on;
// tagged builds keep dynver's release version. Bump it after each release.
ThisBuild / version := {
  val v = (ThisBuild / version).value
  if (v.endsWith("-SNAPSHOT")) "0.1.1-SNAPSHOT" else v
}

lazy val root = (project in file("."))
  .settings(
    name := "scalus-crypto-jni",
    crossPaths := false,
    autoScalaLibrary := false,
    javacOptions ++= Seq("--release", "11"),
    libraryDependencies += "org.scijava" % "native-lib-loader" % "2.5.0",
    libraryDependencies += "com.github.sbt" % "junit-interface" % "0.13.3" % Test
  )
