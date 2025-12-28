// Standalone build for scalus-secp256k1-jni
// Run sbt from this directory to build/publish
// CI publishes with: sbt ci-release

ThisBuild / organization := "org.scalus"
ThisBuild / homepage := Some(url("https://github.com/scalus3/scalus"))
ThisBuild / licenses := List("Apache-2.0" -> url("https://www.apache.org/licenses/LICENSE-2.0"))
ThisBuild / developers := List(
  Developer("nau", "Alexander Nemish", "anemish@gmail.com", url("https://github.com/nau"))
)
// Use secp256k1-jni-v* tags for versioning
ThisBuild / dynverTagPrefix := "secp256k1-jni-v"

lazy val root = (project in file("."))
  .settings(
    name := "scalus-secp256k1-jni",
    // Java-only project
    crossPaths := false,
    autoScalaLibrary := false,
    libraryDependencies += "org.scijava" % "native-lib-loader" % "2.5.0"
  )
