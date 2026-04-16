val scalusVersion = "0.16.0"

name := "lottery-complete"
organization := "org.scalus"
version := "0.1.0"
scalaVersion := "3.3.7"

libraryDependencies ++= Seq(
  "org.scalus" %% "scalus" % scalusVersion,
  "org.scalus" %% "scalus-cardano-ledger" % scalusVersion,
  "org.scalus" %% "scalus-testkit" % scalusVersion % Test,
  "org.scalatest" %% "scalatest" % "3.2.19" % Test,
)

addCompilerPlugin("org.scalus" %% "scalus-plugin" % scalusVersion)
