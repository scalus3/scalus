val scalusVersion = "0.18.0"

name := "lottery-complete"
organization := "org.scalus"
version := "0.1.0"
// scalus 0.18.0's compiler plugin is published for Scala 3.3.7 (scalus-plugin_3.3.7).
scalaVersion := "3.3.7"

libraryDependencies ++= Seq(
  "org.scalus" %% "scalus" % scalusVersion,
  "org.scalus" %% "scalus-cardano-ledger" % scalusVersion,
  "org.scalus" %% "scalus-testkit" % scalusVersion % Test,
  "org.scalatest" %% "scalatest" % "3.2.19" % Test,
)

// The plugin is cross-published per exact compiler version, not binary _3.
addCompilerPlugin(("org.scalus" %% "scalus-plugin" % scalusVersion).cross(CrossVersion.full))
