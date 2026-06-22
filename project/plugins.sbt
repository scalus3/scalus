//resolvers += Resolver.sonatypeCentralSnapshots
// Scala.js
addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.21.0")
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.3.2")
// Scala Native
addSbtPlugin("org.scala-native" % "sbt-scala-native" % "0.5.12")
addSbtPlugin("org.portable-scala" % "sbt-scala-native-crossproject" % "1.3.2")
// other
addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.6.1")
addSbtPlugin("com.github.sbt" % "sbt-ci-release" % "1.11.2")
// show welcome message
addSbtPlugin("com.github.reibitto" % "sbt-welcome" % "0.6.0")
addSbtPlugin("pl.project13.scala" % "sbt-jmh" % "0.4.8")
// sbt plugin to unify scaladoc/javadoc across multiple projects
addSbtPlugin("com.github.sbt" % "sbt-unidoc" % "0.6.1")
// Migration Manager for Scala
addSbtPlugin("com.typesafe" % "sbt-mima-plugin" % "1.1.5")
// buildinfo
addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.13.1")
// shared-source shim used by scalus-sbt-plugin (compiled here via unmanagedSourceDirectories below)
addSbtPlugin("com.github.sbt" % "sbt2-compat" % "0.1.0")

Compile / unmanagedSourceDirectories += baseDirectory.value.getParentFile / "scalus-sbt-plugin" / "src" / "main" / "scala"
