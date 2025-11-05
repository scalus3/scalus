//resolvers += Resolver.sonatypeCentralSnapshots
// Scala.js
addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.20.1")
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.3.2")
addSbtPlugin("ch.epfl.scala" % "sbt-scalajs-bundler" % "0.21.1")
// Scala Native
addSbtPlugin("org.scala-native" % "sbt-scala-native" % "0.5.9")
addSbtPlugin("org.portable-scala" % "sbt-scala-native-crossproject" % "1.3.2")
// other
addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.5.5")
addSbtPlugin("org.scalameta" % "sbt-mdoc" % "2.7.2")
addSbtPlugin("com.github.sbt" % "sbt-ci-release" % "1.11.2")
// show welcome message
addSbtPlugin("com.github.reibitto" % "sbt-welcome" % "0.5.0")
addSbtPlugin("pl.project13.scala" % "sbt-jmh" % "0.4.7")
// sbt plugin to unify scaladoc/javadoc across multiple projects
addSbtPlugin("com.github.sbt" % "sbt-unidoc" % "0.6.0")
// Migration Manager for Scala
addSbtPlugin("com.typesafe" % "sbt-mima-plugin" % "1.1.4")
// buildinfo
addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.13.1")
