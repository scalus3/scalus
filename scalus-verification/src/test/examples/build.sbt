// This build has no project dependency on the repository build. Refresh its inputs explicitly
// with `sbt scalusVerification/prepareVerificationExamples` from the repository root.
def preparedLines(base: File, name: String): Seq[String] = {
    val path = base / ".." / ".." / ".." / "target" / "verification-examples" / name
    require(
      path.isFile,
      "Run sbt scalusVerification/prepareVerificationExamples from the repository root first."
    )
    IO.readLines(path)
}

scalaVersion := preparedLines(baseDirectory.value, "scala-version").head

name := "scalus-verification-examples"
publish / skip := true
Compile / unmanagedSources := Nil

Test / unmanagedSources := {
    val module = baseDirectory.value.getParentFile.getParentFile.getParentFile
    (baseDirectory.value * "*.scala").get ++
        ((module.getParentFile / "scalus-verification-scalatest" / "src" / "main" / "scala") ** "*.scala").get
}

Test / unmanagedJars ++= preparedLines(baseDirectory.value, "classpath").map { path =>
    val entry = file(path)
    require(entry.exists, s"Missing $path; rerun scalusVerification/prepareVerificationExamples.")
    Attributed.blank(entry)
}

// The helper lives outside this build's source root; avoid writing SemanticDB beside its source.
Test / scalacOptions ++= preparedLines(baseDirectory.value, "scalac-options")
    .filterNot(_ == "-Xsemanticdb")
Test / fork := true
Test / javaOptions += "-Xss64M"
// Suppress sun.misc.Unsafe deprecation warnings from Scala 3.3.x lazy vals (Java 23+)
Test / javaOptions ++= (if (sys.props("java.specification.version").toInt >= 23)
                            Seq("--sun-misc-unsafe-memory-access=allow")
                        else Nil)
Test / envVars += "SCALUS_REQUIRE_LEAN" -> "1"
