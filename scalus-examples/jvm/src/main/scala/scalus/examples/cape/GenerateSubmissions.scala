package scalus.examples.cape

import scalus.utils.BuildInfo

import java.nio.file.{Files, Path}
import java.time.Instant
import scala.sys.process.*
import scala.util.Try

/** Writes `Scalus_<version>_nau` submission dirs for all `CapeScenarios` into a UPLC-CAPE clone.
  *
  * Usage: `runMain scalus.examples.cape.GenerateSubmissions <cape-repo-dir> [<version>]`
  *
  * The version defaults to `BuildInfo.version`; pass an explicit version to override it (e.g. when
  * running from a worktree whose build version is a `-SNAPSHOT`).
  */
@main def GenerateSubmissions(args: String*): Unit = {
    val capeRepo = Path.of(
      args.headOption.getOrElse(sys.error("usage: GenerateSubmissions <cape-repo-dir> [version]"))
    )
    require(
      Files.isDirectory(capeRepo.resolve("submissions")),
      s"$capeRepo is not a UPLC-CAPE checkout"
    )
    val version = args.lift(1).getOrElse(BuildInfo.version)
    val commit = "git rev-parse HEAD".!!.trim
    require(commit.matches("^[a-f0-9]{40}$"), s"bad git commit: $commit")
    // The compiler is the release tagged `v<version>` when that tag exists; the scenario sources
    // may live in a later commit.
    val compilerCommit = Try(
      Seq("git", "rev-list", "-n", "1", s"v$version").!!(ProcessLogger(_ => ())).trim
    ).getOrElse(commit)
    val date = Instant.now().toString

    for s <- CapeScenarios.all do {
        val dir = capeRepo.resolve("submissions").resolve(s.name).resolve(s"Scalus_${version}_nau")
        Files.createDirectories(dir)
        val program = s.program()
        Files.writeString(dir.resolve(s"${s.name}.uplc"), program.show)
        Files.writeString(
          dir.resolve("metadata.json"),
          CapeMetadata.render(
            version,
            compilerCommit,
            date,
            commit,
            s.implementationNotes
          )
        )
        Files.writeString(dir.resolve("README.md"), readme(s, version, commit))
        println(f"${s.name}%-28s ${program.cborByteString.length}%6d bytes -> $dir")
    }
}

private def readme(s: CapeScenario, version: String, commit: String): String = {
    val submissionId = s"Scalus_${version}_nau"
    val sourcePath = s"scalus-examples/jvm/src/main/scala/scalus/examples/cape/${s.sourceSubdir}/"
    // A fixed scenario is reviewed by its source, so quote the file that translates the algorithm.
    val compliance = s.prescribedSource.fold("") { file =>
        val code = Files.readString(Path.of(sourcePath, file)).trim
        s"""
           |## Algorithm Compliance
           |
           |`$file` translates the prescribed algorithm one to one: the same base case, and the same
           |recursive calls with the same arguments. The Scalus compiler adds only general-purpose
           |optimizations, such as the recursion encoding and inlining.
           |
           |```scala
           |$code
           |```
           |""".stripMargin
    }
    s"""# Benchmark Implementation Notes
       |
       |**Scenario**: `${s.name}`
       |
       |**Submission ID**: `$submissionId` (Format: `Language_Version_GitHubHandle`)
       |
       |## Implementation Details
       |
       |- **Compiler**: Scalus $version
       |- **Implementation Approach**: ${s.readmeApproach}
       |- **Compilation Flags**: `Options.release`
       |
       |## Performance Results
       |
       |- See [metrics.json](metrics.json) for detailed performance measurements
       |$compliance
       |## Reproducibility
       |
       |### Source Code
       |
       |- **Source Available**: true
       |
       |#### For External Repository (recommended approach):
       |
       |- **Direct Link**: [${s.sourceSubdir}](${CapeMetadata.SourceRepository}/tree/$commit/$sourcePath)
       |- **Repository**: ${CapeMetadata.SourceRepository}
       |- **Tag/Commit**: `$commit`
       |- **Source Path**: `$sourcePath`
       |
       |### Build Instructions
       |
       |```sh
       |git clone ${CapeMetadata.SourceRepository}
       |cd scalus
       |git checkout $commit
       |nix develop   # provides JDK 21 and sbt
       |sbt "scalusExamplesJVM/runMain scalus.examples.cape.GenerateSubmissions <path-to-UPLC-CAPE> $version"
       |```
       |
       |This writes `submissions/<scenario>/$submissionId/` for all 8 scenarios: the `.uplc` file,
       |`metadata.json` and this README. Then run `treefmt` in the UPLC-CAPE checkout to apply the
       |`pretty-uplc` layout, which changes only whitespace. `scripts/cape-submit.sh` in the
       |Scalus repository runs all of this, then `cape submission verify` and `measure`.
       |
       |### Compilation Configuration
       |
       |- **Optimization Level**: release
       |- **Compilation Flags**: `Options.release`
       |- **Notable Configuration**: Targets Plutus V3, protocol version 11 (vanRossem), plutus-core 1.1.0. No traces.
       |
       |## Notes
       |
       |${s.implementationNotes}
       |""".stripMargin
}
