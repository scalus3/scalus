import java.nio.charset.StandardCharsets.UTF_8

import sbt.*
import sbt.Keys.*
import sbt.plugins.JvmPlugin
import sbt.util.Digest

/** Adds the test inputs that sbt does not see to the key of each cached test result.
  *
  * `sbt test` reuses a passing suite when its declared inputs did not change. Two inputs are not
  * declared: the Nix flake (the JDK, the `uplc` binary and the `plutus-conformance` corpus come
  * from `flake.nix` and `flake.lock`) and the value of `SCALUS_TEST_ENV`. A change of either one
  * reruns every suite. Spec [ci.alias.extra-digests] in
  * docs/superpowers/specs/2026-11-21-ci-speed-design.md.
  *
  * An AutoPlugin, not a `ThisBuild` setting: sbt 2.1 defines `extraTestDigests` in each project, so
  * a `ThisBuild` value never reaches a project.
  *
  * The digests cover test results only. A cached compile still misses a corpus change, because
  * `Macros.conformanceCases` reads the corpus at compile time; CI keys its task cache by the flake
  * hash for that ([ci.task.key-flake]).
  */
object TestInputDigests extends AutoPlugin {
    override def trigger = allRequirements
    override def requires = JvmPlugin

    override def projectSettings: Seq[Def.Setting[?]] = Seq(
      Test / extraTestDigests ++= {
          val root = (LocalRootProject / baseDirectory).value
          // A copy of the build without the flake, for example a source export, still tests.
          val flakeFiles = Seq("flake.nix", "flake.lock").map(root / _).filter(_.exists)
          val testEnv = sys.env.getOrElse("SCALUS_TEST_ENV", "")
          flakeFiles.map(f => Digest.sha256Hash(f.toPath)) :+
              Digest.sha256Hash(s"SCALUS_TEST_ENV=$testEnv".getBytes(UTF_8))
      }
    )
}
