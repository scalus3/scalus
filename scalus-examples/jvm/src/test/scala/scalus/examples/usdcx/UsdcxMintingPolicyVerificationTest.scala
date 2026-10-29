package scalus.examples.usdcx

import org.scalatest.funsuite.AnyFunSuite
import scalus.ScalaCompilerVersion
import scalus.utils.Hex
import scalus.verify.uplcblaster.LeanProofs

import java.io.File
import java.nio.file.{Files, Path}
import java.util.concurrent.TimeUnit
import scala.jdk.CollectionConverters.*

/** What is proved of [[UsdcxMintingPolicy]]: the statements IOG's CardanoLedgerApiBlaster makes of
  * the Plinth policy (`Tests/Scripts/MintingPolicy/Properties.lean`), unchanged, of the script
  * compiled here. They are in the Lean workspace `src/test/lean/UsdcxMintingPolicy`, which reads
  * the script kept as `minting_policy_scalus.hex`.
  *
  * The workspace needs `lake`, Z3 5.3.1 or later, and a first `lake build` of its packages (about
  * 30 minutes). Where one is missing, the statements' test is canceled, or fails with
  * `SCALUS_REQUIRE_LEAN` set. CI does not build this workspace.
  *
  * That the recreation behaves as the original does is [[UsdcxMintingPolicyTest]].
  */
class UsdcxMintingPolicyVerificationTest extends AnyFunSuite {
    import UsdcxMintingPolicyVerificationTest.*

    test("the script of the Lean workspace is the one compiled here") {
        // Scala 3.8 and later compile some code more leanly, so the script kept is that of the
        // Scala 3.3 LTS build.
        assume(
          !ScalaCompilerVersion.hasLeanDesugaring,
          s"the kept script is Scala 3.3's; this is ${ScalaCompilerVersion.version}"
        )
        val compiled = UsdcxMintingPolicyContract.compiled.program
        assert(
          Hex.bytesToHex(compiled.cborEncoded) ==
              UsdcxMintingPolicyTest.resource("minting_policy_scalus.hex")
        )
    }

    test("IOG's statements of the Plinth policy hold of the script") {
        requireLean()
        val (finished, exitCode, output) = lakeBuild()
        assert(finished, s"lake build did not finish in $timeoutMinutes minutes:\n$output")
        val found = verdicts(output)
        found.foreach((statement, verdict) => info(s"$statement: $verdict"))
        assert(exitCode == 0, output)
        assert(found == expected, output)
    }

    /** Cancels the test where the workspace cannot be checked here, or fails it with
      * `SCALUS_REQUIRE_LEAN` set.
      */
    private def requireLean(): Unit = {
        missing.foreach { reason =>
            val message = s"$reason, to check the statements in $workspace"
            if sys.env.contains("SCALUS_REQUIRE_LEAN") then fail(message) else cancel(message)
        }
    }
}

object UsdcxMintingPolicyVerificationTest {

    val workspace: Path = LeanProofs.inSources(
      "scalus-examples",
      "jvm",
      "src",
      "test",
      "lean",
      "UsdcxMintingPolicy"
    )

    /** What Blaster says of each statement. The third is stated so as to be refuted. */
    val expected: Map[String, String] = Map(
      "minting_policy_success_imp_mintScript_info_withdrawal" -> "✅ Valid",
      "logic_not_in_withdrawal_imp_minting_policy_fail" -> "✅ Valid",
      "minting_policy_success_imp_logic_not_in_withdrawal" -> "✅ Expected Falsified"
    )

    /** About 7 minutes where the script is new, and a replay of Lake's results otherwise. */
    val timeoutMinutes = 45

    private val minZ3 = Seq(5, 3, 1)

    /** Why the workspace cannot be checked here, if it cannot. */
    def missing: Option[String] = {
        if !onPath("lake") then Some("requires lake on the PATH")
        else if !z3Version.exists(isAtLeast(_, minZ3)) then
            Some(s"requires Z3 ${minZ3.mkString(".")} or later on the PATH, found $z3Version")
        else if !Files.isDirectory(workspace.resolve(".lake/packages/CardanoLedgerApi/.lake/build"))
        then Some("requires the packages built once: run `lake build` there")
        else None
    }

    /** Runs `lake build` in the workspace: whether it finished in time, its exit code, and its
      * output.
      */
    def lakeBuild(): (Boolean, Int, String) = {
        val log = Files.createTempFile("usdcx-lake-build", ".log")
        try
            val process = new ProcessBuilder("lake", "build")
                .directory(workspace.toFile)
                .redirectErrorStream(true)
                .redirectOutput(log.toFile)
                .start()
            val finished = process.waitFor(timeoutMinutes.toLong, TimeUnit.MINUTES)
            if !finished then
                // Lean and the solver run as its children.
                process.descendants().forEach(_.destroyForcibly())
                process.destroyForcibly().waitFor()
            (finished, if finished then process.exitValue() else -1, Files.readString(log))
        finally Files.deleteIfExists(log)
    }

    private val message = """^(?:info|warning|error): \S*Properties\.lean:(\d+):\d+: (.*)$""".r
    private val theorem = """^theorem\s+(\w+).*""".r
    private val blasterOf = """^#blaster.*\[\s*(\w+)\s*\]\s*$""".r

    /** Each statement's verdict in `output`: a message of `Properties.lean` belongs to the last
      * statement that starts at or before its line, a `theorem`, or a `#blaster` of a named one.
      */
    def verdicts(output: String): Map[String, String] = {
        val lines = Files.readAllLines(workspace.resolve("UsdcxMintingPolicy/Properties.lean"))
        val starts = lines.asScala.zipWithIndex.collect {
            case (theorem(name), index)   => (index + 1, name)
            case (blasterOf(name), index) => (index + 1, name)
        }.toList
        output.linesIterator.flatMap {
            case message(line, text) =>
                starts.filter(_._1 <= line.toInt).lastOption.map((_, name) => name -> text.trim)
            case _ => None
        }.toMap
    }

    private def onPath(command: String): Boolean =
        sys.env
            .getOrElse("PATH", "")
            .split(File.pathSeparator)
            .exists(directory => Files.isExecutable(Path.of(directory, command)))

    /** The version of the `z3` on the PATH, as `Seq(5, 3, 1)`. */
    private lazy val z3Version: Option[Seq[Int]] =
        if !onPath("z3") then None
        else
            val process = new ProcessBuilder("z3", "--version").redirectErrorStream(true).start()
            val output = new String(process.getInputStream.readAllBytes())
            process.waitFor()
            """version (\d+)\.(\d+)\.(\d+)""".r
                .findFirstMatchIn(output)
                .map(m => (1 to 3).map(m.group(_).toInt))

    private def isAtLeast(version: Seq[Int], minimum: Seq[Int]): Boolean =
        version.zip(minimum).find(_ != _).forall((v, m) => v > m)
}
