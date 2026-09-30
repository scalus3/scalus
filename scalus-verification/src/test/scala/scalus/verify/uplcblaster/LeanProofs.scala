package scalus.verify.uplcblaster

import org.scalatest.Assertions
import scalus.uplc.Constant
import scalus.verify.*

import java.io.File
import java.nio.file.{Files, Path}

/** Runs [[UplcBlaster]] on statements through Lean, for test suites. */
trait LeanProofs extends Assertions {

    protected val leanDirectory: Path = Path.of("scalus-verification", "src", "main", "lean")

    /** Whether Lean can run here: `lake` on the `PATH` and a built workspace. The ci-jvm shell has
      * neither, so the tests that run Lean are canceled there. Build the workspace with
      * `lake build` (see the module README) to run them.
      */
    protected lazy val leanAvailable: Boolean =
        sys.env
            .getOrElse("PATH", "")
            .split(File.pathSeparator)
            .exists(directory => Files.isExecutable(Path.of(directory, "lake"))) &&
            Files.isRegularFile(
              leanDirectory.resolve(".lake/build/lib/lean/ScalusProofs/Run.olean")
            )

    /** Cancels the test when Lean cannot run here, unless the `SCALUS_REQUIRE_LEAN` environment
      * variable is set, as in the Lean-Proofs workflow: there a missing Lean fails the test, so the
      * proofs cannot pass by not running.
      */
    protected def requireLean(): Unit = {
        val missing = s"requires lake and a built Lean workspace in $leanDirectory"
        if sys.env.contains("SCALUS_REQUIRE_LEAN") then assert(leanAvailable, missing)
        else assume(leanAvailable, missing)
    }

    /** Declares `prop` in a fresh verifier and runs [[UplcBlaster]] on it through Lean. */
    protected def run(
        prop: Prop,
        budget: Int,
        functions: Seq[FunctionDef[?, ?]]
    ): (Verifier, Statement, VerificationResult) = {
        requireLean()
        val verifier = Verifier.empty
        functions.foreach(verifier.addFunction)
        val statement = verifier.statement(prop)
        (verifier, statement, verifier.verify(statement, UplcBlaster(budget, leanDirectory)))
    }

    /** Proves `prop`, and returns how the proof was checked. */
    protected def proven(prop: Prop, budget: Int, functions: FunctionDef[?, ?]*): ProofKind =
        run(prop, budget, functions) match
            case (verifier, statement, VerificationResult.Proven(proof)) =>
                val artifact = proof.artifact.asInstanceOf[UplcBlaster.Artifact]
                artifact.kind match
                    case ProofKind.Blaster    => assert(artifact.output.contains("✅ Valid"))
                    case ProofKind.LeanNative => assert(!artifact.output.contains("error"))
                    case other                => fail(s"unexpected proof kind $other")
                assert(verifier.theorems.exists(_.statement eq statement))
                artifact.kind
            case (_, _, other) => fail(s"expected a proof, got $other")

    /** The replayed counterexample of a refuted statement. */
    protected def refuted(
        prop: Prop,
        budget: Int,
        functions: FunctionDef[?, ?]*
    ): Map[String, Constant] =
        run(prop, budget, functions) match
            case (verifier, _, VerificationResult.Refuted(proof)) =>
                assert(verifier.theorems.isEmpty)
                proof.artifact.asInstanceOf[UplcBlaster.Artifact].counterexample.toMap
            case (_, _, other) => fail(s"expected a refutation, got $other")

    protected def integer(value: Constant): BigInt = value match
        case Constant.Integer(integer) => integer
        case other                     => fail(s"expected an integer, got $other")
}
