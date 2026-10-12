package scalus.verify.uplcblaster

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.verify.*
import scalus.verify.Props.*

import java.nio.file.Files

/** [[LeanProofs]] as it is, without [[LeanProofsSuite]]: what whoever states proofs in another test
  * framework, or in none, has.
  */
class LeanProofsTest extends AnyFunSuite {

    private final class Skipped(reason: String) extends RuntimeException(reason)

    /** A workspace Lean has built nothing in, so nothing is checked in it. */
    private def unbuilt[A](body: java.nio.file.Path => A): A = {
        val directory = Files.createTempDirectory("scalus-lean-proofs")
        try body(directory)
        finally Files.delete(directory)
    }

    test("a statement that cannot be checked here is the caller's to say") {
        // Under SCALUS_REQUIRE_LEAN a missing Lean is a failure, and not the caller's.
        assume(!sys.env.contains("SCALUS_REQUIRE_LEAN"))
        unbuilt { workspace =>
            val proofs = LeanProofs(workspace, None, reason => throw new Skipped(reason))
            try
                val skipped = intercept[Skipped] {
                    proofs.proven(forAll[BigInt](x => x + BigInt(0) == x), 40)
                }
                assert(skipped.getMessage.contains("requires lake and a built Lean workspace"))
                assert(skipped.getMessage.contains(workspace.toString))
            finally proofs.close()
        }
    }

    test("without a caller's word, a statement that cannot be checked throws NotChecked") {
        assume(!sys.env.contains("SCALUS_REQUIRE_LEAN"))
        unbuilt { workspace =>
            val proofs = LeanProofs(workspace)
            try
                intercept[LeanProofs.NotChecked] {
                    proofs.refuted(forAll[BigInt](x => x + BigInt(1) == x), 40)
                }
            finally proofs.close()
        }
    }

    test(
      "two lines state proofs through Lean, and an expectation that fails is an AssertionError"
    ) {
        val proofs = LeanProofs(LeanProofs.libraryWorkspace, None, reason => cancel(reason))
        import proofs.*
        try
            assert(proven(forAll[BigInt](x => x + BigInt(0) == x), 40) == ProofKind.Blaster)
            // The counterexample has the statement's one variable, whatever its value.
            assert(refuted(forAll[BigInt](x => x + BigInt(1) == x), 40).size == 1)
            val failed = intercept[AssertionError] {
                proven(forAll[BigInt](x => x + BigInt(1) == x), 40)
            }
            assert(failed.getMessage.startsWith("expected a proof, got"))
        finally close()
    }

    test("closing ends the proofs' server, and no check runs after") {
        val proofs = LeanProofs(LeanProofs.libraryWorkspace, None, reason => cancel(reason))
        proofs.requireLean()
        val server = proofs.leanServer
        proofs.close()
        assert(server.isClosed)
        proofs.close()
        val failed = intercept[AssertionError](proofs.leanServer)
        assert(failed.getMessage.contains("closed"))
    }
}
