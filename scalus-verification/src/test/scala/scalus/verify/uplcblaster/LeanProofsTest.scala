package scalus.verify.uplcblaster

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.verify.*
import scalus.verify.Props.*
import scalus.verify.lean.Directories

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

/** [[LeanProofs]] as it is, without [[LeanProofsSuite]]: what whoever states proofs in another test
  * framework, or in none, has.
  */
class LeanProofsTest extends AnyFunSuite {

    private final class Skipped(reason: String) extends RuntimeException(reason)

    /** A workspace Lean has built nothing in, so nothing is checked in it. */
    private def unbuilt[A](body: Path => A): A = {
        val directory = Files.createTempDirectory("scalus-lean-proofs")
        try body(directory)
        finally Directories.remove(directory)
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

    test("the project's workspace is the one the build names, made ready") {
        val root = Files.createTempDirectory("scalus-lean-proofs")
        try
            // The root of a project, and the class path of the tests of one of its modules, on
            // which the build says where the module's workspace is.
            Files.createDirectory(root.resolve(".git"))
            val workspace = root.resolve("contracts/src/test/lean/Htlc")
            val classes = root.resolve("classes")
            val said = classes.resolve(LeanProofs.workspaceResource)
            Files.createDirectories(said.getParent)
            Files.writeString(said, workspace.toString)
            val loader = new java.net.URLClassLoader(Array(classes.toUri.toURL), null)
            try
                assert(LeanProofs.projectWorkspace(loader) == workspace.toAbsolutePath.normalize)
                assert(Files.isRegularFile(workspace.resolve("lakefile.lean")))
                assert(
                  Files.isRegularFile(root.resolve(".scalus/ScalusProofs/ScalusProofs/Run.lean"))
                )
            finally loader.close()
        finally Directories.remove(root)
    }

    test("where no build names a workspace, it is the library's in these sources") {
        val loader = new java.net.URLClassLoader(Array.empty[java.net.URL], null)
        try assert(LeanProofs.projectWorkspace(loader) == LeanProofs.libraryWorkspace)
        finally loader.close()
    }

    test("two lines state proofs through Lean, until they are closed") {
        val proofs = LeanProofs(LeanProofs.libraryWorkspace, None, reason => cancel(reason))
        import proofs.*
        try
            assert(proven(forAll[BigInt](x => x + BigInt(0) == x), 40) == ProofKind.Blaster)
            // The counterexample has the statement's one variable, whatever its value.
            assert(refuted(forAll[BigInt](x => x + BigInt(1) == x), 40).size == 1)
            // An expectation that does not hold is a failure of any test framework.
            val failed = intercept[AssertionError] {
                proven(forAll[BigInt](x => x + BigInt(1) == x), 40)
            }
            assert(failed.getMessage.startsWith("expected a proof, got"))

            val server = leanServer
            close()
            assert(server.isClosed)
            close()
            val after = intercept[AssertionError](leanServer)
            assert(after.getMessage.contains("closed"))
        finally close()
    }

    test("a result is kept under what runs, and the count of its statements") {
        val directory = Files.createTempDirectory("scalus-lean-proofs")
        val kept = directory.resolve("kept.proofs.json")
        var runs = "first"
        val proofs =
            LeanProofs(
              LeanProofs.libraryWorkspace,
              Some(kept),
              reason => cancel(reason),
              () => runs
            )
        import proofs.*
        try
            requireLean()
            val holds = forAll[BigInt](x => x + BigInt(0) == x)
            proven(holds, 40)
            proven(holds, 40)
            runs = "second"
            proven(holds, 40)
            // A name for a while, in place of what runs, and from one each time it is given.
            during("a while") {
                proven(holds, 40)
                proven(holds, 40)
            }
            during("a while")(proven(holds, 40))
            proven(holds, 40)
            runs = "first"
            proven(holds, 40)

            val text = Files.readString(kept, StandardCharsets.UTF_8)
            val names = """"([^"]+ #\d+)"""".r.findAllMatchIn(text).map(_.group(1)).toSet
            assert(
              names == Set(
                "first #1",
                "first #2",
                "first #3",
                "second #1",
                "second #2",
                "a while #1",
                "a while #2"
              )
            )
        finally
            close()
            Directories.remove(directory)
    }
}
