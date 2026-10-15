package scalus.verify.uplcblaster

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.verify.*
import scalus.verify.Props.*
import scalus.verify.lean.Directories

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

    test(
      "an unnamed statement is kept under its content, and goes once its test no longer states it"
    ) {
        val directory = Files.createTempDirectory("scalus-lean-proofs")
        val kept = directory.resolve("kept.proofs.json")
        var runs = "first"
        def proofs(): LeanProofs =
            LeanProofs(
              LeanProofs.libraryWorkspace,
              Some(kept),
              reason => cancel(reason),
              () => runs
            )
        val holds = forAll[BigInt](x => x + BigInt(0) == x)
        val commutes = forAll[BigInt, BigInt]((x, y) => x + y == y + x)
        val grows = forAll[BigInt](x => x + BigInt(1) > x)
        def named(prop: Prop): String = Verifier.empty.statement(prop).name
        // The entries of the file, by their names, with the tests that state them.
        def entries: Map[String, List[String]] = scalus.verify.KeptResultsTest.testsIn(kept)
        try
            val before = proofs()
            try
                before.requireLean()
                before.proven(holds, 40)
                // Stated again, it is the same statement, with one entry.
                before.proven(holds, 40)
                before.proven(commutes, 40)
                before.during("a while")(before.proven(grows, 40))
            finally before.close()
            assert(
              entries == Map(
                named(holds) -> List("first"),
                named(commutes) -> List("first"),
                named(grows) -> List("a while")
              )
            )
            // A later run, in which "first" states only one of its statements, and "a while"
            // states none: both ran to their end, and what they no longer state goes.
            val after = proofs()
            try
                after.proven(holds, 40)
                after.finished("first")
                after.during("a while")(())
            finally after.close()
            assert(entries == Map(named(holds) -> List("first")))
        finally Directories.remove(directory)
    }

    test("the checks of a statement are in a directory named after it, and only after it") {
        // A plain name is the directory's name.
        assert(
          LeanProofs.directoryName("payment_splitter.naive.only_a_payee_submits") ==
              "payment_splitter.naive.only_a_payee_submits"
        )
        // Another is made one, with a hash of the name, so that names do not meet in one.
        val spaced = LeanProofs.directoryName("a b")
        val underscored = LeanProofs.directoryName("a_b")
        assert(spaced.startsWith("a_b-") && spaced != underscored)
        // A file system may not tell case apart: names that differ in it differ in their hash.
        assert(LeanProofs.directoryName("Split").toLowerCase != LeanProofs.directoryName("split"))
        assert(!LeanProofs.directoryName("x/../y").contains("/"))
        assert(LeanProofs.directoryName("..") != "..")
        // A statement named by its content is in a directory of the test and its hash.
        val verifier = Verifier.empty
        val content = verifier.statement(Prop(true))
        val hash = content.name.drop(1)
        def checks(test: String) = LeanProofs.checksDirectoryName(content, test)
        assert(checks("Only a payee submits") == s"only_a_payee_submits.$hash")
        assert(checks("") == hash)
        // Of a long test, or one in other letters, what a file system takes.
        assert(checks("x" * 300) == "x" * 100 + s".$hash")
        assert(checks("Сума додатна") == s"____________.$hash")
        val plain = verifier.statement("plain", Prop(false))
        assert(LeanProofs.checksDirectoryName(plain, "a test") == "plain")
    }
}
