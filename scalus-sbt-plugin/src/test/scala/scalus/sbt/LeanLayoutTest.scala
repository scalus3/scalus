package scalus.sbt

import org.scalatest.funsuite.AnyFunSuite
import sbt.io.IO

import java.io.File
import java.nio.file.Files

class LeanLayoutTest extends AnyFunSuite {

    test("packageName joins the words of a project's name, each with a capital") {
        assert(LeanLayout.packageName("my-contracts") == "MyContracts")
        assert(LeanLayout.packageName("htlc") == "Htlc")
        assert(LeanLayout.packageName("scalus_examples.jvm") == "ScalusExamplesJvm")
        assert(LeanLayout.packageName("LinearVesting") == "LinearVesting")
    }

    test("packageName starts with a letter") {
        assert(LeanLayout.packageName("3d-auction") == "DAuction")
        assert(LeanLayout.packageName("2024") == "Proofs")
        assert(LeanLayout.packageName("--") == "Proofs")
    }

    test("a workspace has proofs by hand where a Lean module is under <Package>/Proofs") {
        val root = Files.createTempDirectory("lean-layout-test").toFile
        try {
            val workspace = new File(root, "Htlc")
            assert(!LeanLayout.hasProofsByHand(workspace))
            val proofs = LeanLayout.proofsDirectory(workspace)
            assert(proofs == new File(workspace, "Htlc/Proofs"))
            // The directories `init` makes, empty, are no proofs.
            assert(new File(proofs, "Refund").mkdirs())
            assert(!LeanLayout.hasProofsByHand(workspace))
            Files.write(new File(proofs, "Refund/Notes.md").toPath, "notes".getBytes)
            assert(!LeanLayout.hasProofsByHand(workspace))
            val proof = "theorem t : True := trivial".getBytes
            Files.write(new File(proofs, "Refund/Committer.lean").toPath, proof)
            assert(LeanLayout.hasProofsByHand(workspace))
            // Statements Scalus writes are not proofs by hand.
            val other = new File(root, "Vesting")
            assert(new File(other, "Vesting/Generated").mkdirs())
            val statement = "def s : Prop := True".getBytes
            Files.write(new File(other, "Vesting/Generated/s.lean").toPath, statement)
            assert(!LeanLayout.hasProofsByHand(other))
        } finally IO.delete(root)
    }

    test("the workspaces of a directory are the Lake packages in it, by name") {
        val root = Files.createTempDirectory("lean-layout-test").toFile
        try {
            def write(file: File, text: String): Unit = {
                assert(file.getParentFile.isDirectory || file.getParentFile.mkdirs())
                Files.write(file.toPath, text.getBytes("UTF-8"))
            }
            write(new File(root, "Vesting/lakefile.lean"), "import Lake")
            write(new File(root, "Htlc/lakefile.toml"), "name = \"Htlc\"")
            // A directory without a lakefile, and a lakefile that is not in a directory of its own.
            assert(new File(root, "Notes/Notes").mkdirs())
            write(new File(root, "lakefile.lean"), "import Lake")
            assert(
              LeanLayout.workspaces(root) == Seq(new File(root, "Htlc"), new File(root, "Vesting"))
            )
            assert(LeanLayout.workspaces(new File(root, "missing")).isEmpty)
        } finally IO.delete(root)
    }

    test("a workspace declares its library of proofs, <Package>Proofs, in its lakefile") {
        val root = Files.createTempDirectory("lean-layout-test").toFile
        try {
            val workspace = new File(root, "LinearVesting")
            assert(workspace.mkdirs())
            assert(LeanLayout.proofsLibrary(workspace) == "LinearVestingProofs")
            val lean = new File(workspace, "lakefile.lean")
            def declares(lakefile: File, text: String): Boolean = {
                Files.write(lakefile.toPath, text.getBytes("UTF-8"))
                LeanLayout.hasProofsLibrary(workspace)
            }
            assert(!LeanLayout.hasProofsLibrary(workspace))
            // As LeanWorkspaceTool writes it, and the library named without guillemets.
            assert(
              declares(
                lean,
                "lean_lib «LinearVesting» where\n\n" +
                    "lean_lib «LinearVestingProofs» where\n  roots := #[]\n"
              )
            )
            assert(declares(lean, "lean_lib LinearVestingProofs where\n"))
            assert(declares(lean, "lean_lib «LinearVestingProofs»"))
            // Only the workspace's own library, and not one that is named in a comment.
            val commented = "lean_lib «LinearVesting» where\n-- `lake build LinearVestingProofs`\n"
            assert(!declares(lean, commented))
            assert(!declares(lean, "lean_lib «LinearVestingProofsOld» where\n"))
            assert(!declares(lean, "lean_lib «OtherProofs» where\n"))
            assert(lean.delete())
            val toml = new File(workspace, "lakefile.toml")
            val tomlPackage = "name = \"LinearVesting\"\n\n[[lean_lib]]\n"
            assert(declares(toml, tomlPackage + "name = \"LinearVestingProofs\"\n"))
            assert(!declares(toml, tomlPackage + "name = \"LinearVesting\"\n"))
        } finally IO.delete(root)
    }

    test("a class path has scalus-verification where it has its workspace tool") {
        val root = Files.createTempDirectory("lean-layout-test").toFile
        try {
            val classes = new File(root, "classes")
            assert(new File(classes, "scalus/verify/lean").mkdirs())
            val other = new File(root, "other")
            assert(other.mkdirs())
            assert(!LeanLayout.hasLeanTool(Seq(other)))
            assert(!LeanLayout.hasLeanTool(Seq(other, classes)))
            val tool = new File(classes, "scalus/verify/lean/LeanWorkspaceTool$.class")
            Files.write(tool.toPath, Array[Byte](0))
            assert(LeanLayout.hasLeanTool(Seq(other, classes)))
            assert(!LeanLayout.hasLeanTool(Seq.empty))
        } finally IO.delete(root)
    }

    test("verify selects tests by name, by pattern, and by a class's simple name") {
        val names = Seq(
          "scalus.examples.htlc.HtlcVerificationTest",
          "scalus.examples.htlc.HtlcVerificationSupportTest",
          "scalus.examples.vesting.VestingVerificationTest"
        )
        def select(patterns: String*) = LeanLayout.selectTests(patterns, names)
        assert(
          select("HtlcVerificationTest") ==
              (Seq("scalus.examples.htlc.HtlcVerificationTest"), Nil)
        )
        assert(
          select("scalus.examples.vesting.VestingVerificationTest") ==
              (Seq("scalus.examples.vesting.VestingVerificationTest"), Nil)
        )
        assert(
          select("*VerificationTest") ==
              (Seq(
                "scalus.examples.htlc.HtlcVerificationTest",
                "scalus.examples.vesting.VestingVerificationTest"
              ),
              Nil)
        )
        assert(select("Htlc*") == (names.take(2).sorted, Nil))
        // A quote that the shell left on a name is not part of it.
        assert(
          select("*HtlcVerificationTest\"") ==
              (Seq("scalus.examples.htlc.HtlcVerificationTest"), Nil)
        )
        // A name that selects nothing is said.
        assert(select("Vesting", "Htlc") == (Nil, Seq("Vesting", "Htlc")))
        // A pattern with a `.` is a test's whole name, not a simple one.
        assert(select("htlc.HtlcVerificationTest") == (Nil, Seq("htlc.HtlcVerificationTest")))
    }

    test("verify runs the suites it is given, without the tests tagged Unfinished") {
        assert(
          LeanLayout.verifyArguments(Seq("a.HtlcTest", "b.VestingTest")) ==
              " a.HtlcTest b.VestingTest -- -l scalus.verify.uplcblaster.Unfinished"
        )
    }
}
