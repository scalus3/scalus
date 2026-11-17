package scalus.verify.uplcblaster

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.onchain.plutus.prelude.Math
import scalus.verify.{FunctionDef, FunctionTable, ProofKind}
import scalus.verify.Props.*
import scalus.verify.lean.LeanServer.{Message, Severity}

import java.nio.file.Files

/** What the tactic `lean-manual` reads of Lean's answer, and what it writes, without Lean. Its
  * checks with Lean are those of the examples' proofs by hand (`AmmVerificationTest`).
  */
class LeanManualTest extends AnyFunSuite {

    private val statementModule = "Vault.Generated.clamp_in_range"

    test("the axioms are read from #print axioms, also where Lean breaks the list into lines") {
        val printed = List(
          Message(3, Severity.Information, "'Vault.Proofs.one' depends on axioms: [propext]"),
          Message(
            5,
            Severity.Information,
            "'Vault.Proofs.two' depends on axioms: [propext,\n Classical.choice,\n Quot.sound]"
          ),
          Message(7, Severity.Information, "'Vault.Proofs.three' does not depend on any axioms")
        )
        assert(LeanManual.axiomsOf(printed, "Vault.Proofs.one").contains(List("propext")))
        assert(
          LeanManual
              .axiomsOf(printed, "Vault.Proofs.two")
              .contains(List("propext", "Classical.choice", "Quot.sound"))
        )
        assert(LeanManual.axiomsOf(printed, "Vault.Proofs.three").contains(Nil))
        assert(LeanManual.axiomsOf(printed, "Vault.Proofs.four").isEmpty)
    }

    test("the axioms say what a proof rests on") {
        def judged(axioms: String*) =
            LeanManual.judgedAxioms(axioms.toList, "Vault", statementModule, "Vault.Proofs.p")
        val lean = List("propext", "Classical.choice", "Quot.sound")
        assert(judged(lean*) == Right(ProofKind.LeanKernelOfOptimizedRuns -> Nil))
        assert(judged() == Right(ProofKind.LeanKernelOfOptimizedRuns -> Nil))
        // Blaster's axiom proves anything where it is used without the tactic: a proof by hand
        // cites what Blaster proved by a statement's `proved` instead.
        assert(
          judged((lean :+ "Blaster.Tactic.blasterProven")*).left.exists(_.contains("blasterProven"))
        )
        assert(judged((lean :+ "Lean.ofReduceBool")*) == Right(ProofKind.LeanNative -> Nil))
        // A result Scalus kept, cited as a lemma, by the module of its statement.
        assert(
          judged((lean :+ "Vault.Generated.clamp_returns.proved")*) ==
              Right(ProofKind.LeanKernelOfOptimizedRuns -> List("clamp_returns"))
        )
        // No proof: sorry, the result of its own statement, or an axiom of someone else's.
        assert(judged((lean :+ "sorryAx")*).left.exists(_.contains("sorry")))
        assert(
          judged((lean :+ s"$statementModule.proved")*).left.exists(_.contains("its own statement"))
        )
        assert(judged((lean :+ "Other.trustMe")*).left.exists(_.contains("Other.trustMe")))
        assert(
          judged((lean :+ "Other.Generated.clamp_returns.proved")*).left.exists(
            _.contains("Other.Generated.clamp_returns.proved")
          )
        )
    }

    test("the check of a proof by hand imports it, states its type, and prints its axioms") {
        val text = LeanManual.checkText("Vault.Proofs.Clamp", statementModule, "Vault.Proofs.p")
        assert(
          text.startsWith("import Vault.Proofs.Clamp\nimport Vault.Generated.clamp_in_range\n")
        )
        assert(text.contains(s"example : $statementModule.statement := Vault.Proofs.p"))
        assert(text.contains("#print axioms Vault.Proofs.p"))
    }

    test("each result a proof cites is checked to be the axiom of its statement's module") {
        val text = LeanManual.citationText(
          "Vault.Proofs.Clamp",
          List("Vault.Generated.clamp_returns", "Vault.Generated.clamp_total")
        )
        assert(text.startsWith("import Vault.Proofs.Clamp\nimport Vault.Generated.clamp_returns\n"))
        assert(
          text.contains(
            "example : Vault.Generated.clamp_returns.statement := Vault.Generated.clamp_returns.proved"
          )
        )
        assert(
          text.contains(
            "example : Vault.Generated.clamp_total.statement := Vault.Generated.clamp_total.proved"
          )
        )
    }

    test("a statement to be proved by hand is written without the axiom of a proof") {
        val clamp = FunctionDef(Math.clamp)
        val inRange = forAll[BigInt, BigInt, BigInt]((x, lo, hi) =>
            (lo <= hi) ==>: callRef(clamp.ref, (x, lo, hi))(r => lo <= r && r <= hi)
        )
        val lowered = UplcBlaster.lower(inRange, FunctionTable(clamp)).fold(fail(_), identity)
        val open = UplcBlaster.statementModule(lowered, 120, None, 400000, statementModule, "p")
        assert(!open.text.contains("axiom proved"), open.text)
        assert(open.text.contains("it is written to be proved by hand"), open.text)
        // The header is read as that of a module of a proved statement.
        assert(UplcBlaster.statementModuleOf(open.text) == Some(120 -> lowered.programHashes))
        val proved = UplcBlaster.statementModule(
          lowered,
          120,
          Some(ProofKind.Blaster),
          400000,
          statementModule,
          "p"
        )
        assert(proved.text.contains("axiom proved : statement"), proved.text)
        assert(!proved.text.contains("to be proved by hand"), proved.text)
    }

    test("the Lean files of the proofs by hand are hashed apart from the workspace's environment") {
        val root = Files.createTempDirectory("scalus-lean-manual")
        try
            val workspace = root.resolve("Vault")
            val proofs = workspace.resolve("Vault").resolve("Proofs")
            Files.createDirectories(proofs)
            Files.writeString(workspace.resolve("lean-toolchain"), "leanprover/lean4:v4.24.0\n")
            val before = LeanManual.proofsHash(workspace)
            Files.writeString(proofs.resolve("Clamp.lean"), "theorem one : 1 = 1 := rfl\n")
            val written = LeanManual.proofsHash(workspace)
            assert(written != before)
            Files.writeString(proofs.resolve("Clamp.lean"), "theorem two : 2 = 2 := rfl\n")
            val changed = LeanManual.proofsHash(workspace)
            assert(changed != written)
            // A file that is no Lean source, as an editor's or Finder's, changes nothing, and is
            // not read: it need not be text.
            Files.write(proofs.resolve(".DS_Store"), Array[Byte](0, -1, -2, 0x7f))
            Files.writeString(proofs.resolve("README.md"), "What belongs here.\n")
            assert(LeanManual.proofsHash(workspace) == changed)
        finally scalus.verify.lean.Directories.remove(root)
    }
}
