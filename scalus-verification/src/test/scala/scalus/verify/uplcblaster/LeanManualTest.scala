package scalus.verify.uplcblaster

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.onchain.plutus.prelude.Math
import scalus.verify.{FunctionDef, FunctionTable, ProofKind}
import scalus.verify.Props.*
import scalus.verify.lean.LeanServer.{Message, Progress, Reached, Severity, Started}

import java.nio.file.Files
import scala.concurrent.duration.*

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
        // A message of a form not read here is none, not a theorem of no axioms.
        val other =
            List(Message(9, Severity.Information, "'Vault.Proofs.five' depends on: sorryAx"))
        assert(LeanManual.axiomsOf(other, "Vault.Proofs.five").isEmpty)
    }

    test("the axioms say what a proof rests on") {
        def judged(axioms: String*) =
            LeanManual.judgedAxioms(
              axioms.toList,
              "Vault",
              statementModule,
              "Vault.Proofs.p",
              false
            )
        val lean = List("propext", "Classical.choice", "Quot.sound")
        assert(judged(lean*) == Right(ProofKind.LeanKernelOfOptimizedRuns -> Nil))
        assert(judged() == Right(ProofKind.LeanKernelOfOptimizedRuns -> Nil))
        // Blaster's axiom proves anything where it is used without the tactic: a proof by hand
        // cites what Blaster proved by a statement's `proved` instead.
        assert(
          judged((lean :+ "Blaster.Tactic.blasterProven")*).left.exists(_.contains("blasterProven"))
        )
        assert(judged((lean :+ "Lean.ofReduceBool")*) == Right(ProofKind.LeanNative -> Nil))
        // Of a statement of runs of any number of steps there is no optimized run: a proof on
        // Lean's axioms is of the runs as Lean defines them.
        assert(
          LeanManual.judgedAxioms(lean, "Vault", statementModule, "Vault.Proofs.p", true) ==
              Right(ProofKind.LeanKernel -> Nil)
        )
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

    test("a check given up at its time limit says how far it had come, and what its worker did") {
        // As on CI on 2026-11-20: a check that takes seconds, given up after 30 minutes.
        val stalled = LeanManual.unfinished(
          "Vault.Proofs.p",
          30.minutes,
          3.seconds,
          Progress(
            Reached.Command(s"example : $statementModule.statement := Vault.Proofs.p"),
            1797.seconds,
            List(Started("lean", 1797.seconds, Some(4.seconds)))
          )
        )
        assert(stalled.startsWith("Lean did not check Vault.Proofs.p within 30 minutes"), stalled)
        assert(stalled.contains("3 s of it went to Lake's builds"), stalled)
        assert(
          stalled.contains(
            s"at `example : $statementModule.statement := Vault.Proofs.p` for 1797 s"
          ),
          stalled
        )
        assert(stalled.contains("lean had run for 1797 s and worked for 4 s"), stalled)
        // Where the system does not say how long a process worked, as macOS does not.
        val waited = LeanManual.unfinished(
          "Vault.Proofs.p",
          30.minutes,
          0.seconds,
          Progress(Reached.NotStarted, 0.seconds, List(Started("lean", 10.seconds, None)))
        )
        assert(waited.contains("its server was at another check"), waited)
        assert(waited.endsWith("lean had run for 10 s"), waited)
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

    test("a statement of runs of any number of steps is written of Lean's runs, with no budget") {
        val clamp = FunctionDef(Math.clamp)
        val inRange = forAll[BigInt, BigInt, BigInt]((x, lo, hi) =>
            (lo <= hi) ==>: callRef(clamp.ref, (x, lo, hi))(r => lo <= r && r <= hi)
        )
        val lowered = UplcBlaster.lower(inRange, FunctionTable(clamp)).fold(fail(_), identity)
        val written = UplcBlaster.anyRunStatementModule(lowered, 400000, statementModule, "p")
        assert(written.text.contains("at any number of steps of the machine"), written.text)
        assert(written.text.contains("it is written to be proved by hand"), written.text)
        assert(!written.text.contains("axiom proved"), written.text)
        // Each test is read of the run itself, which `#prep_uplc_run` does not optimize.
        assert(written.text.contains("ScalusProofs.Runs.returns"), written.text)
        assert(written.text.contains("import ScalusProofs.Runs"), written.text)
        assert(!written.text.contains("#prep_uplc_run"), written.text)
        assert(UplcBlaster.statementModuleOf(written.text).isEmpty, "no budget in the header")
        assert(written.programs.map(_._2) == lowered.leaves)
    }

    test("a proof's sources are its module and the proofs it imports, and only they are hashed") {
        val root = Files.createTempDirectory("scalus-lean-manual")
        try
            val workspace = root.resolve("Vault")
            val proofs = workspace.resolve("Vault").resolve("Proofs")
            Files.createDirectories(proofs)
            Files.writeString(workspace.resolve("lean-toolchain"), "leanprover/lean4:v4.24.0\n")
            val clamp = proofs.resolve("Clamp.lean")
            val lemmas = proofs.resolve("Lemmas.lean")
            val other = proofs.resolve("Other.lean")
            Files.writeString(lemmas, "theorem one : 1 = 1 := rfl\n")
            Files.writeString(other, "theorem two : 2 = 2 := rfl\n")
            Files.writeString(
              clamp,
              "import Vault.Generated.clamp_in_range\nimport Vault.Proofs.Lemmas\n\n" +
                  "theorem p : 1 = 1 := one\n"
            )
            assert(
              LeanManual.proofSources(workspace, "Vault.Proofs.Clamp").toSet == Set(clamp, lemmas)
            )
            val before = LeanManual.proofsHash(workspace, "Vault.Proofs.Clamp")
            // A proof it does not import changes nothing, nor does a file that is no Lean source,
            // as an editor's or Finder's, which is not read, and need not be text.
            Files.writeString(other, "theorem three : 3 = 3 := rfl\n")
            Files.write(proofs.resolve(".DS_Store"), Array[Byte](0, -1, -2, 0x7f))
            Files.writeString(proofs.resolve("README.md"), "What belongs here.\n")
            assert(LeanManual.proofsHash(workspace, "Vault.Proofs.Clamp") == before)
            // One it imports does.
            Files.writeString(lemmas, "theorem one : 1 = 1 := by rfl\n")
            assert(LeanManual.proofsHash(workspace, "Vault.Proofs.Clamp") != before)

            // A source that turns off Lean's kernel check makes the proof none.
            assert(LeanManual.skippingTheKernel(workspace, "Vault.Proofs.Clamp").isEmpty)
            Files.writeString(
              lemmas,
              "set_option debug.skipKernelTC true in\ntheorem one : 1 = 1 := rfl\n"
            )
            assert(LeanManual.skippingTheKernel(workspace, "Vault.Proofs.Clamp").contains(lemmas))
        finally scalus.verify.lean.Directories.remove(root)
    }

    test("a module is held to the code by its text and by its programs") {
        val clamp = FunctionDef(Math.clamp)
        val inRange = forAll[BigInt, BigInt, BigInt]((x, lo, hi) =>
            (lo <= hi) ==>: callRef(clamp.ref, (x, lo, hi))(r => lo <= r && r <= hi)
        )
        val lowered = UplcBlaster.lower(inRange, FunctionTable(clamp)).fold(fail(_), identity)
        val root = Files.createTempDirectory("scalus-lean-manual")
        try
            val written = UplcBlaster.statementModule(
              lowered,
              120,
              Some(ProofKind.Blaster),
              400000,
              statementModule,
              "Vault/Generated/clamp_in_range"
            )
            val file = root.resolve("Vault/Generated/clamp_in_range.lean")
            assert(written.unlike(root, file).exists(_.contains("not in the workspace")))
            Files.createDirectories(root.resolve("Vault/Generated/clamp_in_range"))
            Files.writeString(file, written.text)
            written.programs.foreach((path, program) =>
                Files.writeString(root.resolve(path), UplcBlaster.programHex(program))
            )
            assert(written.unlike(root, file).isEmpty)
            // A module of the same header without the axiom of its proof, as one written before it.
            Files.writeString(file, written.text.replace("axiom proved : statement", ""))
            assert(written.unlike(root, file).exists(_.contains("not the statement's")))
            Files.writeString(file, written.text)
            Files.writeString(root.resolve(written.programs.head._1), "00")
            assert(written.unlike(root, file).exists(_.contains("programs")))
        finally scalus.verify.lean.Directories.remove(root)
    }
}
