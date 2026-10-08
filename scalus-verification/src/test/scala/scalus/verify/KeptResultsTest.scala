package scalus.verify

import org.scalatest.funsuite.AnyFunSuite

import java.nio.file.{Files, Path}

/** The results a verifier keeps, with a tactic that only counts how often it is asked: no backend
  * runs.
  */
class KeptResultsTest extends AnyFunSuite {

    private object Evidence extends ProofArtifact {
        override def kind: ProofKind = ProofKind.Blaster
    }

    /** A tactic whose answer and whose fingerprint the test sets. */
    private final class Counting extends Tactic {
        override type Prepared = Goal

        var runs = 0
        var started = List.empty[Kept]
        var answer: () => ExecutionResult = () => proved
        var backend = "1.0"
        var text = "s-1"
        var programs = List("p-1", "p-2")
        var found = "5"
        var stands = true

        def proved: ExecutionResult = VerificationResult.Proven(Proof(Evidence, Nil))
        def refuted: ExecutionResult = VerificationResult.Refuted(Proof(Evidence, Nil))

        override val name: String = "counting"
        override def prepare(goal: Goal): Either[CompatibilityReport, Prepared] = Right(goal)
        override def run(prepared: Prepared): ExecutionResult = {
            runs += 1
            answer()
        }
        override def run(prepared: Prepared, earlier: Kept): ExecutionResult = {
            started = earlier :: started
            run(prepared)
        }
        override def fingerprint(prepared: Prepared): Option[Fingerprint] =
            Some(Fingerprint(Map("backend" -> backend, "library" -> "7"), text, programs))
        override def keep(prepared: Prepared, result: ExecutionResult): Option[Kept] = result match
            case VerificationResult.Proven(_) =>
                Some(Kept(Kept.Result.Proved, Map("budget" -> "160"), Map.empty))
            case VerificationResult.Refuted(_) =>
                Some(
                  Kept(Kept.Result.Refuted, Map.empty, Map("x" -> found, "d" -> """{"int":3}"""))
                )
            case _ => None
        override def restore(prepared: Prepared, kept: Kept): Option[ExecutionResult] =
            kept.result match
                case Kept.Result.Proved => Some(proved)
                case Kept.Result.Refuted =>
                    Option.when(stands && kept.counterexample.contains("x"))(refuted)
    }

    /** A test with a file for the results, which is not there at its start. */
    private def kept(name: String)(body: Path => Unit): Unit = test(name) {
        val directory = Files.createTempDirectory("scalus-kept-results-")
        try body(directory.resolve("Suite.proofs.json"))
        finally {
            val left = Files.list(directory)
            try left.forEach(Files.delete(_))
            finally left.close()
            Files.delete(directory)
        }
    }

    /** One statement, named `name`, verified in a verifier of its own, as a test does. */
    private def verify(
        file: Path,
        mode: KeptResults.Mode,
        tactic: Tactic,
        name: String = "holds"
    ): VerificationResult = {
        val verifier = Verifier.keeping(KeptResults.in(file, mode).under("a test"))
        verifier.verify(verifier.statement(name, Prop(true)), tactic)
    }

    private def proved(result: VerificationResult): Boolean =
        result.isInstanceOf[VerificationResult.Proven]

    kept("a result is asked for once, and is the result from then on") { file =>
        val tactic = new Counting
        assert(proved(verify(file, KeptResults.Mode.Use, tactic)))
        assert(tactic.runs == 1)
        // A later run, in another verifier, takes what the file has.
        assert(proved(verify(file, KeptResults.Mode.Use, tactic)))
        assert(proved(verify(file, KeptResults.Mode.Frozen, tactic)))
        assert(tactic.runs == 1)

        // The file reads as text: the backend's side once, and the entry under the name of its
        // test and statement, with the parts of its fingerprint.
        assert(
          Files.readString(file) ==
              """{
                |  "environment": {
                |    "counting": {
                |      "backend": "1.0",
                |      "library": "7"
                |    }
                |  },
                |  "results": {
                |    "a test holds": {
                |      "tactic": "counting",
                |      "result": "proved",
                |      "statement": "s-1",
                |      "programs": [
                |        "p-1",
                |        "p-2"
                |      ],
                |      "notes": {
                |        "budget": "160"
                |      }
                |    }
                |  }
                |}
                |""".stripMargin
        )
    }

    kept("a statement that changed is asked for again, and its entry replaced") { file =>
        val tactic = new Counting
        verify(file, KeptResults.Mode.Use, tactic)
        tactic.programs = List("p-1", "p-9")
        assert(proved(verify(file, KeptResults.Mode.Use, tactic)))
        assert(tactic.runs == 2)
        // The run did not start from what was kept for other programs.
        assert(tactic.started.isEmpty)
        val text = Files.readString(file)
        assert(text.contains("\"p-9\"") && !text.contains("\"p-2\""), text)
        // And kept again for the programs as they are now.
        verify(file, KeptResults.Mode.Use, tactic)
        assert(tactic.runs == 2)
    }

    kept("a refutation is kept with its counterexample, and checked again when it is taken") {
        file =>
            val tactic = new Counting
            tactic.answer = () => tactic.refuted
            assert(
              verify(file, KeptResults.Mode.Use, tactic).isInstanceOf[VerificationResult.Refuted]
            )
            val text = Files.readString(file)
            assert(text.contains("\"result\": \"refuted\""), text)
            // The values are JSON in the file, not text in a string.
            assert(text.contains("\"x\": 5"), text)
            assert(text.contains("\"d\": {\"int\":3}"), text)
            assert(
              verify(file, KeptResults.Mode.Frozen, tactic).isInstanceOf[VerificationResult.Refuted]
            )
            assert(tactic.runs == 1)
            // Another result that is kept writes the file again, and leaves this one as it was.
            verify(file, KeptResults.Mode.Use, tactic, "is false too")
            val again = Files.readString(file)
            assert(again.contains("\"x\": 5") && again.contains("\"d\": {\"int\":3}"), again)
            assert(again.linesIterator.count(_.contains("\"x\": 5")) == 2, again)
    }

    kept("without a backend, a statement nothing is kept for is stale, and says why") { file =>
        val tactic = new Counting
        def stale(): String = verify(file, KeptResults.Mode.Frozen, tactic) match
            case VerificationResult.Inconclusive(reason) =>
                assert(reason.startsWith(KeptResults.stale), reason)
                reason
            case other => fail(s"expected a stale statement, got $other")

        assert(stale().contains("no result of counting is kept for it"))
        verify(file, KeptResults.Mode.Use, tactic)
        assert(tactic.runs == 1)

        // Each part of the fingerprint that moved is named.
        tactic.text = "s-2"
        assert(stale().contains("was proved with another the statement"))
        tactic.text = "s-1"
        tactic.programs = List("p-1", "p-9")
        assert(stale().contains("with another the program of test 2"))
        tactic.programs = List("p-1", "p-2")
        tactic.backend = "2.0"
        assert(stale().contains("with another backend of the backend"))
        tactic.backend = "1.0"
        // No backend was asked, and the file is as it was.
        assert(tactic.runs == 1)
        assert(proved(verify(file, KeptResults.Mode.Frozen, tactic)))
    }

    kept("a run that recalculates asks every time, and fails where the result is another") { file =>
        val tactic = new Counting
        verify(file, KeptResults.Mode.Use, tactic)
        assert(proved(verify(file, KeptResults.Mode.Recalculate, tactic)))
        assert(tactic.runs == 2)
        // It started from what was kept: the budget, for a tactic that searches for one.
        assert(tactic.started.map(_.notes) == List(Map("budget" -> "160")))

        // The proof no longer comes out.
        tactic.answer = () => VerificationResult.Inconclusive("the solver did not answer")
        val before = Files.readString(file)
        verify(file, KeptResults.Mode.Recalculate, tactic) match
            case VerificationResult.Failed(reason) =>
                assert(reason.startsWith("the result kept for holds is Proved, and this run gives"))
                assert(reason.contains("the solver did not answer"), reason)
            case other => fail(s"expected a failure, got $other")
        tactic.answer = () => tactic.refuted
        assert(
          verify(file, KeptResults.Mode.Recalculate, tactic).isInstanceOf[VerificationResult.Failed]
        )
        // What is kept stays as it was.
        assert(Files.readString(file) == before)
    }

    kept("a run that recalculates leaves an entry as it is where the result comes out again") {
        file =>
            val tactic = new Counting
            tactic.answer = () => tactic.refuted
            verify(file, KeptResults.Mode.Use, tactic)
            val before = Files.readString(file)
            // This run comes to another counterexample of the same statement: a solver gives
            // any of them. The file is not written for it, so one that such a run did write had
            // an entry that was missing or stale.
            tactic.found = "-7"
            assert(
              verify(file, KeptResults.Mode.Recalculate, tactic)
                  .isInstanceOf[VerificationResult.Refuted]
            )
            assert(tactic.runs == 2)
            assert(Files.readString(file) == before)
            // A statement nothing was kept for is kept by it.
            verify(file, KeptResults.Mode.Recalculate, tactic, "new")
            assert(Files.readString(file) != before)
    }

    kept(
      "what is kept and no longer stands is stale, with every part of its fingerprint as it was"
    ) { file =>
        val tactic = new Counting
        tactic.answer = () => tactic.refuted
        verify(file, KeptResults.Mode.Use, tactic)
        // The tactic does not stand for the kept counterexample any more: it is none of the
        // statement as it is now, though the fingerprint is the same.
        tactic.stands = false
        verify(file, KeptResults.Mode.Frozen, tactic) match
            case VerificationResult.Inconclusive(reason) =>
                assert(reason.startsWith(KeptResults.stale), reason)
                assert(reason.endsWith("does not stand for the statement as it is now"), reason)
            case other => fail(s"expected a stale statement, got $other")
        // With a backend it is asked again.
        assert(
          verify(file, KeptResults.Mode.Use, tactic).isInstanceOf[VerificationResult.Refuted]
        )
        assert(tactic.runs == 2)
    }

    kept("another side of the backend takes the results that were kept for the old one") { file =>
        val tactic = new Counting
        verify(file, KeptResults.Mode.Use, tactic, "first")
        verify(file, KeptResults.Mode.Use, tactic, "second")
        tactic.backend = "2.0"
        verify(file, KeptResults.Mode.Use, tactic, "first")
        val text = Files.readString(file)
        assert(text.contains("\"backend\": \"2.0\"") && !text.contains("\"1.0\""), text)
        // The other statement was proved with the old backend, and is not said to be proved
        // with this one.
        assert(text.contains("a test first") && !text.contains("a test second"), text)
    }

    kept("a tactic whose results are not kept runs as before") { file =>
        var runs = 0
        val plain = new Tactic {
            override type Prepared = Goal
            override val name: String = "plain"
            override def prepare(goal: Goal): Either[CompatibilityReport, Prepared] = Right(goal)
            override def run(prepared: Prepared): ExecutionResult = {
                runs += 1
                VerificationResult.Proven(Proof(Evidence, Nil))
            }
        }
        assert(proved(verify(file, KeptResults.Mode.Frozen, plain)))
        assert(proved(verify(file, KeptResults.Mode.Use, plain)))
        assert(runs == 2)
        assert(!Files.exists(file))
    }

    test("the hash of a fingerprint is short, and the same for the same bytes") {
        assert(Fingerprint.hash("statement") == Fingerprint.hash("statement"))
        assert(Fingerprint.hash("statement") != Fingerprint.hash("statements"))
        assert(Fingerprint.hash("statement").matches("[0-9a-f]{16}"))
    }
}
