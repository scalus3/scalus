package scalus.verify.uplcblaster

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.cardano.onchain.plutus.prelude.{require, Math}
import scalus.uplc.{Constant, PlutusV3}
import scalus.uplc.Term.asTerm
import scalus.uplc.eval.{PlutusVM, Result}
import scalus.verify.*
import scalus.verify.Props.*
import scalus.verify.lean.{LeanServer, LeanServerProvider}
import scalus.verify.uplcblaster.UplcBlaster.LeafFormula

/** A success that implies tests, of values a statement quantifies over, is checked in the rejecting
  * form: `if claim then error else function` fails on every value, and the claim returns on every
  * value.
  */
class RejectingFormTest extends AnyFunSuite with LeanProofsSuite {
    import proofs.*

    private given PlutusVM = PlutusVM.makePlutusV3VM()

    /** Returns its argument where it is positive, and fails elsewhere. */
    private val positive = FunctionDef.named(
      "positive",
      (x: BigInt) => {
          require(x > BigInt(0), "not positive")
          x
      }
    )

    /** Returns its argument. */
    private val anything = FunctionDef.named("anything", (x: BigInt) => x)

    private def lowered(prop: Prop): UplcBlaster.Lowered =
        UplcBlaster.lower(prop, FunctionTable(positive, anything)).fold(fail(_), identity)

    test("a success that implies a test of values quantified over is in the rejecting form") {
        val goal = lowered(forAll[BigInt](x => succeeds(positive, x) ==> (x > BigInt(0))))
        // Lean reads that the claim returns, and that the program `if claim then error else
        // positive(x)` fails.
        goal.body match
            case LeafFormula.Forall(
                  _,
                  LeafFormula.And(
                    LeafFormula.Denotes(claim),
                    LeafFormula.Not(LeafFormula.Denotes(rejecting))
                  )
                ) =>
                assert(claim != rejecting)
            case other => fail(s"expected the rejecting form, got $other")
        // The replay decides by the statement as written, over the same binders.
        goal.unrewritten match
            case Some(written) =>
                assert(written.binders == goal.binders)
                written.body match
                    case LeafFormula.Forall(
                          _,
                          LeafFormula.Implies(LeafFormula.Test(_), LeafFormula.Test(_))
                        ) =>
                        ()
                    case other => fail(s"expected the statement as written, got $other")
            case None => fail("expected the statement as written")
        // A `denotes` of an expression is a success too, and a conjunction of tests is one claim,
        // `a && b`, with the premise in one program.
        val both = lowered(
          forAll[BigInt](x =>
              denotes(BigInt(10) / x) ==>
                  (Prop(x != BigInt(0)) && Prop(BigInt(10) / x <= BigInt(10)))
          )
        )
        both.body match
            case LeafFormula.Forall(
                  _,
                  LeafFormula.And(LeafFormula.Denotes(_), LeafFormula.Not(LeafFormula.Denotes(_)))
                ) =>
                ()
            case other => fail(s"expected one rejecting form of both tests, got $other")
        assert(both.leaves.size == 2 && both.unrewritten.isDefined)
    }

    test("an implication that is not a claim of a success is checked as written") {
        def asWritten(prop: Prop): Unit = {
            val goal = lowered(prop)
            assert(goal.unrewritten.isEmpty, goal.body)
        }
        // In a negative position, where the stronger rejecting form would prove less.
        asWritten(forAll[BigInt](x => !(succeeds(positive, x) ==> (x > BigInt(0)))))
        asWritten(
          forAll[BigInt](x => (succeeds(positive, x) ==> (x > BigInt(0))) ==> (x > BigInt(-5)))
        )
        asWritten(
          forAll[BigInt](x =>
              (succeeds(positive, x) ==> (x > BigInt(0))) <=> (x > BigInt(-1000000))
          )
        )
        // A test as the premise: the conclusion may fail where it is false.
        asWritten(forAll[BigInt](x => (x != BigInt(0)) ==> (BigInt(10) / x > BigInt(-100))))
        // A call whose continuation is a test, for the same reason.
        asWritten(
          forAll[BigInt](x =>
              call(positive, x)(r => r > BigInt(1)) ==> (BigInt(10) / (x - BigInt(1)) > BigInt(
                -100
              ))
          )
        )
        // A partial call: its continuation calls the function, which need not return.
        asWritten(forAll[BigInt](x => whenReturnsRef(positive.ref, x)(r => r > BigInt(0))))
        // A closed statement, which Lean evaluates.
        asWritten(succeeds(positive, BigInt(5)) ==> (BigInt(5) > BigInt(0)))
    }

    test("a claim of a success is proved and refuted in the rejecting form") {
        requireLean()
        provenAt(forAll[BigInt](x => succeeds(positive, x) ==> (x > BigInt(0))), positive)
        // positive accepts 1, where the claim is false.
        val one = refuted(
          forAll[BigInt](x => succeeds(positive, x) ==> (x > BigInt(1))),
          Budget.Auto,
          positive
        )
        assert(one.values.toList == List(Constant.Integer(1)), one)
        // A claim that fails where the function returns is false there, as the statement reads a
        // failing test.
        val zero = refuted(
          forAll[BigInt](x => succeeds(anything, x) ==> (BigInt(10) / x >= BigInt(-10))),
          Budget.Auto,
          anything
        )
        assert(zero.values.toList == List(Constant.Integer(0)), zero)
    }

    test("a claim that fails where the function fails too is checked as written instead") {
        requireLean()
        // At 0 the claim fails, and so does positive: the statement holds there, but the rejecting
        // form's program fails because the claim does, which proves nothing. The statement as
        // written is proved.
        provenAt(
          forAll[BigInt](x => succeeds(positive, x) ==> (BigInt(10) / x >= BigInt(0))),
          positive
        )
    }
    test("==>? is the rejecting form alone, in any position") {
        def rejecting(formula: LeafFormula): Boolean = formula match
            case LeafFormula.Not(LeafFormula.Denotes(_)) => true
            case _                                       => false
        val claimed = lowered(forAll[BigInt](x => succeeds(positive, x) ==>? (x > BigInt(0))))
        claimed.body match
            case LeafFormula.Forall(_, body) => assert(rejecting(body), body)
            case other                       => fail(s"expected a forAll, got $other")
        // The rejecting form is what it says: there is no other statement as written, and no claim
        // that the claim returns.
        assert(claimed.unrewritten.isEmpty)
        val negated = lowered(forAll[BigInt](x => !(succeeds(positive, x) ==>? (x > BigInt(0)))))
        negated.body match
            case LeafFormula.Forall(_, LeafFormula.Not(body)) => assert(rejecting(body), body)
            case other => fail(s"expected the negated rejecting form, got $other")
        // A test as the premise too.
        val tested =
            lowered(forAll[BigInt](x => (x != BigInt(0)) ==>? (BigInt(10) / x > BigInt(-100))))
        tested.body match
            case LeafFormula.Forall(_, body) => assert(rejecting(body), body)
            case other                       => fail(s"expected a forAll, got $other")
    }

    test("==>? holds where the claim fails, and is refuted where it is false") {
        requireLean()
        // At 0 the claim fails: with ==>? that is no counterexample, and the rejecting form proves
        // the statement at once.
        provenAt(
          forAll[BigInt](x => succeeds(positive, x) ==>? (BigInt(10) / x >= BigInt(0))),
          positive
        )
        // anything returns at 0, where the claim fails: ==> is refuted there, and ==>? holds.
        provenAt(
          forAll[BigInt](x => succeeds(anything, x) ==>? (BigInt(10) / x >= BigInt(-10))),
          anything
        )
        val one = refuted(
          forAll[BigInt](x => succeeds(positive, x) ==>? (x > BigInt(1))),
          Budget.Auto,
          positive
        )
        assert(one.values.toList == List(Constant.Integer(1)), one)
    }
    test("a function a rejecting program calls is the registered program") {
        // A program that is not the one `Math.clamp` compiles to, registered under its name: the
        // claim and the premise inside the rejecting program call it, and not a copy made from the
        // source.
        val standIn = FunctionDef(Math.clamp).withRepresentation(
          Representation.Uplc,
          PlutusV3
              .compile((x: BigInt, lo: BigInt, hi: BigInt) => BigInt(-1))(using
                UplcBlaster.options
              )
              .program
        )
        val goal = UplcBlaster
            .lower(
              forAll[BigInt](x =>
                  denotes(Math.clamp(x, BigInt(0), BigInt(9))) ==>
                      (Math.clamp(x, BigInt(0), BigInt(9)) == BigInt(-1))
              ),
              FunctionTable(standIn)
            )
            .fold(fail(_), identity)
        goal.body match
            case LeafFormula.Forall(
                  _,
                  LeafFormula.And(_, LeafFormula.Not(LeafFormula.Denotes(rejecting)))
                ) =>
                // The registered program returns -1, so the claim holds, and the program fails.
                (goal.leaves(rejecting) $ BigInt(5).asTerm).term.evaluateDebug match
                    case _: Result.Failure => ()
                    case success: Result.Success =>
                        fail(s"the rejecting program returned ${success.term}: it ran a copy")
            case other => fail(s"expected the rejecting form, got $other")
    }

    test("within a budget, a claim of a success is checked as written") {
        // A run the budget cuts counts for the statement, and the rejecting program runs longer
        // than the premise: the bound would not be the one the statement says.
        val nothing = new LeanServerProvider {
            override def server(): Either[String, LeanServer] = Left("Lean was asked")
            override def workspace: Option[java.nio.file.Path] = None
        }
        val statement = Verifier.empty.statement(
          forAll[BigInt](x => succeeds(positive, x) ==> (x > BigInt(0)))
        )
        UplcBlaster(Budget.Within(100), nothing)
            .prepare(Goal(statement, FunctionTable(positive), Nil)) match
            case Right(goal) =>
                assert(goal.within.contains(100) && goal.unrewritten.isEmpty)
                goal.body match
                    case LeafFormula.Forall(
                          _,
                          LeafFormula.Implies(LeafFormula.Test(_), LeafFormula.Test(_))
                        ) =>
                        ()
                    case other => fail(s"expected the statement as written, got $other")
            case Left(report) => fail(report.toString)
    }

    test("read with ==> on a value, a claim of ==>? that fails there is false") {
        // `==>?` holds where its claim fails, so this is proved of every value, vacuously: the
        // claim fails wherever `positive` returns. Read with `==>` at 5, which `positive` accepts,
        // it is false; and a claim that holds there is true.
        def at(prop: Prop, x: BigInt): Option[Boolean] =
            UplcBlaster
                .holdsAsImplicationOn(prop, FunctionTable(positive), List(Constant.Integer(x)))
                .fold(fail(_), identity)
        val vacuous =
            forAll[BigInt](x => succeeds(positive, x) ==>? (BigInt(10) / (x - x) > BigInt(0)))
        assert(at(vacuous, BigInt(5)) == Some(false))
        assert(
          at(forAll[BigInt](x => succeeds(positive, x) ==>? (x > BigInt(0))), BigInt(5)) == Some(
            true
          )
        )
        // Where the function does not accept the value, the statement holds there either way.
        assert(at(vacuous, BigInt(-5)) == Some(true))
    }

    test("a result the statement as written decided says so, and keeps it") {
        val goal = lowered(forAll[BigInt](x => succeeds(positive, x) ==> (x > BigInt(0))))
        val artifact = UplcBlaster.Artifact(
          goal.programHashes,
          120,
          "",
          Nil,
          "",
          ProofKind.Blaster,
          kept = false
        )
        val proved: ExecutionResult = VerificationResult.Proven(Proof(artifact))
        val asWritten = UplcBlaster.markedAsWritten(proved)
        val kept = UplcBlaster.kept(goal, asWritten).getOrElse(fail("nothing kept"))
        assert(kept.notes.get(UplcBlaster.formNote).contains(UplcBlaster.asWrittenForm), kept.notes)
        UplcBlaster.restored(goal, kept) match
            case Right(VerificationResult.Proven(proof)) =>
                assert(proof.artifact.asInstanceOf[UplcBlaster.Artifact].asWritten)
            case other => fail(s"expected the kept proof, got $other")
        // The rejecting form's own result keeps no such note.
        val own = UplcBlaster.kept(goal, proved).getOrElse(fail("nothing kept"))
        assert(!own.notes.contains(UplcBlaster.formNote), own.notes)
    }
}
