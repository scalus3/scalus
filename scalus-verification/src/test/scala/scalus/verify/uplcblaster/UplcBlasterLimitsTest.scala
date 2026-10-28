package scalus.verify.uplcblaster

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
// Wildcard, not a selective import: `List.filter` and friends are extension methods that a
// selective import would leave out of scope. This shadows scala.List and scala.Option here.
import scalus.cardano.onchain.plutus.prelude.*
import scalus.uplc.builtin.Data
import scalus.verify.*
import scalus.verify.Props.*
import scalus.verify.lean.{Directories, LeanServer, LeanServerProvider}

import java.nio.file.Files
import scala.concurrent.duration.*

/** Statements that are natural to write and that [[UplcBlaster]] cannot settle: their programs
  * loop, and how long the loop runs depends on a quantified value.
  *
  * None of them is provable at any budget, because some input needs more steps than the budget;
  * that takes induction. What differs is the cost of finding that out: seconds for a loop that only
  * walks, and no result at all once each round of the loop makes a choice, or leaves arithmetic to
  * the solver. The timings are in `docs/design/verification-details/uplc-blaster.md`.
  *
  * A test here that starts to fail because Lean finishes is good news: the statement then needs a
  * larger budget, or is no longer a limit.
  */
class UplcBlasterLimitsTest extends AnyFunSuite with LeanProofsSuite {
    import proofs.*

    /** The time a statement that Lean can finish gets. They take seconds. */
    private val finishes = 2.minutes

    /** The time before a statement that Lean cannot finish is given up. They gave no result in
      * minutes where this was written.
      */
    private val givenUp = 15.seconds

    private val length = FunctionDef.named("length", (xs: List[BigInt]) => xs.length)

    private val positives =
        FunctionDef.named("positives", (xs: List[BigInt]) => xs.filter(_ > BigInt(0)).length)

    private val gcd = FunctionDef(Math.gcd)

    private def lengthIsNotNegative =
        forAll[Data](d => whenReturns(length, d.to[List[BigInt]])(n => n >= BigInt(0)))

    private def filterDoesNotLengthen = forAll[Data](d =>
        whenReturns(positives, d.to[List[BigInt]])(n => n <= d.to[List[BigInt]].length)
    )

    private def gcdIsNotNegative =
        forAll[BigInt, BigInt]((x, y) => call(gcd, (x, y))(r => r >= BigInt(0)))

    test("within a budget, a loop over a whole list is proved of the lists the budget reaches") {
        // A run the budget cuts is read in the statement's favour, so the statement is proved of
        // the lists whose runs end within the budget: a proof within it, which is no theorem.
        assert(provenWithin(lengthIsNotNegative, 400, length).budget == 400)
        provenWithin(filterDoesNotLengthen, 100, positives)
        // A counterexample is one whose runs ended, and is replayed as any other.
        val short = forAll[Data](d => whenReturns(length, d.to[List[BigInt]])(n => n < BigInt(2)))
        refuted(short, Budget.Within(400), length).values.toList match
            case scala.List(scalus.uplc.Constant.Data(Data.List(items))) =>
                assert(items.length >= BigInt(2), items)
            case other => fail(s"expected one list, got $other")
        // A run the budget cuts is no witness: a statement that asks for one is not checked so.
        val own = Verifier.empty
        own.prepare(
          own.statement(exists[BigInt](x => x == BigInt(1))),
          UplcBlaster(Budget.Within(100), lean)
        ) match
            case Left(report) => assert(report.toString.contains("stand for every value"), report)
            case Right(_)     => fail("expected the statement to be refused")
    }

    test("with limits, a loop over a whole list is proved of the lists that meet them") {
        // Each test checks the limits first, by a walk that ends, and fails beyond them, where the
        // limits, the premise, are false. Every run ends, so the budget a search finds proves the
        // statement itself: a theorem, not a proof within a budget.
        val atMostFive = forAllDataLimited[List[BigInt]](xs => xs.length <= 5)(xs =>
            whenReturns(length, xs)(n => n <= BigInt(5))
        )
        assert(provenAt(atMostFive, length) > 0)
        // A counterexample meets the limits: a list of five elements.
        val atMostFour = forAllDataLimited[List[BigInt]](xs => xs.length <= 5)(xs =>
            whenReturns(length, xs)(n => n <= BigInt(4))
        )
        refuted(atMostFour, Budget.Auto, length).values.toList match
            case scala.List(scalus.uplc.Constant.Data(Data.List(items))) =>
                assert(items.length == BigInt(5), items)
            case other => fail(s"expected one list, got $other")
        // A least length, as a most.
        val atLeastTwo = forAllDataLimited[List[BigInt]](xs => xs.length >= 2 && xs.length <= 3)(
          xs => whenReturns(length, xs)(n => n >= BigInt(2))
        )
        assert(provenAt(atLeastTwo, length) > 0)
    }

    test("a limit of each element of a list is checked with the list's own") {
        val total = FunctionDef.named(
          "total",
          (xss: List[List[BigInt]]) => xss.foldLeft(BigInt(0))((sum, xs) => sum + xs.length)
        )
        val atMostFour = forAllDataLimited[List[List[BigInt]]](xss =>
            xss.length <= 2 && xss.forall(xs => xs.length <= 2)
        )(xss => whenReturns(total, xss)(n => n <= BigInt(4)))
        assert(provenAt(atMostFour, total) > 0)
    }

    test("a proof within a budget is kept for its budget, apart from a proof") {
        // A provider that says its workspace and has no server: whatever asks Lean fails.
        val silent = new LeanServerProvider {
            override def server(): Either[String, LeanServer] = Left("Lean was asked")
            override def workspace: scala.Option[java.nio.file.Path] = scala.Some(leanWorkspace)
        }
        val directory = Files.createTempDirectory("scalus-kept-")
        val file = directory.resolve("Within.proofs.json")
        // The statement, in a verifier of its own that keeps its results as `mode` says.
        def stated(mode: KeptResults.Mode): (Verifier, Statement) = {
            val verifier = Verifier.keeping(KeptResults.in(file, mode))
            verifier.addFunction(length)
            verifier -> verifier.statement("length", lengthIsNotNegative)
        }
        def verified(mode: KeptResults.Mode, tactic: UplcBlaster): VerificationResult = {
            val (verifier, statement) = stated(mode)
            verifier.verify(statement, tactic)
        }
        def stale(result: VerificationResult): Boolean = result match
            case VerificationResult.Inconclusive(reason) => reason.startsWith(KeptResults.stale)
            case _                                       => false
        try
            // With Lean, once.
            val asked = verified(KeptResults.Mode.Use, UplcBlaster(Budget.Within(400), lean))
            assert(asked.isInstanceOf[VerificationResult.ProvenWithin], asked)
            // Without Lean: within the same budget it is the proof that was kept,
            verified(KeptResults.Mode.Frozen, UplcBlaster(Budget.Within(400), silent)) match
                case VerificationResult.ProvenWithin(proof, bound) =>
                    val artifact = proof.artifact.asInstanceOf[UplcBlaster.Artifact]
                    assert(artifact.kept && artifact.budget == 400)
                    assert(bound == UplcBlaster.withinBound(400))
                case other => fail(s"expected the kept proof within 400 steps, got $other")
            // and it is none within another budget, which reaches other inputs, or without one.
            val wider = UplcBlaster(Budget.Within(800), silent)
            assert(stale(verified(KeptResults.Mode.Frozen, wider)))
            assert(stale(verified(KeptResults.Mode.Frozen, UplcBlaster(Budget.Auto, silent))))
            // The file keeps one result for a name: a run that checks one statement in two ways
            // would keep each in place of the other.
            val (verifier, statement) = stated(KeptResults.Mode.Use)
            verifier.verify(statement, UplcBlaster(Budget.Within(400), silent))
            val twice = intercept[IllegalStateException](verifier.verify(statement, wider))
            assert(twice.getMessage.contains("as another statement"), twice.getMessage)
        finally Directories.remove(directory)
        // A proof of one is none of the other, wherever a kept result comes from.
        val goal = UplcBlaster.lower(lengthIsNotNegative, FunctionTable(length)) match
            case Right(lowered) => lowered.copy(within = Some(400))
            case Left(reason)   => fail(reason)
        val artifact =
            UplcBlaster.Artifact(
              goal.programHashes,
              400,
              "",
              Nil,
              "",
              ProofKind.Blaster,
              kept = false
            )
        val proof: ExecutionResult =
            VerificationResult.ProvenWithin(Proof(artifact), UplcBlaster.withinBound(400))
        val kept = UplcBlaster.kept(goal, proof).getOrElse(fail("a proof within a budget is kept"))
        assert(kept.result == Kept.Result.ProvedWithin)
        assert(UplcBlaster.restored(goal.copy(within = None), kept).isLeft)
    }

    test("a loop that only walks a list is inconclusive in seconds, at any budget", Unfinished) {
        // One path per length of the list. Lean's counterexample is a list too long for the
        // budget, on which the statement holds.
        for budget <- Seq(100, 800) do
            val reason = inconclusive(lengthIsNotNegative, budget, finishes, length)
            assert(reason.contains("spurious"), reason)
    }

    test("no budget is found for a loop over a whole list", Unfinished) {
        // Every counterexample is a list too long for the budget, and asks for the steps of
        // that list. The next budget gives a longer one, and so on to a time limit.
        val sought =
            inconclusive(lengthIsNotNegative, UplcBlaster(Budget.Auto, lean, 40.seconds), length)
        assert(sought.contains("The tactic sought the budget, and tried 100"), sought)
    }

    test("a loop that makes a choice per element is not finished by Lean", Unfinished) {
        // Each element is kept or dropped, so the paths double with it. At a small budget the
        // loop is cut after a few elements.
        val small = inconclusive(filterDoesNotLengthen, 100, finishes, positives)
        assert(small.contains("spurious"), small)
        // At budget 400 Lean still finishes, in 20 s. At 800 it does not: the time goes to
        // Blaster's symbolic run of each leaf, before the solver is asked.
        val large = inconclusive(filterDoesNotLengthen, 800, givenUp, positives)
        assert(large.contains("did not finish"), large)
        assert(large.contains("symbolically"), large)
        assert(large.contains("had not come to the solver"), large)
    }

    test("a recursion over integers is not finished by the solver", Unfinished) {
        val small = inconclusive(gcdIsNotNegative, 200, finishes, gcd)
        assert(small.contains("spurious"), small)
        // Lean is done in seconds; Z3 is then left with nested remainders, and takes gigabytes.
        val large = inconclusive(gcdIsNotNegative, 400, givenUp, gcd)
        assert(large.contains("did not finish"), large)
        assert(large.contains("Blaster had started the solver"), large)
    }
}
