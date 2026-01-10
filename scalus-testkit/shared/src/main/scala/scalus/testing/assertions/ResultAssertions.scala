package scalus.testing.assertions

import scalus.cardano.ledger.ExUnits
import scalus.testing.eval.MultiEvalResult
import scalus.uplc.{Constant, Term}
import scalus.uplc.eval.Result

import scala.reflect.ClassTag

/** Expected outcome for evaluation assertions.
  *
  * Uses predicate-based matching for flexible assertions.
  */
enum Expected:
    /** Any successful evaluation */
    case SuccessAny

    /** Successful evaluation returning a specific term (alpha-equivalent) */
    case Success(term: Term)

    /** All evaluators should return the same result (for multi-backend testing) */
    case SuccessSame

    /** Evaluation should fail, with predicate for matching */
    case Failure(predicate: Result.Failure => Boolean)

object Expected:
    // ----- Success smart constructors -----

    /** Expect success with a value that can be lifted to Term */
    def success[A: Constant.LiftValue](value: A): Expected.Success =
        Expected.Success(Term.Const(Constant.asConstant(value)))

    // ----- Failure smart constructors -----

    /** Expect any failure */
    val failure: Expected.Failure = Expected.Failure(_ => true)

    /** Expect failure with message substring in logs or exception */
    def failureWith(msg: String): Expected.Failure = Expected.Failure { failure =>
        failure.logs.exists(_.contains(msg)) ||
        Option(failure.exception.getMessage).exists(_.contains(msg))
    }

    /** Expect failure matching a custom predicate */
    def failureMatching(predicate: Result.Failure => Boolean): Expected.Failure =
        Expected.Failure(predicate)

    /** Expect failure with specific exception type */
    def failureWithException[E <: Throwable: ClassTag]: Expected.Failure =
        Expected.Failure { failure =>
            summon[ClassTag[E]].runtimeClass.isInstance(failure.exception)
        }

/** Assertion error with detailed information. */
case class AssertionError(
    message: String,
    expected: Expected,
    actual: Result,
    logs: Seq[String] = Seq.empty
) extends java.lang.AssertionError(
      s"$message\nExpected: $expected\nActual: $actual\nLogs: ${logs.mkString("\n")}"
    )

/** Budget assertion error. */
case class BudgetAssertionError(
    message: String,
    expected: ExUnits,
    actual: ExUnits
) extends java.lang.AssertionError(
      s"$message\nExpected budget: $expected\nActual budget: $actual"
    )

/** Core assertion functions for Result.
  *
  * These are framework-agnostic - they throw exceptions on failure. Test frameworks can catch these
  * and convert to their assertion format.
  */
object ResultAssertions:

    /** Assert that the result matches the expected outcome.
      *
      * @param expected
      *   The expected outcome
      * @param result
      *   The actual evaluation result
      * @throws AssertionError
      *   if assertion fails
      */
    def assertResult(expected: Expected, result: Result): Unit =
        (expected, result) match
            case (Expected.SuccessAny, _: Result.Success) => ()

            case (Expected.Success(expectedTerm), Result.Success(actualTerm, _, _, _)) =>
                if !(expectedTerm α_== actualTerm) then
                    throw AssertionError(
                      "Term mismatch",
                      expected,
                      result
                    )

            case (Expected.Failure(predicate), failure: Result.Failure) =>
                if !predicate(failure) then
                    throw AssertionError(
                      "Failure predicate not satisfied",
                      expected,
                      result,
                      failure.logs
                    )

            case _ =>
                throw AssertionError(
                  "Unexpected result type",
                  expected,
                  result,
                  result.logs
                )

    /** Assert that the result is a success. */
    def assertSuccess(result: Result): Unit =
        assertResult(Expected.SuccessAny, result)

    /** Assert that the result is a failure. */
    def assertFailure(result: Result): Unit =
        assertResult(Expected.failure, result)

    /** Assert that the result is a failure with a specific message. */
    def assertFailureWith(message: String, result: Result): Unit =
        assertResult(Expected.failureWith(message), result)

    /** Assert that the result term equals the expected term (alpha-equivalent). */
    def assertTermEquals(expected: Term, result: Result): Unit =
        assertResult(Expected.Success(expected), result)

    /** Assert that the budget is within the specified limits.
      *
      * @param maxBudget
      *   Maximum allowed budget
      * @param result
      *   The evaluation result
      * @throws BudgetAssertionError
      *   if budget exceeds limits
      */
    def assertBudgetWithin(maxBudget: ExUnits, result: Result): Unit =
        result match
            case Result.Success(_, actualBudget, _, _) =>
                if actualBudget.memory > maxBudget.memory || actualBudget.steps > maxBudget.steps
                then
                    throw BudgetAssertionError(
                      "Budget exceeds maximum",
                      maxBudget,
                      actualBudget
                    )
            case _ => () // Don't check budget on failures

    /** Assert that the budget equals the expected budget exactly. */
    def assertBudgetEquals(expectedBudget: ExUnits, result: Result): Unit =
        result match
            case Result.Success(_, actualBudget, _, _) =>
                if actualBudget != expectedBudget then
                    throw BudgetAssertionError(
                      "Budget mismatch",
                      expectedBudget,
                      actualBudget
                    )
            case _ =>
                throw AssertionError(
                  "Cannot check budget on failed result",
                  Expected.SuccessAny,
                  result
                )

/** Assertions for multi-backend evaluation results. */
object MultiResultAssertions:

    /** Assert all evaluations succeeded with the same result (alpha-equivalent). */
    def assertAllSame(multiResult: MultiEvalResult): Unit =
        if !multiResult.allSucceeded then
            val failures = multiResult.results.filter(_._2.isFailure)
            throw new java.lang.AssertionError(s"Some evaluations failed: $failures")
        if !multiResult.allTermsEqual then
            throw new java.lang.AssertionError("Results differ across backends/VMs")

    /** Assert all evaluations failed. */
    def assertAllFailed(multiResult: MultiEvalResult): Unit =
        if !multiResult.allFailed then
            val successes = multiResult.results.filter(_._2.isSuccess)
            throw new java.lang.AssertionError(s"Some evaluations succeeded: $successes")

    /** Assert budget consistency across backends (within tolerance). */
    def assertBudgetConsistency(
        multiResult: MultiEvalResult,
        tolerancePercent: Double = 0.0
    ): Unit =
        val budgets = multiResult.budgets.values.toSeq
        if budgets.size > 1 then
            val reference = budgets.head
            budgets.tail.foreach { budget =>
                val memDiff =
                    math.abs(budget.memory - reference.memory).toDouble / reference.memory
                val stepDiff =
                    math.abs(budget.steps - reference.steps).toDouble / reference.steps
                if memDiff > tolerancePercent || stepDiff > tolerancePercent then
                    throw new java.lang.AssertionError(
                      s"Budget inconsistency exceeds $tolerancePercent%: $budget vs $reference"
                    )
            }
