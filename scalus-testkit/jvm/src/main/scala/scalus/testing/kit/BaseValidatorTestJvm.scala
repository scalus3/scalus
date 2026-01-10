package scalus.testing.kit

import scalus.testing.assertions.Expected
import scalus.uplc.*
import scalus.uplc.eval.BuiltinError

import scala.util.{Failure, Success, Try}

/** JVM-specific base test class for validator testing.
  *
  * Extends BaseValidatorTest with UplcCli support for comparing results across different
  * evaluators.
  */
abstract class BaseValidatorTestJvm extends BaseValidatorTest {

    /** Assert that both the CEK machine and UplcCli produce the same result.
      *
      * This compares the result from the Scala CEK machine with the external uplc CLI tool.
      *
      * @param expected
      *   The expected outcome
      * @param program
      *   The program to evaluate
      */
    protected final def assertSameResultWithUplcCli(expected: Expected)(program: Program): Unit =
        val uplcResult = UplcCli.evalFlat(program)
        val cekResult = Try(program.deBruijnedProgram.evaluate)
        (expected, uplcResult, cekResult) match
            case (
                  Expected.SuccessAny | Expected.SuccessSame,
                  UplcEvalResult.Success(term1, _),
                  Success(term2)
                ) =>
                val normalized1 = DeBruijn.fromDeBruijnTerm(DeBruijn.deBruijnTerm(term1))
                assert(
                  normalized1 α_== term2,
                  s"Results differ. UplcCli: $normalized1, CEK: $term2"
                )
            case (Expected.Success(term), UplcEvalResult.Success(term1, _), Success(term2)) =>
                assert(term α_== term1, s"UplcCli result mismatch. Expected: $term, got: $term1")
                assert(term α_== term2, s"CEK result mismatch. Expected: $term, got: $term2")
            case (Expected.Failure(_), UplcEvalResult.UplcFailure(_, _), Failure(_)) =>
                () // Both failed as expected
            case _ =>
                cekResult match
                    case Failure(e: BuiltinError) =>
                        println(e.term.showHighlighted)
                    case _ =>
                fail(
                  s"Expected $expected, but got uplc evaluate: $uplcResult\nCEK evaluate: $cekResult"
                )

    /** Assert the result using only UplcCli (external uplc tool).
      *
      * @param expected
      *   The expected outcome
      * @param program
      *   The program to evaluate
      */
    protected final def assertUplcEvalResult(expected: Expected)(program: Program): Unit =
        val result = UplcCli.evalFlat(program)
        (expected, result) match
            case (Expected.SuccessAny, UplcEvalResult.Success(_, _)) =>
                ()
            case (Expected.Success(term), UplcEvalResult.Success(term1, _)) =>
                assert(term α_== term1)
            case (Expected.Failure(_), UplcEvalResult.UplcFailure(_, _)) =>
                ()
            case _ =>
                fail(s"Expected $expected, but got uplc evaluate: $result")
}
