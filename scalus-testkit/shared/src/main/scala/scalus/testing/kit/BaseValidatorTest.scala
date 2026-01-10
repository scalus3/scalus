package scalus.testing.kit

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.builtin.ByteString.*
import scalus.ledger.api.v1.*
import scalus.prelude.Option.*
import scalus.testing.assertions.Expected
import scalus.uplc.*
import scalus.uplc.eval.{BuiltinError, PlutusVM}

import scala.util.{Failure, Success, Try}

/** Base test class for validator testing.
  *
  * Provides assertion methods for evaluating Programs and checking results. This base class works
  * on all platforms (JVM/JS/Native).
  *
  * For JVM-only testing with UplcCli support, use `BaseValidatorTestJvm` instead.
  */
abstract class BaseValidatorTest
    extends AnyFunSuite
    with ScalaCheckPropertyChecks
    with ArbitraryInstances {

    given PlutusVM = PlutusVM.makePlutusV2VM()

    /** Assert that the program evaluates to the expected result.
      *
      * @param expected
      *   The expected outcome
      * @param program
      *   The program to evaluate
      */
    protected final def assertEvalResult(expected: Expected)(program: Program): Unit =
        val result = Try(program.deBruijnedProgram.evaluate)
        (expected, result) match
            case (Expected.SuccessAny, Success(_)) =>
                ()
            case (Expected.SuccessSame, Success(_)) =>
                // SuccessSame on shared platform just checks success
                ()
            case (Expected.Success(expectedTerm), Success(actualTerm)) =>
                assert(
                  expectedTerm α_== actualTerm,
                  s"Terms not equal. Expected: $expectedTerm, got: $actualTerm"
                )
            case (Expected.Failure(predicate), Failure(e)) =>
                // Create a mock Result.Failure to check the predicate
                val mockFailure: scalus.uplc.eval.Result.Failure = scalus.uplc.eval.Result.Failure(
                  e,
                  scalus.cardano.ledger.ExUnits(0, 0),
                  Map.empty,
                  Seq.empty
                )
                assert(predicate(mockFailure), s"Failure predicate not satisfied: $e")
            case _ =>
                result match
                    case Failure(e) =>
                        e match
                            case eb: BuiltinError =>
                                println(eb.term.showHighlighted)
                            case _ =>
                                println(e.getMessage)
                                e.printStackTrace()
                    case Success(r) =>
                        println(s"result = ${r.showHighlighted}")
                fail(s"Expected $expected, but got $result")

    /** Legacy alias for assertEvalResult. */
    protected final def assertSameResult(expected: Expected)(program: Program): Unit =
        assertEvalResult(expected)(program)

    // Common test fixtures
    protected final val hoskyMintTxOutRef = TxOutRef(
      TxId(hex"1ab6879fc08345f51dc9571ac4f530bf8673e0d798758c470f9af6f98e2f3982"),
      0
    )
    protected final val hoskyMintTxOut = TxOut(
      address = Address(
        Credential.PubKeyCredential(
          PubKeyHash(
            hex"61822dde476439a526070f36d3d1667ad099b462c111cd85e089f5e7f6"
          )
        ),
        None
      ),
      Value.lovelace(BigInt("10000000")),
      None
    )
}
