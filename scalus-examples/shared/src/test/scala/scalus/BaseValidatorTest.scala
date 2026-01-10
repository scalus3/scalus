package scalus

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.builtin.ByteString.*
import scalus.ledger.api.v1.*
import scalus.prelude.Option.*
import scalus.testing.kit.ArbitraryInstances
import scalus.uplc.*
import scalus.uplc.eval.{BuiltinError, PlutusVM}

import scala.util.{Failure, Success, Try}

/** @deprecated Use scalus.testing.assertions.Expected instead */
@deprecated("Use scalus.testing.assertions.Expected from scalus-testkit module instead", "0.14.2")
enum Expected:
    case SuccessAny
    case Success(term: Term)
    case Failure(description: String)

/** @deprecated Use scalus.testing.kit.BaseValidatorTest instead */
@deprecated("Use scalus.testing.kit.BaseValidatorTest from scalus-testkit module instead", "0.14.2")
abstract class BaseValidatorTest
    extends AnyFunSuite
    with ScalaCheckPropertyChecks
    with ArbitraryInstances {

    given PlutusVM = PlutusVM.makePlutusV2VM()

    protected final def assertSameResult(expected: Expected)(program: Program) = {
        // println(s"Program: ${program.show}")
        // val result1 = UplcCli.evalFlat(program)
        val result = Try(program.deBruijnedProgram.evaluate)
        // println(s"$result1 == $result2")
        (expected, result) match
            case (Expected.SuccessAny, Success(term)) =>
            case (Expected.Success(termExpected), Success(term)) =>
                assert(termExpected == term)
            case (Expected.Failure(_), Failure(e2)) =>
                // println(s"Error: $err and $e2")
                assert(true)
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
                fail(
                  s"Expected $expected, but got $result"
                )
    }

    protected final def assertUplcEvalResult(expected: Expected)(program: Program) = {
        val result1 = UplcCli.evalFlat(program)
        (expected, result1) match
            case (Expected.Success(term), UplcEvalResult.Success(term1, _)) => assert(term == term1)
            case (Expected.Failure(_), UplcEvalResult.UplcFailure(_, err))  => assert(true)
            case _ => fail(s"Expected $expected, but got uplc evaluate: $result1")
    }

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
