package scalus

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.builtin.ByteString.*
import scalus.ledger.api.v1.{ArbitraryInstances as _, *}
import scalus.prelude.Option.*
import scalus.uplc.*
import scalus.uplc.eval.BuiltinError
import scalus.uplc.eval.PlutusVM
import scalus.uplc.test.ArbitraryInstances

import scala.util.Failure
import scala.util.Success
import scala.util.Try

/** @deprecated Use scalus.testing.assertions.Expected instead */
@deprecated("Use scalus.testing.assertions.Expected from scalus-testkit module instead", "0.14.2")
enum Expected:
    case SuccessSame
    case Success(term: Term)
    case Failure(description: String)

/** @deprecated Use scalus.testing.kit.BaseValidatorTestJvm instead */
@deprecated(
  "Use scalus.testing.kit.BaseValidatorTestJvm from scalus-testkit module instead",
  "0.14.2"
)
abstract class BaseValidatorTest
    extends AnyFunSuite
    with ScalaCheckPropertyChecks
    with ArbitraryInstances {

    given PlutusVM = PlutusVM.makePlutusV2VM()

    protected final def assertSameResult(expected: Expected)(program: Program) = {
        val result1 = UplcCli.evalFlat(program)
        val result2 = Try(program.deBruijnedProgram.evaluate)
        // println(s"$result1 == $result2")
        (expected, result1, result2) match
            case (Expected.SuccessSame, UplcEvalResult.Success(term1, _), Success(term2)) =>
                val normalized1 = DeBruijn.fromDeBruijnTerm(DeBruijn.deBruijnTerm(term1))
                assert(normalized1 == term2)
            case (Expected.Success(term), UplcEvalResult.Success(term1, _), Success(term2)) =>
                assert(term == term1)
                assert(term == term2)
            case (Expected.Failure(_), UplcEvalResult.UplcFailure(_, err), Failure(e2)) =>
                // println(s"Error: $err and $e2")
                assert(true)
            case _ =>
                result2 match
                    case Failure(e: BuiltinError) =>
                        println(e.term.showHighlighted)
                    case _ =>
                fail(
                  s"Expected $expected, but got uplc evaluate: $result1\nCek.evalUPLCProgram => $result2"
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
