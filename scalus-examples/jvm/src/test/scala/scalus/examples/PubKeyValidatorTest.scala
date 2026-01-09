package scalus.examples

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.*
import scalus.builtin.ByteString.*
import scalus.builtin.Data
import scalus.compiler.sir.TargetLoweringBackend
import scalus.compiler.{compile, Options}
import scalus.ledger.api.v1.*
import scalus.prelude.List.{Cons, Nil}
import scalus.uplc.*
import scalus.uplc.eval.PlutusVM

import scala.language.implicitConversions

class PubKeyValidatorTest extends AnyFunSuite with ScalaCheckPropertyChecks:

    given Options = Options(
      targetLoweringBackend = TargetLoweringBackend.SirToUplcV3Lowering,
      generateErrorTraces = true,
      optimizeUplc = false,
      debug = false
    )

    test("PubKey Validator example") {
        val scriptContext =
            ScriptContext(
              TxInfo(
                Nil,
                Nil,
                Value.zero,
                Value.zero,
                Nil,
                Nil,
                Interval.always,
                Cons(PubKeyHash(hex"deadbeef"), Nil),
                Nil,
                TxId(hex"bb")
              ),
              ScriptPurpose.Spending(TxOutRef(TxId(hex"deadbeef"), 0))
            )

        val compiled = compile { PubKeyValidator.validator }

        // println(compiled.show)
        val term = compiled.toUplc()
        val script = term.plutusV1
        val flatBytesLen = script.flatEncoded.length
//    println(Utils.bytesToHex(flatBytes))
        // println(term.show)
        assert(flatBytesLen == 142)
        import Data.*
        import TermDSL.given
//    println(scriptContext.toData)
        val appliedValidator = script $ () $ () $ scriptContext.toData
        given PlutusVM = PlutusVM.makePlutusV1VM()
        assert(appliedValidator.deBruijnedProgram.evaluateDebug.isSuccess)
        assert(PubKeyValidator.validator((), (), scriptContext.toData) == ())
    }
