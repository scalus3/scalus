package scalus.examples

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.*
import scalus.uplc.builtin.ByteString.*
import scalus.uplc.builtin.Data
import scalus.uplc.builtin.Data.toData
import scalus.compiler.sir.TargetLoweringBackend
import scalus.compiler.{compile, Options}
import scalus.cardano.ledger.ExUnits
import scalus.cardano.onchain.plutus.v1.*
import scalus.cardano.onchain.plutus.prelude.List
import scalus.cardano.onchain.plutus.prelude.List.{Cons, Nil}
import scalus.cardano.onchain.plutus.prelude.{AssocMap, SortedMap}
import scalus.cardano.onchain.plutus.v2
import scalus.cardano.onchain.plutus.v3
import scalus.uplc.*
import scalus.uplc.eval.{Log, PlutusVM, RestrictingBudgetSpender}

import scala.util.Try

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
        assert(flatBytesLen == 120)
        import TermDSL.given
//    println(scriptContext.toData)
        val appliedValidator = script $ () $ () $ scriptContext.toData
        given PlutusVM = PlutusVM.makePlutusV1VM()
        val evalResult = appliedValidator.deBruijnedProgram.evaluateDebug
        assert(evalResult.isSuccess)
        assert(evalResult.budget == ExUnits(memory = 10345, steps = 2_855689))
        assert(PubKeyValidator.validator((), (), scriptContext.toData) == ())
    }

    private val other = PubKeyHash(hex"0badc0de")
    private val signer = PubKeyHash(hex"deadbeef")

    private def v1Context(signatories: List[PubKeyHash]) =
        ScriptContext(
          TxInfo(
            Nil,
            Nil,
            Value.zero,
            Value.zero,
            Nil,
            Nil,
            Interval.always,
            signatories,
            Nil,
            TxId(hex"bb")
          ),
          ScriptPurpose.Spending(TxOutRef(TxId(hex"deadbeef"), 0))
        )

    private def v2Context(signatories: List[PubKeyHash]) =
        v2.ScriptContext(
          v2.TxInfo(
            inputs = Nil,
            referenceInputs = Nil,
            outputs = Nil,
            fee = Value.zero,
            mint = Value.zero,
            dcert = Nil,
            withdrawals = SortedMap.empty,
            validRange = Interval.always,
            signatories = signatories,
            redeemers = AssocMap.empty,
            data = SortedMap.empty,
            id = TxId(hex"bb")
          ),
          ScriptPurpose.Spending(TxOutRef(TxId(hex"deadbeef"), 0))
        )

    private def v3Context(signatories: List[PubKeyHash]) =
        v3.ScriptContext(
          txInfo = v3.TxInfo(inputs = Nil, signatories = signatories, id = v3.TxId(hex"bb")),
          scriptInfo = v3.ScriptInfo.SpendingScript(v3.TxOutRef(v3.TxId(hex"deadbeef"), 0))
        )

    /** Evaluates under the mainnet per-transaction budget, so a non-terminating script fails with
      * an out-of-budget error instead of hanging the test.
      */
    private def evaluate(applied: Program, vm: PlutusVM): (Try[Term], Seq[String]) = {
        val logger = Log()
        val budget = RestrictingBudgetSpender(ExUnits(memory = 14_000_000, steps = 10_000_000_000L))
        val result =
            Try(vm.evaluateDeBruijnedTerm(applied.deBruijnedProgram.term, budget, logger, true))
        (result, logger.getLogs.toSeq)
    }

    private def evalV1(ctx: ScriptContext) = {
        import TermDSL.given
        val script = compile { PubKeyValidator.validator }.toUplc().plutusV1
        evaluate(script $ () $ () $ ctx.toData, PlutusVM.makePlutusV1VM())
    }

    private def evalV2(ctx: v2.ScriptContext) = {
        import TermDSL.given
        val script = compile { PubKeyValidator.validatorV2(hex"deadbeef") }.toUplc().plutusV2
        evaluate(script $ () $ () $ ctx.toData, PlutusVM.makePlutusV2VM())
    }

    private def evalV3(ctx: v3.ScriptContext) = {
        val script = compile { PubKeyValidator.validatorV3(hex"deadbeef") }.toUplc().plutusV3
        evaluate(script $ ctx.toData, PlutusVM.makePlutusV3VM())
    }

    test("V1: the signer after another signatory is found") {
        val ctx = v1Context(Cons(other, Cons(signer, Nil)))
        val (result, logs) = evalV1(ctx)
        assert(result.isSuccess, (result, logs.takeRight(5)))
        assert(PubKeyValidator.validator((), (), ctx.toData) == ())
    }

    test("Fail: V1 without the signer reports 'Signature not found'") {
        val ctx = v1Context(Cons(other, Nil))
        val (result, logs) = evalV1(ctx)
        assert(result.isFailure)
        assert(logs.exists(_.contains("Signature not found")), (result, logs.takeRight(5)))
        val e = intercept[RuntimeException](PubKeyValidator.validator((), (), ctx.toData))
        assert(e.getMessage == "Signature not found")
    }

    test("V2: the signer after another signatory is found") {
        val (result, logs) = evalV2(v2Context(Cons(other, Cons(signer, Nil))))
        assert(result.isSuccess, (result, logs.takeRight(5)))
    }

    test("Fail: V2 without the signer reports 'Signature not found'") {
        val (result, logs) = evalV2(v2Context(Cons(other, Nil)))
        assert(result.isFailure)
        assert(logs.exists(_.contains("Signature not found")), (result, logs.takeRight(5)))
    }

    test("V3: the signer after another signatory is found") {
        val (result, logs) = evalV3(v3Context(Cons(other, Cons(signer, Nil))))
        assert(result.isSuccess, (result, logs.takeRight(5)))
    }

    test("Fail: V3 without the signer reports 'Signature not found'") {
        val (result, logs) = evalV3(v3Context(Cons(other, Nil)))
        assert(result.isFailure)
        assert(logs.exists(_.contains("Signature not found")), (result, logs.takeRight(5)))
    }
