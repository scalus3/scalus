package scalus.uplc.eval

import io.bullet.borer.Cbor
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.Network
import scalus.cardano.ledger.{CardanoInfo, ExUnits, JsSlotConfig, JsUtxo, Output, SlotConfig, TransactionInput, TransactionOutput, Value}
import scalus.testing.kit.Party.Alice
import scalus.uplc.*
import scalus.uplc.Constant.given
import scalus.uplc.DefaultFun.*
import scalus.uplc.Term.*
import scalus.uplc.TermDSL.given
import scalus.utils.Hex

import scalus.utils.scalajs.internal.*
import scala.language.implicitConversions
import scala.scalajs.js
import scala.scalajs.js.JSConverters.*

class JEvaluatorTest extends AnyFunSuite {

    private val mainnetV3 = JEvaluationOptions.mainnet("PlutusV3")

    private def scriptInputs(program: Program): Seq[js.Any] =
        Seq(
          Hex.bytesToHex(program.cborEncoded),
          program.doubleCborHex,
          program.cborEncoded.toUint8Array,
          program.doubleCborEncoded.toUint8Array
        )

    private def error(result: JEvaluationResult): JEvaluationError = {
        assert(!js.isUndefined(result.error))
        result.error.asInstanceOf[JEvaluationError]
    }

    private def typeErrorFrom(body: => Any): js.TypeError =
        intercept[js.JavaScriptException](body).exception match
            case e: js.TypeError => e
            case other           => fail(s"expected TypeError, got $other")

    test("evaluates every script envelope with multiple mixed CBOR arguments") {
        val program = λ("first", "second")(()).plutusV3
        val arguments = js.Array[js.Any]("182a", Hex.hexToBytes("42182a").toUint8Array)

        scriptInputs(program).foreach { script =>
            val result = JEvaluator.evaluateScript(script, arguments, mainnetV3)
            assert(result.isSuccess)
            assert(js.isUndefined(result.error))
            assert(result.logs.isEmpty)
            assert(result.budget.memory > js.BigInt(0))
            assert(result.budget.steps > js.BigInt(0))
        }
    }

    test("a call that cannot be read throws TypeError") {
        val program = ().asTerm.plutusV3
        typeErrorFrom(
          JEvaluator.evaluateScript(program.doubleCborHex, js.Array(), null)
        )
        typeErrorFrom(JEvaluator.evaluateScript(42, js.Array(), mainnetV3))
        typeErrorFrom(JEvaluator.evaluateScript("zz", js.Array(), mainnetV3))
        val badSecond = typeErrorFrom(
          JEvaluator.evaluateScript(
            program.doubleCborHex,
            js.Array[js.Any]("182a", "0x00"),
            mainnetV3
          )
        )
        assert(badSecond.message.startsWith("args[1]"), badSecond.message)
    }

    test("a script is not vetted against the language and protocol it is run under") {
        // Nothing here decides whether a chain would admit the script. A program a node would
        // refuse outright still runs, and still reports what it cost. Admission is
        // `Script.isWellFormed`'s question, and it is asked somewhere else.
        val v1AtPv10 = js.Dynamic
            .literal(
              plutusVersion = "PlutusV1",
              protocolMajorVersion = 10,
              costModel = JEvaluationOptions.mainnet("PlutusV1").costModel
            )
            .asInstanceOf[JEvaluationOptions]

        // UPLC 1.1.0 under a protocol that only ever had 1.0.0.
        val pv11Program = ().asTerm.plutusV3
        val ranAnyway = JEvaluator.evaluateScript(pv11Program.doubleCborHex, js.Array(), v1AtPv10)
        assert(ranAnyway.isSuccess)
        assert(ranAnyway.budget.steps > js.BigInt(0))

        // A builtin that protocol never had, in a branch the machine never reaches.
        val unavailableInDeadBranch =
            (λ("ignored")((): Term) $ ~(ExpModInteger: Term)).plutusV1
        assert(
          JEvaluator
              .evaluateScript(unavailableInDeadBranch.doubleCborHex, js.Array(), v1AtPv10)
              .isSuccess
        )
    }

    test("a non-unit V3 result fails the return rule but keeps its budget and traces") {
        // The return rule is checked after the machine stops, so this is what makes the single
        // evaluator usable for costing a pure on-chain function: read `error.code` rather than
        // `isSuccess`, and the budget and traces are the ones the program really produced.
        val pureFunction = (!Trace $ "completed" $ 42).plutusV3
        val validator = (!Trace $ "completed" $ ()).plutusV3

        val rejected = JEvaluator.evaluateScript(pureFunction.doubleCborHex, js.Array(), mainnetV3)
        val accepted = JEvaluator.evaluateScript(validator.doubleCborHex, js.Array(), mainnetV3)

        assert(!rejected.isSuccess)
        assert(error(rejected).code == "INVALID_RETURN_VALUE")
        assert(rejected.budget.memory == accepted.budget.memory)
        assert(rejected.budget.steps == accepted.budget.steps)
        assert(rejected.logs.toSeq == accepted.logs.toSeq)

        assert(accepted.isSuccess)
        assert(js.isUndefined(accepted.error))
        assert(accepted.logs.size == 1)
        assert(accepted.logs.head == "completed")
    }

    test("V1 and V2 accept any result, as the ledger does") {
        val program = (!Trace $ "completed" $ 42).plutusV1
        Seq("PlutusV1", "PlutusV2").foreach { version =>
            val result = JEvaluator.evaluateScript(
              program.doubleCborHex,
              js.Array(),
              JEvaluationOptions.mainnet(version)
            )
            assert(result.isSuccess, version)
            assert(js.isUndefined(result.error), version)
        }
    }

    test("classifies script and builtin failures while preserving spent budget and logs") {
        val fixtures = Seq(
          (
            (λ("ignored")(Error()) $ (!Trace $ "script trace" $ ())).plutusV3,
            "SCRIPT_FAILURE",
            "script trace"
          ),
          (
            (λ("ignored")(DivideInteger $ 1 $ 0) $ (!Trace $ "builtin trace" $ ())).plutusV3,
            "BUILTIN_FAILURE",
            "builtin trace"
          )
        )

        fixtures.foreach { case (program, code, trace) =>
            val result = JEvaluator.evaluateScript(program.doubleCborHex, js.Array(), mainnetV3)
            assert(!result.isSuccess)
            assert(error(result).code == code)
            assert(result.budget.memory > js.BigInt(0))
            assert(result.budget.steps > js.BigInt(0))
            assert(result.logs.toSeq == Seq(trace))
            // An own enumerable property, and the bigint budget serializes through toJSON.
            assert(js.Object.keys(result.asInstanceOf[js.Object]).contains("error"))
            assert(js.JSON.stringify(result.asInstanceOf[js.Any]).contains(code))
        }
    }

    test("maxBudget stops the script with OUT_OF_BUDGET, reporting what it spent") {
        val program = (λ("ignored")(()) $ (!Trace $ "ran" $ ())).plutusV3
        val unbounded = JEvaluator.evaluateScript(program.doubleCborHex, js.Array(), mainnetV3)
        val spent = unbounded.budget.steps
        def limited(steps: js.BigInt) = {
            val o = js.Object.assign(js.Object(), mainnetV3).asInstanceOf[js.Dynamic]
            o.updateDynamic("maxBudget")(
              js.Dynamic.literal(memory = js.BigInt("1000000"), steps = steps)
            )
            JEvaluator.evaluateScript(
              program.doubleCborHex,
              js.Array(),
              o.asInstanceOf[JEvaluationOptions]
            )
        }
        val enough = limited(spent)
        assert(enough.isSuccess)
        assert(enough.budget.steps.toString == unbounded.budget.steps.toString)

        val short = limited(spent - js.BigInt(1))
        assert(!short.isSuccess)
        assert(error(short).code == "OUT_OF_BUDGET")
        assert(short.budget.steps > spent - js.BigInt(1))
        assert(js.isUndefined(unbounded.profileJson))
    }

    test(
      "legacy construction leaves error undefined, and unexpected faults become INTERNAL_ERROR"
    ) {
        val legacy = new JEvaluationResult(
          false,
          new JExUnits(js.BigInt(0), js.BigInt(0)),
          js.Array("legacy"),
          js.undefined
        )
        assert(js.isUndefined(legacy.error))

        val builtin = JEvaluationResult.of(
          Result.Failure(new BuiltinException("typed"), ExUnits.zero, Map.empty, Seq.empty)
        )
        assert(error(builtin).code == "BUILTIN_FAILURE")

        val internal = JEvaluationResult.of(
          Result.Failure(
            new IllegalStateException("injected evaluator fault"),
            ExUnits.zero,
            Map.empty,
            Seq.empty
          )
        )
        assert(error(internal).code == "INTERNAL_ERROR")
        assert(error(internal).message == "injected evaluator fault")
        assert(internal.logs.isEmpty)
    }

    // Plain records, built the way a JavaScript caller builds them.
    private given Conversion[js.Dynamic, JSlotConfigLike] = _.asInstanceOf[JSlotConfigLike]
    private given Conversion[js.Dynamic, JCostModelsLike] = _.asInstanceOf[JCostModelsLike]

    private val protocol = CardanoInfo.mainnet.majorProtocolVersion.version.toDouble
    private val mainnetModels = CardanoInfo.mainnet.protocolParams.costModels

    private def slotConfigRecord: js.Dynamic = js.Dynamic.literal(
      zeroTime = SlotConfig.mainnet.zeroTime.toDouble,
      zeroSlot = SlotConfig.mainnet.zeroSlot.toDouble,
      slotLength = SlotConfig.mainnet.slotLength.toDouble
    )

    private def costModelsRecord: js.Dynamic = js.Dynamic.literal(
      PlutusV2 = js.Array(mainnetModels.models(1).map(_.toDouble)*),
      PlutusV3 = js.Array(mainnetModels.models(2).map(_.toDouble)*)
    )

    private def pairBytes(entry: (TransactionInput, TransactionOutput)): Array[Byte] =
        Cbor.encode(entry).toByteArray

    private def budgets(redeemers: js.Array[JRedeemerBudget]): Seq[(String, Int, String, String)] =
        redeemers.toSeq.map(r =>
            (r.tag, r.index, r.budget.memory.toString, r.budget.steps.toString)
        )

    test("evaluateTx takes pairs as hex or bytes and agrees with the CBOR map") {
        // spec [TX-1] [TX-2] [TX-3] [TX-9]
        val (tx, utxos) = SampleTransactions.withdrawal(SampleTransactions.succeedingV3)
        val pairs = utxos.toSeq.map(pairBytes)
        val fromHex = JEvaluator.evaluateTx(
          Hex.bytesToHex(tx.toCbor),
          js.Array[js.Any](pairs.map(Hex.bytesToHex)*),
          slotConfigRecord,
          costModelsRecord,
          protocol
        )
        val fromBytes = JEvaluator.evaluateTx(
          tx.toCbor.toUint8Array,
          js.Array[js.Any](pairs.map(_.toUint8Array)*),
          slotConfigRecord,
          costModelsRecord,
          protocol
        )
        val fromMap = JScalus.evalPlutusScripts(
          tx.toCbor.toUint8Array,
          JsCbor.encode(utxos),
          JsSlotConfig.mainnet,
          mainnetModels.models.toSeq.sortBy(_._1).map(_._2.map(_.toDouble).toJSArray).toJSArray,
          protocol.toInt
        )
        assert(budgets(fromHex) == budgets(fromMap))
        assert(budgets(fromBytes) == budgets(fromMap))
        assert(fromMap.length == 1 && fromMap(0).tag == "Reward")
    }

    test("a repeated input takes the later pair") {
        // spec [TX-4]
        val (_, utxos) = SampleTransactions.withdrawal(SampleTransactions.succeedingV3)
        val (input, first) = utxos.head
        val second = Output(Alice.address(Network.Mainnet), Value.ada(4999))
        assert(second != first)
        val resolved = JEvaluator.utxoMapOf(
          js.Array[js.Any](
            Hex.bytesToHex(pairBytes(input -> first)),
            Hex.bytesToHex(pairBytes(input -> second))
          )
        )
        assert(resolved == Map(input -> second))
    }

    test("a Utxo is taken as it is, mixed with pairs, and the later entry still wins") {
        val (_, utxos) = SampleTransactions.withdrawal(SampleTransactions.succeedingV3)
        val (input, first) = utxos.head
        val second = Output(Alice.address(Network.Mainnet), Value.ada(4999))
        val handle = JsUtxo.wrap(input, second)
        assert(JEvaluator.utxoMapOf(js.Array[js.Any](handle)) == Map(input -> second))
        val mixed = js.Array[js.Any](handle, Hex.bytesToHex(pairBytes(input -> first)))
        assert(JEvaluator.utxoMapOf(mixed) == Map(input -> first))
        val e = intercept[js.JavaScriptException](
          JEvaluator.utxoMapOf(js.Array[js.Any](handle, js.Dynamic.literal()))
        ).exception
        assert(e.isInstanceOf[js.TypeError] && e.toString.contains("utxos[1]"), e)
    }

    test("bigint slot fields are accepted and mean the same as numbers") {
        // spec [TX-5]
        val (tx, utxos) = SampleTransactions.withdrawal(SampleTransactions.succeedingV3)
        val pairs = js.Array[js.Any](utxos.toSeq.map(pairBytes).map(_.toUint8Array)*)
        val bigintSlots = js.Dynamic.literal(
          zeroTime = SlotConfig.mainnet.zeroTime.toJsBigInt,
          zeroSlot = SlotConfig.mainnet.zeroSlot.toJsBigInt,
          slotLength = SlotConfig.mainnet.slotLength.toDouble,
          startEpoch = 208 // an extra field, as MeshJS's slot configs carry
        )
        val withBigint =
            JEvaluator.evaluateTx(
              tx.toCbor.toUint8Array,
              pairs,
              bigintSlots,
              costModelsRecord,
              protocol
            )
        val withNumbers = JEvaluator.evaluateTx(
          tx.toCbor.toUint8Array,
          pairs,
          slotConfigRecord,
          costModelsRecord,
          protocol
        )
        assert(budgets(withBigint) == budgets(withNumbers))
    }

    test("inputs that cannot be read throw TypeError") {
        // spec [TX-10]
        val (tx, utxos) = SampleTransactions.withdrawal(SampleTransactions.succeedingV3)
        val pairs = js.Array[js.Any](utxos.toSeq.map(pairBytes).map(_.toUint8Array)*)
        val txBytes = tx.toCbor.toUint8Array
        typeErrorFrom(
          JEvaluator.evaluateTx(42, pairs, slotConfigRecord, costModelsRecord, protocol)
        )
        typeErrorFrom(
          JEvaluator.evaluateTx("zz", pairs, slotConfigRecord, costModelsRecord, protocol)
        )
        typeErrorFrom(
          // valid hex, but not the CBOR of a transaction: goes through the NonFatal decode
          // branch, not the hex-parsing one above.
          JEvaluator.evaluateTx("00", pairs, slotConfigRecord, costModelsRecord, protocol)
        )
        typeErrorFrom(
          JEvaluator.evaluateTx(
            txBytes,
            "not an array",
            slotConfigRecord,
            costModelsRecord,
            protocol
          )
        )
        typeErrorFrom(
          JEvaluator.evaluateTx(
            txBytes,
            js.Array[js.Any]("zz"),
            slotConfigRecord,
            costModelsRecord,
            protocol
          )
        )
        typeErrorFrom(
          // valid hex, but not the CBOR of an [input, output] pair: same NonFatal decode branch.
          JEvaluator.evaluateTx(
            txBytes,
            js.Array[js.Any]("00"),
            slotConfigRecord,
            costModelsRecord,
            protocol
          )
        )
        typeErrorFrom(JEvaluator.evaluateTx(txBytes, pairs, null, costModelsRecord, protocol))
        typeErrorFrom(
          JEvaluator.evaluateTx(
            txBytes,
            pairs,
            js.Dynamic.literal(zeroTime = "0", zeroSlot = 0, slotLength = 1000),
            costModelsRecord,
            protocol
          )
        )
        typeErrorFrom(
          JEvaluator.evaluateTx(
            txBytes,
            pairs,
            slotConfigRecord,
            js.Dynamic.literal(PlutusV3 = "x"),
            protocol
          )
        )
        typeErrorFrom(
          JEvaluator.evaluateTx(txBytes, pairs, slotConfigRecord, costModelsRecord, 1.5)
        )
        typeErrorFrom(
          // a safe integer, but too big for Int: would otherwise saturate through .toInt
          JEvaluator.evaluateTx(txBytes, pairs, slotConfigRecord, costModelsRecord, 1099511627776.0)
        )
    }

    test("an input no pair resolves is a native Error, not a script failure") {
        // spec [TX-12]
        val (tx, _) = SampleTransactions.withdrawal(SampleTransactions.succeedingV3)
        val caught = intercept[js.JavaScriptException] {
            JEvaluator.evaluateTx(
              tx.toCbor.toUint8Array,
              js.Array[js.Any](),
              slotConfigRecord,
              costModelsRecord,
              protocol
            )
        }
        caught.exception match
            case e: JPlutusScriptEvaluationError => fail(s"not a script failure: $e")
            case e: js.TypeError                 => fail(s"not an input problem: $e")
            case e: js.Error                     => assert(e.message.nonEmpty)
            case other                           => fail(s"expected a JS Error, got $other")
    }

    test("a failing script throws the enriched error through evaluateTx") {
        // spec [TX-11] [ER-14]
        val (tx, utxos) = SampleTransactions.withdrawal(SampleTransactions.failingV3)
        val pairs = js.Array[js.Any](utxos.toSeq.map(pairBytes).map(_.toUint8Array)*)
        val caught = intercept[js.JavaScriptException] {
            JEvaluator.evaluateTx(
              tx.toCbor.toUint8Array,
              pairs,
              slotConfigRecord,
              costModelsRecord,
              protocol
            )
        }
        caught.exception match
            case e: JPlutusScriptEvaluationError =>
                assert(e.redeemer.toOption.map(_.tag).contains("Reward"))
                assert(e.args.toOption.isDefined)
            case other => fail(s"expected PlutusScriptEvaluationError, got $other")
    }
}
