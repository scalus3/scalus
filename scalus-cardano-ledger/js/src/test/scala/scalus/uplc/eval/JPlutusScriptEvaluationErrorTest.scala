package scalus.uplc.eval

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.{CardanoInfo, CostModels, SlotConfig, Transaction, Utxos}
import scalus.uplc.builtin.Data.{toCbor, toData}
import scalus.utils.Hex

import scala.scalajs.js

class JPlutusScriptEvaluationErrorTest extends AnyFunSuite {

    // Read a `js.UndefOr` inside `assert` through `.toOption`: ScalaTest's macro decomposing a raw
    // `js.UndefOr` chain, such as `assert(r.errorRule.contains(x))`, crashes the Scala.js backend
    // ("Cannot emit primitive conversion ... to Lscala/scalajs/js/$bar;").

    private val costModels: CostModels = CardanoInfo.mainnet.protocolParams.costModels
    private val protocol: Int = CardanoInfo.mainnet.majorProtocolVersion.version

    private def thrownBy(tx: Transaction, utxos: Utxos): JPlutusScriptEvaluationError = {
        val caught = intercept[js.JavaScriptException] {
            JEvaluator.evaluate(tx, utxos, SlotConfig.mainnet, costModels, protocol)
        }
        caught.exception match
            case err: JPlutusScriptEvaluationError => err
            case other => fail(s"expected JPlutusScriptEvaluationError, got $other")
    }

    test(
      "a failing script throws an Error named PlutusScriptEvaluationError with four own fields"
    ) {
        // spec [ER-1] [ER-2] [ER-3] [ER-4] [ER-5] [ER-6] [ER-7] [ER-8] [MSG-1]
        val (tx, utxos) = SampleTransactions.withdrawal(SampleTransactions.failingV3)
        val err = thrownBy(tx, utxos)
        assert((err: Any).isInstanceOf[js.Error])
        assert(err.name == "PlutusScriptEvaluationError")
        val redeemer = err.redeemer.getOrElse(fail("redeemer"))
        assert(redeemer.tag == "Reward" && redeemer.index == 0)
        assert(redeemer.budget.steps > js.BigInt(0))
        assert(err.scriptHash.toOption.contains(SampleTransactions.failingV3.scriptHash.toHex))
        assert(err.code.toOption.contains("SCRIPT_FAILURE"))
        assert(err.logs.toSeq == Seq("boom"))
        val own = js.Object.keys(err.asInstanceOf[js.Object]).toSet
        assert(Set("redeemer", "scriptHash", "code", "logs").subsetOf(own), own.toString)
        assert(!own.contains("args"), own.toString)
        assert(err.message.startsWith("Reward[0] failed: "), err.message)
    }

    test("args carries every Data argument the evaluator applied, in order") {
        // spec [ER-10]
        val cases = Seq(
          (SampleTransactions.withdrawal(SampleTransactions.failingV3), 1),
          (SampleTransactions.failingV2Spend, 3)
        )
        for ((tx, utxos), argCount) <- cases do
            val err = thrownBy(tx, utxos)
            val args = err.args.getOrElse(fail("args"))
            assert(args.length == argCount)
            // datum, redeemer, context, in that order - the V2 spend fixture's inline datum is
            // `42.toData`, and a V3 script takes the context alone.
            if argCount == 3 then assert(args(0) == Hex.bytesToHex(42.toData.toCbor))
    }

    test("args is readable but never printed with the error") {
        // spec [ER-8]
        val (tx, utxos) = SampleTransactions.withdrawal(SampleTransactions.failingV3)
        val err = thrownBy(tx, utxos)
        val args = err.args.getOrElse(fail("args"))
        assert(args.nonEmpty)
        val own = js.Object.keys(err.asInstanceOf[js.Object]).toSet
        assert(!own.contains("args"), own.toString)
        // No replacer: the bigint budget serializes through `ExUnits.toJSON`.
        val json = js.JSON.stringify(err.asInstanceOf[js.Any])
        assert(!json.contains(args.head), json)
        assert(json.contains("\"budget\":{\"memory\":\""), json)
    }

    test("the (message, logs) constructor still works and leaves the new fields undefined") {
        // spec [ER-13]
        val err = new JPlutusScriptEvaluationError("m", js.Array("a"))
        assert(err.message == "m" && err.logs.toSeq == Seq("a"))
        assert(
          err.redeemer.toOption.isEmpty && err.scriptHash.toOption.isEmpty &&
              err.code.toOption.isEmpty
        )
        assert(err.args.toOption.isEmpty)
    }

    test(
      "a script that returns a non-unit value fails with INVALID_RETURN_VALUE on the transaction path"
    ) {
        // spec [ER-5]
        val (tx, utxos) = SampleTransactions.withdrawal(SampleTransactions.returning42V3)
        val err = thrownBy(tx, utxos)
        assert(err.code.toOption.contains("INVALID_RETURN_VALUE"))
        assert(err.logs.isEmpty)
        assert(err.args.getOrElse(fail("args")).length == 1)
    }

    test("any other evaluator failure surfaces as a native Error carrying its message") {
        // spec [TX-12]
        val (tx, _) = SampleTransactions.withdrawal(SampleTransactions.failingV3)
        val caught = intercept[js.JavaScriptException] {
            JEvaluator.evaluate(tx, Map.empty, SlotConfig.mainnet, costModels, protocol)
        }
        caught.exception match
            case e: JPlutusScriptEvaluationError => fail(s"not a script failure: $e")
            case e: js.Error                     => assert(e.message.nonEmpty)
            case other                           => fail(s"expected a JS Error, got $other")
    }
}
