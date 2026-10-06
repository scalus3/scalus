package scalus.uplc.eval

import scalus.cardano.ledger.{PlutusScriptEvaluator, Transaction}
import scalus.interop.{TsName, TsType}
import scalus.utils.scalajs.internal.*

import scala.scalajs.js
import scala.scalajs.js.JSConverters.*
import scala.scalajs.js.annotation.JSExportTopLevel

/** What a `TxEvaluator` evaluates under: the arguments `evaluator.evaluateTx` takes besides the
  * transaction and its inputs.
  */
@TsName("TxEvaluatorOptions")
trait JTxEvaluatorOptions extends js.Object {

    /** The chain's slot arithmetic: a `SlotConfig`, or any object with the same three fields. */
    val slotConfig: JSlotConfigLike

    /** Cost parameters per language, keyed by name: a `CostModels`, or any object with some of its
      * fields.
      */
    val costModels: JCostModelsLike

    /** Picks the builtin semantics and the costing rules. */
    val protocolMajorVersion: Double

    /** The execution units all of a transaction's scripts may spend together, usually the
      * protocol's maximum transaction execution units. Without it, execution is not bounded.
      */
    val maxBudget: js.UndefOr[JExUnitsLike] = js.undefined
}

/** Evaluates the Plutus scripts of transactions under one set of protocol parameters.
  *
  * It does what `evaluator.evaluateTx` does, but prepares the cost models once, when it is created,
  * instead of on every call. Keep one for as long as the parameters stay the same, and create a new
  * one when they change.
  *
  * ```ts
  * const ev = new TxEvaluator({ slotConfig, costModels, protocolMajorVersion: 11 })
  * ev.evaluate(tx1, utxos1)
  * ev.evaluate(tx2, utxos2)
  * ```
  *
  * @throws TypeError
  *   if an option cannot be read
  */
@JSExportTopLevel("TxEvaluator")
class JTxEvaluator(options: JTxEvaluatorOptions) extends js.Object {

    private val evaluator: PlutusScriptEvaluator = {
        // Read untyped: a typed read of a wrong-typed field is undefined behaviour in Scala.js.
        val record = options.asInstanceOf[js.Dynamic]
        JEvaluator.evaluatorOf(
          record.slotConfig,
          record.costModels,
          record.protocolMajorVersion,
          record.maxBudget
        )
    }

    /** Evaluates every Plutus script of a transaction and reports what each redeemer cost, as
      * `evaluator.evaluateTx` does.
      *
      * @param tx
      *   the transaction, as hex or bytes
      * @param utxos
      *   the resolved inputs and reference inputs, each a `Utxo` or an `[input, output]` pair as
      *   hex or bytes
      * @return
      *   one entry per redeemer, carrying the units that redeemer's script spent
      * @throws PlutusScriptEvaluationError
      *   if a script fails
      * @throws TypeError
      *   if an input cannot be read
      */
    def evaluate(
        @TsType("string | Uint8Array") tx: js.Any,
        @TsType("readonly (string | Uint8Array | Utxo)[]") utxos: js.Any
    ): js.Array[JRedeemerBudget] = surfacingErrors {
        val transaction = decodeOf(tx, "tx")(Transaction.fromCbor(_))
        JEvaluator.run(evaluator, transaction, JEvaluator.utxoMapOf(utxos)).toJSArray
    }
}
