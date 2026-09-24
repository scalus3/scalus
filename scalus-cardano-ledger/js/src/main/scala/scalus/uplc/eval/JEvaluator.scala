package scalus.uplc.eval

import io.bullet.borer.Cbor
import scalus.cardano.ledger.*
import scalus.interop.{TsName, TsType}
import scalus.utils.scalajs.internal.*

import scala.scalajs.js
import scala.scalajs.js.JSConverters.*
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

/** Synchronous evaluation of one script, or of every script of a transaction. */
@JSExportTopLevel("evaluator")
object JEvaluator {
    bindExports(this)

    /** Applies the arguments to the script, runs it, and reports what it cost.
      *
      * A script that fails comes back as a result with `isSuccess: false` and an `error` saying
      * why, together with the budget it spent and the traces it emitted. A Plutus V3 script must
      * return unit (CIP-117); V1 and V2 may return anything, as in the ledger.
      *
      * The script is not checked against the language and protocol you name: a builtin that
      * protocol never had still runs. Whether a chain would accept the script is
      * `Script.isWellFormed`'s question.
      *
      * @param script
      *   the script, as hex or bytes of raw flat, single CBOR or double CBOR
      * @param args
      *   the arguments, each a CBOR-encoded `Data`, applied left to right
      * @param options
      *   the language, protocol version and cost model, and optionally a `maxBudget`
      * @throws TypeError
      *   if the script, an argument or the options cannot be read
      */
    @JSExport
    def evaluateScript(
        @TsType("string | Uint8Array") script: js.Any,
        @TsType("readonly (string | Uint8Array)[]") args: js.Any,
        options: JEvaluationOptions
    ): JEvaluationResult = {
        val vm = JEvaluationOptions.machine(options)
        val spender = JEvaluationOptions
            .maxBudget(options)
            .fold[BudgetSpender](CountingBudgetSpender())(RestrictingBudgetSpender(_))
        val program = JUplc.applyAll(JUplc.decodeScript(script), JUplc.decodeArgs(args))
        val logger = Log()
        // Every exception is a failed result: the machine's own errors are script failures,
        // anything else a defect in Scalus, which `error.code` reports as INTERNAL_ERROR.
        JEvaluationResult.of(
          try
              val term = vm.evaluateScript(program, spender, logger)
              Result.Success(term, spender.getSpentBudget, Map.empty, logger.getLogs.toSeq)
          catch
              case e: Exception =>
                  Result.Failure(e, spender.getSpentBudget, Map.empty, logger.getLogs.toSeq)
        )
    }

    /** Evaluates every Plutus script of a transaction and reports what each redeemer cost.
      *
      * The resolved inputs arrive as CBOR `[input, output]` pairs, `transaction_unspent_output` in
      * the ledger CDDL: CML's `TransactionUnspentOutput.to_cbor_bytes()`, CST's
      * `TransactionUnspentOutput.toCbor()` and a CIP-30 wallet's `getUtxos()` all produce it. A
      * later pair with the same input replaces an earlier one.
      *
      * @param tx
      *   the transaction, as hex or bytes
      * @param utxos
      *   the resolved inputs and reference inputs, one `[input, output]` pair each, as hex or bytes
      * @param slotConfig
      *   the chain's slot arithmetic: a `SlotConfig`, or any object with the same three fields
      * @param costModels
      *   cost parameters per language, keyed by name: a `CostModels`, or any object with some of
      *   its fields
      * @param protocolMajorVersion
      *   picks the builtin semantics and the costing rules
      * @return
      *   one entry per redeemer, carrying the units that redeemer's script spent
      * @throws PlutusScriptEvaluationError
      *   if a script fails; it names the redeemer and carries the arguments the script saw
      * @throws TypeError
      *   if an input cannot be read as described
      * @throws Error
      *   if the transaction evaluator stops for any other reason: an input no pair resolves, a
      *   script the transaction does not carry, a missing datum
      */
    @JSExport
    def evaluateTx(
        @TsType("string | Uint8Array") tx: js.Any,
        @TsType("readonly (string | Uint8Array)[]") utxos: js.Any,
        slotConfig: JSlotConfigLike,
        costModels: JCostModelsLike,
        protocolMajorVersion: Double
    ): js.Array[JRedeemerBudget] = surfacingErrors {
        val transaction = decodeOf(tx, "tx")(Transaction.fromCbor(_))
        val resolved = utxoMapOf(utxos)
        val slots = slotConfig.asInstanceOf[js.Dynamic]
        val slotsConfig = SlotConfig(
          zeroTime = longOf(slots.zeroTime, "slotConfig.zeroTime"),
          zeroSlot = longOf(slots.zeroSlot, "slotConfig.zeroSlot"),
          slotLength = intOf(slots.slotLength, "slotConfig.slotLength")
        )
        val models = costModels.asInstanceOf[js.Dynamic]
        val byLanguage = for
            language <- Seq(Language.PlutusV1, Language.PlutusV2, Language.PlutusV3)
            model = models.selectDynamic(language.toString)
            if !js.isUndefined(model) && model != null
        yield language.ordinal -> arrayOf(model, language.toString).map(longOf)
        val protocol = intOf(protocolMajorVersion, "protocolMajorVersion")
        evaluate(
          transaction,
          resolved,
          slotsConfig,
          CostModels(byLanguage.toMap),
          protocol
        ).toJSArray
    }

    /** The UTxO map the evaluator wants, from `[input, output]` pairs. A later pair wins. */
    private[eval] def utxoMapOf(utxos: js.Any): Map[TransactionInput, TransactionOutput] =
        arrayOf(utxos, "utxos")
            .map(decodeOf(_, _)(Cbor.decode(_).to[(TransactionInput, TransactionOutput)].value))
            .toMap

    /** The evaluation every transaction-level entry point runs, and the one place a failing script
      * becomes a `PlutusScriptEvaluationError`.
      */
    private[scalus] def evaluate(
        tx: Transaction,
        utxo: Map[TransactionInput, TransactionOutput],
        slotConfig: SlotConfig,
        costModels: CostModels,
        protocolMajorVersion: Int
    ): Seq[JRedeemerBudget] = surfacingErrors {
        val evaluator = PlutusScriptEvaluator(
          slotConfig = slotConfig,
          initialBudget = ExUnits(Long.MaxValue, Long.MaxValue),
          protocolMajorVersion = MajorProtocolVersion(protocolMajorVersion),
          costModels = costModels,
          mode = EvaluatorMode.EvaluateAndComputeCost
        )
        try
            for r <- evaluator.evalPlutusScripts(tx, utxo)
            yield new JRedeemerBudget(r.tag.toString, r.index, JExUnits(r.exUnits))
        catch
            case e: PlutusScriptEvaluationException =>
                throw js.JavaScriptException(JPlutusScriptEvaluationError.fromException(e))
    }
}

/** The slot arithmetic `evaluator.evaluateTx` reads. A `SlotConfig` has it, and so does a plain
  * object from any SDK; other fields are ignored.
  *
  * @param zeroTime
  *   POSIX time in milliseconds at which slot `zeroSlot` starts.
  * @param zeroSlot
  *   The slot the configuration is anchored at.
  * @param slotLength
  *   Slot length in milliseconds.
  */
@TsName("SlotConfigLike")
trait JSlotConfigLike extends js.Object {
    @TsType("number | bigint") val zeroTime: js.Any
    @TsType("number | bigint") val zeroSlot: js.Any
    val slotLength: Double
}

/** Cost parameters per Plutus language, each in protocol-parameter order and each a safe-integer
  * `number` or a `bigint`. A `CostModels` has them, and so does a plain object from any SDK; a
  * language the transaction does not use may be absent, and other fields are ignored.
  */
@TsName("CostModelsLike")
trait JCostModelsLike extends js.Object {
    @TsType("readonly (number | bigint)[]") val PlutusV1: js.UndefOr[js.Any] = js.undefined
    @TsType("readonly (number | bigint)[]") val PlutusV2: js.UndefOr[js.Any] = js.undefined
    @TsType("readonly (number | bigint)[]") val PlutusV3: js.UndefOr[js.Any] = js.undefined
}
