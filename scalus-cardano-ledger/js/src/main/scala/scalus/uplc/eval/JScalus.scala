package scalus.uplc.eval

import scalus.uplc.builtin.Data
import scalus.cardano.ledger.*
import scalus.uplc.{Constant, DeBruijnedProgram, Term}
import scalus.utils.scalajs.internal.*

import scala.scalajs.js
import scala.scalajs.js.JSConverters.*
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}
import scala.scalajs.js.typedarray.Uint8Array

/** The `Scalus` namespace object.
  *
  * @deprecated
  *   Use the top-level exports instead; this namespace object remains for backwards compatibility.
  *   `applyDataArgToScript` becomes `uplc.applyParamsToScript`, `evaluateScript` becomes
  *   `evaluator.evaluateScript` with an `EvaluationOptions`, `evalPlutusScripts` becomes
  *   `evaluator.evaluateTx`, and `evaluateScriptProfile` is not replaced.
  */
@JSExportTopLevel("Scalus")
object JScalus {
    bindExports(this)

    /** Applies one argument to a Plutus script and returns the applied script. Use it to bake a
      * parameter into a parameterized validator before you compute its script hash.
      *
      * @param doubleCborHex
      *   The double-CBOR-encoded hex representation of the Plutus script.
      * @param data
      *   The argument in the standard Plutus Data JSON encoding, for example `{"int":42}` or
      *   `{"constructor":0,"fields":[{"bytes":"deadbeef"}]}`.
      * @return
      *   The double-CBOR-encoded hex representation of the script with the argument applied.
      * @deprecated
      *   (since 1.2.0) Use `uplc.applyParamsToScript`, which takes a list of parameters and accepts
      *   hex or bytes. It reads Data as CBOR rather than JSON, so convert first;
      *   `uplc.applyParamsToScript(script, [cborHex])` then returns what this function returns.
      */
    @JSExport
    @JSExportTopLevel("applyDataArgToScript")
    def applyDataArgToScript(doubleCborHex: String, data: String): String = {
        val program = DeBruijnedProgram.fromDoubleCborHex(doubleCborHex)
        val arg = Data.fromJson(data)
        val applied = program $ Term.Const(Constant.Data(arg))
        applied.doubleCborHex
    }

    /** Evaluates a Plutus script on its own, with no transaction around it. The script runs on a
      * Plutus V3 machine with the mainnet cost models of the current mainnet protocol major version
      * (11, van Rossem).
      *
      * This never throws. A script that fails, and a script whose hex cannot even be decoded, both
      * come back as a result with `isSuccess: false` and the message in `logs[0]`.
      *
      * @param doubleCborHex
      *   The double-CBOR-encoded hex representation of the Plutus script.
      * @return
      *   The outcome, with the units spent and the trace logs.
      * @deprecated
      *   (since 1.2.0) Use `evaluator.evaluateScript(script, args, options)`, which takes the
      *   language, protocol version and cost model explicitly instead of assuming Plutus V3 on
      *   mainnet, and reports why a script failed in `error`.
      *   `EvaluationOptions.mainnet("PlutusV3")` reproduces this function's configuration.
      */
    @JSExport
    @JSExportTopLevel("evaluateScript")
    def evaluateScript(doubleCborHex: String): JEvaluationResult =
        try
            val program = DeBruijnedProgram.fromDoubleCborHex(doubleCborHex)
            JEvaluationResult.legacy(PlutusVM.makePlutusV3VM().evaluateScriptDebug(program))
        catch case exception: Exception => JEvaluationResult.unreadable(exception.getMessage)

    /** Evaluates a Plutus script with profiling enabled.
      *
      * Same evaluation and same never-throws contract as [[evaluateScript]], but the result also
      * carries the machine's profiling data as JSON in `profileJson`: cost per source location,
      * cost per builtin, and the transition edges between them.
      *
      * @param doubleCborHex
      *   The double-CBOR-encoded hex representation of the Plutus script.
      * @return
      *   The outcome, with `profileJson` populated.
      * @deprecated
      *   (since 1.2.0) Not replaced: `evaluator.evaluateScript` does not profile. Attributing cost
      *   to source lines needs the compiler output that produced the script, so profile from the
      *   JVM, where `PlutusVM.evaluateScriptProfile` and `ProfileFormatter` render the full report.
      *   This function keeps working meanwhile.
      */
    @JSExport
    @JSExportTopLevel("evaluateScriptProfile")
    def evaluateScriptProfile(doubleCborHex: String): JEvaluationResult =
        try
            val program = DeBruijnedProgram.fromDoubleCborHex(doubleCborHex)
            JEvaluationResult.legacy(PlutusVM.makePlutusV3VM().evaluateScriptProfile(program))
        catch case exception: Exception => JEvaluationResult.unreadable(exception.getMessage)

    /** Evaluates every Plutus script a transaction runs, and reports what each one costs. Use it to
      * fill in a transaction's execution units before you balance and submit it.
      *
      * @param txCborBytes
      *   CBOR bytes of the transaction whose scripts should run.
      * @param utxoCborBytes
      *   CBOR bytes of the UTxO set the scripts see: a CBOR map whose keys are transaction inputs
      *   (a `[transactionHash, outputIndex]` pair) and whose values are transaction outputs, as in
      *   the Cardano ledger CDDL. It must resolve every input and reference input of the
      *   transaction.
      * @param slotConfig
      *   Slot arithmetic for the target network, used to turn the transaction's validity interval
      *   into the POSIX times the scripts observe.
      * @param costModels
      *   One cost model per Plutus language version, indexed by position: `costModels[0]` is Plutus
      *   V1, `[1]` is V2, `[2]` is V3. Each inner array holds that version's cost parameters in
      *   protocol-parameter order. Give a model for every version the transaction uses; since the
      *   position is the version, an earlier version cannot be skipped.
      * @param protocolMajorVersion
      *   Cardano protocol major version, which picks the builtin semantics and the costing rules.
      *   Defaults to the current mainnet version, 11 (van Rossem).
      * @return
      *   One entry per redeemer of the transaction, carrying the units that redeemer's script
      *   spent.
      * @throws PlutusScriptEvaluationError
      *   if a script fails; it names the redeemer, classifies the failure, carries the script's
      *   traces and the arguments the script saw.
      * @throws Error
      *   for any other failure: malformed transaction or UTxO CBOR, an input no entry of the UTxO
      *   set resolves, a script the transaction does not carry.
      * @deprecated
      *   (since 1.2.0) Use `evaluator.evaluateTx(tx, utxos, slotConfig, costModels,
      *   protocolMajorVersion)`, the same evaluation. It takes the resolved inputs as CBOR
      *   `[input, output]` pairs instead of one map, the cost models by language name instead of by
      *   position, and the protocol version explicitly instead of defaulting to mainnet's.
      */
    @JSExport
    @JSExportTopLevel("evalPlutusScripts")
    def evalPlutusScripts(
        txCborBytes: Uint8Array,
        utxoCborBytes: Uint8Array,
        slotConfig: JsSlotConfig,
        costModels: js.Array[js.Array[Double]],
        protocolMajorVersion: Int = CardanoInfo.mainnet.majorProtocolVersion.version
    ): js.Array[JRedeemerBudget] = surfacingErrors {
        val tx = Transaction.fromCbor(txCborBytes.toByteArray)
        val utxo = JsCbor.decode[Map[TransactionInput, TransactionOutput]](utxoCborBytes)
        val cms = CostModels(costModels.zipWithIndex.map { case (cm, lang) =>
            lang -> cm.toIndexedSeq.map(_.toLong)
        }.toMap)
        JEvaluator.evaluate(tx, utxo, slotConfig.underlying, cms, protocolMajorVersion).toJSArray
    }
}
