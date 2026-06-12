package scalus.uplc.eval

import io.bullet.borer.Cbor
import scalus.uplc.builtin.Data
import scalus.cardano.ledger.*
import scalus.uplc.{Constant, DeBruijnedProgram, Term}

import scala.scalajs.js
import scala.scalajs.js.JSConverters.*
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}
import scala.scalajs.js.typedarray.Uint8Array

@JSExportTopLevel("Scalus")
object JScalus {

    extension (self: ExUnits)
        /** Converts ExUnits to a JavaScript BigInt representation. */
        def toJSExUnits: JSExUnits = new JSExUnits(
          steps = js.BigInt(self.steps.toString),
          memory = js.BigInt(self.memory.toString)
        )

    extension (self: Result)
        /** Converts Result to JSResult. When the result carries profiling data (i.e. it was
          * produced with profiling enabled), the `profileJson` field is set to the profile rendered
          * as JSON; otherwise it is `undefined`.
          *
          * Only the lightweight JSON rendering is exposed here on purpose: the HTML report and its
          * source-annotation machinery live in [[ProfileFormatter.toHtml]] / `loadSources`, which
          * are deliberately not referenced from the JS facade so they stay dead-code-eliminated out
          * of the (transaction-builder) `scalus.js` bundle.
          */
        def toJSResult: JSResult =
            val profileJson: js.UndefOr[String] = self.profile match
                case Some(p) => ProfileFormatter.toJson(p)
                case None    => js.undefined
            self match
                case s: Result.Success =>
                    JSResult(
                      isSuccess = true,
                      budget = s.budget.toJSExUnits,
                      logs = js.Array(s.logs*),
                      profileJson = profileJson
                    )
                case f: Result.Failure =>
                    JSResult(
                      isSuccess = false,
                      budget = f.budget.toJSExUnits,
                      logs = js.Array(f.exception.getMessage +: f.logs*),
                      profileJson = profileJson
                    )

    @JSExportTopLevel("ExUnits")
    class JSExUnits(val memory: js.BigInt, val steps: js.BigInt) extends js.Object

    @JSExportTopLevel("Result")
    class JSResult(
        val isSuccess: Boolean,
        val budget: JSExUnits,
        val logs: js.Array[String],
        /** Profiling data as JSON; `undefined` unless the script was evaluated with profiling (see
          * [[evaluateScriptProfile]]).
          */
        val profileJson: js.UndefOr[String]
    ) extends js.Object

    @JSExportTopLevel("Redeemer")
    class Redeemer(
        val tag: String,
        val index: Int,
        val budget: JSExUnits
    ) extends js.Object

    @JSExportTopLevel("PlutusScriptEvaluationError")
    class JSPlutusScriptEvaluationError(
        val message: String,
        val logs: js.Array[String]
    ) extends js.Object

    /** Applies a data argument to a Plutus script given its CBOR hex representation.
      *
      * @param doubleCborHex
      *   The double-CBOR-encoded hex representation of the Plutus script.
      * @param data
      *   The JSON representation of the [[scalus.uplc.builtin.Data]] argument to apply.
      * @return
      *   The double-CBOR-encoded hex representation of the script with the data argument applied.
      */
    @JSExport
    def applyDataArgToScript(doubleCborHex: String, data: String): String = {
        // Parse script and data from hex
        val program = DeBruijnedProgram.fromDoubleCborHex(doubleCborHex)
        val arg = Data.fromJson(data)
        val applied = program $ Term.Const(Constant.Data(arg))
        applied.doubleCborHex
    }

    /** Evaluates a Plutus script with the given CBOR hex representation.
      *
      * @param doubleCborHex
      *   The double-CBOR-encoded hex representation of the Plutus script.
      * @return
      *   A JSResult containing the evaluation result, budget, and logs.
      */
    @JSExport
    def evaluateScript(doubleCborHex: String): JSResult = {
        try
            // Parse script from hex
            val program = DeBruijnedProgram.fromDoubleCborHex(doubleCborHex)
            // Create appropriate VM based on version
            val vm = PlutusVM.makePlutusV3VM()
            // Evaluate script
            vm.evaluateScriptDebug(program).toJSResult
        catch
            case exception: Exception =>
                JSResult(
                  isSuccess = false,
                  budget = ExUnits.zero.toJSExUnits,
                  logs = js.Array(exception.getMessage),
                  profileJson = js.undefined
                )
    }

    /** Evaluates a Plutus script with profiling enabled.
      *
      * Like [[evaluateScript]], but the returned [[JSResult]] additionally carries the CEK machine
      * profiling data as JSON in `profileJson` (per-source-location and per-builtin cost, plus the
      * transition edges). To turn that data into the interactive HTML report (sortable tables, hot
      * paths/edges, annotated source) use the Scala/JVM `ProfileFormatter`; the HTML renderer is
      * intentionally kept out of `scalus.js` to keep the transaction-builder bundle small.
      *
      * @param doubleCborHex
      *   The double-CBOR-encoded hex representation of the Plutus script.
      * @return
      *   A JSResult with `profileJson` populated.
      */
    @JSExport
    def evaluateScriptProfile(doubleCborHex: String): JSResult = {
        try
            val program = DeBruijnedProgram.fromDoubleCborHex(doubleCborHex)
            val vm = PlutusVM.makePlutusV3VM()
            vm.evaluateScriptProfile(program).toJSResult
        catch
            case exception: Exception =>
                JSResult(
                  isSuccess = false,
                  budget = ExUnits.zero.toJSExUnits,
                  logs = js.Array(exception.getMessage),
                  profileJson = js.undefined
                )
    }

    /** Evaluates all Plutus scripts in a transaction against the provided UTxO set.
      *
      * @param txCborBytes
      *   The CBOR bytes of the transaction containing the Plutus scripts to evaluate.
      * @param utxoCborBytes
      *   The CBOR bytes of the UTxO [[Map[TransactionInput, TransactionOutput]]] to use for
      *   evaluation.
      * @return
      */
    @JSExport
    def evalPlutusScripts(
        txCborBytes: Uint8Array,
        utxoCborBytes: Uint8Array,
        slotConfig: SlotConfig,
        costModels: js.Array[js.Array[Double]],
        protocolMajorVersion: Int = CardanoInfo.mainnet.majorProtocolVersion.version
    ): js.Array[Redeemer] = {
        try
            val tx = Transaction.fromCbor(txCborBytes.toArray.map(_.toByte))
            val utxo =
                Cbor.decode(utxoCborBytes.toArray.map(_.toByte))
                    .to[Map[TransactionInput, TransactionOutput]]
                    .value
            val cms = CostModels(costModels.zipWithIndex.map { case (cm, lang) =>
                lang -> cm.toIndexedSeq.map(_.toLong)
            }.toMap)
            val evaluator = PlutusScriptEvaluator(
              slotConfig = slotConfig,
              initialBudget = ExUnits(Long.MaxValue, Long.MaxValue),
              protocolMajorVersion = MajorProtocolVersion(protocolMajorVersion),
              costModels = cms,
              mode = EvaluatorMode.EvaluateAndComputeCost
            )
            val results =
                for r <- evaluator.evalPlutusScripts(tx, utxo)
                yield new Redeemer(
                  tag = r.tag.toString,
                  index = r.index,
                  budget = JSExUnits(
                    steps = js.BigInt(r.exUnits.steps.toString),
                    memory = js.BigInt(r.exUnits.memory.toString)
                  )
                )
            results.toJSArray
        catch
            case e: PlutusScriptEvaluationException =>
                throw js.JavaScriptException(
                  JSPlutusScriptEvaluationError(e.getMessage, js.Array(e.logs*))
                )
    }

}
