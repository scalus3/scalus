package scalus.uplc.eval

import io.bullet.borer.Cbor
import scalus.builtin.Data
import scalus.cardano.ledger.*
import scalus.uplc.{Constant, DeBruijnedProgram, Term}

import scala.scalajs.js
import scala.scalajs.js.JSConverters.*
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

@JSExportTopLevel("Scalus")
object JScalus {

    extension (self: ExUnits)
        /** Converts ExUnits to a JavaScript BigInt representation. */
        def toJSExUnits: JSExUnits = new JSExUnits(
          steps = js.BigInt(self.steps.toString),
          memory = js.BigInt(self.memory.toString)
        )

    extension (self: Result)
        /** Converts Result to JSResult. */
        def toJSResult: JSResult = self match
            case Result.Success(_, budget, _, logs) =>
                JSResult(
                  isSuccess = true,
                  budget = budget.toJSExUnits,
                  logs = js.Array(logs*)
                )
            case Result.Failure(exception, budget, _, logs) =>
                JSResult(
                  isSuccess = false,
                  budget = budget.toJSExUnits,
                  logs = js.Array(exception.getMessage +: logs*)
                )

    @JSExportTopLevel("ExUnits")
    class JSExUnits(val memory: js.BigInt, val steps: js.BigInt) extends js.Object

    @JSExportTopLevel("Result")
    class JSResult(val isSuccess: Boolean, val budget: JSExUnits, val logs: js.Array[String])
        extends js.Object

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
      *   The JSON representation of the [[Data]] argument to apply.
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
                  logs = js.Array(exception.getMessage)
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
        txCborBytes: js.Array[Byte],
        utxoCborBytes: js.Array[Byte],
        slotConfig: SlotConfig,
        costModels: js.Array[js.Array[Long]]
    ): js.Array[Redeemer] = {
        try
            val tx = Transaction.fromCbor(txCborBytes.toArray)
            val utxo =
                Cbor.decode(utxoCborBytes.toArray)
                    .to[Map[TransactionInput, TransactionOutput]]
                    .value
            val cms = CostModels(costModels.zipWithIndex.map { case (cm, lang) =>
                lang -> cm.toIndexedSeq
            }.toMap)
            val evaluator = PlutusScriptEvaluator(
              slotConfig = slotConfig,
              initialBudget = ExUnits(Long.MaxValue, Long.MaxValue),
              protocolMajorVersion = CardanoInfo.mainnet.majorProtocolVersion,
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
