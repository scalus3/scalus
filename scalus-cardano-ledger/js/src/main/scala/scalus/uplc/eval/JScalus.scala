package scalus.uplc.eval

import io.bullet.borer.Cbor
import scalus.builtin.Data
import scalus.cardano.ledger.*
import scalus.uplc.{Constant, DeBruijnedProgram, Term}
import scalus.utils.Macros

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}
import js.JSConverters.*

@JSExportTopLevel("Scalus")
object JScalus {

    extension (self: ExBudget)
        /** Converts ExBudget to a JavaScript BigInt representation. */
        def toJSExBudget: JSExBudget = new JSExBudget(
          cpu = js.BigInt(self.cpu.toString),
          memory = js.BigInt(self.memory.toString)
        )

    extension (self: Result)
        /** Converts Result to JSResult. */
        def toJSResult: JSResult = self match
            case Result.Success(_, budget, _, logs) =>
                JSResult(
                  isSuccess = true,
                  budget = budget.toJSExBudget,
                  logs = js.Array(logs*)
                )
            case Result.Failure(exception, budget, _, logs) =>
                JSResult(
                  isSuccess = false,
                  budget = budget.toJSExBudget,
                  logs = js.Array(exception.getMessage +: logs*)
                )

    @JSExportTopLevel("ExBudget")
    class JSExBudget(val cpu: js.BigInt, val memory: js.BigInt) extends js.Object

    @JSExportTopLevel("Result")
    class JSResult(val isSuccess: Boolean, val budget: JSExBudget, val logs: js.Array[String])
        extends js.Object

    @JSExportTopLevel("Redeemer")
    class Redeemer(
        val tag: String,
        val index: Int,
        val budget: JSExBudget
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
                  budget = ExBudget.zero.toJSExBudget,
                  logs = js.Array(exception.getMessage)
                )
    }

    private inline def inlineResource(name: String): String =
        ${ Macros.inlineResource('name) }

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
        slotConfig: SlotConfig
    ): js.Array[Redeemer] = {
        val tx = Transaction.fromCbor(txCborBytes.toArray)
        val utxo =
            Cbor.decode(utxoCborBytes.toArray).to[Map[TransactionInput, TransactionOutput]].value
        val params: ProtocolParams = CardanoInfo.mainnet.protocolParams
        val costModels = params.costModels
        val evaluator = PlutusScriptEvaluator(
          slotConfig = slotConfig,
          initialBudget = ExBudget.enormous,
          protocolMajorVersion = CardanoInfo.mainnet.majorProtocolVersion,
          costModels = costModels,
          mode = EvaluatorMode.EvaluateAndComputeCost,
          debugDumpFilesForTesting = false
        )
        val results =
            for r <- evaluator.evalPlutusScripts(tx, utxo)
            yield new Redeemer(
              tag = r.tag.toString,
              index = r.index,
              budget = JSExBudget(
                cpu = js.BigInt(r.exUnits.steps.toString),
                memory = js.BigInt(r.exUnits.memory.toString)
              )
            )
        results.toJSArray
    }
}
