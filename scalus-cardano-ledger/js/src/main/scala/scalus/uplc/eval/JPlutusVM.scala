package scalus.uplc.eval

import scalus.cardano.ledger.ExUnits
import scalus.interop.{TsIgnore, TsType}

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel

/** A Plutus machine, built once from its options, to evaluate many scripts.
  *
  * `evaluator.evaluateScript` builds the machine from the cost model on every call. A `PlutusVM`
  * builds it once, so repeated evaluations skip that work.
  *
  * ```ts
  * const vm = new PlutusVM(EvaluationOptions.mainnet("PlutusV3"))
  * for (const script of scripts) vm.evaluateScript(script, [])
  * ```
  *
  * @param options
  *   the language, protocol version and cost model, and optionally a `maxBudget` that applies to
  *   every evaluation
  * @throws TypeError
  *   if the options cannot be read
  */
@JSExportTopLevel("PlutusVM")
class JPlutusVM(options: JEvaluationOptions) extends js.Object {

    @TsIgnore private[eval] final val machine: PlutusVM = JEvaluationOptions.machine(options)

    @TsIgnore private[eval] final val maxBudget: Option[ExUnits] =
        JEvaluationOptions.maxBudget(options)

    /** Applies the arguments to the script, runs it, and reports what it cost, as
      * `evaluator.evaluateScript` does with this machine's options.
      *
      * @param script
      *   the script, as hex or bytes of raw flat, single CBOR or double CBOR, or a `Program`
      * @param args
      *   the arguments, each a CBOR-encoded `Data`, applied left to right
      * @throws TypeError
      *   if the script or an argument cannot be read
      */
    def evaluateScript(
        @TsType("string | Uint8Array | Program") script: js.Any,
        @TsType("readonly (string | Uint8Array)[]") args: js.Any
    ): JEvaluationResult = {
        val spender =
            maxBudget.fold[BudgetSpender](CountingBudgetSpender())(RestrictingBudgetSpender(_))
        val program = JUplc.applyAll(JProgram.of(script), JUplc.decodeArgs(args))
        val logger = Log()
        // Every exception is a failed result: the machine's own errors are script failures,
        // anything else a defect in Scalus, which `error.code` reports as INTERNAL_ERROR.
        JEvaluationResult.of(
          try
              val term = machine.evaluateScript(program, spender, logger)
              Result.Success(term, spender.getSpentBudget, Map.empty, logger.getLogs.toSeq)
          catch
              case e: Exception =>
                  Result.Failure(e, spender.getSpentBudget, Map.empty, logger.getLogs.toSeq)
        )
    }
}
