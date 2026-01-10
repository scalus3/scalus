package scalus.testing.eval

import scalus.compiler.sir.SIR
import scalus.toUplc
import scalus.uplc.{DeBruijn, UplcCli, UplcEvalResult}
import scalus.uplc.eval.Result

/** JVM-specific extensions for Evaluator that include UplcCli support. */
object EvaluatorJvm:

    /** Evaluate SIR using external uplc CLI tool.
      *
      * Only available on JVM platform where the uplc binary can be executed.
      *
      * @param sir
      *   The SIR to evaluate
      * @param generateErrorTraces
      *   Whether to include error traces
      * @return
      *   UplcEvalResult from the external tool
      */
    def evalUplcCli(sir: SIR, generateErrorTraces: Boolean = true): UplcEvalResult =
        val program = sir.toUplc(generateErrorTraces).plutusV3
        UplcCli.evalFlat(program)

    /** Evaluate SIR across multiple backends and VMs, including UplcCli.
      *
      * This is the JVM-specific version that includes UplcCli evaluation.
      *
      * @param sir
      *   The SIR to evaluate
      * @param config
      *   Evaluation configuration
      * @return
      *   MultiEvalResult with results from all backends/VMs and UplcCli
      */
    def evalMultiWithUplcCli(sir: SIR, config: EvalConfig): MultiEvalResult =
        val baseResult = Evaluator.evalMulti(sir, config)

        val uplcResult =
            if config.useUplcCli then
                Some(evalUplcCli(sir, config.compilerOptions.generateErrorTraces))
            else None

        baseResult.copy(uplcCliResult = uplcResult)

    /** Compare CEK result with UplcCli result.
      *
      * @param cekResult
      *   Result from CEK evaluation
      * @param uplcResult
      *   Result from UplcCli evaluation
      * @return
      *   true if both results are consistent (both success with same term or both failure)
      */
    def resultsConsistent(cekResult: Result, uplcResult: UplcEvalResult): Boolean =
        (cekResult, uplcResult) match
            case (Result.Success(cekTerm, _, _, _), UplcEvalResult.Success(uplcTerm, _)) =>
                val normalizedUplc = DeBruijn.fromDeBruijnTerm(DeBruijn.deBruijnTerm(uplcTerm))
                cekTerm α_== normalizedUplc
            case (_: Result.Failure, _: UplcEvalResult.UplcFailure) =>
                true
            case _ =>
                false
