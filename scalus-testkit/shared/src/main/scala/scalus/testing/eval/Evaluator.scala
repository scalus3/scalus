package scalus.testing.eval

import scalus.cardano.ledger.ExUnits
import scalus.compiler.Options
import scalus.compiler.sir.{SIR, TargetLoweringBackend}
import scalus.toUplc
import scalus.uplc.eval.{PlutusVM, Result}
import scalus.uplc.{Compiled, DeBruijn, Program, Term, UplcEvalResult}

/** Configuration for multi-backend evaluation.
  *
  * @param vms
  *   PlutusVM instances to test against
  * @param backends
  *   TargetLoweringBackend configurations to test
  * @param useUplcCli
  *   Whether to include UplcCli evaluation (JVM-only, ignored on JS/Native)
  * @param compilerOptions
  *   Compiler options to use for SIR lowering
  */
case class EvalConfig(
    vms: Seq[PlutusVM] = Seq(PlutusVM.makePlutusV3VM()),
    backends: Seq[TargetLoweringBackend] = Seq(TargetLoweringBackend.SirToUplcV3Lowering),
    useUplcCli: Boolean = false,
    compilerOptions: Options = Options.default
)

object EvalConfig:
    /** Default config with V3 VM only */
    val default: EvalConfig = EvalConfig()

    /** Config for comprehensive stdlib testing - all backends, all VMs */
    val comprehensive: EvalConfig = EvalConfig(
      vms = Seq(
        PlutusVM.makePlutusV1VM(),
        PlutusVM.makePlutusV2VM(),
        PlutusVM.makePlutusV3VM()
      ),
      backends = TargetLoweringBackend.values.toSeq,
      useUplcCli = true
    )

    /** Config for prelude tests - all backends with UplcCli */
    val prelude: EvalConfig = EvalConfig(
      backends = TargetLoweringBackend.values.toSeq,
      useUplcCli = true
    )

/** Result from evaluating across multiple backends/VMs.
  *
  * @param results
  *   Map from (backend, vm) pair to Result
  * @param uplcCliResult
  *   Optional result from UplcCli (JVM-only)
  */
case class MultiEvalResult(
    results: Map[(TargetLoweringBackend, PlutusVM), Result],
    uplcCliResult: Option[UplcEvalResult] = None
):
    /** Check if all results are successful */
    def allSucceeded: Boolean =
        results.values.forall(_.isSuccess) &&
            uplcCliResult.forall(_.isInstanceOf[UplcEvalResult.Success])

    /** Check if all results are failures */
    def allFailed: Boolean =
        results.values.forall(_.isFailure) &&
            uplcCliResult.forall(_.isInstanceOf[UplcEvalResult.UplcFailure])

    /** Check if all successful results have the same term (alpha-equivalent) */
    def allTermsEqual: Boolean =
        val successTerms = results.values.collect { case r: Result.Success => r.term }.toSeq
        successTerms.sliding(2).forall {
            case Seq(a, b) => a α_== b
            case _         => true
        }

    /** Get all budgets from successful evaluations */
    def budgets: Map[(TargetLoweringBackend, PlutusVM), ExUnits] =
        results.collect { case (k, r: Result.Success) => k -> r.budget }

/** Core evaluation functions - framework agnostic, pure functions returning Result.
  *
  * These primitives form the foundation of the testing API.
  */
object Evaluator:

    /** Evaluate a Term on a specific PlutusVM.
      *
      * @param term
      *   The UPLC term to evaluate
      * @param vm
      *   The PlutusVM to use for evaluation
      * @return
      *   Result containing success/failure with budget and logs
      */
    def evalTerm(term: Term)(using vm: PlutusVM): Result =
        DeBruijn.deBruijnTerm(term).evaluateDebug

    /** Evaluate a Program on a specific PlutusVM.
      *
      * @param program
      *   The UPLC program to evaluate
      * @param vm
      *   The PlutusVM to use for evaluation
      * @return
      *   Result containing success/failure with budget and logs
      */
    def evalProgram(program: Program)(using vm: PlutusVM): Result =
        vm.evaluateScriptDebug(program.deBruijnedProgram)

    /** Evaluate SIR by lowering to UPLC and evaluating.
      *
      * @param sir
      *   The SIR to evaluate
      * @param backend
      *   The lowering backend to use
      * @param generateErrorTraces
      *   Whether to include error traces
      * @param vm
      *   The PlutusVM to use
      * @return
      *   Result of evaluation
      */
    def evalSIR(
        sir: SIR,
        backend: TargetLoweringBackend = TargetLoweringBackend.SirToUplcV3Lowering,
        generateErrorTraces: Boolean = true
    )(using vm: PlutusVM): Result =
        val term = sir.toUplc(generateErrorTraces, backend)
        DeBruijn.deBruijnTerm(term).evaluateDebug

    /** Evaluate a Compiled script.
      *
      * This evaluates the already-lowered UPLC program from the Compiled instance.
      *
      * @param compiled
      *   The compiled script
      * @param vm
      *   The PlutusVM to use
      * @return
      *   Result of evaluation
      */
    def evalCompiled[A](compiled: Compiled[A])(using vm: PlutusVM): Result =
        vm.evaluateScriptDebug(compiled.program.deBruijnedProgram)

    /** Evaluate SIR across multiple backends and VMs.
      *
      * @param sir
      *   The SIR to evaluate
      * @param config
      *   Evaluation configuration
      * @return
      *   MultiEvalResult with results from all backends/VMs
      */
    def evalMulti(sir: SIR, config: EvalConfig = EvalConfig.default): MultiEvalResult =
        val results = for
            backend <- config.backends
            vm <- config.vms
        yield
            given PlutusVM = vm
            (backend, vm) -> evalSIR(sir, backend, config.compilerOptions.generateErrorTraces)

        // UplcCli is platform-specific - will be populated by JVM extension
        MultiEvalResult(results.toMap, None)
