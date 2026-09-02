package scalus.compiler.sir.lowering

import scalus.cardano.ledger.Language
import scalus.compiler.Options
import scalus.compiler.sir.lowering.simple.{ScottEncodingLowering, SumOfProductsLowering}
import scalus.compiler.sir.transform.{BooleanOptimizer, StaticArgumentTransformation}
import scalus.compiler.sir.{MutualRecursionElimination, RemoveTraces, SIR, TargetLoweringBackend}
import scalus.uplc.Term
import scalus.uplc.transform.{Optimizer, V1V2Optimizer, V3Optimizer}

/** The single SIR -> UPLC pipeline, shared by [[scalus.uplc.CompiledPlutus]] and the `sir.toUplc`
  * extensions:
  *
  * removeTraces? -> BooleanOptimizer? -> MutualRecursionElimination ->
  * StaticArgumentTransformation? -> lower(backend) -> optimize? -> fill positions
  *
  * BooleanOptimizer and StaticArgumentTransformation both run when `options.optimizeUplc` is set.
  * MutualRecursionElimination is unconditional (backends reject multi-binding recursive lets);
  * running StaticArgumentTransformation after it lifts the peers-as-params static arguments MRE
  * introduces. The backends keep their own MRE calls as safety nets for direct construction - MRE
  * is idempotent, so the second call is a no-op walk.
  *
  * ScalusTag is NOT applied here - it is a program-level concern
  * ([[scalus.uplc.CompiledPlutus.program]]).
  */
object UplcPipeline {

    /** Runs the full SIR -> UPLC pipeline.
      *
      * @param sir
      *   the SIR to lower
      * @param options
      *   drives every step (traces, optimization, backend, protocol version)
      * @param language
      *   the backend's target language, also selecting the default optimizer
      * @param optimizer
      *   what runs when `options.optimizeUplc` is set and `options.uplcOptimizers` is empty
      *   (`uplcOptimizers`, when non-empty, replaces it)
      */
    def run(sir: SIR, options: Options, language: Language, optimizer: Optimizer): Term =
        run(sir, options, language, optimizer, optimizeBooleans = options.optimizeUplc)

    /** [[run]] with the boolean pass forced on or off, so a measurement can compare the two
      * pipelines exactly. Production callers use the four-argument overload, where the pass follows
      * `options.optimizeUplc`.
      */
    private[scalus] def run(
        sir: SIR,
        options: Options,
        language: Language,
        optimizer: Optimizer,
        optimizeBooleans: Boolean
    ): Term = {
        val sir0 = if options.removeTraces then RemoveTraces.transform(sir) else sir
        val sir1 = if optimizeBooleans then BooleanOptimizer.optimize(sir0) else sir0
        val sir2 = MutualRecursionElimination(sir1)
        val sirToLower =
            if options.optimizeUplc then StaticArgumentTransformation(sir2) else sir2
        val uplc = options.targetLoweringBackend match
            case TargetLoweringBackend.ScottEncodingLowering =>
                ScottEncodingLowering(
                  sir = sirToLower,
                  generateErrorTraces = options.generateErrorTraces,
                  targetLanguage = language,
                  targetProtocolVersion = options.targetProtocolVersion
                ).lower()
            case TargetLoweringBackend.SumOfProductsLowering =>
                SumOfProductsLowering(
                  sir = sirToLower,
                  generateErrorTraces = options.generateErrorTraces,
                  targetLanguage = language,
                  targetProtocolVersion = options.targetProtocolVersion
                ).lower()
            case TargetLoweringBackend.SirToUplcV3Lowering =>
                SirToUplcV3Lowering(
                  sir = sirToLower,
                  generateErrorTraces = options.generateErrorTraces,
                  debug = options.debug,
                  warnListConversions = options.warnListConversions,
                  noWarn = options.noWarn,
                  targetLanguage = language,
                  targetProtocolVersion = options.targetProtocolVersion,
                  intrinsicModules = IntrinsicResolver.intrinsicModulesFor(options.valueBuiltins),
                  supportModules = IntrinsicResolver.defaultSupportModules
                ).lower()
        val optimized =
            if options.uplcOptimizers.nonEmpty then
                options.uplcOptimizers.foldLeft(uplc)((term, opt) => opt(term))
            else if options.optimizeUplc then optimizer(uplc)
            else uplc
        // Give every still-position-less node a source location, so profiling and source-traces can
        // attribute the cost of generated/optimized spines (the UPLC optimizer rebuilds Apply/Case/
        // Constr nodes without positions). Run on the FINAL term, after optimization: bottom-up so a
        // spine node inherits the location of the leaf it operates on (where positions actually sit),
        // then top-down to fill any node with no positioned descendant from its nearest positioned
        // ancestor. Positions never affect flat encoding, budget, or evaluation — only diagnostics.
        optimized.fillEmptyPosBottomUp._1.fillEmptyPosTopDown(scalus.utils.ScalusSourcePos.empty)
    }

    /** The optimizer `run` should use when the caller has no version-specific one: V1/V2 ->
      * [[V1V2Optimizer]] (V3Optimizer's CaseConstrApply emits Case/Constr terms that are illegal
      * before Plutus V3); otherwise [[V3Optimizer]] configured from the options.
      */
    def defaultOptimizer(language: Language, options: Options): Optimizer =
        language match
            case Language.PlutusV1 | Language.PlutusV2 => new V1V2Optimizer()
            case _ => new V3Optimizer(options.cseIterations, options.cceEnabled)
}
