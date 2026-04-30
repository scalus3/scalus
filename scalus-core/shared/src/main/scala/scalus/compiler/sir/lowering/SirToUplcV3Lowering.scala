package scalus.compiler.sir.lowering

import scalus.cardano.ledger.{Language, MajorProtocolVersion}
import scalus.compiler.sir.lowering.*
import scalus.compiler.sir.{Module, SIR, SIRType}
import scalus.uplc.*

import scala.collection.mutable.Map as MutableMap

class SirToUplcV3Lowering(
    sir: SIR,
    generateErrorTraces: Boolean = false,
    upcastTo: SIRType = SIRType.FreeUnificator,
    representation: LoweredValueRepresentation = TypeVarRepresentation(true),
    debug: Boolean = false,
    warnListConversions: Boolean = false,
    noWarn: Boolean = false,
    targetLanguage: Language = Language.PlutusV3,
    targetProtocolVersion: MajorProtocolVersion = MajorProtocolVersion.changPV,
    intrinsicModules: Map[String, Module] = Map.empty,
    supportModules: Map[String, Module] = Map.empty
) {

    private var _lastLoweredValue: Option[LoweredValue] = None

    def toLoweredValue(lctx: LoweringContext = newLoweringContext): LoweredValue = {
        given LoweringContext = lctx
        val v0 = Lowering.lowerSIR(sir)
        val v1 =
            if upcastTo != SIRType.FreeUnificator then v0.upcastOne(upcastTo, v0.pos)
            else {
                // if prod have parent, upcast to parenttype.  Needed for comparing two cases of one sum class in tests
                if SIRType.isProd(v0.sirType) then {
                    SIRType.prodParent(v0.sirType) match
                        case Some(parentType) =>
                            val r = v0.upcastOne(parentType, v0.pos)
                            r
                        case None =>
                            v0
                } else v0
            }
        val targetRepresentation = {
            if representation == TypeVarRepresentation(true) then
                lctx.typeGenerator(v1.sirType).defaultRepresentation(v1.sirType)
            else representation
        }
        val retV = v1.toRepresentation(targetRepresentation, v1.pos)
        // Wrap with any top-level helpers registered during lowering (e.g. SumUplcEq helpers
        // cached by LoweringEq.generateSumUplcConstrEquals). Entries in `pendingTopLevelLetRecs`
        // are appended by innermost-completing helpers first (a helper's `+=` runs AFTER any
        // transitively-triggered sub-helpers have completed their own `+=`). So with `foldRight`,
        // the first entry becomes outermost — this is intentional: outer helpers are the
        // dependencies of later-added (inner) helpers, and UPLC `letrec` binds only the single
        // var in its binder, so inner helpers can see their outer dependencies.
        //
        // NOTE: this ordering is sound ONLY for (self-)recursive sums. Mutually recursive sums
        // (where helper A's rhs references helper B AND B's rhs references A) would require a
        // multi-binding `letrec` — not currently generated. Callers that would produce mutual
        // recursion should be detected at helper-construction time.
        // Only wrap with letrec entries actually reachable from `retV`. Eager support-binding
        // init (ScalusRuntime.initSupportBindings) materializes every support def, and those
        // lowerings may push conversion-helper entries (e.g. `$builtinListToUplcConstr`) into
        // `pendingTopLevelLetRecs`. Unconditionally wrapping the root with them would emit
        // `(λ helper root) rhs` even when neither `root` nor any kept letrec references
        // `helper`. Reachability: start from `retV.usedUplevelVars`, then expand through any
        // accepted letrec entry's rhs (`eqFnRhs.usedUplevelVars`) until fixed.
        val pending = lctx.pendingTopLevelLetRecs.toList
        val reachableIds: Set[String] = {
            var ids = retV.usedUplevelVars.map(_.id)
            var changed = true
            while changed do {
                changed = false
                pending.foreach { case (eqFnVar, eqFnRhs) =>
                    if ids.contains(eqFnVar.id) then
                        val newIds = ids ++ eqFnRhs.usedUplevelVars.map(_.id)
                        if newIds.size != ids.size then
                            ids = newIds
                            changed = true
                }
            }
            ids
        }
        val wrapped = pending.foldRight(retV) { case ((eqFnVar, eqFnRhs), acc) =>
            if reachableIds.contains(eqFnVar.id) then
                LetRecLoweredValue(eqFnVar, eqFnRhs, acc, eqFnVar.pos)
            else acc
        }
        wrapped
    }

    def lower(): Term = {
        val lctx = newLoweringContext
        try
            val retV = toLoweredValue(lctx)
            _lastLoweredValue = Some(retV)
            val gctx = TermGenerationContext(
              generatedVars = Set.empty
            )
            val term =
                try retV.termWithNeededVars(gctx)
                catch
                    case e: IllegalStateException =>
                        val debugGtx = gctx.copy(processUndefinedValues = true, debug = true)
                        val uplc = retV.termWithNeededVars(debugGtx)
                        println(s"generated uplc: ${uplc.pretty.render(100)}")
                        throw LoweringException(
                          s"Error generating term of type ${retV.sirType.show}\n" +
                              s"value: ${retV.show}\n" +
                              s"uplc: ${uplc.pretty.render(100)}",
                          retV.pos,
                          e
                        )
            if lctx.zCombinatorNeeded then Term.Apply(Term.LamAbs("__Z", term), ExprBuilder.ZTerm)
            else term
        catch
            case e: LoweringException =>
                throw e
    }

    def lastLoweredValue: Option[LoweredValue] = _lastLoweredValue

    def newLoweringContext: LoweringContext = {
        // Backward compat: if targetLanguage == PlutusV4, force vanRossemPV
        val effectivePV =
            if targetLanguage == Language.PlutusV4 then MajorProtocolVersion.vanRossemPV
            else targetProtocolVersion
        val retval = LoweringContext(
          zCombinatorNeeded = false,
          decls = MutableMap.empty,
          targetLanguage = targetLanguage,
          targetProtocolVersion = effectivePV,
          generateErrorTraces = generateErrorTraces,
          warnListConversions = warnListConversions,
          noWarn = noWarn,
          debug = debug,
          intrinsicModules = intrinsicModules,
          supportModules = supportModules
        )
        ScalusRuntime.initContext(retval)
        retval
    }

}

object SirToUplcV3Lowering {

    /** Create a SirToUplcV3Lowering from compiler Options, using default intrinsic/support modules.
      */
    def fromOptions(
        sir: SIR,
        options: scalus.compiler.Options,
        debug: Boolean = false
    ): SirToUplcV3Lowering =
        val transformedSir = sir
        SirToUplcV3Lowering(
          sir = transformedSir,
          generateErrorTraces = options.generateErrorTraces,
          debug = debug,
          warnListConversions = options.warnListConversions,
          noWarn = options.noWarn,
          targetLanguage = options.targetLanguage,
          targetProtocolVersion = options.targetProtocolVersion,
          intrinsicModules = IntrinsicResolver.defaultIntrinsicModules,
          supportModules = IntrinsicResolver.defaultSupportModules
        )
}
