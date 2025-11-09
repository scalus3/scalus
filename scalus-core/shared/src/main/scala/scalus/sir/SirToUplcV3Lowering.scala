package scalus.sir

import scalus.compiler.sir.lowering.*
import scalus.pretty

import scala.collection.mutable.Map as MutableMap
import scalus.uplc.*

class SirToUplcV3Lowering(
    sir: SIR,
    generateErrorTraces: Boolean = false,
    upcastTo: SIRType = SIRType.FreeUnificator,
    representation: LoweredValueRepresentation = TypeVarRepresentation(true),
    debug: Boolean = false
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
        // println(s"lowered  value: ${retV.pretty.render(100)}")
        retV
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
            if lctx.zCombinatorNeeded then
                Term.Apply(Term.LamAbs("__z_combinator__", term), ExprBuilder.ZTerm)
            else term
        catch
            case e: LoweringException =>
                throw e
    }

    def lastLoweredValue: Option[LoweredValue] = _lastLoweredValue

    def newLoweringContext: LoweringContext = {
        val retval = LoweringContext(
          zCombinatorNeeded = false,
          decls = MutableMap.empty,
          plutusVersion = 3,
          generateErrorTraces = generateErrorTraces,
          debug = debug
        )
        ScalusRuntime.initContext(retval)
        retval
    }

}
