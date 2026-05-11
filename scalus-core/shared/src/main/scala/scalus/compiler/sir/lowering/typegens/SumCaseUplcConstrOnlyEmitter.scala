package scalus.compiler.sir.lowering
package typegens

import scalus.compiler.sir.*

object SumCaseUplcConstrOnlyEmitter extends SumCaseUplcConstrCommon {

    override def defaultDataRepresentation(tp: SIRType)(using
        LoweringContext
    ): LoweredValueRepresentation =
        SumUplcConstrOps.buildSumUplcConstr(tp)

    /** No Data form exists (lambdas can't be Data); the Fixed-TypeVar shape is the same as the
      * default — `SumUplcConstr`. `bridgeFromKind` then handles all kinds correctly without
      * special-casing.
      */
    override def defaultTypeVarReperesentation(
        tp: SIRType
    )(using LoweringContext): LoweredValueRepresentation =
        SumUplcConstrOps.buildSumUplcConstr(tp)

    override def canBeConvertedToData(tp: SIRType)(using lctx: LoweringContext): Boolean = false

    /** Already-sum values just relabel. Unlike the Data-compatible sibling, `canBeConvertedToData =
      * false` here so there is no constrIndex-driven Data-shaped fallback recursion; anything other
      * than `ProdUplcConstr` (handled by the base) / `SumUplcConstr` is a bug.
      */
    override protected def upcastOneOther(
        input: LoweredValue,
        targetType: SIRType,
        pos: SIRPosition
    )(using lctx: LoweringContext): LoweredValue = input.representation match
        case sum: SumCaseClassRepresentation.SumUplcConstr =>
            TypeRepresentationProxyLoweredValue(input, targetType, sum, pos)
        case other =>
            throw LoweringException(
              s"SumCaseUplcConstrOnlyEmitter.upcastOne: expected ProdUplcConstr or SumUplcConstr representation, got $other for ${input.sirType.show}",
              pos
            )

    override def genConstrLowered(
        constr: SIR.Constr,
        loweredArgs: scala.List[LoweredValue],
        optTargetType: Option[SIRType]
    )(using lctx: LoweringContext): LoweredValue = {
        ProdUplcConstrOps.genConstr(constr, loweredArgs)
    }

    override def genSelect(sel: SIR.Select, loweredScrutinee: LoweredValue)(using
        lctx: LoweringContext
    ): LoweredValue = {
        throw LoweringException(
          s"SumCaseUplcConstrOnlyEmitter does not support select",
          sel.anns.pos
        )
    }

    override def genMatch(
        matchData: SIR.Match,
        loweredScrutinee: LoweredValue,
        optTargetType: Option[SIRType]
    )(using
        lctx: LoweringContext
    ): LoweredValue = {
        SumUplcConstrOps.genMatchUplcConstr(matchData, loweredScrutinee, optTargetType)
    }

}
