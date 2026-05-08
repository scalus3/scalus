package scalus.compiler.sir.lowering
package typegens

import scalus.compiler.sir.*

object SumCaseUplcOnlyEmitter extends SirTypeUplcGenerator {

    override def defaultRepresentation(tp: SIRType)(using
        LoweringContext
    ): LoweredValueRepresentation =
        SumUplcConstrEmitter.buildSumUplcConstr(tp)

    override def defaultDataRepresentation(tp: SIRType)(using
        LoweringContext
    ): LoweredValueRepresentation =
        SumUplcConstrEmitter.buildSumUplcConstr(tp)

    // TODO: set position in LoweringContext
    override def defaultTypeVarReperesentation(
        tp: SIRType
    )(using LoweringContext): LoweredValueRepresentation =
        throw LoweringException(
          "Type variables with lambdas are not supported in sum cases yet",
          SIRPosition.empty
        )

    override def canBeConvertedToData(tp: SIRType)(using lctx: LoweringContext): Boolean = false

    override def upcastOne(input: LoweredValue, targetType: SIRType, pos: SIRPosition)(using
        lctx: LoweringContext
    ): LoweredValue = {
        // The `ProdUplcConstr` arm mirrors
        // `SumCaseUplcConstrEmitter.upcastOne`: a child variant becomes a
        // single-entry `SumUplcConstr` parent. Single-entry (no overlay) is
        // load-bearing — see the `hasTransparentFields` note in the sibling
        // typegen. Already-sum values just relabel. Unlike the Data-compatible
        // sibling, `canBeConvertedToData = false` here so there is no
        // constrIndex-driven Data-shaped fallback recursion; anything other than
        // ProdUplcConstr / SumUplcConstr is a bug.
        input.representation match
            case prod: ProductCaseClassRepresentation.ProdUplcConstr =>
                val sumRepr = SumCaseClassRepresentation.SumUplcConstr(
                  scala.collection.immutable.SortedMap(prod.tag -> prod)
                )
                TypeRepresentationProxyLoweredValue(input, targetType, sumRepr, pos)
            case sum: SumCaseClassRepresentation.SumUplcConstr =>
                TypeRepresentationProxyLoweredValue(input, targetType, sum, pos)
            case other =>
                throw LoweringException(
                  s"SumCaseUplcOnlyEmitter.upcastOne: expected ProdUplcConstr or SumUplcConstr representation, got $other for ${input.sirType.show}",
                  pos
                )
    }

    override def genConstrLowered(
        constr: SIR.Constr,
        loweredArgs: scala.List[LoweredValue],
        optTargetType: Option[SIRType]
    )(using lctx: LoweringContext): LoweredValue = {
        ProdUplcConstrEmitter.genConstr(constr, loweredArgs)
    }

    override def genSelect(sel: SIR.Select, loweredScrutinee: LoweredValue)(using
        lctx: LoweringContext
    ): LoweredValue = {
        throw LoweringException(
          s"SumCaseUplcOnlyEmitter does not support select",
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
        SumUplcConstrEmitter.genMatchUplcConstr(matchData, loweredScrutinee, optTargetType)
    }

}
