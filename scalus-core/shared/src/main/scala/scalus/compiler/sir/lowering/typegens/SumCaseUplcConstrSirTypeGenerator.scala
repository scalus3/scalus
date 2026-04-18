package scalus.compiler.sir.lowering
package typegens

import scalus.compiler.sir.*

/** Type generator for sum types (enums) annotated with @UplcRepr(UplcConstr).
  *
  * Like SumCaseUplcOnlySirTypeGenerator but for Data-compatible types:
  *   - canBeConvertedToData = true
  *   - defaultDataRepresentation returns DataConstr
  *   - defaultTypeVarRepresentation returns DataConstr
  */
object SumCaseUplcConstrSirTypeGenerator extends SirTypeUplcGenerator {

    override def defaultRepresentation(tp: SIRType)(using
        LoweringContext
    ): LoweredValueRepresentation =
        SumUplcConstrSirTypeGenerator.buildSumUplcConstr(tp)

    private def isListType(tp: SIRType): Boolean =
        SIRType.retrieveDataDecl(tp) match
            case Right(decl) =>
                decl.name == SIRType.List.dataDecl.name ||
                decl.name == SIRType.BuiltinList.dataDecl.name
            case _ => false

    override def defaultDataRepresentation(tp: SIRType)(using
        lctx: LoweringContext
    ): LoweredValueRepresentation =
        if isListType(tp) then SumCaseClassRepresentation.PackedSumDataList
        else SumCaseClassRepresentation.DataConstr

    override def defaultTypeVarReperesentation(
        tp: SIRType
    )(using lctx: LoweringContext): LoweredValueRepresentation =
        if isListType(tp) then SumCaseClassRepresentation.PackedSumDataList
        else SumCaseClassRepresentation.DataConstr

    override def canBeConvertedToData(tp: SIRType)(using lctx: LoweringContext): Boolean = true

    override def toRepresentation(
        input: LoweredValue,
        representation: LoweredValueRepresentation,
        pos: SIRPosition
    )(using lctx: LoweringContext): LoweredValue = {
        // Dispatch based on input representation to avoid cycles:
        // intermediates with SumBuiltinList repr should go through SumBuiltinList handler directly.
        (input.representation, representation) match
            // Identity
            case (a, b) if a == b => input
            case (a, b) if a.isCompatibleOn(input.sirType, b, pos) =>
                RepresentationProxyLoweredValue(input, representation, pos)
            // SumBuiltinList → DataConstr: listData(input) → PackedSumDataList → DataConstr
            case (
                  SumCaseClassRepresentation.SumBuiltinList(elemRepr),
                  SumCaseClassRepresentation.DataConstr
                ) =>
                val asPackedList = new SumBuiltinListSirTypeGenerator(elemRepr)
                    .toRepresentation(input, SumCaseClassRepresentation.PackedSumDataList, pos)
                LoweredValue.Builder.lvBuiltinApply(
                  SIRBuiltins.listData,
                  asPackedList,
                  input.sirType,
                  SumCaseClassRepresentation.DataConstr,
                  pos
                )
            // SumBuiltinList input → delegate to SumBuiltinList handler
            case (SumCaseClassRepresentation.SumBuiltinList(elemRepr), _) =>
                new SumBuiltinListSirTypeGenerator(elemRepr)
                    .toRepresentation(input, representation, pos)
            // PackedSumDataList input → delegate to SumBuiltinList handler
            case (SumCaseClassRepresentation.PackedSumDataList, _) =>
                new SumBuiltinListSirTypeGenerator(PrimitiveRepresentation.PackedData)
                    .toRepresentation(input, representation, pos)
            // DataConstr/PairIntDataList → SumBuiltinList/PackedSumDataList/PairIntDataList:
            // delegate to SumCaseSirTypeGenerator which handles these without BuiltinList type hack
            case (
                  SumCaseClassRepresentation.DataConstr,
                  _: SumCaseClassRepresentation.SumBuiltinList
                ) | (
                  SumCaseClassRepresentation.DataConstr,
                  SumCaseClassRepresentation.PackedSumDataList
                ) | (
                  SumCaseClassRepresentation.DataConstr,
                  SumCaseClassRepresentation.PairIntDataList
                ) | (
                  SumCaseClassRepresentation.PairIntDataList,
                  _: SumCaseClassRepresentation.SumBuiltinList
                ) | (
                  SumCaseClassRepresentation.PairIntDataList,
                  SumCaseClassRepresentation.PackedSumDataList
                ) =>
                SumCaseSirTypeGenerator.toRepresentation(input, representation, pos)
            // TypeVar source: dispatch by kind, then delegate or convert appropriately
            case (inTvr: TypeVarRepresentation, _) =>
                import SIRType.TypeVarKind.*
                inTvr.kind match
                    case Transparent =>
                        // Wildcard source — delegate (existing behavior).
                        SumUplcConstrSirTypeGenerator.toRepresentation(input, representation, pos)
                    case Unwrapped =>
                        // Source bytes are in defaultRepresentation form for input.sirType.
                        val sourceUnderlying = defaultRepresentation(input.sirType)
                        val r0 = RepresentationProxyLoweredValue(input, sourceUnderlying, pos)
                        toRepresentation(r0, representation, pos)
                    case Fixed =>
                        // Source bytes are Data-shaped — delegate (existing behavior).
                        SumUplcConstrSirTypeGenerator.toRepresentation(input, representation, pos)
            // TypeVar target: dispatch by kind
            case (_, outTvr: TypeVarRepresentation) =>
                import SIRType.TypeVarKind.*
                outTvr.kind match
                    case Transparent => input
                    case Unwrapped =>
                        val targetUnderlying = defaultRepresentation(input.sirType)
                        val converted = input.toRepresentation(targetUnderlying, pos)
                        new RepresentationProxyLoweredValue(converted, outTvr, pos)
                    case Fixed =>
                        val targetUnderlying = defaultTypeVarReperesentation(input.sirType)
                        val converted = input.toRepresentation(targetUnderlying, pos)
                        new RepresentationProxyLoweredValue(converted, outTvr, pos)
            // SumUplcConstr, DataConstr, PairIntDataList → SumUplcConstrSirTypeGenerator
            case (_: SumCaseClassRepresentation.SumUplcConstr, _) |
                (SumCaseClassRepresentation.DataConstr, _) |
                (SumCaseClassRepresentation.PairIntDataList, _) =>
                SumUplcConstrSirTypeGenerator.toRepresentation(input, representation, pos)
            case (inRepr, outRepr) =>
                val trace = Thread.currentThread().getStackTrace.take(30).mkString("\n  ")
                throw LoweringException(
                  s"SumCaseUplcConstrSirTypeGenerator: unhandled conversion $inRepr → $outRepr for ${input.sirType.show}\n  $trace",
                  pos
                )
    }

    override def upcastOne(input: LoweredValue, targetType: SIRType, pos: SIRPosition)(using
        lctx: LoweringContext
    ): LoweredValue = {
        input.representation match
            case prod: ProductCaseClassRepresentation.ProdUplcConstr =>
                // Use actual variant field reprs, not DataDecl TypeVars
                val sumRepr = SumCaseClassRepresentation.SumUplcConstr(Map(prod.tag -> prod))
                TypeRepresentationProxyLoweredValue(input, targetType, sumRepr, pos)
            case _ =>
                // Non-UplcConstr repr — convert to ProdUplcConstr first, then upcast
                val targetSum = SumUplcConstrSirTypeGenerator.buildSumUplcConstr(targetType)
                val constrIndex =
                    ProductCaseSirTypeGenerator.retrieveConstrIndex(input.sirType, pos)
                targetSum.variants.get(constrIndex) match
                    case Some(targetProd) =>
                        val converted = input.toRepresentation(targetProd, pos)
                        upcastOne(converted, targetType, pos)
                    case None =>
                        throw LoweringException(
                          s"SumCaseUplcConstrSirTypeGenerator.upcastOne: variant $constrIndex not found in target ${targetType.show}",
                          pos
                        )
    }

    override def genConstr(constr: SIR.Constr)(using lctx: LoweringContext): LoweredValue = {
        // Resolve the concrete CaseClass type from the SumCaseClass
        val caseClassType = constr.data.constrType(constr.name)
        ProductCaseSirTypeGenerator.genConstrUplcConstr(constr.copy(tp = caseClassType))
    }

    override def genSelect(sel: SIR.Select, loweredScrutinee: LoweredValue)(using
        lctx: LoweringContext
    ): LoweredValue = {
        ProductCaseUplcOnlySirTypeGenerator.genSelect(sel, loweredScrutinee)
    }

    override def genMatch(
        matchData: SIR.Match,
        loweredScrutinee: LoweredValue,
        optTargetType: Option[SIRType]
    )(using
        lctx: LoweringContext
    ): LoweredValue = {
        // The SIR type is UplcConstr but the lowered value may still carry a SumBuiltinList
        // repr (e.g. elements produced before the @UplcRepr was picked up during elementReprFor
        // inference). Dispatch on the actual representation so the Case's branch order matches
        // the runtime dispatch convention:
        //   - SumUplcConstr → tag-ordered Case (Nil=0, Cons=1) via genMatchUplcConstr
        //   - SumBuiltinList → caseList-ordered Case (Cons=0, Nil=1) via SumBuiltinList gen
        //
        // TODO(strategic): replace with a LoweringContext.typeGenerator(sirType, repr) API so
        // every gen* method picks the repr-appropriate generator automatically.
        loweredScrutinee.representation match
            case _: SumCaseClassRepresentation.SumUplcConstr =>
                SumUplcConstrSirTypeGenerator.genMatchUplcConstr(
                  matchData,
                  loweredScrutinee,
                  optTargetType
                )
            case sumBL: SumCaseClassRepresentation.SumBuiltinList =>
                new SumBuiltinListSirTypeGenerator(sumBL.elementRepr).genMatch(
                  matchData,
                  loweredScrutinee,
                  optTargetType
                )
            case _ =>
                val targetRepr = defaultRepresentation(loweredScrutinee.sirType)
                val coerced = loweredScrutinee.toRepresentation(targetRepr, matchData.anns.pos)
                SumUplcConstrSirTypeGenerator.genMatchUplcConstr(
                  matchData,
                  coerced,
                  optTargetType
                )
    }

}
