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
    )(using lctx: LoweringContext): LoweredValue =
        SumDispatch.dispatcherBypass("SumCaseUplcConstrSirTypeGenerator")

    override def upcastOne(input: LoweredValue, targetType: SIRType, pos: SIRPosition)(using
        lctx: LoweringContext
    ): LoweredValue = {
        input.representation match
            case prod: ProductCaseClassRepresentation.ProdUplcConstr =>
                // Use actual variant field reprs, not DataDecl TypeVars
                val sumRepr = SumCaseClassRepresentation.SumUplcConstr(
                  scala.collection.immutable.SortedMap(prod.tag -> prod)
                )
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

    override def genConstrLowered(
        constr: SIR.Constr,
        loweredArgs: scala.List[LoweredValue],
        optTargetType: Option[SIRType]
    )(using lctx: LoweringContext): LoweredValue = {
        // `genConstrUplcConstr` needs a CaseClass tp. After `dispatchNil` `constr.tp` may be a
        // sum target (possibly `Annotated`) carrying caller-supplied substituted args — preserve
        // them as the rebuilt CaseClass's parent rather than dropping them via the static
        // `decl.constrType(name)` lookup (which would substitute back to abstract decl typevars).
        val effectiveTp =
            if SIRType.isProd(constr.tp) then constr.tp
            else if SIRType.isSum(constr.tp) then
                preservedParentCaseClassForm(constr.tp, constr.data, constr.name)
            else constr.data.constrType(constr.name)
        ProductCaseSirTypeGenerator.genConstrUplcConstr(
          constr.copy(tp = effectiveTp),
          loweredArgs
        )
    }

    /** Build a CaseClass form for `ctorName` using `parent` as its parent field, preserving any
      * `Annotated`/substituted args on `parent`. The constructor's own shape (typeParams, typeArgs)
      * comes from `decl.constrType(ctorName)`; only the parent reference is swapped.
      */
    private def preservedParentCaseClassForm(
        parent: SIRType,
        decl: scalus.compiler.sir.DataDecl,
        ctorName: String
    ): SIRType = decl.constrType(ctorName) match
        case SIRType.TypeLambda(params, SIRType.CaseClass(c, args, _)) =>
            SIRType.TypeLambda(params, SIRType.CaseClass(c, args, Some(parent)))
        case SIRType.CaseClass(c, args, _) =>
            SIRType.CaseClass(c, args, Some(parent))
        case other => other

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
