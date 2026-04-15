package scalus.compiler.sir.lowering
package typegens

import scalus.compiler.sir.*

/** Type generator for product case classes that cannot be converted to Data (contain functions or
  * BLS elements). Uses UplcConstr representation.
  */
object ProductCaseUplcOnlySirTypeGenerator extends SirTypeUplcGenerator {

    override def defaultRepresentation(tp: SIRType)(using
        lctx: LoweringContext
    ): LoweredValueRepresentation = {
        val constrIndex = ProductCaseSirTypeGenerator.retrieveConstrIndex(tp, SIRPosition.empty)
        val constrDecl = ProductCaseSirTypeGenerator.retrieveConstrDecl(tp, SIRPosition.empty)
        val fieldReprs = constrDecl.params.map { param =>
            val paramType = lctx.resolveTypeVarIfNeeded(param.tp)
            lctx.typeGenerator(paramType).defaultRepresentation(paramType)
        }
        ProductCaseClassRepresentation.ProdUplcConstr(constrIndex, fieldReprs)
    }

    override def defaultDataRepresentation(
        tp: SIRType
    )(using LoweringContext): LoweredValueRepresentation =
        throw IllegalStateException(
          "Can't ask defaultDataRepresentation for product case class with non-Data fields"
        )

    override def defaultTypeVarReperesentation(tp: SIRType)(using
        lctx: LoweringContext
    ): LoweredValueRepresentation = defaultRepresentation(tp)

    override def canBeConvertedToData(tp: SIRType)(using LoweringContext): Boolean = false

    override def toRepresentation(
        input: LoweredValue,
        outputRepresentation: LoweredValueRepresentation,
        pos: SIRPosition
    )(using lctx: LoweringContext): LoweredValue = {
        if input.representation == outputRepresentation then input
        else ProductCaseSirTypeGenerator.toRepresentation(input, outputRepresentation, pos)
    }

    override def upcastOne(input: LoweredValue, targetType: SIRType, pos: SIRPosition)(using
        LoweringContext
    ): LoweredValue = {
        TypeRepresentationProxyLoweredValue(input, targetType, input.representation, pos)
    }

    override def genConstr(constr: SIR.Constr)(using LoweringContext): LoweredValue =
        ProductCaseSirTypeGenerator.genConstrUplcConstr(constr)

    override def genSelect(sel: SIR.Select, loweredScrutinee: LoweredValue)(using
        lctx: LoweringContext
    ): LoweredValue = {
        import scalus.uplc.{Term, UplcAnnotation}

        val pos = sel.anns.pos
        val constrDecl = ProductCaseSirTypeGenerator.retrieveConstrDecl(
          loweredScrutinee.sirType,
          pos
        )
        val fieldIndex = constrDecl.params.indexWhere(_.name == sel.field)
        if fieldIndex < 0 then
            throw LoweringException(s"Unknown field ${sel.field} for ${constrDecl.name}", pos)

        // Get field repr from the ProdUplcConstr stored in the scrutinee's representation
        val puc = loweredScrutinee.representation match
            case p: ProductCaseClassRepresentation.ProdUplcConstr => p
            case s: SumCaseClassRepresentation.SumUplcConstr =>
                val constrIndex = ProductCaseSirTypeGenerator.retrieveConstrIndex(
                  loweredScrutinee.sirType,
                  pos
                )
                s.variants.getOrElse(
                  constrIndex,
                  throw LoweringException(
                    s"Variant $constrIndex not found in SumUplcConstr for select",
                    pos
                  )
                )
            case other =>
                throw LoweringException(
                  s"genSelect on UplcConstr: unexpected repr $other",
                  pos
                )

        val rawFieldType = lctx.resolveTypeVarIfNeeded(constrDecl.params(fieldIndex).tp)
        val fieldRepr = puc.fieldReprs(fieldIndex)
        val fieldParam = constrDecl.params(fieldIndex)
        val fieldType =
            if fieldParam.annotations.data.contains("uplcRepr") then
                SIRType.Annotated(rawFieldType, fieldParam.annotations)
            else rawFieldType

        // Case(scrutinee, [λf0.λf1...λfN. fi]) — extract field i
        val fieldNames = constrDecl.params.indices.map(i => lctx.uniqueVarName(s"_sel_f$i"))
        val selectedFieldName = fieldNames(fieldIndex)

        new ComplexLoweredValue(Set.empty, loweredScrutinee) {
            override def sirType: SIRType = fieldType
            override def representation: LoweredValueRepresentation = fieldRepr
            override def pos: SIRPosition = sel.anns.pos

            override def termInternal(gctx: TermGenerationContext): Term = {
                val innerCtx = gctx.copy(generatedVars = gctx.generatedVars ++ fieldNames)
                val body = Term.Var(scalus.uplc.NamedDeBruijn(selectedFieldName))
                val branch = fieldNames.foldRight(body: Term) { (name, inner) =>
                    Term.LamAbs(name, inner, UplcAnnotation(sel.anns.pos))
                }
                // Pad with Error branches for tags < this constructor's tag
                val errorBranches =
                    scala.List.fill(puc.tag)(Term.Error(UplcAnnotation(sel.anns.pos)))
                Term.Case(
                  loweredScrutinee.termWithNeededVars(gctx),
                  errorBranches :+ branch,
                  UplcAnnotation(sel.anns.pos)
                )
            }

            override def docDef(ctx: LoweredValue.PrettyPrintingContext): org.typelevel.paiges.Doc =
                org.typelevel.paiges.Doc.text(s"${sel.field}(") + loweredScrutinee.docRef(ctx) +
                    org.typelevel.paiges.Doc.text(")")
            override def docRef(ctx: LoweredValue.PrettyPrintingContext): org.typelevel.paiges.Doc =
                docDef(ctx)
        }
    }

    override def genMatch(
        matchData: SIR.Match,
        loweredScrutinee: LoweredValue,
        optTargetType: Option[SIRType]
    )(using LoweringContext): LoweredValue = {
        SumUplcConstrSirTypeGenerator.genMatchUplcConstr(matchData, loweredScrutinee, optTargetType)
    }

}
