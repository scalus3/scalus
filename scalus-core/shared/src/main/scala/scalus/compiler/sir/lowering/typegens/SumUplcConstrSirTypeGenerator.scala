package scalus.compiler.sir.lowering
package typegens

import org.typelevel.paiges.Doc
import scalus.cardano.ledger.MajorProtocolVersion
import scalus.compiler.sir.*
import scalus.compiler.sir.SIR.Pattern
import scalus.compiler.sir.lowering.LoweredValue.Builder.*
import scalus.compiler.sir.lowering.SumCaseClassRepresentation.*
import scalus.uplc.{Term, UplcAnnotation}

/** Type generator for sum types using UplcConstr representation.
  *
  * Handles construction, pattern matching, field selection, and representation conversions for
  * types stored as native UPLC Constr(tag, [fields]).
  *
  * Used for:
  *   - Types with function/BLS fields (canBeConvertedToData = false)
  *   - Data-compatible types annotated with @UplcRepr("UplcConstr") (canBeConvertedToData = true)
  */
object SumUplcConstrSirTypeGenerator {

    /** Build SumUplcConstr with variant info from the type's DataDecl. */
    def buildSumUplcConstr(tp: SIRType)(using
        lctx: LoweringContext
    ): SumUplcConstr = {
        val (constructors, typeArgs) = tp match
            case SIRType.SumCaseClass(decl, tArgs) => (decl.constructors, tArgs)
            case SIRType.CaseClass(cd, tArgs, Some(parent)) =>
                parent match
                    case SIRType.SumCaseClass(decl, pArgs) => (decl.constructors, pArgs)
                    case _                                 => (scala.List(cd), tArgs)
            case SIRType.CaseClass(cd, tArgs, None) => (scala.List(cd), tArgs)
            case SIRType.TypeLambda(_, body)        => return buildSumUplcConstr(body)
            case SIRType.TypeProxy(ref)             => return buildSumUplcConstr(ref)
            case _                                  => return SumUplcConstr(Map.empty)

        val variants = constructors.zipWithIndex.map { (constrDecl, idx) =>
            val fieldReprs = constrDecl.params.map { param =>
                val paramType = lctx.resolveTypeVarIfNeeded(param.tp)
                lctx.typeGenerator(paramType).defaultRepresentation(paramType)
            }
            idx -> ProductCaseClassRepresentation.ProdUplcConstr(idx, fieldReprs)
        }.toMap
        SumUplcConstr(variants)
    }

    // ===================== Match =====================

    /** Match on UplcConstr using Case builtin. Case(scrutinee, [branch0, branch1, ...]) — each
      * branch is a nested lambda.
      */
    def genMatchUplcConstr(
        matchData: SIR.Match,
        loweredScrutinee: LoweredValue,
        optTargetType: Option[SIRType]
    )(using lctx: LoweringContext): LoweredValue = {
        val prevScope = lctx.scope
        val orderedCases =
            SumCaseSirTypeGenerator.prepareCases(matchData, loweredScrutinee)
        val pos = matchData.anns.pos

        val branches = orderedCases.map { preparedCase =>
            genMatchUplcConstrCase(preparedCase.sirCase, optTargetType)
        }

        lctx.scope = prevScope

        val resultRepr = branches.head.representation
        val resultType = optTargetType.getOrElse(matchData.tp)

        new ComplexLoweredValue(Set.empty, (loweredScrutinee :: branches.toList)*) {
            override def sirType: SIRType = resultType
            override def representation: LoweredValueRepresentation = resultRepr
            override def pos: SIRPosition = matchData.anns.pos

            override def termInternal(gctx: TermGenerationContext): Term =
                Term.Case(
                  loweredScrutinee.termWithNeededVars(gctx),
                  branches.map(_.termWithNeededVars(gctx)).toList,
                  UplcAnnotation(matchData.anns.pos)
                )

            override def docDef(ctx: LoweredValue.PrettyPrintingContext): Doc = {
                import Doc.*
                val branchDocs = branches.zipWithIndex.map { (b, i) =>
                    line + text(s"$i ->") + (lineOrSpace + b.docRef(ctx)).nested(2)
                }
                ((text("case") + space + loweredScrutinee.docRef(ctx) + space + text("of"))
                    + branchDocs.foldLeft(empty)(_ + _.grouped)).aligned
            }

            override def docRef(ctx: LoweredValue.PrettyPrintingContext): Doc = docDef(ctx)
        }
    }

    /** Single branch for UplcConstr match. Constr patterns: λf0.λf1.…λfN.body */
    private def genMatchUplcConstrCase(
        sirCase: SIR.Case,
        optTargetType: Option[SIRType]
    )(using lctx: LoweringContext): LoweredValue = {
        val caseScope = lctx.scope
        val pos = sirCase.anns.pos

        sirCase.pattern match
            case constrPattern: Pattern.Constr =>
                val constrDecl = constrPattern.constr
                val fieldVars =
                    constrPattern.bindings.zip(constrDecl.params).map { (name, typeBinding) =>
                        val tp = lctx.resolveTypeVarIfNeeded(typeBinding.tp)
                        val fieldRepr = lctx.typeGenerator(tp).defaultRepresentation(tp)
                        val fieldVar = new VariableLoweredValue(
                          id = lctx.uniqueVarName(name),
                          name = name,
                          sir = SIR.Var(name, tp, AnnotationsDecl(pos)),
                          representation = fieldRepr
                        )
                        lctx.scope = lctx.scope.add(fieldVar)
                        fieldVar
                    }

                val body = lctx.lower(sirCase.body, optTargetType)
                lctx.scope = caseScope

                fieldVars.foldRight(body) { (fieldVar, inner) =>
                    new ComplexLoweredValue(Set(fieldVar), inner) {
                        override def sirType: SIRType = inner.sirType
                        override def representation: LoweredValueRepresentation =
                            inner.representation
                        override def pos: SIRPosition = sirCase.anns.pos

                        override def termInternal(gctx: TermGenerationContext): Term = {
                            val innerCtx =
                                gctx.copy(generatedVars = gctx.generatedVars + fieldVar.id)
                            Term.LamAbs(
                              fieldVar.id,
                              inner.termWithNeededVars(innerCtx),
                              UplcAnnotation(sirCase.anns.pos)
                            )
                        }

                        override def docDef(ctx: LoweredValue.PrettyPrintingContext): Doc =
                            Doc.text(s"λ${fieldVar.name}.") + inner.docRef(ctx)
                        override def docRef(ctx: LoweredValue.PrettyPrintingContext): Doc =
                            docDef(ctx)
                    }
                }

            case Pattern.Wildcard =>
                val body = lctx.lower(sirCase.body, optTargetType)
                lctx.scope = caseScope
                body

            case other =>
                throw LoweringException(s"Unexpected pattern in UplcConstr match: $other", pos)
    }

    // ===================== toRepresentation =====================

    /** Handle all UplcConstr-related representation conversions. */
    def toRepresentation(
        input: LoweredValue,
        representation: LoweredValueRepresentation,
        pos: SIRPosition
    )(using lctx: LoweringContext): LoweredValue = {
        (input.representation, representation) match
            // PairIntDataList → SumUplcConstr: tag dispatch + per-field rebuild
            case (PairIntDataList, outSum: SumUplcConstr) =>
                pairIntDataListToSumUplcConstr(input, outSum, pos)
            // SumUplcConstr → DataConstr: Case dispatch, convert fields to Data
            case (inSum: SumUplcConstr, DataConstr) =>
                sumUplcConstrToDataConstr(input, inSum, pos)
            // SumUplcConstr → PairIntDataList: go through DataConstr
            case (_: SumUplcConstr, PairIntDataList) =>
                input
                    .toRepresentation(DataConstr, pos)
                    .toRepresentation(PairIntDataList, pos)
            // SumUplcConstr → SumBuiltinList: future (List UplcConstr encoding)
            case (_: SumUplcConstr, SumBuiltinList(_)) =>
                throw LoweringException(
                  s"SumUplcConstr → SumBuiltinList not yet supported for ${input.sirType.show}.",
                  pos
                )
            // SumUplcConstr → PackedSumDataList: go through DataConstr
            case (_: SumUplcConstr, PackedSumDataList) =>
                input
                    .toRepresentation(DataConstr, pos)
                    .toRepresentation(PackedSumDataList, pos)
            // ProdUplcConstr → SumUplcConstr: check field reprs compatibility
            case (
                  inProd: ProductCaseClassRepresentation.ProdUplcConstr,
                  outSum: SumUplcConstr
                ) =>
                prodUplcConstrToSumUplcConstr(input, inProd, outSum, pos)
            // SumUplcConstr → SumUplcConstr: check per-variant compatibility
            case (inSum: SumUplcConstr, outSum: SumUplcConstr) =>
                sumUplcConstrToSumUplcConstr(input, inSum, outSum, pos)
            // DataConstr → SumUplcConstr: go through PairIntDataList
            case (DataConstr, _: SumUplcConstr) =>
                input
                    .toRepresentation(PairIntDataList, pos)
                    .toRepresentation(representation, pos)
            // TypeVarRepresentation → UplcConstr: go through defaultTypeVarRepresentation
            case (tvr: TypeVarRepresentation, _: SumUplcConstr) =>
                if tvr.isBuiltin then RepresentationProxyLoweredValue(input, representation, pos)
                else
                    val typeGen = lctx.typeGenerator(input.sirType)
                    val tvRepr = typeGen.defaultTypeVarReperesentation(input.sirType)
                    val r0 = input.toRepresentation(tvRepr, pos)
                    r0.toRepresentation(representation, pos)
            // UplcConstr/ProdUplcConstr → TypeVarRepresentation: go through defaultTypeVarRepresentation
            case (
                  _: SumUplcConstr | _: ProductCaseClassRepresentation.ProdUplcConstr,
                  tvr: TypeVarRepresentation
                ) =>
                if tvr.isBuiltin then input
                else
                    val typeGen = lctx.typeGenerator(input.sirType)
                    val tvRepr = typeGen.defaultTypeVarReperesentation(input.sirType)
                    input
                        .toRepresentation(tvRepr, pos)
            case _ =>
                throw LoweringException(
                  s"SumUplcConstrSirTypeGenerator: unsupported conversion from ${input.representation} to $representation for ${input.sirType.show}",
                  pos
                )
    }

    private def pairIntDataListToSumUplcConstr(
        input: LoweredValue,
        outSum: SumUplcConstr,
        pos: SIRPosition
    )(using lctx: LoweringContext): LoweredValue = {
        val tagVar = lvNewLazyIdVar(
          lctx.uniqueVarName("_dc_tag"),
          SIRType.Integer,
          PrimitiveRepresentation.Constant,
          lvBuiltinApply(
            SIRBuiltins.fstPair,
            input,
            SIRType.Integer,
            PrimitiveRepresentation.Constant,
            pos
          ),
          pos
        )
        val dataListRepr = SumBuiltinList(DataData)
        val dataListVar = lvNewLazyIdVar(
          lctx.uniqueVarName("_dc_fields"),
          SIRType.List(SIRType.Data.tp),
          dataListRepr,
          lvBuiltinApply(
            SIRBuiltins.sndPair,
            input,
            SIRType.List(SIRType.Data.tp),
            dataListRepr,
            pos
          ),
          pos
        )
        val constructors = SumCaseSirTypeGenerator.findConstructors(input.sirType, pos)
        val branches = constructors.zipWithIndex.map { (constrDecl, idx) =>
            val variantRepr = outSum.variants.getOrElse(
              idx,
              ProductCaseClassRepresentation.ProdUplcConstr(
                idx,
                constrDecl.params.map { p =>
                    val tp = lctx.resolveTypeVarIfNeeded(p.tp)
                    lctx.typeGenerator(tp).defaultDataRepresentation(tp)
                }
              )
            )
            var currentList: LoweredValue = dataListVar
            val fields = constrDecl.params.zip(variantRepr.fieldReprs).map { (param, fieldRepr) =>
                val tp = lctx.resolveTypeVarIfNeeded(param.tp)
                val dataRepr = lctx.typeGenerator(tp).defaultDataRepresentation(tp)
                val head = lvBuiltinApply(SIRBuiltins.headList, currentList, tp, dataRepr, pos)
                currentList = lvBuiltinApply(
                  SIRBuiltins.tailList,
                  currentList,
                  SIRType.List(SIRType.Data.tp),
                  dataListRepr,
                  pos
                )
                head.toRepresentation(fieldRepr, pos)
            }
            val inPos = pos
            new ComplexLoweredValue(Set.empty, fields*) {
                override def sirType = input.sirType
                override def representation = variantRepr
                override def pos = inPos
                override def termInternal(gctx: TermGenerationContext) =
                    Term.Constr(
                      scalus.cardano.ledger.Word64(idx.toLong),
                      fields.map(_.termWithNeededVars(gctx)).toList,
                      UplcAnnotation(inPos)
                    )
                override def docDef(ctx: LoweredValue.PrettyPrintingContext) =
                    Doc.text(s"PairIntDataList→UplcConstr($idx)")
                override def docRef(ctx: LoweredValue.PrettyPrintingContext) = docDef(ctx)
            }: LoweredValue
        }
        val result =
            if lctx.targetProtocolVersion >= MajorProtocolVersion.vanRossemPV then
                lvCaseInteger(tagVar, branches.toList, pos, None)
            else
                branches.zipWithIndex.foldRight(branches.last: LoweredValue) {
                    case ((branch, idx), acc) =>
                        if idx == branches.size - 1 then acc
                        else
                            lvIfThenElse(
                              lvEqualsInteger(tagVar, lvIntConstant(idx, pos), pos),
                              branch,
                              acc,
                              pos
                            )
                }
        ScopeBracketsLoweredValue(Set(tagVar, dataListVar), result)
    }

    private def sumUplcConstrToDataConstr(
        input: LoweredValue,
        inSum: SumUplcConstr,
        pos: SIRPosition
    )(using lctx: LoweringContext): LoweredValue = {
        val constructors = SumCaseSirTypeGenerator.findConstructors(input.sirType, pos)
        val branches = constructors.zipWithIndex.map { (constrDecl, idx) =>
            val variantRepr = inSum.variants.getOrElse(
              idx,
              ProductCaseClassRepresentation.ProdUplcConstr(
                idx,
                constrDecl.params.map { p =>
                    val tp = lctx.resolveTypeVarIfNeeded(p.tp)
                    lctx.typeGenerator(tp).defaultRepresentation(tp)
                }
              )
            )
            val fieldVars = constrDecl.params.zip(variantRepr.fieldReprs).map { (param, repr) =>
                val tp = lctx.resolveTypeVarIfNeeded(param.tp)
                val name = lctx.uniqueVarName(s"_uc2dc_f")
                new VariableLoweredValue(
                  id = name,
                  name = name,
                  sir = SIR.Var(name, tp, AnnotationsDecl(pos)),
                  representation = repr
                )
            }
            val dataListNil = lvDataDataListNil(pos)
            val dataList = fieldVars.zip(constrDecl.params).foldRight(dataListNil: LoweredValue) {
                case ((fv, param), acc) =>
                    val tp = lctx.resolveTypeVarIfNeeded(param.tp)
                    val dataRepr = lctx.typeGenerator(tp).defaultDataRepresentation(tp)
                    val asData = fv.toRepresentation(dataRepr, pos)
                    lvBuiltinApply2(
                      SIRBuiltins.mkCons,
                      asData,
                      acc,
                      SIRType.List(SIRType.Data.tp),
                      SumBuiltinList(DataData),
                      pos
                    )
            }
            val tagConst = lvIntConstant(idx, pos)
            val constrData =
                lvBuiltinApply2(
                  SIRBuiltins.constrData,
                  tagConst,
                  dataList,
                  input.sirType,
                  DataConstr,
                  pos
                )
            val inPos = pos
            fieldVars.foldRight(constrData: LoweredValue) { (fv, inner) =>
                new ComplexLoweredValue(Set(fv), inner) {
                    override def sirType = inner.sirType
                    override def representation = inner.representation
                    override def pos = inPos
                    override def termInternal(gctx: TermGenerationContext) = {
                        val ctx = gctx.copy(generatedVars = gctx.generatedVars + fv.id)
                        Term.LamAbs(fv.id, inner.termWithNeededVars(ctx), UplcAnnotation(inPos))
                    }
                    override def docDef(ctx: LoweredValue.PrettyPrintingContext) =
                        Doc.text(s"λ${fv.name}.") + inner.docRef(ctx)
                    override def docRef(ctx: LoweredValue.PrettyPrintingContext) = docDef(ctx)
                }
            }
        }
        val inPos = pos
        new ComplexLoweredValue(Set.empty, (input :: branches.toList)*) {
            override def sirType = input.sirType
            override def representation = DataConstr
            override def pos = inPos
            override def termInternal(gctx: TermGenerationContext) =
                Term.Case(
                  input.termWithNeededVars(gctx),
                  branches.map(_.termWithNeededVars(gctx)).toList,
                  UplcAnnotation(inPos)
                )
            override def docDef(ctx: LoweredValue.PrettyPrintingContext) =
                Doc.text("UplcConstr→DataConstr(") + input.docRef(ctx) + Doc.text(")")
            override def docRef(ctx: LoweredValue.PrettyPrintingContext) = docDef(ctx)
        }
    }

    private def prodUplcConstrToSumUplcConstr(
        input: LoweredValue,
        inProd: ProductCaseClassRepresentation.ProdUplcConstr,
        outSum: SumUplcConstr,
        pos: SIRPosition
    )(using lctx: LoweringContext): LoweredValue = {
        outSum.variants.get(inProd.tag) match
            case Some(expectedProd)
                if inProd.fieldReprs.size == expectedProd.fieldReprs.size
                    && inProd.fieldReprs.zip(expectedProd.fieldReprs).forall { (inR, outR) =>
                        inR.isCompatibleOn(SIRType.FreeUnificator, outR, pos)
                    } =>
                new RepresentationProxyLoweredValue(input, outSum, pos)
            case Some(expectedProd) =>
                throw LoweringException(
                  s"ProdUplcConstr → SumUplcConstr field repr mismatch for tag ${inProd.tag}: " +
                      s"got ${inProd.fieldReprs} but expected ${expectedProd.fieldReprs}",
                  pos
                )
            case None =>
                new RepresentationProxyLoweredValue(input, outSum, pos)
    }

    private def sumUplcConstrToSumUplcConstr(
        input: LoweredValue,
        inSum: SumUplcConstr,
        outSum: SumUplcConstr,
        pos: SIRPosition
    )(using lctx: LoweringContext): LoweredValue = {
        val allCompatible = inSum.variants.forall { (tag, inProd) =>
            outSum.variants.get(tag) match
                case None => true
                case Some(outProd) =>
                    inProd.fieldReprs.size == outProd.fieldReprs.size &&
                    inProd.fieldReprs.zip(outProd.fieldReprs).forall { (inR, outR) =>
                        inR.isCompatibleOn(SIRType.FreeUnificator, outR, pos)
                    }
        }
        if allCompatible then new RepresentationProxyLoweredValue(input, outSum, pos)
        else throw LoweringException(s"SumUplcConstr → SumUplcConstr variant repr mismatch", pos)
    }
}
