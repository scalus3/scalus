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
        val (constructors, typeArgs, typeParams) = tp match
            case SIRType.SumCaseClass(decl, tArgs) => (decl.constructors, tArgs, decl.typeParams)
            case SIRType.CaseClass(cd, tArgs, Some(parent)) =>
                parent match
                    case SIRType.SumCaseClass(decl, pArgs) =>
                        (decl.constructors, pArgs, decl.typeParams)
                    case _ => (scala.List(cd), tArgs, cd.typeParams)
            case SIRType.CaseClass(cd, tArgs, None) => (scala.List(cd), tArgs, cd.typeParams)
            case SIRType.TypeLambda(_, body)        => return buildSumUplcConstr(body)
            case SIRType.TypeProxy(ref)             => return buildSumUplcConstr(ref)
            case SIRType.Annotated(tp1, _)          => return buildSumUplcConstr(tp1)
            case _                                  => return SumUplcConstr(Map.empty)

        // Build name-based substitution: TypeVar name → concrete type
        val typeSubstByName: Map[String, SIRType] =
            typeParams.map(_.name).zip(typeArgs).toMap

        def extractDeclName(t: SIRType): Option[String] = t match
            case SIRType.SumCaseClass(d, _)       => Some(d.name)
            case SIRType.CaseClass(_, _, Some(p)) => extractDeclName(p)
            case SIRType.TypeLambda(_, body)      => extractDeclName(body)
            case SIRType.TypeProxy(ref)           => extractDeclName(ref)
            case _                                => None

        val selfDeclName = extractDeclName(tp)

        // Use a mutable proxy for self-referential fields (e.g., tail: List[A]).
        // After building variants, set proxy.ref to the real SumUplcConstr.
        val selfProxy = SumCaseClassRepresentation.SumReprProxy(null)
        val variants = constructors.zipWithIndex.map { (constrDecl, idx) =>
            val fieldReprs = constrDecl.params.map { param =>
                // Substitute DataDecl TypeVars with concrete type args (name-based)
                val paramType = param.tp match
                    case tv: SIRType.TypeVar =>
                        typeSubstByName.getOrElse(tv.name, tv)
                    case SIRType.TypeProxy(ref) =>
                        ref match
                            case tv: SIRType.TypeVar =>
                                typeSubstByName.getOrElse(tv.name, param.tp)
                            case _ => lctx.resolveTypeVarIfNeeded(param.tp)
                    case _ => lctx.resolveTypeVarIfNeeded(param.tp)
                // Check for field-level @UplcRepr annotation override
                SirTypeUplcGenerator
                    .resolveFieldRepr(param, paramType)
                    .getOrElse {
                        val isSelfRef = selfDeclName.exists { sn =>
                            extractDeclName(paramType).contains(sn)
                        }
                        if isSelfRef then selfProxy
                        else
                            paramType match
                                // In UplcConstr, TypeVar fields are Transparent (native repr)
                                case tv: SIRType.TypeVar =>
                                    TypeVarRepresentation(SIRType.TypeVarKind.Transparent)
                                case _ =>
                                    lctx.typeGenerator(paramType).defaultRepresentation(paramType)
                    }
            }
            idx -> ProductCaseClassRepresentation.ProdUplcConstr(idx, fieldReprs)
        }.toMap
        val result = SumUplcConstr(variants)
        selfProxy.ref = result
        result
    }

    /** Direct Case-based match on a UplcConstr list — used by ScalusRuntime conversions.
      *
      * Generates: Case(scrutinee, [nilBranch, λh.λt.consBranch(h, t)]) List enum: Nil=tag 0,
      * Cons(head, tail)=tag 1.
      */
    def genMatchUplcConstrDirect(
        scrutinee: LoweredValue,
        scrutineeRepr: SumCaseClassRepresentation.SumUplcConstr,
        listType: SIRType,
        outType: SIRType,
        outRepr: LoweredValueRepresentation,
        pos: SIRPosition,
        nilBody: LoweredValue,
        consBody: (IdentifiableLoweredValue, IdentifiableLoweredValue) => LoweredValue
    )(using lctx: LoweringContext): LoweredValue = {
        val elemType = SumCaseClassRepresentation.SumBuiltinList
            .retrieveListElementType(listType)
            .getOrElse(SIRType.Data.tp)
        // Get element and tail reprs from Cons variant
        val consVariant = scrutineeRepr.variants.values.find(_.fieldReprs.nonEmpty)
        val elemRepr = consVariant
            .map(_.fieldReprs.head)
            .getOrElse(lctx.typeGenerator(elemType).defaultRepresentation(elemType))
        val tailRepr = consVariant
            .flatMap(_.fieldReprs.lift(1))
            .getOrElse(scrutineeRepr)
        // Cons branch: λhead.λtail.consBody(head, tail)
        val headId = lctx.uniqueVarName("h")
        val headVar = new VariableLoweredValue(
          id = headId,
          name = headId,
          sir = SIR.Var(headId, elemType, AnnotationsDecl(pos)),
          representation = elemRepr
        )
        val tailId = lctx.uniqueVarName("t")
        val tailVar = new VariableLoweredValue(
          id = tailId,
          name = tailId,
          sir = SIR.Var(tailId, listType, AnnotationsDecl(pos)),
          representation = tailRepr
        )
        lctx.scope = lctx.scope.add(headVar).add(tailVar)
        val consResult = consBody(headVar, tailVar)
        val constrPos = AnnotationsDecl.empty.pos // avoid capturing outer `pos` in closure
        val consBranch = new ComplexLoweredValue(Set(headVar, tailVar), consResult) {
            override def sirType: SIRType = outType
            override def representation: LoweredValueRepresentation = outRepr
            override def pos: SIRPosition = constrPos
            override def termInternal(gctx: TermGenerationContext): Term = {
                val ngctx = gctx.addGeneratedVar(headVar.id).addGeneratedVar(tailVar.id)
                Term.LamAbs(
                  headVar.id,
                  Term.LamAbs(
                    tailVar.id,
                    consResult.termWithNeededVars(ngctx),
                    UplcAnnotation(constrPos)
                  ),
                  UplcAnnotation(constrPos)
                )
            }
            override def docDef(ctx: LoweredValue.PrettyPrintingContext): Doc =
                Doc.text(s"λ${headVar.id}.λ${tailVar.id}.${consResult.docRef(ctx)}")
            override def docRef(ctx: LoweredValue.PrettyPrintingContext): Doc = docDef(ctx)
        }
        // Result: Case(scrutinee, [nilBody, consBranch])
        new ComplexLoweredValue(Set.empty, scrutinee, nilBody, consBranch) {
            override def sirType: SIRType = outType
            override def representation: LoweredValueRepresentation = outRepr
            override def pos: SIRPosition = constrPos
            override def termInternal(gctx: TermGenerationContext): Term =
                Term.Case(
                  scrutinee.termWithNeededVars(gctx),
                  scala.List(nilBody.termWithNeededVars(gctx), consBranch.termWithNeededVars(gctx)),
                  UplcAnnotation(constrPos)
                )
            override def docDef(ctx: LoweredValue.PrettyPrintingContext): Doc =
                Doc.text(
                  s"case ${scrutinee.docRef(ctx)} of Nil→${nilBody.docRef(ctx)} | Cons→${consBranch.docRef(ctx)}"
                )
            override def docRef(ctx: LoweredValue.PrettyPrintingContext): Doc = docDef(ctx)
        }
    }

    // ===================== Match =====================

    /** Match on UplcConstr using Case builtin. Case(scrutinee, [branch0, branch1, ...]) — each
      * branch is a nested lambda.
      */
    /** Select a field from a UplcConstr-represented value. */
    def genSelectUplcConstr(sel: SIR.Select, loweredScrutinee: LoweredValue)(using
        LoweringContext
    ): LoweredValue =
        ProductCaseUplcOnlySirTypeGenerator.genSelect(sel, loweredScrutinee)

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
            genMatchUplcConstrCase(
              preparedCase.sirCase,
              optTargetType,
              loweredScrutinee,
              preparedCase.constrIndex.getOrElse(-1)
            )
        }

        lctx.scope = prevScope

        val resultType = optTargetType.getOrElse(matchData.tp)
        val branchesUpcasted = branches.map(_.maybeUpcast(resultType, pos))

        // Check if any branch has UplcConstr repr with Transparent TypeVar fields.
        // If so, branches can't be aligned to DataConstr — cascade to SumUplcConstr.
        def hasTransparentFields(repr: LoweredValueRepresentation): Boolean = repr match
            case puc: ProductCaseClassRepresentation.ProdUplcConstr =>
                puc.fieldReprs.exists {
                    case TypeVarRepresentation(SIRType.TypeVarKind.Transparent) => true
                    case _                                                      => false
                }
            case suc: SumCaseClassRepresentation.SumUplcConstr =>
                suc.variants.values.exists(puc => hasTransparentFields(puc))
            case _ => false
        val hasTransparentUplcConstr =
            branchesUpcasted.exists(b => hasTransparentFields(b.representation))

        val (resultRepr, branchesAligned) =
            if hasTransparentUplcConstr then
                // Build SumUplcConstr from the result type's data declaration.
                val dataDecl = SIRType.retrieveDataDecl(resultType) match
                    case Right(dd) => dd
                    case Left(msg) =>
                        throw LoweringException(
                          s"Cannot retrieve data declaration for UplcConstr cascade: $msg",
                          pos
                        )
                // Collect ProdUplcConstr from branches by tag
                val branchPucByTag = branchesUpcasted.flatMap { b =>
                    b.representation match
                        case puc: ProductCaseClassRepresentation.ProdUplcConstr =>
                            Some(puc.tag -> puc)
                        case _ => None
                }.toMap
                // Build variants for all constructors
                val variants = dataDecl.constructors.zipWithIndex.map { (constrDecl, idx) =>
                    branchPucByTag.get(idx) match
                        case Some(puc) => (idx, puc)
                        case None =>
                            val fieldReprs = constrDecl.params.map { param =>
                                val tp = lctx.resolveTypeVarIfNeeded(param.tp)
                                lctx.typeGenerator(tp).defaultRepresentation(tp)
                            }.toList
                            (idx, ProductCaseClassRepresentation.ProdUplcConstr(idx, fieldReprs))
                }
                val sumRepr = SumUplcConstr(variants.toMap)
                // Align each branch: convert DataConstr to SumUplcConstr,
                // keep ProdUplcConstr as-is (it's already a variant of the sum)
                val aligned = branchesUpcasted.map { branch =>
                    branch.representation match
                        case _: ProductCaseClassRepresentation.ProdUplcConstr |
                            _: SumCaseClassRepresentation.SumUplcConstr =>
                            // Already UplcConstr — compatible with SumUplcConstr
                            branch
                        case ErrorRepresentation =>
                            // Error/fail() — keep as-is, always compatible
                            branch
                        case _ =>
                            // DataConstr → SumUplcConstr via toRepresentation chain
                            SumCaseSirTypeGenerator.toRepresentation(branch, sumRepr, pos)
                }
                (sumRepr: LoweredValueRepresentation, aligned)
            else
                val repr = LoweredValue.chooseCommonRepresentation(
                  branchesUpcasted,
                  resultType,
                  pos
                )
                (repr, branchesUpcasted.map(_.toRepresentation(repr, pos)))

        new ComplexLoweredValue(Set.empty, (loweredScrutinee :: branchesAligned.toList)*) {
            override def sirType: SIRType = resultType
            override def representation: LoweredValueRepresentation = resultRepr
            override def pos: SIRPosition = matchData.anns.pos

            override def termInternal(gctx: TermGenerationContext): Term =
                Term.Case(
                  loweredScrutinee.termWithNeededVars(gctx),
                  branchesAligned.map(_.termWithNeededVars(gctx)).toList,
                  UplcAnnotation(matchData.anns.pos)
                )

            override def docDef(ctx: LoweredValue.PrettyPrintingContext): Doc = {
                import Doc.*
                val branchDocs = branchesAligned.zipWithIndex.map { (b, i) =>
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
        optTargetType: Option[SIRType],
        loweredScrutinee: LoweredValue,
        constrIndex: Int
    )(using lctx: LoweringContext): LoweredValue = {
        val caseScope = lctx.scope
        val pos = sirCase.anns.pos

        // Get variant field reprs from the scrutinee's UplcConstr representation.
        // Unwrap SumReprProxy to get the real variants.
        // Handle ProdUplcConstr directly (product types with known field reprs).
        val variantFieldReprs: Option[scala.List[LoweredValueRepresentation]] =
            loweredScrutinee.representation match
                case proxy: SumCaseClassRepresentation.SumReprProxy =>
                    proxy.ref match
                        case sum: SumUplcConstr => sum.variants.get(constrIndex).map(_.fieldReprs)
                        case _                  => None
                case sum: SumUplcConstr =>
                    sum.variants.get(constrIndex).map(_.fieldReprs)
                case prod: ProductCaseClassRepresentation.ProdUplcConstr =>
                    Some(prod.fieldReprs)
                case _ => None

        sirCase.pattern match
            case constrPattern: Pattern.Constr =>
                val constrDecl = constrPattern.constr
                val fieldVars =
                    constrPattern.bindings.zipWithIndex.map { (name, fieldIdx) =>
                        val typeBinding = constrDecl.params(fieldIdx)
                        val tp = lctx.resolveTypeVarIfNeeded(typeBinding.tp)
                        val fieldRepr = variantFieldReprs match
                            case Some(reprs) if fieldIdx < reprs.size => reprs(fieldIdx)
                            case _ => lctx.typeGenerator(tp).defaultRepresentation(tp)
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
        // Unwrap SumReprProxy — recurse with the underlying repr
        input.representation match
            case proxy: SumCaseClassRepresentation.SumReprProxy =>
                return toRepresentation(
                  RepresentationProxyLoweredValue(input, proxy.ref, pos),
                  representation,
                  pos
                )
            case _ =>
        representation match
            case proxy: SumCaseClassRepresentation.SumReprProxy =>
                return toRepresentation(input, proxy.ref, pos)
            case _ =>
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
            // SumUplcConstr → SumUplcConstr
            case (inSum: SumUplcConstr, outSum: SumUplcConstr) =>
                if inSum.isCompatibleOn(input.sirType, outSum, pos) then
                    RepresentationProxyLoweredValue(input, representation, pos)
                else sumUplcConstrToSumUplcConstr(input, inSum, outSum, pos)
            // SumBuiltinList → SumUplcConstr: iterate builtin list and rebuild as Constr chain
            case (inBl: SumBuiltinList, outSum: SumUplcConstr) =>
                ScalusRuntime.builtinListToUplcConstr(input, outSum, input.sirType, pos)
            // SumUplcConstr → SumBuiltinList: iterate Constr chain, convert elements, build builtin list
            case (inSum: SumUplcConstr, outBl: SumBuiltinList) =>
                // Check if any variant has Transparent TypeVar fields —
                // these can't be converted to Data for SumBuiltinList
                val hasTransparent = inSum.variants.values.exists { prod =>
                    prod.fieldReprs.exists {
                        case TypeVarRepresentation(SIRType.TypeVarKind.Transparent) => true
                        case _                                                      => false
                    }
                }
                if hasTransparent then
                    throw LoweringException(
                      s"Cannot convert SumUplcConstr with Transparent TypeVar fields to SumBuiltinList. " +
                          s"Type: ${input.sirType.show}. " +
                          s"The return type needs @UplcRepr(UplcConstr) or the containing function needs Transparent-compatible output.",
                      pos
                    )
                ScalusRuntime.uplcConstrToBuiltinList(input, outBl, pos)
            // SumUplcConstr → PackedSumDataList: go through SumBuiltinList → listData
            case (_: SumUplcConstr, PackedSumDataList) =>
                val elemType = SumCaseClassRepresentation.SumBuiltinList
                    .retrieveListElementType(input.sirType)
                    .getOrElse(SIRType.Data.tp)
                val elemRepr = lctx.typeGenerator(elemType).defaultDataRepresentation(elemType)
                val builtinListRepr = SumCaseClassRepresentation.SumBuiltinList(elemRepr)
                val asBuiltinList = input.toRepresentation(builtinListRepr, pos)
                LoweredValue.Builder.lvBuiltinApply(
                  SIRBuiltins.listData,
                  asBuiltinList,
                  input.sirType,
                  PackedSumDataList,
                  pos
                )
            // PackedSumDataList → SumUplcConstr: unListData → builtinListToUplcConstr
            case (PackedSumDataList, outSum: SumUplcConstr) =>
                val elemType = SumCaseClassRepresentation.SumBuiltinList
                    .retrieveListElementType(input.sirType)
                    .getOrElse(SIRType.Data.tp)
                val elemRepr = lctx.typeGenerator(elemType).defaultDataRepresentation(elemType)
                val dataListRepr = SumCaseClassRepresentation.SumBuiltinList(elemRepr)
                val builtinListType = SIRType.BuiltinList(elemType)
                val asBuiltinList = LoweredValue.Builder.lvBuiltinApply(
                  SIRBuiltins.unListData,
                  input,
                  builtinListType,
                  dataListRepr,
                  pos
                )
                ScalusRuntime.builtinListToUplcConstr(asBuiltinList, outSum, input.sirType, pos)
            // ProdUplcConstr → SumUplcConstr: check field reprs compatibility
            case (
                  inProd: ProductCaseClassRepresentation.ProdUplcConstr,
                  outSum: SumUplcConstr
                ) =>
                prodUplcConstrToSumUplcConstr(input, inProd, outSum, pos)
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
                    tvRepr match
                        case _: TypeVarRepresentation =>
                            throw LoweringException(
                              s"Cannot convert unresolved TypeVar to SumUplcConstr for ${input.sirType.show}. " +
                                  s"TypeVar repr=$tvr, defaultTypeVarRepr=$tvRepr",
                              pos
                            )
                        case _ =>
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

    /** Convert SumUplcConstr → SumUplcConstr with different field reprs. Detects SumReprProxy
      * (self-referential tail) to decide:
      *   - No proxy → direct Case conversion
      *   - One proxy → letrec recursive conversion
      *   - Multiple different proxies → throw
      */
    private def sumUplcConstrToSumUplcConstr(
        input: LoweredValue,
        inSum: SumUplcConstr,
        outSum: SumUplcConstr,
        pos: SIRPosition
    )(using lctx: LoweringContext): LoweredValue = {
        // Find SumReprProxy fields across all variants
        val proxies = (inSum.variants.values.flatMap(_.fieldReprs) ++
            outSum.variants.values.flatMap(_.fieldReprs)).collect {
            case p: SumCaseClassRepresentation.SumReprProxy => p
        }.toSet
        if proxies.size > 1 then
            throw LoweringException(
              s"SumUplcConstr→SumUplcConstr: multiple different SumReprProxy found, not supported. Type: ${input.sirType.show}",
              pos
            )

        def makeBranches(recCall: Option[LoweredValue]): Seq[LoweredValue] = {
            val constructors = SumCaseSirTypeGenerator.findConstructors(input.sirType, pos)
            constructors.zipWithIndex.map { (constrDecl, idx) =>
                val inPuc = inSum.variants.getOrElse(
                  idx,
                  ProductCaseClassRepresentation.ProdUplcConstr(idx, Nil)
                )
                val outPuc = outSum.variants.getOrElse(
                  idx,
                  ProductCaseClassRepresentation.ProdUplcConstr(idx, Nil)
                )
                val fieldVars = constrDecl.params.zip(inPuc.fieldReprs).map { (param, repr) =>
                    val tp = lctx.resolveTypeVarIfNeeded(param.tp)
                    val name = lctx.uniqueVarName(s"_uc2uc_f")
                    new VariableLoweredValue(
                      id = name,
                      name = name,
                      sir = SIR.Var(name, tp, AnnotationsDecl(pos)),
                      representation = repr
                    )
                }
                val convertedFields = fieldVars.zip(inPuc.fieldReprs).zip(outPuc.fieldReprs).map {
                    case ((fv, inRepr), outRepr) =>
                        if inRepr == outRepr then fv
                        else
                            // For SumReprProxy fields (recursive tail), use the letrec call
                            (inRepr, outRepr) match
                                case (_: SumCaseClassRepresentation.SumReprProxy, _) |
                                    (_, _: SumCaseClassRepresentation.SumReprProxy) =>
                                    recCall match
                                        case Some(go) =>
                                            lvApplyDirect(go, fv, input.sirType, outSum, pos)
                                        case None =>
                                            // Should not happen — proxy found but no recCall
                                            throw LoweringException(
                                              s"SumReprProxy field but no letrec — internal error",
                                              pos
                                            )
                                case _ =>
                                    fv.toRepresentation(outRepr, pos)
                }
                val inPos = pos
                val result = new ComplexLoweredValue(fieldVars.toSet, convertedFields*) {
                    override def sirType = input.sirType
                    override def representation = outPuc
                    override def pos = inPos
                    override def termInternal(gctx: TermGenerationContext) = {
                        val innerCtx =
                            gctx.copy(generatedVars = gctx.generatedVars ++ fieldVars.map(_.id))
                        Term.Constr(
                          scalus.cardano.ledger.Word64(idx.toLong),
                          convertedFields.map(_.termWithNeededVars(innerCtx)).toList,
                          UplcAnnotation(inPos)
                        )
                    }
                    override def docDef(ctx: LoweredValue.PrettyPrintingContext) =
                        Doc.text(s"UplcConstr→UplcConstr($idx)")
                    override def docRef(ctx: LoweredValue.PrettyPrintingContext) = docDef(ctx)
                }
                fieldVars.foldRight(result: LoweredValue) { (fv, inner) =>
                    new ComplexLoweredValue(Set(fv), inner) {
                        override def sirType = inner.sirType
                        override def representation = inner.representation
                        override def pos = inPos
                        override def termInternal(gctx: TermGenerationContext) = {
                            val ngctx = gctx.copy(generatedVars = gctx.generatedVars + fv.id)
                            Term.LamAbs(
                              fv.id,
                              inner.termWithNeededVars(ngctx),
                              UplcAnnotation(inPos)
                            )
                        }
                        override def docDef(ctx: LoweredValue.PrettyPrintingContext) =
                            inner.docRef(ctx)
                        override def docRef(ctx: LoweredValue.PrettyPrintingContext) = docDef(ctx)
                    }
                }
            }
        }

        def makeCase(inputVal: LoweredValue, branches: Seq[LoweredValue]): LoweredValue = {
            new ComplexLoweredValue(Set.empty, (inputVal :: branches.toList)*) {
                override def sirType = input.sirType
                override def representation = outSum
                override def pos = input.pos
                override def termInternal(gctx: TermGenerationContext) =
                    Term.Case(
                      inputVal.termWithNeededVars(gctx),
                      branches.map(_.termWithNeededVars(gctx)).toList,
                      UplcAnnotation(pos)
                    )
                override def docDef(ctx: LoweredValue.PrettyPrintingContext) =
                    Doc.text("UplcConstr→UplcConstr(") + inputVal.docRef(ctx) + Doc.text(")")
                override def docRef(ctx: LoweredValue.PrettyPrintingContext) = docDef(ctx)
            }
        }

        if proxies.isEmpty then
            // No self-referential fields — direct Case conversion
            makeCase(input, makeBranches(None))
        else
            // Has SumReprProxy — use letrec for recursive conversion
            val goType = input.sirType ->: input.sirType
            val goRepr = LambdaRepresentation(
              goType,
              InOutRepresentationPair(inSum, outSum)
            )
            lvLetRec(
              lctx.uniqueVarName("$uc2uc"),
              goType,
              goRepr,
              go =>
                  lvLamAbs(
                    "uc2uc_in",
                    input.sirType,
                    inSum,
                    inVal => makeCase(inVal, makeBranches(Some(go))),
                    pos
                  ),
              inVal => lvApplyDirect(inVal, input, input.sirType, outSum, pos),
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
            val branchConstr: LoweredValue = new ComplexLoweredValue(Set.empty, fields*) {
                override def sirType = input.sirType
                override def representation = outSum
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
            }
            branchConstr
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

}
