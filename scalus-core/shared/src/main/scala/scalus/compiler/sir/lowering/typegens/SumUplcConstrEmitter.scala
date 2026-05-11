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
object SumUplcConstrEmitter {

    /** A Case-branch-body lambda wrapper used inside UplcConstr pattern matching. Represents
      * `λfieldVar. inner` where `inner` is the branch's body. Unlike a plain `ComplexLoweredValue`
      * wrap, `toRepresentation` is pushed into `inner` rather than applied to the whole lambda —
      * because at the UPLC level, a branch is applied to the `Constr`'s fields before its body is
      * evaluated. Wrapping the lambda itself in a repr-conversion (e.g. `Case(λh.λt.h, ...)`) fails
      * at runtime with "non-constructor scrutinized" because the scrutinee is a lambda.
      */
    private final class BranchLambdaWrapper(
        fieldVar: IdentifiableLoweredValue,
        val inner: LoweredValue,
        casePos: SIRPosition
    ) extends ComplexLoweredValue(Set(fieldVar), inner) {
        override def sirType: SIRType = inner.sirType
        override def representation: LoweredValueRepresentation = inner.representation
        override def pos: SIRPosition = casePos

        override def termInternal(gctx: TermGenerationContext): Term = {
            val innerCtx = gctx.copy(generatedVars = gctx.generatedVars + fieldVar.id)
            Term.LamAbs(fieldVar.id, inner.termWithNeededVars(innerCtx), UplcAnnotation(casePos))
        }

        override def toRepresentation(
            newRepr: LoweredValueRepresentation,
            inPos: SIRPosition
        )(using lctx: LoweringContext): LoweredValue = {
            if newRepr == representation then this
            else
                val convertedInner = inner.toRepresentation(newRepr, inPos)
                if convertedInner eq inner then this
                else new BranchLambdaWrapper(fieldVar, convertedInner, casePos)
        }

        override def docDef(ctx: LoweredValue.PrettyPrintingContext): Doc =
            Doc.text(s"λ${fieldVar.name}.") + inner.docRef(ctx)
        override def docRef(ctx: LoweredValue.PrettyPrintingContext): Doc = docDef(ctx)
    }

    /** Deref TypeProxy / Annotated / TypeLambda wrappers to find the underlying
      * [[SIRType.SumCaseClass]] so callers can read its typeArgs. Visited TypeProxies are tracked
      * to avoid infinite loops on recursive types (e.g. List's tail refers back to List).
      */
    private def derefToSumCaseClass(
        tp: SIRType,
        visited: Set[SIRType.TypeProxy] = Set.empty
    ): Option[SIRType.SumCaseClass] = tp match
        case sc: SIRType.SumCaseClass => Some(sc)
        case p: SIRType.TypeProxy if p.ref != null && !visited.contains(p) =>
            derefToSumCaseClass(p.ref, visited + p)
        case SIRType.Annotated(inner, _) => derefToSumCaseClass(inner, visited)
        case SIRType.TypeLambda(_, body) => derefToSumCaseClass(body, visited)
        case _                           => None

    /** DataDecl-level TypeVar substitution for [[inputType]]. Maps the sum's declared type
      * parameters to the concrete type args at `inputType`'s call site. Empty when the type args
      * are missing or the shape mismatches the DataDecl.
      */
    private def dataDeclSubst(inputType: SIRType): Map[SIRType.TypeVar, SIRType] =
        SIRType.retrieveDataDecl(inputType) match
            case Right(decl) =>
                derefToSumCaseClass(inputType) match
                    case Some(SIRType.SumCaseClass(_, typeArgs))
                        if typeArgs.length == decl.typeParams.length =>
                        decl.typeParams.zip(typeArgs).toMap
                    case _ => Map.empty
            case _ => Map.empty

    /** Per-constructor TypeVar substitution, composing [[dataDeclSubst]] with the constructor-local
      * parent- and typeParam-level mappings. `constrDecl.params.tp` uses the constructor's own
      * TypeVar instances (distinct from the DataDecl's); `parentTypeArgs` encodes how those map
      * onto DataDecl typeParams. Combining all three resolves field types to concrete forms at
      * `inputType`'s call site.
      */
    private def constrSubstFor(
        inputType: SIRType,
        constrDecl: ConstrDecl,
        dataDeclSubst: Map[SIRType.TypeVar, SIRType]
    ): Map[SIRType.TypeVar, SIRType] = {
        val inputArgs = derefToSumCaseClass(inputType).map(_.typeArgs).getOrElse(Nil)
        val parentSubst: Map[SIRType.TypeVar, SIRType] =
            if constrDecl.parentTypeArgs.nonEmpty
                && inputArgs.length == constrDecl.parentTypeArgs.length
            then
                constrDecl.parentTypeArgs
                    .zip(inputArgs)
                    .collect { case (tv: SIRType.TypeVar, concrete) => (tv, concrete) }
                    .toMap
            else Map.empty
        val typeParamSubst: Map[SIRType.TypeVar, SIRType] =
            if constrDecl.typeParams.nonEmpty
                && inputArgs.length == constrDecl.typeParams.length
            then constrDecl.typeParams.zip(inputArgs).toMap
            else Map.empty
        dataDeclSubst ++ parentSubst ++ typeParamSubst
    }

    /** Build SumUplcConstr with variant info from the type's DataDecl.
      *
      * `pos` is used in error messages so we can locate the SIR site that triggered the build
      * (otherwise these errors only carry the JVM stack, no SIR file/line context).
      */
    def buildSumUplcConstr(tp: SIRType, pos: SIRPosition = SIRPosition.empty)(using
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
            case SIRType.TypeLambda(_, body)        => return buildSumUplcConstr(body, pos)
            case SIRType.TypeProxy(ref)             => return buildSumUplcConstr(ref, pos)
            case SIRType.Annotated(tp1, _)          => return buildSumUplcConstr(tp1, pos)
            case _ =>
                return SumUplcConstr(scala.collection.immutable.SortedMap.empty)

        // Build name-based substitution: TypeVar name → concrete type.
        // Resolve each typeArg through `lctx.typeUnifyEnv` — at call sites like
        // `ps.points.headOption2`, the enclosing signature may still carry the
        // extension method's abstract `A` in typeArgs; the call site's TypeVar
        // binding (e.g. A → Point) lives in `typeUnifyEnv.filledTypes`.
        val typeSubstByName: Map[String, SIRType] =
            typeParams.map(_.name).zip(typeArgs.map(lctx.resolveTypeVarIfNeeded)).toMap

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
        val variants = constructors.zipWithIndex
            .map { (constrDecl, idx) =>
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
                                    // Preserve passthrough kinds in the field repr:
                                    //   Transparent → Transparent (wildcard)
                                    //   Unwrapped   → Unwrapped (concrete-default form)
                                    //   Fixed       → ERROR. A Fixed (Data-wrapped) TypeVar in
                                    //                 a UplcConstr field is a producer/consumer
                                    //                 mismatch — UplcConstr fields are native
                                    //                 Constr children and shouldn't carry an
                                    //                 abstract Data marker. Caller should resolve
                                    //                 the TypeVar to a concrete type or use
                                    //                 Transparent/Unwrapped.
                                    case tv: SIRType.TypeVar =>
                                        import SIRType.TypeVarKind.*
                                        tv.kind match
                                            case Transparent | Unwrapped =>
                                                TypeVarRepresentation(tv.kind)
                                            case Fixed =>
                                                // A Fixed TypeVar in a UplcConstr field means the
                                                // caller didn't resolve the type param before we got
                                                // here (e.g. an extension method whose type param is
                                                // stamped Fixed by the plugin and isn't bound via
                                                // `typeUnifyEnv` at this call site). Fall back to the
                                                // type generator's default rep — for a Fixed TypeVar
                                                // that's `TypeVarRepresentation(Fixed)` (Data-wrapped
                                                // abstract), matching the pre-guard behavior.
                                                SirTypeUplcGenerator.defaultRepresentation(tv)
                                    // Unsubstituted DataDecl TypeVars (e.g., `Option[*]` at a
                                    // constructor call site where Scala inferred `Nothing`/`Any`
                                    // for the type arg) substitute to FreeUnificator via
                                    // `typeSubstByName`. Using `defaultRepresentation(FreeUnificator)
                                    // = TypeVarRepresentation(Fixed)` here labels the field as
                                    // Data-wrapped even when the actual runtime value is native
                                    // Constr — a representation lie. Use Transparent (passthrough)
                                    // so the field takes whatever native repr the value carries.
                                    case SIRType.FreeUnificator =>
                                        TypeVarRepresentation(SIRType.TypeVarKind.Transparent)
                                    case _ =>
                                        // Interpretation (a) for `@UplcRepr(UplcConstr)`: nested
                                        // field reprs are the field type's stable default.
                                        SirTypeUplcGenerator.defaultRepresentation(paramType)
                        }
                }
                idx -> ProductCaseClassRepresentation.ProdUplcConstr(idx, fieldReprs)
            }
            .to(scala.collection.immutable.SortedMap)
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
            .getOrElse(SirTypeUplcGenerator.defaultRepresentation(elemType))
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

    /** Generic N-variant Case dispatch on a SumUplcConstr scrutinee.
      *
      * Builds `Term.Case(scrutinee, [branch_0, branch_1, ...])` where each branch is a nested
      * lambda binding the variant's fields and evaluating the per-variant body. Variant order
      * matches the DataDecl constructor order (tag = position).
      *
      * The `perVariant` callback receives `(constrIdx, fieldVars)`. Each `fieldVar` is an
      * `IdentifiableLoweredValue` with the variant's field type substituted with the sum's concrete
      * type args, and the field repr from `scrutineeRepr.variants(constrIdx)`.
      *
      * All per-variant bodies must produce values of `outType` with `outRepr` representation — the
      * caller is responsible for any normalization.
      */
    def genMatchUplcConstrAllVariants(
        scrutinee: LoweredValue,
        scrutineeRepr: SumCaseClassRepresentation.SumUplcConstr,
        sumType: SIRType,
        outType: SIRType,
        outRepr: LoweredValueRepresentation,
        pos: SIRPosition,
        perVariant: (Int, Seq[IdentifiableLoweredValue]) => LoweringContext ?=> LoweredValue
    )(using lctx: LoweringContext): LoweredValue = {
        val (decl, typeArgs) = sumType match
            case SIRType.SumCaseClass(d, ta)                       => (d, ta)
            case SIRType.Annotated(SIRType.SumCaseClass(d, ta), _) => (d, ta)
            case other =>
                throw LoweringException(
                  s"genMatchUplcConstrAllVariants: expected SumCaseClass type, got ${other.show}",
                  pos
                )
        val typeSubstByName: Map[String, SIRType] =
            decl.typeParams.map(_.name).zip(typeArgs).toMap
        val constrPos = AnnotationsDecl.empty.pos
        val branches = decl.constructors.zipWithIndex.map { case (constrDecl, tagIdx) =>
            val variantRepr = scrutineeRepr.variants.getOrElse(
              tagIdx,
              throw LoweringException(
                s"genMatchUplcConstrAllVariants: missing variant tag $tagIdx in $scrutineeRepr",
                pos
              )
            )
            val fieldReprs = variantRepr.fieldReprs
            val params = constrDecl.params
            if params.length != fieldReprs.length then
                throw LoweringException(
                  s"genMatchUplcConstrAllVariants: variant tag $tagIdx has ${params.length} params but ${fieldReprs.length} field reprs",
                  pos
                )
            // Substitute typeparams in field types
            val fieldVars: Seq[VariableLoweredValue] =
                params.zip(fieldReprs).map { case (param, repr) =>
                    val rawType = param.tp match
                        case tv: SIRType.TypeVar => typeSubstByName.getOrElse(tv.name, tv)
                        case other               => other
                    val fieldType = lctx.resolveTypeVarIfNeeded(rawType)
                    val varId = lctx.uniqueVarName(s"v_${tagIdx}_${param.name}")
                    new VariableLoweredValue(
                      id = varId,
                      name = varId,
                      sir = SIR.Var(varId, fieldType, AnnotationsDecl(pos)),
                      representation = repr
                    )
                }
            val savedScope = lctx.scope
            fieldVars.foreach(v => lctx.scope = lctx.scope.add(v))
            val branchBody =
                try perVariant(tagIdx, fieldVars)(using lctx)
                finally lctx.scope = savedScope
            new ComplexLoweredValue(fieldVars.toSet, branchBody) {
                override def sirType: SIRType = outType
                override def representation: LoweredValueRepresentation = outRepr
                override def pos: SIRPosition = constrPos
                override def termInternal(gctx: TermGenerationContext): Term = {
                    val ngctx = fieldVars.foldLeft(gctx)((g, v) => g.addGeneratedVar(v.id))
                    val innerTerm = branchBody.termWithNeededVars(ngctx)
                    fieldVars.foldRight(innerTerm) { (v, body) =>
                        Term.LamAbs(v.id, body, UplcAnnotation(constrPos))
                    }
                }
                override def docDef(ctx: LoweredValue.PrettyPrintingContext): Doc = {
                    val params = fieldVars
                        .map(v => Doc.text(s"λ${v.id}."))
                        .foldLeft(Doc.empty)(_ + _)
                    params + branchBody.docRef(ctx)
                }
                override def docRef(ctx: LoweredValue.PrettyPrintingContext): Doc = docDef(ctx)
            }
        }
        new ComplexLoweredValue(Set.empty, (scrutinee :: branches.toList)*) {
            override def sirType: SIRType = outType
            override def representation: LoweredValueRepresentation = outRepr
            override def pos: SIRPosition = constrPos
            override def termInternal(gctx: TermGenerationContext): Term =
                Term.Case(
                  scrutinee.termWithNeededVars(gctx),
                  branches.map(_.termWithNeededVars(gctx)).toList,
                  UplcAnnotation(constrPos)
                )
            override def docDef(ctx: LoweredValue.PrettyPrintingContext): Doc = {
                val branchDocs = branches.zipWithIndex.map { case (b, i) =>
                    Doc.line + Doc.text(s"$i ->") + (Doc.lineOrSpace + b.docRef(ctx)).nested(2)
                }
                ((Doc.text("case") + Doc.space + scrutinee.docRef(ctx) + Doc.space + Doc.text("of"))
                    + branchDocs.foldLeft(Doc.empty)(_ + _.grouped)).aligned
            }
            override def docRef(ctx: LoweredValue.PrettyPrintingContext): Doc = docDef(ctx)
        }
    }

    /** Select a field from a UplcConstr-represented value. */
    def genSelectUplcConstr(sel: SIR.Select, loweredScrutinee: LoweredValue)(using
        LoweringContext
    ): LoweredValue =
        ProductCaseUplcConstrOnlyEmitter.genSelect(sel, loweredScrutinee)

    def genMatchUplcConstr(
        matchData: SIR.Match,
        loweredScrutinee: LoweredValue,
        optTargetType: Option[SIRType]
    )(using lctx: LoweringContext): LoweredValue = {
        val (scaffolding, branches) =
            emitMatchConditionUplcConstr(matchData, loweredScrutinee, optTargetType)
        SumDispatch.assembleMatch(
          scaffolding,
          branches,
          optTargetType,
          matchData.tp,
          matchData.anns.pos
        )
    }

    /** The scrutinee-repr-driven half of `genMatchUplcConstr` (Phase 4b): lower each case body in
      * the scope of the variant's field bindings, return the `Term.Case` scaffolding plus the
      * unaligned branch values. The result repr is decided by `SumDispatch.assembleMatch`, not
      * here.
      */
    private def emitMatchConditionUplcConstr(
        matchData: SIR.Match,
        loweredScrutinee: LoweredValue,
        optTargetType: Option[SIRType]
    )(using lctx: LoweringContext): (SumDispatch.MatchScaffolding, Seq[LoweredValue]) = {
        val prevScope = lctx.scope
        val orderedCases =
            DataConstrEmitter.prepareCases(matchData, loweredScrutinee)
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
        (uplcConstrScaffolding(loweredScrutinee, pos), branches)
    }

    /** `Term.Case`-based scaffolding: assembles a `ComplexLoweredValue` whose term is
      * `Term.Case(scrutinee, branches)`. Branch order matches the SumUplcConstr tag order (set by
      * `prepareCases` in `emitMatchConditionUplcConstr`).
      */
    private def uplcConstrScaffolding(
        scrutinee: LoweredValue,
        matchPos: SIRPosition
    ): SumDispatch.MatchScaffolding = new SumDispatch.MatchScaffolding {
        override def assemble(
            branches: Seq[LoweredValue],
            targetRepr: LoweredValueRepresentation,
            resultType: SIRType,
            pos: SIRPosition
        )(using LoweringContext): LoweredValue = {
            val branchesList = branches.toList
            new ComplexLoweredValue(Set.empty, (scrutinee :: branchesList)*) {
                override def sirType: SIRType = resultType
                override def representation: LoweredValueRepresentation = targetRepr
                override def pos: SIRPosition = matchPos

                override def termInternal(gctx: TermGenerationContext): Term =
                    Term.Case(
                      scrutinee.termWithNeededVars(gctx),
                      branchesList.map(_.termWithNeededVars(gctx)),
                      UplcAnnotation(matchPos)
                    )

                override def docDef(ctx: LoweredValue.PrettyPrintingContext): Doc = {
                    import Doc.*
                    val branchDocs = branchesList.zipWithIndex.map { (b, i) =>
                        line + text(s"$i ->") + (lineOrSpace + b.docRef(ctx)).nested(2)
                    }
                    ((text("case") + space + scrutinee.docRef(ctx) + space + text("of"))
                        + branchDocs.foldLeft(empty)(_ + _.grouped)).aligned
                }

                override def docRef(ctx: LoweredValue.PrettyPrintingContext): Doc =
                    docDef(ctx)
            }
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

        // Scrutinee's @UplcRepr annotation is propagated to self-referential field bindings
        // (fields whose runtime repr is a SumReprProxy back to the scrutinee, e.g. `tail: List[A]`
        // in `List.Cons(h, tail)`). Without this, `tail`'s sirType would be a bare `List[A]` with
        // no annotation, causing downstream intrinsic dispatch to fall through to the default
        // (SumBuiltinList) path.
        val scrutineeUplcReprAnns: Option[AnnotationsDecl] =
            loweredScrutinee.sirType match
                case SIRType.Annotated(_, anns) if anns.data.contains("uplcRepr") => Some(anns)
                case _                                                            => None

        sirCase.pattern match
            case constrPattern: Pattern.Constr =>
                val constrDecl = constrPattern.constr
                val fieldVars =
                    constrPattern.bindings.zipWithIndex.map { (name, fieldIdx) =>
                        val typeBinding = constrDecl.params(fieldIdx)
                        val tp0 = lctx.resolveTypeVarIfNeeded(typeBinding.tp)
                        val fieldRepr = variantFieldReprs match
                            case Some(reprs) if fieldIdx < reprs.size => reprs(fieldIdx)
                            case _ => SirTypeUplcGenerator.defaultRepresentation(tp0)
                        val tp = fieldRepr match
                            case _: SumCaseClassRepresentation.SumReprProxy
                                if scrutineeUplcReprAnns.isDefined &&
                                    !tp0.isInstanceOf[SIRType.Annotated] =>
                                SIRType.Annotated(tp0, scrutineeUplcReprAnns.get)
                            case _ => tp0
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
                    BranchLambdaWrapper(fieldVar, inner, sirCase.anns.pos)
                }

            case Pattern.Wildcard =>
                val body = lctx.lower(sirCase.body, optTargetType)
                lctx.scope = caseScope
                body

            case other =>
                throw LoweringException(s"Unexpected pattern in UplcConstr match: $other", pos)
    }

    // ===================== emitConvert =====================

    /** Handle all UplcConstr-related representation conversions (Phase 5). */
    def emitConvert(
        input: LoweredValue,
        representation: LoweredValueRepresentation,
        pos: SIRPosition
    )(using lctx: LoweringContext): LoweredValue = {
        // Unwrap SumReprProxy — recurse with the underlying repr
        input.representation match
            case proxy: SumCaseClassRepresentation.SumReprProxy =>
                return emitConvert(
                  RepresentationProxyLoweredValue(input, proxy.ref, pos),
                  representation,
                  pos
                )
            case _ =>
        representation match
            case proxy: SumCaseClassRepresentation.SumReprProxy =>
                return emitConvert(input, proxy.ref, pos)
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
                via(DataConstr)(input, PairIntDataList, pos)
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
                // Guard: if any variant has Transparent TypeVar fields AND the input's
                // element type is itself an unresolved TypeVar, we have no concrete type
                // to drive the per-field conversion — bail out. If the element type IS
                // concrete (e.g., List[ChessSet]), the runtime conversion dispatches on
                // the actual head value's sirType even when the field repr is a
                // Transparent TypeVar left over from intrinsic-body lowering.
                val elemTypeRaw = SumCaseClassRepresentation.SumBuiltinList
                    .retrieveListElementType(input.sirType)
                    .getOrElse(SIRType.Data.tp)
                // If element type is a TypeVar, try to resolve via lctx.filledTypes first;
                // only treat it as "unresolved Fixed" if it's still a non-passthrough
                // TypeVar. Passthrough TypeVars (Transparent/Unwrapped) have passthrough
                // repr semantics — the runtime value has the correct native shape regardless.
                val elemType = lctx.resolveTypeVarIfNeeded(elemTypeRaw)
                val elemIsUnresolvedFixed = elemType match
                    case tv: SIRType.TypeVar => !tv.isBuiltin
                    case _                   => false
                val hasTransparent = inSum.variants.values.exists { prod =>
                    prod.fieldReprs.exists {
                        case tvr: TypeVarRepresentation => tvr.isBuiltin
                        case _                          => false
                    }
                }
                if hasTransparent && elemIsUnresolvedFixed then
                    throw LoweringException(
                      s"Cannot convert SumUplcConstr with Transparent TypeVar fields to SumBuiltinList " +
                          s"when element type is itself unresolved. Type: ${input.sirType.show}. " +
                          s"The return type needs @UplcRepr(UplcConstr) or the containing function needs Transparent-compatible output.",
                      pos
                    )
                ScalusRuntime.uplcConstrToBuiltinList(input, outBl, pos)
            // SumUplcConstr → PackedSumDataList: go through SumBuiltinList → listData
            case (_: SumUplcConstr, PackedSumDataList) =>
                val elemType = SumCaseClassRepresentation.SumBuiltinList
                    .retrieveListElementType(input.sirType)
                    .getOrElse(SIRType.Data.tp)
                val elemRepr = SirTypeUplcGenerator.defaultDataRepresentation(elemType)
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
                val elemRepr = SirTypeUplcGenerator.defaultDataRepresentation(elemType)
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
                via(PairIntDataList)(input, representation, pos)
            // TypeVarRepresentation → SumUplcConstr/SumBuiltinList: dispatch by source kind
            case (
                  tvr: TypeVarRepresentation,
                  _: SumUplcConstr | _: SumCaseClassRepresentation.SumBuiltinList
                ) =>
                TypeVarEmitter.bridgeFromKind(input, tvr, representation, pos)
            // UplcConstr/ProdUplcConstr → TypeVarRepresentation: dispatch by target kind
            case (
                  _: SumUplcConstr | _: ProductCaseClassRepresentation.ProdUplcConstr,
                  tvr: TypeVarRepresentation
                ) =>
                // Phase 5 Fixed canonicalization: Unwrapped/Fixed go through bridgeToKind
                // (canonical "always relabel as target"). The Fixed arm previously skipped the
                // relabel and returned a value with concrete repr — see the design doc for the
                // audit. Transparent stays open-coded as `input` (no relabel) because that is
                // the current semantics across most sum-side emitters.
                if tvr.kind == SIRType.TypeVarKind.Transparent then input
                else TypeVarEmitter.bridgeToKind(input, tvr, pos)
            case _ =>
                throw LoweringException(
                  s"SumUplcConstrEmitter: unsupported conversion from ${input.representation} to $representation for ${input.sirType.show}",
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
        // Find SumReprProxy fields, counted per-side. Each side normally has at most
        // one proxy (a single self-recursive field, e.g., List's tail). >1 within one
        // side means the type has multiple recursive fields per variant (e.g., a binary
        // tree Branch(l, r)), which the single recCall letrec below cannot handle.
        // inSum and outSum each have their OWN SumReprProxy instance self-referring to
        // themselves — combining them and using reference equality would falsely trip.
        val inProxies = inSum.variants.values
            .flatMap(_.fieldReprs)
            .collect { case p: SumCaseClassRepresentation.SumReprProxy =>
                p
            }
            .toSet
        val outProxies = outSum.variants.values
            .flatMap(_.fieldReprs)
            .collect { case p: SumCaseClassRepresentation.SumReprProxy =>
                p
            }
            .toSet
        if inProxies.size > 1 || outProxies.size > 1 then
            throw LoweringException(
              s"SumUplcConstr→SumUplcConstr: multiple recursive proxies within one side " +
                  s"(tree-like types with >1 self-reference per variant), not supported. " +
                  s"Type: ${input.sirType.show}",
              pos
            )

        val baseSubst = dataDeclSubst(input.sirType)
        def makeBranches(recCall: Option[LoweredValue]): Seq[LoweredValue] = {
            val constructors = DataConstrEmitter.findConstructors(input.sirType, pos)
            constructors.zipWithIndex.map { (constrDecl, idx) =>
                val inPuc = inSum.variants.getOrElse(
                  idx,
                  ProductCaseClassRepresentation.ProdUplcConstr(idx, Nil)
                )
                val outPuc = outSum.variants.getOrElse(
                  idx,
                  ProductCaseClassRepresentation.ProdUplcConstr(idx, Nil)
                )
                val constrSubst = constrSubstFor(input.sirType, constrDecl, baseSubst)
                val fieldVars = constrDecl.params.zip(inPuc.fieldReprs).map { (param, repr) =>
                    val paramTpSubst =
                        if constrSubst.isEmpty then param.tp
                        else SIRType.substitute(param.tp, constrSubst, Map.empty)
                    val tp = lctx.resolveTypeVarIfNeeded(paramTpSubst)
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

        if inProxies.isEmpty && outProxies.isEmpty then
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
        // Substitute the sum's concrete type args into per-field types below. Without this,
        // `value: A` of `Option.Some` stays abstract when `input.sirType` is `Option[Tile]`,
        // and `head.toRepresentation(fieldRepr)` with a concrete `fieldRepr` dispatches via
        // `TypeVarSirTypeGenerator.convertAbstractFixedToTarget` — a pure relabel that leaves
        // Data-shape bytes under a native-shape label.
        val baseSubst = dataDeclSubst(input.sirType)
        val constructors = DataConstrEmitter.findConstructors(input.sirType, pos)
        val branches = constructors.zipWithIndex.map { (constrDecl, idx) =>
            val constrSubst = constrSubstFor(input.sirType, constrDecl, baseSubst)
            def substParamTp(paramTp: SIRType): SIRType =
                if constrSubst.isEmpty then paramTp
                else SIRType.substitute(paramTp, constrSubst, Map.empty)
            val variantRepr = outSum.variants.getOrElse(
              idx,
              ProductCaseClassRepresentation.ProdUplcConstr(
                idx,
                constrDecl.params.map { p =>
                    val tp = lctx.resolveTypeVarIfNeeded(substParamTp(p.tp))
                    SirTypeUplcGenerator.defaultDataRepresentation(tp)
                }
              )
            )
            var currentList: LoweredValue = dataListVar
            val fields = constrDecl.params.zip(variantRepr.fieldReprs).map { (param, fieldRepr) =>
                val tp = lctx.resolveTypeVarIfNeeded(substParamTp(param.tp))
                val dataRepr = SirTypeUplcGenerator.defaultDataRepresentation(tp)
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
        val constructors = DataConstrEmitter.findConstructors(input.sirType, pos)
        val baseSubst = dataDeclSubst(input.sirType)
        val branches = constructors.zipWithIndex.map { (constrDecl, idx) =>
            val constrSubst = constrSubstFor(input.sirType, constrDecl, baseSubst)
            def substParamTp(paramTp: SIRType): SIRType =
                if constrSubst.isEmpty then paramTp
                else SIRType.substitute(paramTp, constrSubst, Map.empty)
            val variantRepr = inSum.variants.getOrElse(
              idx,
              ProductCaseClassRepresentation.ProdUplcConstr(
                idx,
                constrDecl.params.map { p =>
                    val tp = lctx.resolveTypeVarIfNeeded(substParamTp(p.tp))
                    SirTypeUplcGenerator.defaultRepresentation(tp)
                }
              )
            )
            val fieldVars = constrDecl.params.zip(variantRepr.fieldReprs).map { (param, repr) =>
                val tp = lctx.resolveTypeVarIfNeeded(substParamTp(param.tp))
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
                    val tp = lctx.resolveTypeVarIfNeeded(substParamTp(param.tp))
                    val dataRepr = SirTypeUplcGenerator.defaultDataRepresentation(tp)
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
