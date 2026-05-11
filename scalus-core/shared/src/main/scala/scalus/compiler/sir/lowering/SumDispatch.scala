package scalus.compiler.sir.lowering

import scalus.compiler.sir.*
import scalus.compiler.sir.lowering.LoweredValue.Builder.*
import scalus.compiler.sir.lowering.typegens.*

/** Dispatch layer for sum-typed operations. Single entry point for Sum-side `toRepresentation`;
  * routing typegens (DataConstrEmitter, SumCaseUplcConstr*, SumListEmitterCommon subclasses, plus
  * the Prod typegens that delegate via ProdDispatch) extend the base `SirTypeUplcGenerator` only —
  * they don't have `toRepresentation` at all, so direct calls are compile-time errors. See
  * `docs/local/claude/compiler/sum-prod-dispatch-design.md`.
  */
object SumDispatch {

    import SumCaseClassRepresentation.*

    def toRepresentation(
        input: LoweredValue,
        target: LoweredValueRepresentation,
        pos: SIRPosition
    )(using lctx: LoweringContext): LoweredValue = {
        val gen = typegens.SirTypeUplcGenerator(input.sirType)
        gen match
            case DataConstrEmitter =>
                sumCaseImpl(input, target, pos)
            case SumCaseUplcConstrEmitter =>
                SumCaseUplcConstrEmitter.emitConvert(input, target, pos)
            case SumCaseUplcOnlyEmitter =>
                sumCaseImpl(input, target, pos)
            case listGen: SumListEmitterCommon =>
                listGen.emitConvert(input, target, pos)
            case ProductCaseEmitter | ProductCaseUplcConstrEmitter | ProductCaseUplcOnlyEmitter |
                _: OneElementWrapperEmitter =>
                ProdDispatch.toRepresentation(input, target, pos)
            case converting: typegens.SirTypeUplcConvertingGenerator =>
                converting.toRepresentation(input, target, pos)
            case other =>
                throw new IllegalStateException(
                  s"SumDispatch.toRepresentation: typegen ${other.getClass.getSimpleName} for " +
                      s"${input.sirType.show} doesn't extend SirTypeUplcConvertingGenerator and " +
                      s"isn't a routing case here. This is a bug in the dispatch table — either " +
                      s"add a routing case or have the typegen extend SirTypeUplcConvertingGenerator."
                )
    }

    /** Plain sum-class source path: handles DataConstr / PairIntDataList / SumBuiltinList /
      * PackedSumDataList sources, delegating to `SumUplcConstrEmitter` for native-UplcConstr cases
      * and to `SumListEmitterCommon.emitConvert` for list-shape conversions.
      *
      * Package-private so `SumCaseUplcConstrEmitter.emitConvert` can delegate cross-typegen arms
      * here (Phase 5 step 4).
      */
    private[lowering] def sumCaseImpl(
        input: LoweredValue,
        representation: LoweredValueRepresentation,
        pos: SIRPosition
    )(using lctx: LoweringContext): LoweredValue = {
        (input.representation, representation) match {
            // === DataConstr source: delegate to DataConstrEmitter (Phase 5) ===
            case (
                  DataConstr,
                  DataConstr | PairIntDataList | _: SumBuiltinList | PackedSumDataList |
                  _: SumUplcConstr
                ) =>
                DataConstrEmitter.emitConvert(input, representation, pos)
            // === PairIntDataList → list reprs: delegate to list generator ===
            case (PairIntDataList, SumBuiltinList(_)) | (PairIntDataList, PackedSumDataList) =>
                val elemType =
                    SumBuiltinList.retrieveListElementType(input.sirType).getOrElse(SIRType.Data.tp)
                val elemRepr = typegens.SirTypeUplcGenerator.defaultDataRepresentation(elemType)
                new SumBuiltinListEmitter(elemRepr).emitConvert(input, representation, pos)
            // PairIntDataList → SumUplcConstr: delegate
            case (PairIntDataList, _: SumUplcConstr) =>
                SumUplcConstrEmitter.emitConvert(input, representation, pos)
            // All SumBuiltinList source conversions delegate to list-shape impl
            case (SumBuiltinList(inElemRepr), _) =>
                new SumBuiltinListEmitter(inElemRepr).emitConvert(input, representation, pos)
            case (PackedSumDataList, PackedSumDataList) =>
                input
            // All other PackedSumDataList source conversions delegate to list-shape impl
            case (PackedSumDataList, _) =>
                val elemType =
                    SumBuiltinList.retrieveListElementType(input.sirType).getOrElse(SIRType.Data.tp)
                val elemRepr = typegens.SirTypeUplcGenerator.defaultDataRepresentation(elemType)
                new SumBuiltinListEmitter(elemRepr).emitConvert(input, representation, pos)
            // All SumUplcConstr source/target conversions delegate to SumUplcConstrEmitter
            case (_: SumUplcConstr, _) =>
                SumUplcConstrEmitter.emitConvert(input, representation, pos)
            case (_, _: SumUplcConstr) =>
                SumUplcConstrEmitter.emitConvert(input, representation, pos)
            case (_: ProductCaseClassRepresentation.ProdUplcConstr, _) =>
                SumUplcConstrEmitter.emitConvert(input, representation, pos)
            case (inTvr: TypeVarRepresentation, _) =>
                // Phase 5: source TypeVar dispatch via shared helper. Replaces hardcoded
                // `DataConstr` with `SirTypeUplcGenerator.default*(input.sirType)`. For
                // DataConstrEmitter-handled types these match `DataConstr`; for
                // SumCaseUplcOnly-handled types the helper resolves to `SumUplcConstr`
                // (defaultRepresentation) or throws (defaultTypeVarReperesentation), which is
                // strictly more correct than mislabelling UC bytes as Data.
                typegens.TypeVarEmitter.bridgeFromKind(input, inTvr, representation, pos)
            case (_, outTvr: TypeVarRepresentation) =>
                // Phase 5 canonicalization: Unwrapped/Fixed go through bridgeToKind, which
                // returns a value whose repr reflects the actual byte shape (no TypeVar relabel
                // on the result) — see design doc §3.6. Transparent stays open-coded as `input`.
                if outTvr.kind == SIRType.TypeVarKind.Transparent then input
                else typegens.TypeVarEmitter.bridgeToKind(input, outTvr, pos)
            case (_, _) =>
                throw LoweringException(
                  s"Unsupported conversion for ${input.sirType.show} from ${input.representation} to $representation",
                  pos
                )
        }
    }

    /** Choose the typegen that should emit this `Constr`, considering both the static type and
      * surrounding-arg/context cues. Centralizes the four rules formerly in
      * `ConstrDispatcher.shouldDelegateToUplcConstr`:
      *
      *   1. Parent sum's default representation is `SumUplcConstr`.
      *   2. `List.Cons` whose tail's actual representation is `SumUplcConstr`.
      *   3. Caller's `optTargetType` is `@UplcRepr(UplcConstr)`-annotated.
      *   4. `lctx.inUplcConstrListScope == true` AND parent sum is List/Option.
      *
      * Returns the typegen to dispatch to; default is the static-type's own typegen.
      */
    def chooseConstrOutputRepr(
        constr: SIR.Constr,
        loweredArgs: scala.List[LoweredValue],
        optTargetType: Option[SIRType]
    )(using lctx: LoweringContext): typegens.SirTypeUplcGenerator = {
        val resolvedType = lctx.resolveTypeVarIfNeeded(constr.tp)

        // 1. Parent's default repr is SumUplcConstr.
        resolvedType match
            case SIRType.CaseClass(_, typeArgs, Some(parent)) =>
                val substitutedParent = parent match
                    case SIRType.TypeLambda(params, body)
                        if typeArgs.nonEmpty && params.length == typeArgs.length =>
                        SIRType.substitute(body, params.zip(typeArgs).toMap, Map.empty)
                    case _ => parent
                val parentGen = typegens.SirTypeUplcGenerator(substitutedParent)
                parentGen.defaultRepresentation(substitutedParent) match
                    case _: SumUplcConstr => return parentGen
                    case _                =>
            case _ =>

        // 2. List.Cons + tail has SumUplcConstr repr.
        resolvedType match
            case SIRType.CaseClass(cd, _, Some(_))
                if cd.name == SIRType.List.Cons.name && loweredArgs.length >= 2 =>
                loweredArgs.last.representation match
                    case _: SumUplcConstr => return typegens.SumCaseUplcConstrEmitter
                    case _                =>
            case _ =>

        // 3. Caller's target type is @UplcRepr(UplcConstr) annotated.
        val targetUsesUplcConstr = optTargetType.exists {
            case SIRType.Annotated(inner, anns) =>
                anns.data.contains("uplcRepr") &&
                IntrinsicResolver.isUplcConstrListOrOption(inner)
            case _ => false
        }
        if targetUsesUplcConstr then return typegens.SumCaseUplcConstrEmitter

        // 4. inUplcConstrListScope flag + this constr's parent sum is List/Option.
        val parentIsListOrOption = resolvedType match
            case SIRType.CaseClass(_, _, Some(parent)) =>
                IntrinsicResolver.isUplcConstrListOrOption(parent)
            case _ =>
                IntrinsicResolver.isUplcConstrListOrOption(resolvedType)
        if lctx.inUplcConstrListScope && parentIsListOrOption then
            return typegens.SumCaseUplcConstrEmitter

        typegens.SirTypeUplcGenerator(resolvedType)
    }

    /** Pick the type generator for a `Nil` constructor.
      *
      * `Lowering.lowerSIR` routes `Nil` straight to `typeGenerator.genConstrLowered`, bypassing
      * `chooseConstrOutputRepr`. So this path also has to apply the `inUplcConstrListScope`
      * override (the rule that, pre-Phase-3a, the typegen- internal
      * `ConstrDispatcher.shouldDelegateToUplcConstr` rule #4 supplied); without it, a Nil produced
      * inside an UplcConstr-scoped intrinsic body whose surrounding target type is unannotated
      * `List[A]` would emerge as `SumBuiltinList`, desyncing from match-branch `SumUplcConstr`
      * reprs.
      *
      * Returns `(generator, effective-constr)`: `effective-constr` rewrites `constr.tp` to
      * `targetType` when one is supplied so the generator sees the right type info for type-correct
      * Nil emission.
      */
    def dispatchNil(
        constr: SIR.Constr,
        resolvedType: SIRType,
        optTargetType: Option[SIRType]
    )(using lctx: LoweringContext): (typegens.SirTypeUplcGenerator, SIR.Constr) = {
        def overrideForListScope(
            tp: SIRType,
            fallback: typegens.SirTypeUplcGenerator
        ): typegens.SirTypeUplcGenerator =
            if fallback == typegens.SumCaseUplcConstrEmitter then fallback
            else if lctx.inUplcConstrListScope && IntrinsicResolver.isUplcConstrListOrOption(tp)
            then typegens.SumCaseUplcConstrEmitter
            else fallback
        optTargetType match
            case Some(targetType) =>
                val gen =
                    overrideForListScope(targetType, typegens.SirTypeUplcGenerator(targetType))
                val effective =
                    if constr.tp eq targetType then constr else constr.copy(tp = targetType)
                (gen, effective)
            case None =>
                val gen =
                    overrideForListScope(resolvedType, typegens.SirTypeUplcGenerator(resolvedType))
                (gen, constr)
    }

    def genConstr(
        constr: SIR.Constr,
        loweredArgs: scala.List[LoweredValue],
        optTargetType: Option[SIRType]
    )(using lctx: LoweringContext): LoweredValue = {
        val gen = chooseConstrOutputRepr(constr, loweredArgs, optTargetType)
        gen.genConstrLowered(constr, loweredArgs, optTargetType)
    }

    /** Representation-aware dispatch for sum-typed `genMatch`. The Case-builtin shape varies by
      * `loweredScrutinee.representation`:
      *
      *   - `SumUplcConstr` → tag-ordered Case via `genMatchUplcConstr` (the scrutinee's UPLC bytes
      *     are already a `Constr(tag, fields)` so branches index by tag directly).
      *   - `DataConstr` → Data-shape Case in `DataConstrEmitter. genMatchDataConstr` (unConstrData
      *     → tag/field-list → if/else chain, or `Case` on integer for V4+).
      *   - `SumBuiltinList(elemRepr)` → caseList-ordered Case via the element-repr-parameterized
      *     `SumBuiltinListEmitter.genMatch`.
      *   - `PackedSumDataList` → unpack to `SumBuiltinList(<elem-default>)`, bind a fresh var,
      *     recurse.
      *   - `TypeVarRepresentation(_)` → relabel to the type's `defaultTypeVarRepresentation` and
      *     recurse.
      *   - everything else → fall back to the type-keyed typegen's `genMatch` (same routing as
      *     before this dispatcher existed).
      *
      * Pre-Phase-4 this match lived inside `DataConstrEmitter.genMatch` (formerly
      * `SumCaseSirTypeGenerator`) and the `SumUplcConstr` short-circuit lived in
      * `Lowering.lowerSIR`; routing them through the dispatcher consolidates the choice.
      */
    def genMatch(
        matchData: SIR.Match,
        loweredScrutinee: LoweredValue,
        optTargetType: Option[SIRType]
    )(using lctx: LoweringContext): LoweredValue = {
        import SumCaseClassRepresentation.*
        loweredScrutinee.representation match
            case _: SumUplcConstr =>
                typegens.SumUplcConstrEmitter
                    .genMatchUplcConstr(matchData, loweredScrutinee, optTargetType)
            case DataConstr =>
                typegens.DataConstrEmitter
                    .genMatchDataConstr(matchData, loweredScrutinee, optTargetType)
            case SumBuiltinList(elemRepr) =>
                new typegens.SumBuiltinListEmitter(elemRepr)
                    .genMatch(matchData, loweredScrutinee, optTargetType)
            case PackedSumDataList =>
                val elemType = SumBuiltinList
                    .retrieveListElementType(loweredScrutinee.sirType)
                    .getOrElse(SIRType.Data.tp)
                val elemRepr = typegens.SirTypeUplcGenerator.defaultDataRepresentation(elemType)
                val listRepr = SumBuiltinList(elemRepr)
                val unpacked = loweredScrutinee.toRepresentation(listRepr, matchData.anns.pos)
                val unpackedVar = lvNewLazyIdVar(
                  lctx.uniqueVarName("_unpacked"),
                  loweredScrutinee.sirType,
                  listRepr,
                  unpacked,
                  matchData.anns.pos
                )
                new typegens.SumBuiltinListEmitter(elemRepr)
                    .genMatch(matchData, unpackedVar, optTargetType)
            case TypeVarRepresentation(_) =>
                val properRepresentation =
                    typegens.SirTypeUplcGenerator
                        .defaultTypeVarReperesentation(loweredScrutinee.sirType)
                val scrutineeWithProperRepr = TypeRepresentationProxyLoweredValue(
                  loweredScrutinee,
                  loweredScrutinee.sirType,
                  properRepresentation,
                  matchData.anns.pos
                )
                genMatch(matchData, scrutineeWithProperRepr, optTargetType)
            case _ =>
                typegens.SirTypeUplcGenerator
                    .genMatch(matchData, loweredScrutinee, optTargetType)
    }

    /** Choose-and-align step for `SumUplcConstr` matches when any branch carries a
      * `(Sum|Prod)UplcConstr` whose field reprs include a Transparent TypeVar. Returns
      * `Some((sumRepr, aligned))` when the override fires, `None` otherwise (caller should fall
      * back to its own convergence — typically `LoweredValue.chooseCommonRepresentation`).
      *
      * The override fires because Transparent TypeVar fields cannot be folded to a Data-shaped repr
      * — we must keep a `SumUplcConstr` shape and align branches structurally. Alignment:
      * `(Sum|Prod)UplcConstr` branches and `ErrorRepresentation` stay as-is; others route through
      * `toRepresentation(sumRepr, pos)` for Data→UplcConstr conversion.
      *
      * The synthesized `SumUplcConstr` uses `buildSumUplcConstr`'s default variants for tags
      * without a matching branch puc; hand-rolled `defaultRepresentation` lookups would produce
      * `TypeVarRepresentation(Fixed)` (because the DataDecl's `A` carries `Fixed` kind), which then
      * leaks into downstream inference and surfaces as `Fixed → Unwrapped` aborts.
      */
    def transparentSumUplcConstrAlignment(
        branches: Seq[LoweredValue],
        resultType: SIRType,
        pos: SIRPosition
    )(using lctx: LoweringContext): Option[(SumUplcConstr, Seq[LoweredValue])] = {
        def hasTransparentFields(repr: LoweredValueRepresentation): Boolean = repr match
            case puc: ProductCaseClassRepresentation.ProdUplcConstr =>
                puc.fieldReprs.exists {
                    case tvr: TypeVarRepresentation => tvr.isBuiltin
                    case _                          => false
                }
            case suc: SumUplcConstr =>
                suc.variants.values.exists(puc => hasTransparentFields(puc))
            case _ => false
        if !branches.exists(b => hasTransparentFields(b.representation)) then None
        else
            val branchPucByTag = branches.flatMap { b =>
                b.representation match
                    case puc: ProductCaseClassRepresentation.ProdUplcConstr =>
                        Some(puc.tag -> puc)
                    case _ => None
            }.toMap
            val defaultSumRepr =
                typegens.SumUplcConstrEmitter.buildSumUplcConstr(resultType, pos)
            val variants = defaultSumRepr.variants.map { case (idx, defaultPuc) =>
                branchPucByTag.get(idx) match
                    case Some(puc) => (idx, puc)
                    case None      => (idx, defaultPuc)
            }
            val sumRepr = SumUplcConstr(variants)
            val aligned = branches.map { branch =>
                branch.representation match
                    case _: ProductCaseClassRepresentation.ProdUplcConstr |
                        _: SumCaseClassRepresentation.SumUplcConstr =>
                        branch
                    case ErrorRepresentation => branch
                    case _                   => branch.toRepresentation(sumRepr, pos)
            }
            Some((sumRepr, aligned))
    }

    /** The scrutinee-repr-driven half of a sum match. A `MatchScaffolding` captures the
      * Case-builtin shape and any pre-bound state derived from the scrutinee (e.g. an unpacked
      * field-list var, a tag binding) — but it does NOT decide the result repr. `assembleMatch`
      * calls `assemble` once it has the aligned branches and the chosen target repr.
      *
      * One scaffolding implementation per scrutinee repr (`Term.Case` for UplcConstr,
      * `CaseListLoweredValue` for SumBuiltinList, …). The trait stays small so future per-repr
      * emitters can implement it without pulling in the assembly machinery.
      */
    trait MatchScaffolding {
        def assemble(
            branches: Seq[LoweredValue],
            targetRepr: LoweredValueRepresentation,
            resultType: SIRType,
            pos: SIRPosition
        )(using LoweringContext): LoweredValue
    }

    /** Branch-driven assembly. Universal across scrutinee reprs: upcasts each branch to the result
      * type, runs the Phase-3b `transparentSumUplcConstrAlignment` override (or
      * `chooseCommonRepresentation` as fallback) to pick the result repr, and asks the scaffolding
      * to assemble the final value.
      */
    def assembleMatch(
        scaffolding: MatchScaffolding,
        branches: Seq[LoweredValue],
        optTargetType: Option[SIRType],
        matchTp: SIRType,
        pos: SIRPosition
    )(using lctx: LoweringContext): LoweredValue = {
        val resultType = optTargetType.getOrElse(matchTp)
        val branchesUpcasted = branches.map(_.maybeUpcast(resultType, pos))
        val (resultRepr, aligned) =
            transparentSumUplcConstrAlignment(branchesUpcasted, resultType, pos)
                .getOrElse {
                    val repr = LoweredValue.chooseCommonRepresentation(
                      branchesUpcasted,
                      resultType,
                      pos
                    )
                    (repr, branchesUpcasted.map(_.toRepresentation(repr, pos)))
                }
        scaffolding.assemble(aligned, resultRepr, resultType, pos)
    }

    def genSelect(
        sel: SIR.Select,
        loweredScrutinee: LoweredValue
    )(using lctx: LoweringContext): LoweredValue =
        typegens.SirTypeUplcGenerator.genSelect(sel, loweredScrutinee)

    /** Pick the parent-sum repr that an `upcastOne` from `input` to `targetType` should produce —
      * the "preserve concrete vs coerce to default" decision:
      *
      *   - `ProdUplcConstr` input → overlay the variant onto `buildSumUplcConstr(targetType)` so
      *     concrete field reprs (e.g. `Unwrapped` element types that differ from the DataDecl's
      *     abstract defaults) survive the upcast. Avoids a Data round-trip that fails for abstract
      *     TypeVar fields in isolation. Fires unconditionally; don't gate on target's default repr.
      *   - `SumUplcConstr` input → keep the refined variant set as-is.
      *   - Same-shape sum input (`SumBuiltinList`, `SumPairBuiltinList`, `PackedSumDataList`,
      *     `SumDataAssocMap`) → keep input's parameterization (preserves element repr).
      *   - Otherwise → target's default repr.
      *
      * Returns the parent-sum repr only. Each emitter still chooses the matching byte-level
      * conversion (`ProdDataList → SumBuiltinList`, `ProdDataConstr → DataConstr`, etc.) before
      * relabeling.
      */
    def chooseUpcastOutputRepr(
        input: LoweredValue,
        targetType: SIRType,
        pos: SIRPosition
    )(using lctx: LoweringContext): SumCaseClassRepresentation = {
        val targetDefault = typegens.SirTypeUplcGenerator.defaultRepresentation(targetType)
        val targetSum = targetDefault match
            case s: SumCaseClassRepresentation => s
            case other =>
                throw LoweringException(
                  s"chooseUpcastOutputRepr: target ${targetType.show} default repr is not a SumCaseClassRepresentation: $other",
                  pos
                )
        (input.representation, targetSum) match
            case (puc: ProductCaseClassRepresentation.ProdUplcConstr, _) =>
                val baseSum =
                    typegens.SumUplcConstrEmitter.buildSumUplcConstr(targetType, pos)
                SumUplcConstr(baseSum.variants.updated(puc.tag, puc))
            case (suc: SumUplcConstr, _: SumUplcConstr)           => suc
            case (suc: SumBuiltinList, _: SumBuiltinList)         => suc
            case (suc: SumPairBuiltinList, _: SumPairBuiltinList) => suc
            case (PackedSumDataList, PackedSumDataList)           => PackedSumDataList
            case (SumDataAssocMap, SumDataAssocMap)               => SumDataAssocMap
            case _                                                => targetSum
    }

    def upcastOne(
        input: LoweredValue,
        targetType: SIRType,
        pos: SIRPosition
    )(using lctx: LoweringContext): LoweredValue =
        typegens.SirTypeUplcGenerator.upcastOne(input, targetType, pos)

}
