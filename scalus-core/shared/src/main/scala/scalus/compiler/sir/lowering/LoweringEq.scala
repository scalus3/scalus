package scalus.compiler.sir.lowering

import scalus.compiler.sir.*
import scalus.compiler.sir.lowering.LoweredValue.Builder.*

/** Equality lowering — recognizes `equalsRepr(a, b)` (and the FunctionalInterface Eq path) and
  * emits the most efficient comparison for the lhs/rhs representation pair.
  *
  * Dispatch order in [[generateEqualsForRepr]]:
  *   - Primitives (Integer, ByteString, String): specialized builtins
  *   - BLS curve elements: bls12_381_G1/G2_equal
  *   - BLS MlResult, Fun: compile error (no equality)
  *   - `@UplcRepr(UplcConstr)` products: field-by-field via [[generateProdUplcConstrEquals]]
  *   - `@UplcRepr(UplcConstr)` sums: native `letrec` via [[generateSumUplcConstrEquals]]
  *   - Everything else: convert to Data, equalsData
  */
object LoweringEq {

    val EqualsReprName: String = "scalus.compiler.intrinsics.IntrinsicHelpers$.equalsRepr"
    val EqTypeName: String = "scalus.cardano.onchain.plutus.prelude.Eq"

    /** Per-process set of source positions where we've already warned about an unrecognized Eq. The
      * same `===` site can be inlined into many callers (e.g. `Eq.scala:13` is the body of the
      * `===` extension method, re-broadcast at every use of `===` in the program). De-duping by
      * call-site position keeps the warning informative without flooding the compile log.
      */
    private val warnedNonDerivedPositions =
        java.util.concurrent.ConcurrentHashMap.newKeySet[String]()

    /** Peel off any number of `SIRType.Annotated` wrappers to expose the underlying type. */
    private def stripAnnotated(t: SIRType): SIRType = t match
        case SIRType.Annotated(inner, _) => stripAnnotated(inner)
        case other                       => other

    /** Check if this is `equalsRepr(a, b)` — the outer Apply of curried application. Handles both
      * `Apply(Apply(ExternalVar, a), b)` and `Apply(Apply(TypeApply(ExternalVar, A), a), b)` (when
      * type parameter is present).
      */
    def isEqualsReprApp(app: SIR.Apply): Boolean = {
        def isEqualsReprRef(sir: SIR): Boolean = sir match
            case SIR.ExternalVar(_, name, _, _) => name == EqualsReprName
            case SIR.Var(name, _, _)            => name == EqualsReprName
            case _                              => false
        app.f match
            case SIR.Apply(f2, _, _, _) =>
                isEqualsReprRef(f2) || (f2 match
                    case SIR.Apply(f3, _, _, _) => isEqualsReprRef(f3)
                    case _                      => false)
            case _ => false
    }

    /** Lower `equalsRepr(a, b)` — picks the optimal comparison for the resolved types/reprs. */
    def lowerEqualsRepr(
        app: SIR.Apply
    )(using lctx: LoweringContext): LoweredValue = {
        // Curried: Apply(Apply(ExternalVar("equalsRepr"), a), b)
        val innerApp = app.f.asInstanceOf[SIR.Apply]
        val lhs = Lowering.lowerSIR(innerApp.arg)
        val rhs = Lowering.lowerSIR(app.arg)
        generateEqualsForRepr(lhs, rhs, app.anns.pos)
    }

    /** Check if this is a fully-applied FunctionalInterface call with known Eq semantics. */
    def isEqIntrinsicApp(app: SIR.Apply): Boolean =
        !SIRType.isPolyFunOrFun(app.tp) && (app.anns.data.get("functionalInterfaceType") match
            case Some(SIR.Const(scalus.uplc.Constant.String(name), _, _)) =>
                name == EqTypeName
            case _ => false)

    /** Lower an Eq application using repr-specific equality.
      *
      * The plugin annotates Apply nodes for FunctionalInterface types with
      * `"functionalInterfaceType"`. For Eq, we always inline an optimal comparison based on the
      * argument representation instead of calling the Eq function. This is sound when the user's Eq
      * instance satisfies the equivalence-relation contract (reflexive / symmetric / transitive AND
      * structurally consistent — i.e. `a === b` iff `a.toData == b.toData`).
      *
      * Custom Eq instances that violate this contract (notably `Eq.keyPairEq[A, B]` which compares
      * only `_1`, ignoring `_2`) are buggy by definition: any code that relies on `===` to mean
      * "structurally equal" — like `List.distinct` — will misbehave. We surface this by emitting a
      * compile-time warning whenever the Eq instance is not provably one of the standard
      * type-default forms (`Eq.derived[T]`, `summon[Eq[T]]`, or a prelude `given Eq[...]`). The
      * warning steers users toward derived/summoned forms; the optimization itself proceeds
      * uniformly in either case to keep the perf characteristics consistent.
      */
    def lowerEqIntrinsic(
        app: SIR.Apply,
        fallback: SIR.Apply => LoweringContext ?=> LoweredValue
    )(using lctx: LoweringContext): LoweredValue = {
        // Curried: Apply(Apply(eq, lhs, ...), rhs, ...)
        val (eqRef, lhsSir, rhsSir) = app.f match
            case innerApp: SIR.Apply => (innerApp.f, innerApp.arg, app.arg)
            case _                   =>
                // Not curried — fall back to normal apply
                return fallback(app)
        if !isTypeDefaultEq(eqRef) then
            val posKey = app.anns.pos.show
            if warnedNonDerivedPositions.add(posKey) then
                lctx.warn(
                  "Eq instance is not provably the structural type-default " +
                      "(e.g. `Eq.derived[T]`, `summon[Eq[T]]`, or a `given Eq[...]` from the prelude). " +
                      "The repr-aware Eq optimization will inline structural equality regardless. " +
                      "Custom Eq instances MUST satisfy the equivalence-relation contract " +
                      "(reflexive / symmetric / transitive AND structurally consistent) — instances like " +
                      "`Eq.keyPairEq` that compare only one component violate this and produce wrong " +
                      "runtime results. Prefer `Eq.derived[T]` to make the type-default explicit.",
                  app.anns.pos
                )
        val lhs = Lowering.lowerSIR(lhsSir)
        val rhs = Lowering.lowerSIR(rhsSir)
        generateEqualsForRepr(lhs, rhs, app.anns.pos)
    }

    /** True when `eqRef` (the function position in `eq(lhs, rhs)`) is recognizably a type-default
      * Eq — one of:
      *   - `given Eq[...]` declarations from the prelude (synthetic name `*.given_Eq_*`)
      *   - `Eq.apply` (= `summon[Eq[T]]`)
      *   - `Eq.derived[T]`
      *
      * For unrecognized shapes (most importantly user-defined `def` / `val` like `Eq.keyPairEq`) we
      * cannot prove the instance matches the type-default and the caller falls back to actually
      * calling the Eq value.
      */
    private def isTypeDefaultEq(eqRef: SIR): Boolean = eqRef match
        case SIR.ExternalVar(_, name, _, _) =>
            name.contains(".given_Eq_") ||
            name.endsWith("$Eq$.apply") ||
            name.endsWith("$Eq$.derived")
        case SIR.Var(name, _, _) =>
            name.contains(".given_Eq_") ||
            name.endsWith("$Eq$.apply") ||
            name.endsWith("$Eq$.derived")
        case SIR.Apply(f, _, _, _) =>
            // `Eq[T]` (= `summon[Eq[T]]`) lowers to `Apply(<Eq.apply ref>, <implicit>)` — recurse.
            isTypeDefaultEq(f)
        case _ => false

    /** Generate repr-specific equality comparison.
      *
      * Dispatches based on resolved type and representation; see [[LoweringEq]] doc for the
      * dispatch order.
      */
    def generateEqualsForRepr(
        lhs: LoweredValue,
        rhs: LoweredValue,
        pos: SIRPosition
    )(using lctx: LoweringContext): LoweredValue = {
        // Resolve TypeVar to concrete type for dispatch
        val resolvedType = lhs.sirType match
            case tv: SIRType.TypeVar =>
                lctx.tryResolveTypeVar(tv).getOrElse(tv)
            case other => other
        resolvedType match
            case SIRType.Integer =>
                generatePrimitiveEquals(SIRBuiltins.equalsInteger, lhs, rhs, pos)
            case SIRType.ByteString =>
                generatePrimitiveEquals(SIRBuiltins.equalsByteString, lhs, rhs, pos)
            case SIRType.String =>
                generatePrimitiveEquals(SIRBuiltins.equalsString, lhs, rhs, pos)
            case SIRType.BLS12_381_G1_Element =>
                generatePrimitiveEquals(SIRBuiltins.bls12_381_G1_equal, lhs, rhs, pos)
            case SIRType.BLS12_381_G2_Element =>
                generatePrimitiveEquals(SIRBuiltins.bls12_381_G2_equal, lhs, rhs, pos)
            case SIRType.BLS12_381_MlResult =>
                throw LoweringException(
                  "Equality is not defined for BLS12_381_MlResult (use finalVerify if intended)",
                  pos
                )
            case SIRType.Fun(_, _) =>
                throw LoweringException(
                  "Equality is not defined for function types",
                  pos
                )
            case SIRType.Annotated(innerType, anns) =>
                anns.data.get("uplcRepr") match
                    case Some(reprSir) =>
                        val repr = typegens.SirTypeUplcGenerator
                            .resolveReprAnnotation(reprSir, innerType)
                        repr match
                            case _: ProductCaseClassRepresentation.ProdUplcConstr =>
                                generateProdUplcConstrEquals(lhs, rhs, innerType, pos)
                            case _: SumCaseClassRepresentation.SumUplcConstr =>
                                generateSumUplcConstrEquals(lhs, rhs, innerType, pos)
                            case _ =>
                                generateDataEquals(lhs, rhs, pos)
                    case None =>
                        generateDataEquals(lhs, rhs, pos)
            case _ =>
                lhs.representation match
                    case _: ProductCaseClassRepresentation.ProdUplcConstr =>
                        generateProdUplcConstrEquals(lhs, rhs, lhs.sirType, pos)
                    case _: SumCaseClassRepresentation.SumUplcConstr =>
                        generateSumUplcConstrEquals(lhs, rhs, lhs.sirType, pos)
                    case _ =>
                        generateDataEquals(lhs, rhs, pos)
    }

    /** Native equality for SumUplcConstr-represented values.
      *
      * Emits `letrec eqSum(a, b) = case a of ...` where each variant branch matches `b` against the
      * same variant tag (returning False on mismatch) and ANDs field comparisons. For fields whose
      * substituted type matches the outer sum type, the recursive `eqSum` reference is used instead
      * of recursing into a fresh `generateEqualsForRepr` (which would emit a duplicate letrec for
      * self-referential types like List).
      *
      * For non-recursive sums (e.g., Option) the `letrec` is harmless overhead — the function never
      * references itself.
      */
    def generateSumUplcConstrEquals(
        lhs: LoweredValue,
        rhs: LoweredValue,
        knownType: SIRType,
        pos: SIRPosition
    )(using lctx: LoweringContext): LoweredValue = {
        // Resolve to a SumUplcConstr representation; coerce lhs if needed.
        val lhsSum: SumCaseClassRepresentation.SumUplcConstr = lhs.representation match
            case s: SumCaseClassRepresentation.SumUplcConstr => s
            case _ =>
                val gen = typegens.SumCaseUplcConstrSirTypeGenerator
                val tgt = gen.defaultRepresentation(knownType) match
                    case s: SumCaseClassRepresentation.SumUplcConstr => s
                    case _ =>
                        return generateDataEquals(lhs, rhs, pos)
                val lhsConv = lhs.toRepresentation(tgt, pos)
                return generateSumUplcConstrEquals(lhsConv, rhs, knownType, pos)
        // Unwrap Annotated to get the underlying SumCaseClass for type-equality checks.
        val baseType = stripAnnotated(knownType)
        baseType match
            case SIRType.SumCaseClass(_, _) => ()
            case _                          => return generateDataEquals(lhs, rhs, pos)
        val rhsConv = rhs.toRepresentation(lhsSum, pos)
        // Cache key must include the concrete `lhsSum` representation: two call sites with
        // the same type but different SumUplcConstr field reprs produce structurally-different
        // helper bodies (field comparisons walk the baked-in field reprs). A type-only key
        // would hand caller B caller A's helper → runtime repr mismatch.
        //
        // Use `stableKey` (not `show`/`toString`) so structurally-equal reprs produce equal
        // keys — avoids over-specializing on `SumReprProxy` identity.
        //
        // Plus `captureFingerprint`: the helper RHS dispatches on field reprs which may consult
        // `lctx.typeVarReprEnv` and the unify env for parametric fields. Without this, the
        // first caller's env wins and later callers under different env state share a helper
        // whose RHS doesn't match — see sessions 11-15 alone-vs-combined Heisenbug.
        val typeKey = sumEqKey(baseType) + "|" + lhsSum.stableKey +
            "|" + lctx.captureFingerprint(baseType)
        val funType = SIRType.Fun(baseType, SIRType.Fun(baseType, SIRType.Boolean))
        val innerFunType = SIRType.Fun(baseType, SIRType.Boolean)
        val innerFunRepr = LambdaRepresentation(
          innerFunType,
          InOutRepresentationPair(lhsSum, PrimitiveRepresentation.Constant)
        )
        val funRepr = LambdaRepresentation(
          funType,
          InOutRepresentationPair(lhsSum, innerFunRepr)
        )
        // Reuse cached helper across emission sites; emit and register on first encounter.
        val eqFnVar = lctx.lookupCachedHelper(typeKey) match
            case Some(v) =>
                LoweringContext.traceLetRec("HIT", "sumEq", typeKey)
                v
            case None =>
                createSumEqHelper(
                  typeKey,
                  baseType,
                  lhsSum,
                  funType,
                  funRepr,
                  innerFunType,
                  innerFunRepr,
                  pos
                )
        lvApplyDirect(
          lvApplyDirect(eqFnVar, lhs, innerFunType, innerFunRepr, pos),
          rhsConv,
          SIRType.Boolean,
          PrimitiveRepresentation.Constant,
          pos
        )
    }

    /** Stable string key for caching `sumEq` helpers. Uses `decl.name` and a structural rendering
      * of the type arguments — sufficient because two equal-keyed types produce identical helper
      * code. Relies on `SIRType.show` as the stable fallback for primitives; if `show` formatting
      * changes, cache lookup behavior changes (acceptable for a per-compile cache).
      */
    private def sumEqKey(t: SIRType): String = t match
        case SIRType.SumCaseClass(decl, args) =>
            decl.name + (if args.isEmpty then "" else args.map(sumEqKey).mkString("[", ",", "]"))
        case SIRType.CaseClass(cd, args, _) =>
            cd.name + (if args.isEmpty then "" else args.map(sumEqKey).mkString("[", ",", "]"))
        case SIRType.Annotated(inner, _) => sumEqKey(inner)
        case other                       => other.show

    /** Build the recursive helper function `eqSum: (T, T) => Boolean` for a given sum type. The
      * helper does outer Case dispatch on `a`, inner Case dispatch on `b`, returns false on tag
      * mismatch, and ANDs field comparisons otherwise. Self-recursive fields (whose type matches
      * the outer sum) reuse the recursive `eqFn` reference; other fields recurse into
      * [[generateEqualsForRepr]] (which may itself emit further cached helpers).
      *
      * The result is registered in [[LoweringContext.pendingTopLevelLetRecs]]; the lowering driver
      * is expected to wrap the lowered SIR root with the corresponding let-recs.
      */
    private def createSumEqHelper(
        typeKey: String,
        baseType: SIRType,
        lhsSum: SumCaseClassRepresentation.SumUplcConstr,
        funType: SIRType,
        funRepr: LoweredValueRepresentation,
        innerFunType: SIRType,
        innerFunRepr: LoweredValueRepresentation,
        pos: SIRPosition
    )(using lctx: LoweringContext): IdentifiableLoweredValue = {
        // Predicate: does this field type match the outer sum type? If yes, use eqFn.
        def isOuterSumType(t: SIRType): Boolean = {
            val st = stripAnnotated(t) match
                case SIRType.TypeProxy(ref) if ref != null => ref.asInstanceOf[SIRType]
                case other                                 => other
            (st, baseType) match
                case (SIRType.SumCaseClass(d1, ta1), SIRType.SumCaseClass(d2, ta2)) =>
                    d1.name == d2.name && ta1.length == ta2.length
                case _ => false
        }
        // Allocate the recursive var up front so we can register it in the cache before
        // computing the rhs — supports forward references during recursion. `uniqueVarName`
        // already disambiguates; the key is mixed in for easier debugging (stripped of
        // non-identifier characters).
        val eqFnId = lctx.uniqueVarName(
          "$sumEq_" + typeKey.filter(c => c.isLetterOrDigit || c == '_' || c == '.')
        )
        val eqFnVar = new VariableLoweredValue(
          id = eqFnId,
          name = eqFnId,
          sir = SIR.Var(eqFnId, funType, AnnotationsDecl(pos)),
          representation = funRepr
        )
        // Register early so recursive references from inside the rhs reuse this same var.
        lctx.cachedTopLevelHelpers(typeKey) = eqFnVar
        val eqFnRhs = lvLamAbs(
          "a",
          baseType,
          lhsSum,
          a =>
              lvLamAbs(
                "b",
                baseType,
                lhsSum,
                b =>
                    typegens.SumUplcConstrSirTypeGenerator.genMatchUplcConstrAllVariants(
                      a,
                      lhsSum,
                      baseType,
                      SIRType.Boolean,
                      PrimitiveRepresentation.Constant,
                      pos,
                      (aTag, aFields) =>
                          typegens.SumUplcConstrSirTypeGenerator
                              .genMatchUplcConstrAllVariants(
                                b,
                                lhsSum,
                                baseType,
                                SIRType.Boolean,
                                PrimitiveRepresentation.Constant,
                                pos,
                                (bTag, bFields) =>
                                    if aTag != bTag then lvBoolConstant(false, pos)
                                    else if aFields.isEmpty then lvBoolConstant(true, pos)
                                    else
                                        val fieldComparisons =
                                            aFields.zip(bFields).map { case (af, bf) =>
                                                if isOuterSumType(af.sirType) then
                                                    // Self-recursion: use the cached eqFnVar
                                                    lvApplyDirect(
                                                      lvApplyDirect(
                                                        eqFnVar,
                                                        af,
                                                        innerFunType,
                                                        innerFunRepr,
                                                        pos
                                                      ),
                                                      bf,
                                                      SIRType.Boolean,
                                                      PrimitiveRepresentation.Constant,
                                                      pos
                                                    )
                                                else generateEqualsForRepr(af, bf, pos)
                                            }
                                        fieldComparisons.reduceLeft { (acc, next) =>
                                            lvIfThenElse(
                                              acc.toRepresentation(
                                                PrimitiveRepresentation.Constant,
                                                pos
                                              ),
                                              next,
                                              lvBoolConstant(false, pos),
                                              pos
                                            )
                                        }
                              )
                    ),
                pos
              ),
          pos
        )
        lctx.pendingTopLevelLetRecs += ((eqFnVar, eqFnRhs))
        LoweringContext.traceLetRec("ADD", "sumEq", typeKey)
        eqFnVar
    }

    def generatePrimitiveEquals(
        builtin: SIR.Builtin,
        lhs: LoweredValue,
        rhs: LoweredValue,
        pos: SIRPosition
    )(using LoweringContext): LoweredValue = {
        val xc = lhs.toRepresentation(PrimitiveRepresentation.Constant, pos)
        val yc = rhs.toRepresentation(PrimitiveRepresentation.Constant, pos)
        lvBuiltinApply2(builtin, xc, yc, SIRType.Boolean, PrimitiveRepresentation.Constant, pos)
    }

    /** Generate equalsData after converting both values to their Data representation. */
    def generateDataEquals(
        lhs: LoweredValue,
        rhs: LoweredValue,
        pos: SIRPosition
    )(using lctx: LoweringContext): LoweredValue = {
        val lhsGen = lctx.typeGenerator(lhs.sirType)
        val rhsGen = lctx.typeGenerator(rhs.sirType)
        val lhsDataRepr = lhsGen.defaultDataRepresentation(lhs.sirType)
        val rhsDataRepr = rhsGen.defaultDataRepresentation(rhs.sirType)
        val lhsData = lhs.toRepresentation(lhsDataRepr, pos)
        val rhsData = rhs.toRepresentation(rhsDataRepr, pos)
        lvBuiltinApply2(
          SIRBuiltins.equalsData,
          lhsData,
          rhsData,
          SIRType.Boolean,
          PrimitiveRepresentation.Constant,
          pos
        )
    }

    /** Generate Case-based field comparison for ProdUplcConstr values. Each field is extracted via
      * genSelect and compared recursively.
      *
      * @param knownType
      *   concrete type for field extraction (may differ from lhs.sirType when lhs has TypeVar type
      *   but the concrete type is known from annotation)
      */
    def generateProdUplcConstrEquals(
        lhs: LoweredValue,
        rhs: LoweredValue,
        knownType: SIRType,
        pos: SIRPosition
    )(using lctx: LoweringContext): LoweredValue = {
        val constrDecl = typegens.ProductCaseSirTypeGenerator.retrieveConstrDecl(knownType, pos)
        val fields = constrDecl.params
        if fields.isEmpty then
            // No fields: always equal (same type, same tag)
            lvBoolConstant(true, pos)
        else
            val gen = lctx.typeGenerator(knownType)
            val fieldComparisons = fields.map { param =>
                val sel = SIR.Select(
                  SIR.Var("_eq_lhs", knownType, AnnotationsDecl(pos)),
                  param.name,
                  lctx.resolveTypeVarIfNeeded(param.tp),
                  AnnotationsDecl(pos)
                )
                val lhsField = gen.genSelect(sel, lhs)
                val rhsField = gen.genSelect(sel, rhs)
                generateEqualsForRepr(lhsField, rhsField, pos)
            }
            // AND all field comparisons: f1 && f2 && ... && fN
            fieldComparisons.reduceLeft { (acc, next) =>
                lvIfThenElse(
                  acc.toRepresentation(PrimitiveRepresentation.Constant, pos),
                  next,
                  lvBoolConstant(false, pos),
                  pos
                )
            }
    }
}
