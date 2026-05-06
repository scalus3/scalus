package scalus.compiler.sir.lowering

import org.typelevel.paiges.Doc
import scalus.compiler.sir.*
import scalus.uplc.Term

/** Resolves intrinsic implementations for method calls based on argument representation and
  * protocol version.
  *
  * When the lowering encounters a method call like `list.isEmpty`, the resolver checks if there is
  * an optimized intrinsic implementation for the argument's runtime representation (e.g.,
  * `SumDataList`). If found, it substitutes the provider's body (with the actual argument SIR) and
  * re-lowers it. Already-lowered arguments are tracked via the annotation-keyed `argCache`
  * mechanism (see `LoweringContext.argCache`), so re-lowering the substituted SIR returns the
  * pre-lowered value without recomputation.
  *
  * The registry is hardcoded — adding new intrinsic providers requires editing the registry and
  * creating the `@Compile` provider object.
  */
object IntrinsicResolver {

    /** Annotation key on `SIR.Var` to signal that the var should be resolved via
      * `lctx.argCache(idx)` instead of normal scope lookup. The annotation value is
      * `SIR.Const(scalus.uplc.Constant.Integer(idx))`. Used by `substituteSelf` to substitute
      * lambda parameters with annotated Var references, so subsequent substitution-walks (which
      * create fresh SIR.Var instances) preserve the cache key in the new annotations.
      */
    val ArgCacheAnnotKey: String = "argCache"

    // Module name constants (must match plugin's td.symbol.fullName for @Compile objects)
    private val ListModule = "scalus.cardano.onchain.plutus.prelude.List$"
    private val PairListModule = "scalus.cardano.onchain.plutus.prelude.PairList$"
    private val SortedMapModule = "scalus.cardano.onchain.plutus.prelude.SortedMap$"
    private val AssocMapModule = "scalus.cardano.onchain.plutus.prelude.AssocMap$"
    private val OptionModule = "scalus.cardano.onchain.plutus.prelude.Option$"

    private val ListOps = "scalus.compiler.intrinsics.BuiltinListOperations$"
    private val ListOpsV11 = "scalus.compiler.intrinsics.BuiltinListOperationsV11$"
    private val NativeListOps = "scalus.compiler.intrinsics.IntrinsicsNativeList$"
    private val UplcConstrListOps = "scalus.compiler.intrinsics.IntrinsicsUplcConstrList$"

    private val PairListOps = "scalus.compiler.intrinsics.BuiltinPairListOperations$"
    private val PairListOpsV11 = "scalus.compiler.intrinsics.BuiltinPairListOperationsV11$"

    private val SortedMapIntrinsicsModule = "scalus.compiler.intrinsics.SortedMapIntrinsics$"
    private val AssocMapIntrinsicsModule = "scalus.compiler.intrinsics.AssocMapIntrinsics$"

    private val UplcConstrOptionOps = "scalus.compiler.intrinsics.IntrinsicsUplcConstrOption$"

    /** Default intrinsic modules, loaded at compile time by the plugin. The plugin intercepts
      * `compiledModules(...)` and replaces it with `SIRLinker.readModules(...)` that accesses the
      * objects' `sirModule` vals.
      */
    def defaultIntrinsicModules: Map[String, Module] = {
        val modules = scalus.compiler.compiledModules(
          "scalus.compiler.intrinsics.BuiltinListOperations",
          "scalus.compiler.intrinsics.BuiltinListOperationsV11",
          "scalus.compiler.intrinsics.BuiltinPairListOperations",
          "scalus.compiler.intrinsics.BuiltinPairListOperationsV11",
          "scalus.compiler.intrinsics.SortedMapIntrinsics",
          "scalus.compiler.intrinsics.AssocMapIntrinsics",
          "scalus.compiler.intrinsics.IntrinsicsNativeList",
          "scalus.compiler.intrinsics.IntrinsicsUplcConstrList",
          "scalus.compiler.intrinsics.IntrinsicsUplcConstrOption"
        )
        // Intrinsic modules that need a non-default TypeVar kind carry author-written
        // `@UplcRepr(TypeVar(...))` annotations on their type parameters, so no blanket
        // post-load stamping is needed. HO function arguments (e.g. Eq in contains) are
        // explicitly wrapped with toDefaultTypeVarRepr in the intrinsic body to convert to
        // Fixed (Data) repr.
        //   - IntrinsicsUplcConstrList:    `@UplcRepr(TypeVar(Unwrapped))`   (bytes at concrete default)
        //   - IntrinsicsUplcConstrOption:  `@UplcRepr(TypeVar(Transparent))` (bytes pass through)
        //   - IntrinsicsNativeList:        `@UplcRepr(TypeVar(Transparent))` (bytes pass through)
        // Other intrinsic modules (e.g. BuiltinListOperations, BuiltinPairListOperations,
        // SortedMapIntrinsics, AssocMapIntrinsics) load with default `Fixed` TypeVars — they
        // operate on Data-encoded inputs and don't need passthrough semantics.
        modules
    }

    /** Support modules — bindings resolved on demand when referenced from intrinsic bodies. Unlike
      * intrinsic modules, these are NOT used for provider substitution.
      *
      * Methods that need a non-default TypeVar kind carry author-written `@UplcRepr(TypeVar(...))`
      * annotations on their type parameters, so no blanket post-load stamping is needed.
      * `NativeListOperations.find` is intentionally left unannotated (loads with default `Fixed`) —
      * its `Option.None`/`Option.Some` if-then-else body has a known interaction with Constr
      * emission that the annotation path doesn't yet handle (see its scaladoc for details). For HO
      * methods that take pre-compiled functions (like contains with Eq), the dispatcher wraps the
      * HO function with representation conversion adapters.
      */
    def defaultSupportModules: Map[String, Module] = {
        scalus.compiler.compiledModules(
          "scalus.compiler.intrinsics.NativeListOperations",
          "scalus.compiler.intrinsics.UplcConstrListOperations",
          "scalus.compiler.intrinsics.UplcConstrOptionOperations"
        )
    }

    // Representation name constants for registry lookup
    private val BuiltinListRepr = "BuiltinList"
    private val NativeBuiltinListRepr = "NativeBuiltinList"
    private val UplcConstrListRepr = "UplcConstrList"
    private val UplcConstrOptionRepr = "UplcConstrOption"
    private val PairListRepr = "PairList"
    private val WildcardRepr = "_"

    /** Registry entry: (representation, minProtocolVersion, providerModule, reprRules,
      * argConvertRules)
      */
    private type RegistryEntry = (
        String,
        Int,
        String,
        Map[String, scalus.compiler.intrinsics.ReprRule],
        Map[String, scalus.compiler.intrinsics.ArgReprConvertRule]
    )

    import scalus.compiler.intrinsics.{ListReprRules, MapReprRules, NativeListReprRules, UplcConstrListReprRules, UplcConstrOptionReprRules}

    private val NoArgConvert: Map[String, scalus.compiler.intrinsics.ArgReprConvertRule] = Map.empty

    /** Registry: targetModule -> List of (representation, minProtocolVersion, providerModule,
      * reprRules, argConvertRules). Use `WildcardRepr` for factory methods whose arguments don't
      * match the module type.
      */
    /** Modules whose ExternalVar references should have their type-signature TypeLambda TypeVars
      * rewritten to `Transparent`. These target modules are dispatched by the intrinsic resolver to
      * providers whose TypeVars are already Transparent (see `defaultIntrinsicModules`
      * post-processing) and whose HO arguments (like `eq`, `ord`) are wrapped with
      * `toDefaultTypeVarRepr` inside the provider body. The Scala plugin stamps `Fixed` on all
      * user-code types, so without this rewrite `Fixed` leaks into the SIR tree and downstream
      * representation inference treats native lists as Data-encoded, causing runtime mismatches
      * (e.g., `unConstrData` applied to a native `Constr`).
      */
    def isIntrinsicDispatchedModule(moduleName: String): Boolean =
        moduleName == ListModule || moduleName == PairListModule ||
            moduleName == SortedMapModule || moduleName == AssocMapModule

    private val registry: Map[String, List[RegistryEntry]] = Map(
      ListModule -> List(
        (UplcConstrListRepr, 0, UplcConstrListOps, UplcConstrListReprRules.rules, NoArgConvert),
        (NativeBuiltinListRepr, 0, NativeListOps, NativeListReprRules.rules, NoArgConvert),
        (BuiltinListRepr, 0, ListOps, ListReprRules.listRules, NoArgConvert),
        (BuiltinListRepr, 11, ListOpsV11, ListReprRules.listRules, NoArgConvert)
      ),
      OptionModule -> List(
        // UplcConstrListRepr == "SumUplcConstr is the source repr name" — shared by any
        // SumUplcConstr (not List-specific). Option-specific dispatch works because the
        // registry key (moduleName=Option$) scopes the lookup.
        (UplcConstrListRepr, 0, UplcConstrOptionOps, UplcConstrOptionReprRules.rules, NoArgConvert)
      ),
      PairListModule -> List(
        (PairListRepr, 0, PairListOps, ListReprRules.pairListRules, NoArgConvert),
        (PairListRepr, 11, PairListOpsV11, ListReprRules.pairListRules, NoArgConvert)
      ),
      SortedMapModule -> List(
        (
          WildcardRepr,
          0,
          SortedMapIntrinsicsModule,
          MapReprRules.factoryRules,
          MapReprRules.factoryArgConvertRules
        )
      ),
      AssocMapModule -> List(
        (
          WildcardRepr,
          0,
          AssocMapIntrinsicsModule,
          MapReprRules.factoryRules,
          MapReprRules.factoryArgConvertRules
        )
      )
    )

    /** Try to resolve an intrinsic for the given application.
      *
      * Called after lowering the argument. Uses the already-lowered arg's representation to decide
      * whether an intrinsic applies. If found, caches the lowered arg, substitutes at the SIR
      * level, re-lowers, and removes the cache entry.
      *
      * @param f
      *   the function being applied (Var or ExternalVar with qualified name)
      * @param argSir
      *   the original SIR argument node (used for substitution and as cache key)
      * @param loweredArg
      *   the already-lowered argument (cached to avoid recomputation)
      * @param appType
      *   the SIR type of the application result
      * @param pos
      *   source position
      * @return
      *   Some(LoweredValue) if an intrinsic was applied, None otherwise
      */
    def gatherApplyChain(sir: SIR): Option[(SIR, scala.List[SIR])] = {
        @scala.annotation.tailrec
        def go(sir: SIR, acc: scala.List[SIR]): Option[(SIR, scala.List[SIR])] = sir match
            case SIR.Apply(f, arg, _, _)         => go(f, arg :: acc)
            case _: SIR.Var | _: SIR.ExternalVar => Some((sir, acc))
            case _                               => None
        go(sir, scala.List.empty)
    }

    def countTopLambdas(sir: SIR): Int = sir match
        case SIR.LamAbs(_, body, _, _) => 1 + countTopLambdas(body)
        case _                         => 0

    def tryResolveFull(
        head: SIR,
        argSirs: scala.List[SIR],
        loweredArgs: scala.List[LoweredValue],
        appType: SIRType,
        pos: SIRPosition
    )(using lctx: LoweringContext): Option[LoweredValue] = {
        require(loweredArgs.length == 1, "tryResolveFull: only the first arg is lowered eagerly")
        if argSirs.isEmpty then return None
        val firstLoweredOnly = loweredArgs.head
        extractModuleAndMethod(head) match
            case None => None
            case Some((moduleName, methodName)) =>
                val firstArgRepr = firstLoweredOnly.representation
                val reprNames = representationNames(firstArgRepr)
                registry.get(moduleName) match
                    case None => None
                    case Some(entries) =>
                        val pvVersion = lctx.targetProtocolVersion.version
                        var bestBinding: Option[Binding] = None
                        var bestPV = -1
                        var bestReprPriority = Int.MaxValue
                        var bestReprRules: Map[String, scalus.compiler.intrinsics.ReprRule] =
                            Map.empty
                        var bestArgConvertRules
                            : Map[String, scalus.compiler.intrinsics.ArgReprConvertRule] =
                            Map.empty
                        for (repr, minPV, providerModuleName, reprRules, argConvertRules) <-
                                entries
                        do {
                            val reprPriority = reprNames.indexOf(repr) match {
                                case -1 if repr == WildcardRepr && reprRules.contains(methodName) =>
                                    reprNames.size
                                case -1 => -1
                                case i  => i
                            }
                            if reprPriority >= 0 && pvVersion >= minPV &&
                                (reprPriority < bestReprPriority || (reprPriority == bestReprPriority && minPV > bestPV))
                            then
                                lctx.findProviderBinding(providerModuleName, methodName).foreach {
                                    b =>
                                        bestBinding = Some(b)
                                        bestPV = minPV
                                        bestReprPriority = reprPriority
                                        bestReprRules = reprRules
                                        bestArgConvertRules = argConvertRules
                                }
                        }
                        bestBinding.flatMap { binding =>
                            val depth = countTopLambdas(binding.value)
                            if depth != argSirs.length then None
                            else {
                                val rewrittenArgs = argSirs.map(rewriteIntrinsicExtVarTypeVars)
                                // Apply argConvertRule to the FIRST arg only (matches single-arg
                                // dispatch semantics).
                                val firstEffective =
                                    bestArgConvertRules.get(methodName) match
                                        case Some(convertRule) =>
                                            convertRule(argSirs.head.tp, appType, lctx) match
                                                case Some(targetRepr) =>
                                                    firstLoweredOnly.toRepresentation(
                                                      targetRepr,
                                                      pos
                                                    )
                                                case None => firstLoweredOnly
                                        case None => firstLoweredOnly
                                val allocatedKeys =
                                    scala.collection.mutable.ListBuffer.empty[Int]
                                // Propagate the self arg's `@UplcRepr` annotation into the final
                                // return position of the expected type, mirroring the
                                // dispatcher/support op's declared return annotation so inner
                                // match lowering sees the correct target representation.
                                val loweringAppType = firstEffective.sirType match
                                    case SIRType.Annotated(_, anns)
                                        if anns.data.contains("uplcRepr") =>
                                        propagateReturnAnnotation(appType, anns)
                                    case _ => appType
                                val lowered =
                                    val savedScope = lctx.inUplcConstrListScope
                                    val savedFilledTypes = lctx.typeUnifyEnv.filledTypes
                                    val savedReprEnv = lctx.typeVarReprEnv
                                    if reprNames.contains(UplcConstrListRepr) then
                                        lctx.inUplcConstrListScope = true
                                        bindIntrinsicListResolverElementTypeVars(
                                          binding,
                                          argSirs.head.tp,
                                          appType,
                                          head.tp,
                                          firstEffective,
                                          lctx
                                        )
                                    try
                                        // Lower tail args UNDER the scope flag — the user's
                                        // lambda's internal sum-type constructors (e.g.
                                        // `Option.Some(...)`) must use the native-Constr
                                        // generator, not the default DataConstr, so its output
                                        // aligns with the operation body's expected native form.
                                        val tailLowered =
                                            rewrittenArgs.tail.map(s => Lowering.lowerSIR(s))
                                        val effectiveLowered = firstEffective :: tailLowered
                                        val substituted = rewrittenArgs
                                            .zip(effectiveLowered)
                                            .foldLeft(
                                              binding.value
                                            ) { case (body, (argSir, loweredArg)) =>
                                                val (nextBody, key) =
                                                    substituteSelf(body, argSir, loweredArg)
                                                allocatedKeys += key
                                                nextBody
                                            }
                                        Lowering.lowerSIR(substituted, Some(loweringAppType))
                                    finally
                                        lctx.inUplcConstrListScope = savedScope
                                        val merged =
                                            savedFilledTypes ++ lctx.typeUnifyEnv.filledTypes
                                        lctx.typeUnifyEnv =
                                            lctx.typeUnifyEnv.copy(filledTypes = merged)
                                        lctx.typeVarReprEnv = savedReprEnv
                                        allocatedKeys.foreach(lctx.argCache.remove)
                                // Apply rule only when the body's natural output and rule's
                                // declared output are in different repr families — same-family
                                // pairs (e.g. SumUplcConstr with Unwrapped vs concrete-default
                                // element reprs) are runtime-equivalent and a `toRepresentation`
                                // would just emit a redundant spine rewriter.
                                val ruleApplied = bestReprRules.get(methodName) match
                                    case Some(rule) =>
                                        val outputRepr =
                                            rule(appType, firstEffective.representation, lctx)
                                        if reprsShareOuterFamily(
                                              lowered.representation,
                                              outputRepr
                                            )
                                        then lowered
                                        else lowered.toRepresentation(outputRepr, pos)
                                    case None => lowered
                                Some(
                                  RepresentationAnnotatedTypeProxyLoweredValue
                                      .wrapIfAnnotated(ruleApplied, pos)
                                )
                            }
                        }
    }

    def tryResolve(
        f: SIR,
        argSir: SIR,
        loweredArg: LoweredValue,
        appType: SIRType,
        pos: SIRPosition
    )(using lctx: LoweringContext): Option[LoweredValue] = {
        extractModuleAndMethod(f) match
            case None => None
            case Some((moduleName, methodName)) =>
                if lctx.debug then
                    lctx.log(
                      s"IntrinsicResolver: module=$moduleName method=$methodName repr=${loweredArg.representation.doc.render(80)}"
                    )
                registry.get(moduleName) match
                    case None => None
                    case Some(entries) =>
                        val reprNames = representationNames(loweredArg.representation)
                        if lctx.debug then
                            lctx.log(
                              s"IntrinsicResolver: reprNames=$reprNames entries=${entries.map(e => s"(${e._1},${e._2},${e._3})").mkString(",")}"
                            )
                        val pvVersion = lctx.targetProtocolVersion.version

                        // Find best matching provider: most specific repr name first,
                        // then highest minPV that satisfies constraints
                        var bestBinding: Option[Binding] = None
                        var bestPV = -1
                        var bestReprPriority = Int.MaxValue
                        var bestReprRules: Map[String, scalus.compiler.intrinsics.ReprRule] =
                            Map.empty
                        var bestArgConvertRules
                            : Map[String, scalus.compiler.intrinsics.ArgReprConvertRule] =
                            Map.empty
                        for (repr, minPV, providerModuleName, reprRules, argConvertRules) <-
                                entries
                        do {
                            val reprPriority = reprNames.indexOf(repr) match {
                                case -1 if repr == WildcardRepr && reprRules.contains(methodName) =>
                                    reprNames.size // wildcard has lowest priority
                                case -1 => -1 // no match
                                case i  => i
                            }
                            if reprPriority >= 0 && pvVersion >= minPV &&
                                (reprPriority < bestReprPriority || (reprPriority == bestReprPriority && minPV > bestPV))
                            then
                                lctx.findProviderBinding(providerModuleName, methodName).foreach {
                                    b =>
                                        bestBinding = Some(b)
                                        bestPV = minPV
                                        bestReprPriority = reprPriority
                                        bestReprRules = reprRules
                                        bestArgConvertRules = argConvertRules
                                }
                        }

                        if lctx.debug && bestBinding.isEmpty then
                            lctx.log(s"IntrinsicResolver: no binding found for $methodName")
                        if lctx.debug && bestBinding.isDefined then
                            lctx.log(
                              s"IntrinsicResolver: FOUND binding for $methodName, reprPriority=$bestReprPriority"
                            )
                        bestBinding.map { binding =>
                            // Before substituting the argSir into the provider body, rewrite any
                            // inner ExternalVar references to intrinsic-dispatched modules so
                            // their TypeLambda TypeVars are Transparent. The Scala plugin stamps
                            // Fixed by default; without this pre-pass, Fixed leaks into enclosing
                            // intrinsic bodies via substitution and corrupts output representation
                            // inference (e.g., `map(quicksort(...))` ends up with element repr
                            // `TypeVarRepresentation(Fixed)` → runtime crash).
                            val rewrittenArgSir = rewriteIntrinsicExtVarTypeVars(argSir)
                            val effectiveArg = bestArgConvertRules.get(methodName) match
                                case Some(convertRule) =>
                                    convertRule(argSir.tp, appType, lctx) match
                                        case Some(targetRepr) =>
                                            loweredArg.toRepresentation(targetRepr, pos)
                                        case None => loweredArg
                                case None => loweredArg
                            val (substituted, allocatedKey) =
                                substituteSelf(binding.value, rewrittenArgSir, effectiveArg)
                            // Propagate the self arg's `@UplcRepr` annotation into the expected
                            // return type's final position so inner match lowering sees the same
                            // target representation as the binding's declared return. (Applies
                            // even with the UplcConstr policy still active, for consistency.)
                            val loweringAppType = effectiveArg.sirType match
                                case SIRType.Annotated(_, anns) if anns.data.contains("uplcRepr") =>
                                    propagateReturnAnnotation(appType, anns)
                                case _ => appType
                            val lowered =
                                try {
                                    // For UplcConstr list intrinsics, temporarily enable the
                                    // UplcConstr-list scope so inline List.Cons / Option.Some
                                    // constructors in user lambdas produce native Constr terms.
                                    // The support ops' type-level `@UplcRepr(UplcConstr)`
                                    // annotations close the rhsRepr mismatch at
                                    // `resolveSupportBinding` (that path no longer needs the
                                    // scope) but the dispatcher path still requires it — user
                                    // lambdas lowered here carry Option.Some/List.Cons
                                    // constructions that must produce native-Constr terms.
                                    val savedScope = lctx.inUplcConstrListScope
                                    val savedFilledTypes = lctx.typeUnifyEnv.filledTypes
                                    val savedReprEnv = lctx.typeVarReprEnv
                                    if reprNames.contains(UplcConstrListRepr) then
                                        lctx.inUplcConstrListScope = true
                                        bindIntrinsicListResolverElementTypeVars(
                                          binding,
                                          argSir.tp,
                                          appType,
                                          f.tp,
                                          loweredArg,
                                          lctx
                                        )
                                    try Lowering.lowerSIR(substituted, Some(loweringAppType))
                                    finally
                                        lctx.inUplcConstrListScope = savedScope
                                        // Keep the TypeVar bindings we added — they may be
                                        // needed when the caller converts the intrinsic's
                                        // result to its target output repr (TypeVar fields
                                        // in the output's SumUplcConstr/ProdUplcConstr shape
                                        // still need to be resolvable). Merging preserves
                                        // both original env and our additions.
                                        val merged =
                                            savedFilledTypes ++ lctx.typeUnifyEnv.filledTypes
                                        lctx.typeUnifyEnv =
                                            lctx.typeUnifyEnv.copy(filledTypes = merged)
                                        lctx.typeVarReprEnv = savedReprEnv
                                } finally lctx.argCache.remove(allocatedKey)
                            // Real byte-level conversion + representation-derived annotation +
                            // outer TypeLambda restoration. Both proxies derive their fields
                            // from `origin`, so neither label can drift from emitted bytes.
                            val ruleApplied = bestReprRules.get(methodName) match
                                case Some(rule) =>
                                    val outputRepr =
                                        rule(appType, effectiveArg.representation, lctx)
                                    if reprsShareOuterFamily(
                                          lowered.representation,
                                          outputRepr
                                        )
                                    then lowered
                                    else lowered.toRepresentation(outputRepr, pos)
                                case None => lowered
                            val annotated =
                                RepresentationAnnotatedTypeProxyLoweredValue
                                    .wrapIfAnnotated(ruleApplied, pos)
                            OuterTypeLambdaWrappedLoweredValue.wrapIfNeeded(annotated, pos)
                        }
    }

    /** Extract module name and method name from a function SIR node. */
    private def extractModuleAndMethod(f: SIR): Option[(String, String)] = f match
        case SIR.Var(name, _, _) =>
            val lastDot = name.lastIndexOf('.')
            if lastDot > 0 then Some((name.substring(0, lastDot), name.substring(lastDot + 1)))
            else None
        case SIR.ExternalVar(moduleName, name, _, _) =>
            val lastDot = name.lastIndexOf('.')
            if lastDot > 0 then Some((moduleName, name.substring(lastDot + 1)))
            else None
        case _ => None

    /** Map a LoweredValueRepresentation to its string name for registry lookup.
      *
      * SumDataList/SumDataPairList are val aliases for SumBuiltinList instances, so the
      * SumBuiltinList patterns handle them via case class equality.
      */
    /** Walk a SIR tree and rewrite every `ExternalVar(module, ...)` whose module is dispatched by
      * the intrinsic resolver so its type's TypeVars become `Transparent`. Other nodes are
      * preserved structurally. Does not recurse into `Decl.data` since DataDecl TypeVars have a
      * different role (they live in the declaration and are bound to concrete types at use sites).
      */
    private def rewriteIntrinsicExtVarTypeVars(sir: SIR): SIR = {
        // Only override Fixed (the scalus-plugin's broken default). Preserve author-written
        // Transparent/Unwrapped on intrinsic-module typeparams so the lowering can act on the
        // declared semantic ("bytes are at concrete-default form" for Unwrapped, etc.).
        def transparentize(tp: SIRType): SIRType =
            SIRType.mapTypeVars(
              tp,
              tv =>
                  if tv.kind == SIRType.TypeVarKind.Fixed
                  then tv.copy(kind = SIRType.TypeVarKind.Transparent)
                  else tv
            )
        def goE(e: AnnotatedSIR): AnnotatedSIR = e match
            case ev @ SIR.ExternalVar(moduleName, name, tp, anns)
                if isIntrinsicDispatchedModule(moduleName) =>
                SIR.ExternalVar(moduleName, name, transparentize(tp), anns)
            case SIR.Apply(f, arg, tp, anns) => SIR.Apply(goE(f), goE(arg), tp, anns)
            case SIR.Select(scr, field, tp, anns) =>
                SIR.Select(go(scr).asInstanceOf[AnnotatedSIR], field, tp, anns)
            case SIR.LamAbs(p, body, tps, anns) => SIR.LamAbs(p, go(body), tps, anns)
            case SIR.Let(bs, body, flags, anns) =>
                val newBs = bs.map(b => scalus.compiler.sir.Binding(b.name, b.tp, go(b.value)))
                SIR.Let(newBs, go(body), flags, anns)
            case SIR.Match(s, cs, tp, anns) =>
                val newCs = cs.map(c => SIR.Case(c.pattern, go(c.body), c.anns))
                SIR.Match(goE(s), newCs, tp, anns)
            case SIR.IfThenElse(c, t, e, tp, anns) =>
                SIR.IfThenElse(goE(c), goE(t), goE(e), tp, anns)
            case SIR.And(l, r, anns)   => SIR.And(goE(l), goE(r), anns)
            case SIR.Or(l, r, anns)    => SIR.Or(goE(l), goE(r), anns)
            case SIR.Not(t, anns)      => SIR.Not(goE(t), anns)
            case SIR.Cast(t, tp, anns) => SIR.Cast(goE(t), tp, anns)
            case SIR.Constr(n, d, args, tp, anns) =>
                SIR.Constr(n, d, args.map(go), tp, anns)
            case other => other
        def go(s: SIR): SIR = s match
            case SIR.Decl(data, body) => SIR.Decl(data, go(body))
            case a: AnnotatedSIR      => goE(a)
        go(sir)
    }

    /** True when both reprs share top-level spine kind, regardless of nested field-repr
      * differences. Skips `toRepresentation` when source and target are runtime-equivalent (e.g.
      * SumUplcConstr with `TypeVarRepresentation(Unwrapped)` element reprs aliasing the concrete
      * default).
      */
    private def reprsShareOuterFamily(
        a: LoweredValueRepresentation,
        b: LoweredValueRepresentation
    ): Boolean = (a, b) match
        case (
              _: SumCaseClassRepresentation.SumUplcConstr,
              _: SumCaseClassRepresentation.SumUplcConstr
            ) =>
            true
        case (
              _: SumCaseClassRepresentation.SumBuiltinList,
              _: SumCaseClassRepresentation.SumBuiltinList
            ) =>
            true
        case (
              _: SumCaseClassRepresentation.SumPairBuiltinList,
              _: SumCaseClassRepresentation.SumPairBuiltinList
            ) =>
            true
        case (
              _: ProductCaseClassRepresentation.ProdUplcConstr,
              _: ProductCaseClassRepresentation.ProdUplcConstr
            ) =>
            true
        case (
              _: ProductCaseClassRepresentation.ProdBuiltinPair,
              _: ProductCaseClassRepresentation.ProdBuiltinPair
            ) =>
            true
        case _ => a == b

    /** All representation names that match a given representation, most specific first. */
    private def representationNames(repr: LoweredValueRepresentation): List[String] = repr match
        case proxy: SumCaseClassRepresentation.SumReprProxy =>
            // Self-referential repr (e.g., Cons tail proxy back to the outer SumUplcConstr) —
            // dispatch by the underlying representation so recursive list ops resolve.
            representationNames(proxy.ref)
        case _: SumCaseClassRepresentation.SumUplcConstr =>
            List(UplcConstrListRepr)
        case SumCaseClassRepresentation.SumBuiltinList(er) if !er.isPackedData =>
            List(NativeBuiltinListRepr, BuiltinListRepr)
        case SumCaseClassRepresentation.SumBuiltinList(_) =>
            List(BuiltinListRepr)
        case SumCaseClassRepresentation.SumPairBuiltinList(_, _) =>
            List(PairListRepr)
        case SumCaseClassRepresentation.PackedSumDataList =>
            List("PackedSumDataList")
        case SumCaseClassRepresentation.DataConstr =>
            List("DataConstr")
        case _ =>
            List(repr.getClass.getSimpleName.stripSuffix("$"))

    /** Substitute the lambda parameter in a provider binding body.
      *
      * Allocates an `argCache` key for `loweredArg`, stores it in `lctx.argCache(key)`, and
      * substitutes references to the lambda parameter with `SIR.Var(paramName, paramType, anns +
      * "argCache" → Const(Integer(key)))`. Subsequent `substituteVarAndTypes` walks preserve the
      * annotation in fresh SIR.Var copies, so every reference resolves to the SAME `LoweredValue`
      * via `lctx.argCache(key)` regardless of SIR object identity.
      *
      * Type-variable bindings from unifying `param.tp` with `arg.tp` are still applied via
      * `substituteVarAndTypes` so concrete types reach the body.
      */
    private def substituteSelf(bindingValue: SIR, arg: SIR, loweredArg: LoweredValue)(using
        lctx: LoweringContext
    ): (SIR, Int) = bindingValue match
        case SIR.LamAbs(param, body, typeParams, _) =>
            val typeEnv: Map[SIRType.TypeVar, SIRType] =
                if typeParams.isEmpty then Map.empty
                else
                    SIRUnify.topLevelUnifyType(
                      param.tp,
                      arg.tp,
                      SIRUnify.Env.empty.withUpcasting
                    ) match
                        case SIRUnify.UnificationSuccess(env, _) => env.filledTypes
                        case other =>
                            throw LoweringException(
                              s"substituteSelf: failed to unify param type ${param.tp.show} with arg type ${arg.tp.show}, result: $other",
                              SIRPosition.empty
                            )
            val cacheKey = lctx.newArgCacheKey()
            lctx.argCache.put(cacheKey, loweredArg)
            val keyConst = SIR.Const(
              scalus.uplc.Constant.Integer(BigInt(cacheKey)),
              SIRType.Integer,
              AnnotationsDecl.empty
            )
            val replacement: AnnotatedSIR = SIR.Var(
              param.name,
              param.tp,
              param.anns + (ArgCacheAnnotKey -> keyConst)
            )
            val substituted =
                if typeEnv.isEmpty then SIR.substituteFreeVar(body, param.name, replacement)
                else substituteVarAndTypes(body, param.name, replacement, typeEnv)
            (substituted, cacheKey)
        case _ =>
            throw LoweringException(
              s"Intrinsic provider binding must be a LamAbs, got: ${bindingValue.getClass.getSimpleName}",
              SIRPosition.empty
            )

    /** Walk SIR tree substituting both a free variable and type variables. */
    private def substituteVarAndTypes(
        sir: SIR,
        varName: String,
        replacement: SIR,
        typeEnv: Map[SIRType.TypeVar, SIRType]
    ): SIR = {
        def subTp(tp: SIRType): SIRType = SIRType.substitute(tp, typeEnv, Map.empty)

        def go(sir: SIR, locals: Set[String]): SIR = sir match
            case SIR.Decl(data, term) =>
                SIR.Decl(data, go(term, locals))
            case ann: AnnotatedSIR =>
                goExpr(ann, locals)

        def goExpr(sir: AnnotatedSIR, locals: Set[String]): AnnotatedSIR = sir match
            case SIR.Var(name, tp, anns) =>
                if locals.contains(name) then SIR.Var(name, subTp(tp), anns)
                else if name == varName then replacement.asInstanceOf[AnnotatedSIR]
                else SIR.Var(name, subTp(tp), anns)
            case SIR.ExternalVar(mod, name, tp, anns) =>
                SIR.ExternalVar(mod, name, subTp(tp), anns)
            case SIR.Const(value, tp, anns) =>
                SIR.Const(value, subTp(tp), anns)
            case SIR.Builtin(bn, tp, anns) =>
                SIR.Builtin(bn, subTp(tp), anns)
            case SIR.LamAbs(param, term, typeParams, anns) =>
                val nParam = SIR.Var(param.name, subTp(param.tp), param.anns)
                val nLocals = locals + param.name
                SIR.LamAbs(nParam, go(term, nLocals), typeParams, anns)
            case SIR.Apply(f, arg, tp, anns) =>
                SIR.Apply(goExpr(f, locals), goExpr(arg, locals), subTp(tp), anns)
            case SIR.Select(scrutinee, field, tp, anns) =>
                SIR.Select(go(scrutinee, locals).asInstanceOf[AnnotatedSIR], field, subTp(tp), anns)
            case SIR.Let(bindings, body, flags, anns) =>
                val (nBindings, nLocals) = bindings.foldLeft(
                  (IndexedSeq.empty[Binding], locals)
                ) { case ((acc, ln), b) =>
                    val nLn = if flags.isRec then ln + b.name else ln
                    val nValue = go(b.value, nLn)
                    val nLnAfter = ln + b.name
                    (acc :+ Binding(b.name, subTp(b.tp), nValue), nLnAfter)
                }
                SIR.Let(nBindings.toList, go(body, nLocals), flags, anns)
            case SIR.IfThenElse(cond, t, f, tp, anns) =>
                SIR.IfThenElse(
                  goExpr(cond, locals),
                  goExpr(t, locals),
                  goExpr(f, locals),
                  subTp(tp),
                  anns
                )
            case SIR.And(a, b, anns) =>
                SIR.And(goExpr(a, locals), goExpr(b, locals), anns)
            case SIR.Or(a, b, anns) =>
                SIR.Or(goExpr(a, locals), goExpr(b, locals), anns)
            case SIR.Not(a, anns) =>
                SIR.Not(goExpr(a, locals), anns)
            case SIR.Constr(name, data, args, tp, anns) =>
                SIR.Constr(name, data, args.map(go(_, locals)), subTp(tp), anns)
            case SIR.Match(scrutinee, cases, tp, anns) =>
                val nScrutinee = goExpr(scrutinee, locals)
                val nCases = cases.map { c =>
                    val nLn = c.pattern match
                        case SIR.Pattern.Wildcard               => locals
                        case SIR.Pattern.Constr(_, bindings, _) => locals ++ bindings
                        case SIR.Pattern.Const(_)               => locals
                    SIR.Case(c.pattern, go(c.body, nLn), c.anns)
                }
                SIR.Match(nScrutinee, nCases, subTp(tp), anns)
            case SIR.Cast(expr, tp, anns) =>
                SIR.Cast(goExpr(expr, locals), subTp(tp), anns)
            case SIR.Error(msg, anns, cause) =>
                SIR.Error(goExpr(msg, locals), anns, cause)

        go(sir, Set.empty)
    }

    /** Bind intrinsic TypeVars to Annotated(elementType, UplcConstr) in the unify env.
      *
      * This lets equalsRepr inside the intrinsic body resolve TypeVars to Annotated types, enabling
      * field-by-field comparison instead of pack+equalsData fallback.
      */
    private def bindIntrinsicListResolverElementTypeVars(
        binding: Binding,
        listType: SIRType,
        appType: SIRType,
        callSiteFunType: SIRType,
        loweredSelfArg: LoweredValue,
        lctx: LoweringContext
    ): Unit = {
        SumCaseClassRepresentation.SumBuiltinList.retrieveListElementType(listType) match {
            case None =>
            case Some(elementType) =>
                bindIntrinsicListResolverElementTypeVars1(
                  binding,
                  appType,
                  callSiteFunType,
                  loweredSelfArg,
                  lctx,
                  elementType
                )

        }
    }

    private def bindIntrinsicListResolverElementTypeVars1(
        binding: Binding,
        appType: SIRType,
        callSiteFunType: SIRType,
        loweredSelfArg: LoweredValue,
        lctx: LoweringContext,
        elemType: SIRType
    ): Unit = {
        // Role-aware TypeVar binding:
        //   1. SELF-PARAMETER TypeVars (e.g., A in map[A, B](self: List[A], ...))
        //      get bound to Annotated(inputElementType, UplcConstr) — this lets
        //      equalsRepr inside intrinsic bodies dispatch on element type structure.
        //   2. OUTPUT-SIDE TypeVars (e.g., B in map[A, B](...): List[B])
        //      get bound by unifying the rest of the binding signature with appType.
        //      Without this, B stays free and downstream lowering can't resolve it
        //      (producing either mis-binding to A's type or StackOverflow).
        def stripTypeLambda(tp: SIRType): SIRType = tp match
            case SIRType.TypeLambda(_, body)           => stripTypeLambda(body)
            case SIRType.TypeProxy(ref) if ref ne null => stripTypeLambda(ref)
            case _                                     => tp
        val bindingTpStripped = stripTypeLambda(binding.tp)
        val (selfParamType, restType) = bindingTpStripped match
            case SIRType.Fun(paramTp, retType) => (paramTp, retType)
            case _                             => (bindingTpStripped, SIRType.Unit)
        val callSiteTpStripped = stripTypeLambda(callSiteFunType)
        val selfTypeVars = scala.collection.mutable.Set.empty[SIRType.TypeVar]
        SIRType.mapTypeVars(selfParamType, tv => { selfTypeVars += tv; tv })
        // Annotate element type with UplcConstr repr (for role 1 — self TypeVars)
        val annotatedElemType = SIRType.Annotated(
          elemType,
          AnnotationsDecl(
            SIRPosition.empty,
            None,
            Map(
              "uplcRepr" -> SIR.Const(
                scalus.uplc.Constant.String("UplcConstr"),
                SIRType.String,
                AnnotationsDecl.empty
              )
            )
          )
        )
        // The compiled intrinsic binding is shared across dispatches, so its internal
        // TypeVars (e.g. `B#523` for `List$.map`'s `[A, B]`) keep the same optIds at
        // every call site. A previous dispatch may have left bindings for those
        // TypeVars in `lctx.typeUnifyEnv.filledTypes`. If the current dispatch's
        // unification fails to produce a fresh binding for one of them (e.g. when
        // `outputBindings` doesn't cover B because of a signature mismatch), the
        // stale entry persists and contaminates downstream lowering — including the
        // `captureFingerprint` of cache keys, where `env=[B#X=u:<stale-type>]` ends
        // up associated with helpers whose body should have been built under the
        // current site's actual element type.
        //
        // Defense: collect `binding.tp`'s internal TypeVars FIRST, then strip them
        // from `lctx.typeUnifyEnv.filledTypes` before merging fresh bindings. Each
        // dispatch starts with a clean slate for binding-internal TVs; pre-existing
        // call-site TypeVars (not in the binding signature) keep their bindings.
        val bindingInternalTvs =
            scala.collection.mutable.Set.empty[SIRType.TypeVar]
        // Non-allocating walk to collect TypeVars from `binding.tp`. We only need
        // identity, not a rebuilt type tree, so avoid `SIRType.mapTypeVars` which
        // would allocate fresh `TypeProxy` / structural nodes per dispatch.
        val seenProxies = new java.util.IdentityHashMap[SIRType.TypeProxy, Boolean]()
        def collectTvs(t: SIRType): Unit = t match
            case tv: SIRType.TypeVar => bindingInternalTvs += tv
            case SIRType.TypeLambda(ps, body) =>
                ps.foreach(bindingInternalTvs += _)
                collectTvs(body)
            case SIRType.Fun(in, out) => collectTvs(in); collectTvs(out)
            case SIRType.CaseClass(_, args, parent) =>
                args.foreach(collectTvs)
                parent.foreach(collectTvs)
            case SIRType.SumCaseClass(_, args) => args.foreach(collectTvs)
            case SIRType.Annotated(t1, _)      => collectTvs(t1)
            case SIRType.TypeProxy(ref) =>
                if ref != null && !seenProxies.containsKey(t) then
                    seenProxies.put(t.asInstanceOf[SIRType.TypeProxy], true)
                    collectTvs(ref)
            case _ => ()
        collectTvs(binding.tp)
        // Strip stale binding-internal TypeVars from the lctx env before merging.
        val cleanedFilledTypes =
            lctx.typeUnifyEnv.filledTypes -- bindingInternalTvs

        // Role 2: unify the binding's curried result with `appType` to bind output TypeVars.
        // `appType` may be either the fully-applied result (e.g. `List[ChessSet]` from
        // `tryResolveFull`) or the still-curried result of a single-step apply (e.g.
        // `(A→B) → List[B]` from `tryResolve` on a partial application). `restType` is the
        // binding signature after stripping the self parameter (e.g. `(A→B) → List[B]` for
        // map). Without depth-aligned stripping, a fully-applied call site with
        // `appType = List[ChessSet]` against `restType = (A→B) → List[B]` would fail to
        // unify shapes; the eqClasses pass below would then incorrectly inherit B from A's
        // equivalence class, producing `B → SolutionEntry` for
        // `descAndNo.quicksort.map { _.board }` (KnightsTest:475 heisenbug). Conversely, on
        // the single-step partial-app path, `appType` is *itself* a function type and we
        // must NOT strip restType past it, or unification fails and binding-internal output
        // typevars stay unrefreshed. Strip restType's Fun layers down to match appType's
        // own Fun depth.
        def funDepth(tp: SIRType): Int = tp match
            case SIRType.Fun(_, out)                   => 1 + funDepth(out)
            case SIRType.TypeProxy(ref) if ref ne null => funDepth(ref)
            case _                                     => 0
        def stripFunDepth(tp: SIRType, n: Int): SIRType =
            if n <= 0 then tp
            else
                tp match
                    case SIRType.Fun(_, out)                   => stripFunDepth(out, n - 1)
                    case SIRType.TypeProxy(ref) if ref ne null => stripFunDepth(ref, n)
                    case _                                     => tp
        val toStrip = (funDepth(restType) - funDepth(appType)).max(0)
        val restResult = stripFunDepth(restType, toStrip)
        val outputBindings: Map[SIRType.TypeVar, SIRType] =
            SIRUnify.topLevelUnifyType(restResult, appType, SIRUnify.Env.empty) match
                case SIRUnify.UnificationSuccess(env, _) => env.filledTypes
                case _                                   => Map.empty
        // Build the final filledTypes map starting from the cleaned env.
        val afterSelf = selfTypeVars.foldLeft(cleanedFilledTypes) { (acc, tv) =>
            acc + (tv -> annotatedElemType)
        }
        val afterOutput = outputBindings.foldLeft(afterSelf) { case (acc, (tv, t)) =>
            if selfTypeVars.contains(tv) then acc // already bound (role 1 wins)
            else acc + (tv -> t)
        }
        // Unify call-site function type with binding signature. Both sides carry free TypeVars
        // (call-site's and binding's). SIRUnify puts paired free TypeVars into env.eqTypes
        // (equivalence sets) rather than filledTypes. We then walk eqTypes: for each call-site
        // TypeVar, find its equivalent binding TypeVar, and look up the binding's concrete
        // type in our afterOutput map.
        val unifyResult =
            SIRUnify.topLevelUnifyType(callSiteTpStripped, bindingTpStripped, SIRUnify.Env.empty)
        val eqClasses: Map[SIRType.TypeVar, Set[SIRType.TypeVar]] = unifyResult match
            case SIRUnify.UnificationSuccess(env, _) => env.eqTypes
            case _                                   => Map.empty
        val filledFromUnify: Map[SIRType.TypeVar, SIRType] = unifyResult match
            case SIRUnify.UnificationSuccess(env, _) => env.filledTypes
            case _                                   => Map.empty
        val afterCrossFilled = filledFromUnify.foldLeft(afterOutput) { case (acc, (tv, t)) =>
            if !acc.contains(tv) then acc + (tv -> t)
            // After the strip-on-entry above, binding-internal TVs only re-enter `acc` via
            // the fresh `selfTypeVars`/`outputBindings` folds — both already reflect the
            // current dispatch. The branch below is a defensive no-op for the typical case
            // where `outputBindings` and `filledFromUnify` agree on the binding-internal TV;
            // if they disagree, prefer `filledFromUnify` (the call-site → binding mapping).
            else if bindingInternalTvs.contains(tv) then acc + (tv -> t)
            else acc // keep call-site
        }
        // Then walk eqClasses: for each TypeVar not yet bound, if any of its equivalents
        // IS bound in afterCrossFilled, inherit that concrete type.
        val finalFilledTypes = eqClasses.foldLeft(afterCrossFilled) { case (acc, (tv, equivs)) =>
            if acc.contains(tv) then acc
            else
                equivs.iterator.flatMap(acc.get).nextOption() match
                    case Some(concrete) => acc + (tv -> concrete)
                    case None           => acc
        }
        lctx.typeUnifyEnv = lctx.typeUnifyEnv.copy(filledTypes = finalFilledTypes)

        // Populate typeVarReprEnv with the caller's concrete representation for each bound
        // TypeVar. The self-parameter TypeVars inherit the element repr from the lowered self
        // arg (e.g., for self: List[A] with repr SumUplcConstr(ProdUplcConstr(1, [elemRepr, ...])),
        // A → elemRepr). Cross-propagation via eqClasses extends these to the call-site's own
        // TypeVar instances (which may have different optIds due to per-invocation renaming).
        val selfElemRepr: Option[LoweredValueRepresentation] =
            loweredSelfArg.representation match
                case sul: SumCaseClassRepresentation.SumUplcConstr =>
                    sul.variants.values
                        .find(_.fieldReprs.nonEmpty)
                        .map(_.fieldReprs.head)
                case sbl: SumCaseClassRepresentation.SumBuiltinList => Some(sbl.elementRepr)
                case _                                              => None
        val reprAfterSelf = selfElemRepr match
            case Some(r) =>
                selfTypeVars.foldLeft(lctx.typeVarReprEnv) { (acc, tv) => acc + (tv -> r) }
            case None => lctx.typeVarReprEnv
        // Cross-propagate to call-site TypeVar aliases via eqClasses (same as filledTypes).
        val finalReprEnv = eqClasses.foldLeft(reprAfterSelf) { case (acc, (tv, equivs)) =>
            if acc.contains(tv) then acc
            else
                equivs.iterator.flatMap(acc.get).nextOption() match
                    case Some(r) => acc + (tv -> r)
                    case None    => acc
        }
        lctx.typeVarReprEnv = finalReprEnv

    }

    /** Walk through Fun / TypeLambda wrappers and wrap the final sum return position with `anns`.
      * Used to propagate a self arg's `@UplcRepr` annotation into the expected return type when
      * lowering an intrinsic / support-op body, so that inner match/chooseCommonRepresentation
      * logic sees the same target annotation as the binding's declared return.
      */
    private[lowering] def propagateReturnAnnotation(
        tp: SIRType,
        anns: AnnotationsDecl
    ): SIRType = tp match
        case SIRType.Fun(arg, ret) =>
            SIRType.Fun(arg, propagateReturnAnnotation(ret, anns))
        case SIRType.TypeLambda(tps, body) =>
            SIRType.TypeLambda(tps, propagateReturnAnnotation(body, anns))
        case SIRType.Annotated(inner, existing) if existing.data.contains("uplcRepr") =>
            tp
        case t if SIRType.isSum(t) =>
            SIRType.Annotated(t, anns)
        case _ => tp

    /** True if `tp` (after peeling `TypeLambda` / `TypeProxy` / `Annotated`) is a
      * `scalus.cardano.onchain.plutus.prelude.List` or `Option`.
      *
      * Used by `IntrinsicResolver.tryResolve` / `tryResolveFull` to detect dispatcher boundaries
      * where `inUplcConstrListScope` should be flipped while selecting the native-Constr provider
      * binding. `LoweringContext.typeGenerator` no longer consults the flag (annotation- driven),
      * so the only remaining effect is provider selection inside the resolver.
      */
    def isUplcConstrListOrOption(tp: SIRType): Boolean = {
        def hasDeclName(t: SIRType, name: String, seen: Set[SIRType.TypeProxy]): Boolean = t match
            case SIRType.SumCaseClass(decl, _) => decl.name == name
            case SIRType.TypeLambda(_, body)   => hasDeclName(body, name, seen)
            case p: SIRType.TypeProxy =>
                if seen.contains(p) || p.ref == null then false
                else hasDeclName(p.ref, name, seen + p)
            case SIRType.Annotated(inner, _) => hasDeclName(inner, name, seen)
            case _                           => false
        hasDeclName(tp, "scalus.cardano.onchain.plutus.prelude.List", Set.empty) ||
        hasDeclName(tp, "scalus.cardano.onchain.plutus.prelude.Option", Set.empty)
    }
}
