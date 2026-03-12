package scalus.compiler.sir.lowering

import scalus.compiler.sir.*

/** Resolves intrinsic implementations for method calls based on argument representation and
  * protocol version.
  *
  * When the lowering encounters a method call like `list.isEmpty`, the resolver checks if there is
  * an optimized intrinsic implementation for the argument's runtime representation (e.g.,
  * `SumDataList`). If found, it substitutes the provider's body (with the actual argument SIR) and
  * re-lowers it. The already-lowered argument is cached in `lctx.precomputedValues` so that
  * re-lowering the substituted SIR finds it without recomputation.
  *
  * The registry is hardcoded — adding new intrinsic providers requires editing the registry and
  * creating the `@Compile` provider object.
  */
object IntrinsicResolver {

    // Module name constants (must match plugin's td.symbol.fullName for @Compile objects)
    private val ListModule = "scalus.cardano.onchain.plutus.prelude.List$"
    private val BuiltinListOps = "scalus.compiler.intrinsics.BuiltinListOperations$"
    private val BuiltinListOpsV11 = "scalus.compiler.intrinsics.BuiltinListOperationsV11$"

    /** Default intrinsic modules, loaded at compile time by the plugin. The plugin intercepts
      * `compiledModules(...)` and replaces it with `SIRLinker.readModules(...)` that accesses the
      * objects' `sirModule` vals.
      */
    lazy val defaultIntrinsicModules: Map[String, Module] =
        scalus.compiler.compiledModules(
          "scalus.compiler.intrinsics.BuiltinListOperations",
          "scalus.compiler.intrinsics.BuiltinListOperationsV11"
        )

    // Representation name constants (must match LoweredValueRepresentation case object names)
    private val SumDataListRepr = "SumDataList"

    /** Registry entry: (representation, minProtocolVersion, providerModule) */
    private type RegistryEntry = (String, Int, String)

    /** Registry: targetModule -> List of (representation, minProtocolVersion, providerModule) */
    private val registry: Map[String, List[RegistryEntry]] = Map(
      ListModule -> List(
        (SumDataListRepr, 0, BuiltinListOps),
        (SumDataListRepr, 11, BuiltinListOpsV11)
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
    def tryResolve(
        f: SIR,
        argSir: SIR,
        loweredArg: LoweredValue,
        appType: SIRType,
        pos: SIRPosition
    )(using lctx: LoweringContext): Option[LoweredValue] = {
        // Extract module name and method name from the function
        val (moduleName, methodName) = extractModuleAndMethod(f).getOrElse(return None)

        // Look up registry entries for this target module
        val entries = registry.getOrElse(moduleName, return None)

        // Map the argument's representation to a string name
        val reprName = representationName(loweredArg.representation)

        // Get protocol version as integer for comparison
        val pvVersion = lctx.targetProtocolVersion.version

        // Filter entries matching representation and protocol version, sorted by version desc
        val matchingEntries = entries
            .filter { case (repr, minPV, _) => repr == reprName && pvVersion >= minPV }
            .sortBy { case (_, minPV, _) => -minPV } // highest version first

        // Try each matching provider (highest version first)
        for (_, _, providerModuleName) <- matchingEntries do
            lctx.findProviderBinding(providerModuleName, methodName) match
                case Some(binding) =>
                    val substituted = substituteSelf(binding.value, argSir)
                    // Cache the already-lowered arg, lower the substituted SIR, remove cache entry
                    lctx.precomputedValues.put(argSir, loweredArg)
                    try
                        val result = Lowering.lowerSIR(substituted, Some(appType))
                        return Some(result)
                    finally lctx.precomputedValues.remove(argSir)
                case None => // method not in this provider, try next
        None
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

    /** Map a LoweredValueRepresentation to its string name for registry lookup. */
    private def representationName(repr: LoweredValueRepresentation): String = repr match
        case SumCaseClassRepresentation.SumDataList       => "SumDataList"
        case SumCaseClassRepresentation.SumDataPairList   => "SumDataPairList"
        case SumCaseClassRepresentation.PackedSumDataList => "PackedSumDataList"
        case SumCaseClassRepresentation.DataConstr        => "DataConstr"
        case _ => repr.getClass.getSimpleName.stripSuffix("$")

    /** Substitute the `self` parameter and type variables in a provider binding body.
      *
      * Unwraps the outer lambda, infers type variable bindings by unifying the parameter type with
      * the actual argument type, then substitutes both the expression variable and all type
      * occurrences in the body.
      */
    private def substituteSelf(bindingValue: SIR, arg: SIR): SIR = bindingValue match
        case SIR.LamAbs(param, body, typeParams, _) =>
            val typeEnv: Map[SIRType.TypeVar, SIRType] =
                if typeParams.isEmpty then Map.empty
                else
                    SIRUnify.topLevelUnifyType(param.tp, arg.tp, SIRUnify.Env.empty) match
                        case SIRUnify.UnificationSuccess(env, _) => env.filledTypes
                        case _                                   => Map.empty
            if typeEnv.isEmpty then SIR.substituteFreeVar(body, param.name, arg)
            else substituteVarAndTypes(body, param.name, arg, typeEnv)
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
}
