package scalus.compiler.sir

import scala.collection.mutable

/** Analyzes TypeVar usage in linked SIR and assigns correct TypeVarKinds.
  *
  * The analysis walks the SIR tree after linking, determines how each TypeVar is used, and produces
  * a new SIR tree with corrected TypeVarKinds:
  *   - Transparent: TypeVar values are only passed through, never inspected or stored in lists
  *   - DefaultRepresentation: TypeVar values are inspected (Eq, Ord comparison) — native repr
  *   - CanBeListAffected: TypeVar values flow through list element positions — Data repr
  *
  * Constraint sources:
  *   - List/PairList constructor or pattern match → CanBeListAffected
  *   - FunctionalInterface parameter (Eq, Ord, etc.) → DefaultRepresentation
  *   - FunctionalInterface parameter (FromData, ToData) → CanBeListAffected
  *   - Apply unification: TypeVars unified via SIRUnify share constraints
  */
object TypeVarKindAnalysis {

    import SIRType.TypeVarKind
    import SIRType.TypeVarKind.*

    /** Annotation key stored in AnnotationsDecl.data by the plugin. Value is the trait full name. */
    val FunctionalInterfaceAnnotationKey = "functionalInterface"

    /** FunctionalInterface traits whose TypeVars need Data representation. */
    private val dataRepresentationTraits: Set[String] = Set(
      "scalus.uplc.builtin.FromData",
      "scalus.uplc.builtin.ToData"
    )

    /** DataDecl names for list-like types whose element TypeVars need Data representation. */
    private val listLikeDeclNames: Set[String] = Set(
      "scalus.cardano.onchain.plutus.prelude.List",
      "scalus.cardano.onchain.plutus.prelude.PairList",
      "scalus.cardano.onchain.plutus.prelude.AssocMap",
      "scalus.cardano.onchain.plutus.prelude.SortedMap",
      "scalus.cardano.onchain.plutus.prelude.Varargs"
    )

    /** Ordering: Transparent(0) < DefaultRepresentation(1) < CanBeListAffected(2). */
    private def kindOrd(k: TypeVarKind): Int = k match {
        case Transparent           => 0
        case DefaultRepresentation => 1
        case CanBeListAffected     => 2
    }

    private def kindMax(a: TypeVarKind, b: TypeVarKind): TypeVarKind =
        if kindOrd(a) >= kindOrd(b) then a else b

    case class Stats(
        total: Int,
        transparent: Int,
        defaultRepresentation: Int,
        canBeListAffected: Int
    ) {
        override def toString: String =
            s"TypeVarKind analysis: total=$total, transparent=$transparent, " +
                s"defaultRepr=$defaultRepresentation, listAffected=$canBeListAffected"
    }

    /** Analyze TypeVar usage and produce new SIR with corrected kinds. */
    def analyze(sir: SIR, debug: Boolean = false): (SIR, Stats) = {
        val ctx = new AnalysisContext(debug)
        ctx.analyzeSir(sir, Map.empty)
        val newSir = ctx.rebuild(sir)
        val stats = ctx.computeStats()
        if debug then println(stats)
        (newSir, stats)
    }

    private type TVKey = (String, Option[Long])

    private case class FunctionInfo(
        typeParams: List[SIRType.TypeVar],
        params: List[SIR.Var],
        body: SIR
    )

    private class AnalysisContext(debug: Boolean) {

        /** All non-builtin TypeVars discovered in the tree. */
        val allScalaTypeVars = mutable.Set.empty[TVKey]

        /** Computed kind for each TypeVar (union-find canonical key → kind). */
        val computedKinds = mutable.Map.empty[TVKey, TypeVarKind]

        /** Fully analyzed functions (by binding identity). */
        val analyzed = mutable.Set.empty[AnyRef]

        /** Functions currently being analyzed (for recursion detection). */
        val inProgress = mutable.Set.empty[AnyRef]

        // ------- Union-Find -------

        private val ufParent = mutable.Map.empty[TVKey, TVKey]

        private def find(key: TVKey): TVKey = {
            ufParent.get(key) match {
                case None              => key
                case Some(p) if p == key => key
                case Some(p) =>
                    val root = find(p)
                    ufParent(key) = root
                    root
            }
        }

        private def union(a: TVKey, b: TVKey): Unit = {
            val ra = find(a)
            val rb = find(b)
            if ra != rb then {
                // Merge: keep the max kind
                val ka = computedKinds.getOrElse(ra, Transparent)
                val kb = computedKinds.getOrElse(rb, Transparent)
                ufParent(ra) = rb
                computedKinds(rb) = kindMax(ka, kb)
            }
        }

        // ------- Single-pass analysis with scope -------

        def analyzeSir(sir: SIR, scope: Map[String, FunctionInfo]): Unit = sir match {
            case SIR.Decl(_, term) => analyzeSir(term, scope)
            case SIR.Let(bindings, body, _, _) =>
                val newScope = bindings.foldLeft(scope) { (sc, b) =>
                    b.value match {
                        case lam: SIR.LamAbs if lam.typeParams.nonEmpty =>
                            val (tps, params) = extractTypeParamsAndParams(lam)
                            registerTypeParams(tps)
                            for tv <- tps do functionForTypeVar(tvKey(tv)) = b.name
                            sc + (b.name -> FunctionInfo(tps, params, lam))
                        case _ => sc
                    }
                }
                for b <- bindings do analyzeSir(b.value, newScope)
                analyzeSir(body, newScope)
            case lam: SIR.LamAbs =>
                registerTypeParams(lam.typeParams)
                checkFunctionalInterfaceParams(lam)
                // Check if TypeVars appear in list element positions in the function's type
                checkTypeForListElements(lam.tp)
                analyzeSir(lam.term, scope)
            case SIR.Constr(_, data, args, tp, _) =>
                if isListLikeDecl(data) then markElementTypeVars(tp)
                args.foreach(analyzeSir(_, scope))
            case SIR.Match(scrutinee, cases, _, _) =>
                checkMatchForList(scrutinee.tp)
                // If scrutinee type IS a non-builtin TypeVar, matching requires known representation
                scrutinee.tp match {
                    case tv: SIRType.TypeVar if !tv.isBuiltin =>
                        upgradeKind(tv, DefaultRepresentation)
                    case _ =>
                }
                analyzeSir(scrutinee, scope)
                for c <- cases do analyzeSir(c.body, scope)
            case SIR.Apply(f, arg, tp, _) =>
                analyzeSir(f, scope); analyzeSir(arg, scope)
                unifyAtApply(f, arg)
                checkBuiltinListOps(f, arg)
                checkCallPropagation(f, arg, scope)
            case SIR.Select(s, field, tp, _) =>
                s.tp match {
                    case tv: SIRType.TypeVar if !tv.isBuiltin =>
                        upgradeKind(tv, DefaultRepresentation)
                    case _ =>
                }
                analyzeSir(s, scope)
            case SIR.Cast(t, _, _)             => analyzeSir(t, scope)
            case SIR.And(a, b, _)              => analyzeSir(a, scope); analyzeSir(b, scope)
            case SIR.Or(a, b, _)               => analyzeSir(a, scope); analyzeSir(b, scope)
            case SIR.Not(a, _)                 => analyzeSir(a, scope)
            case SIR.IfThenElse(c, t, f, _, _) => analyzeSir(c, scope); analyzeSir(t, scope); analyzeSir(f, scope)
            case SIR.Error(msg, _, _)          => analyzeSir(msg, scope)
            case _                             =>
        }

        /** At each Apply(f, arg), unify the function's parameter type with the argument type.
          * This discovers TypeVar equivalences (e.g., B#91030 ≡ B#76537) via SIRUnify.
          */
        private def unifyAtApply(f: SIR, arg: SIR): Unit = {
            val paramTp = extractArgType(f.tp)
            paramTp match {
                case None => // not a function type
                case Some(pt) =>
                    SIRUnify.topLevelUnifyType(pt, arg.tp, SIRUnify.Env.empty.withUpcasting) match {
                        case SIRUnify.UnificationSuccess(env, _) =>
                            // filledTypes maps TypeVars → their resolved types
                            if debug && env.filledTypes.nonEmpty then
                            println(s"  unifyApply filledTypes: ${env.filledTypes.map((k, v) => s"${k.name}#${k.optId.getOrElse(0)} → ${v.show}").mkString(", ")}")
                        if debug && env.eqTypes.nonEmpty then
                            println(s"  unifyApply eqTypes: ${env.eqTypes.map((k, v) => s"${k.name}#${k.optId.getOrElse(0)} → ${v.map(tv => s"${tv.name}#${tv.optId.getOrElse(0)}").mkString(",")}").mkString("; ")}")
                        for (tv, resolved) <- env.filledTypes do {
                                resolved match {
                                    case rtv: SIRType.TypeVar
                                        if tv.optId.isDefined && rtv.optId.isDefined =>
                                        union(tvKey(tv), tvKey(rtv))
                                    case _ =>
                                }
                            }
                            for (tv, eqSet) <- env.eqTypes do
                                for eq <- eqSet
                                    if tv.optId.isDefined && eq.optId.isDefined
                                do union(tvKey(tv), tvKey(eq))
                        case _ => // unification failed, skip
                    }
            }
        }

        /** Extract the parameter type from a function type. */
        private def extractArgType(tp: SIRType): Option[SIRType] = tp match {
            case SIRType.Fun(in, _)            => Some(in)
            case SIRType.TypeLambda(_, body)    => extractArgType(body)
            case tp: SIRType.TypeProxy if tp.ref != null => extractArgType(tp.ref)
            case _                             => None
        }

        /** Check a type for list element positions — TypeVars in List[T]/PairList[A,B] etc.
          * are CanBeListAffected even if the function body doesn't directly construct/match lists.
          */
        private def checkTypeForListElements(tp: SIRType): Unit = {
            val visited = new java.util.IdentityHashMap[SIRType.TypeProxy, Boolean]()
            def walk(t: SIRType, inListElement: Boolean): Unit = t match {
                case tv: SIRType.TypeVar =>
                    if inListElement then upgradeKind(tv, CanBeListAffected)
                case SIRType.Fun(in, out) =>
                    walk(in, false); walk(out, false)
                case SIRType.CaseClass(cd, typeArgs, parent) =>
                    val isListLike = parent match {
                        case Some(SIRType.SumCaseClass(decl, _)) => listLikeDeclNames.contains(decl.name)
                        case _ => false
                    }
                    typeArgs.foreach(a => walk(a, isListLike || inListElement))
                    parent.foreach(p => walk(p, false))
                case SIRType.SumCaseClass(decl, typeArgs) =>
                    val isListLike = listLikeDeclNames.contains(decl.name)
                    typeArgs.foreach(a => walk(a, isListLike || inListElement))
                case SIRType.TypeLambda(_, body) =>
                    walk(body, false)
                case tp: SIRType.TypeProxy =>
                    if tp.ref != null && !visited.containsKey(tp) then {
                        visited.put(tp, true)
                        walk(tp.ref, inListElement)
                    }
                case _ =>
            }
            walk(tp, false)
        }

        /** Detect builtin list operations (mkCons, constrData, mapData, listData).
          * When mkCons(element, tail) is called, the element's TypeVars are list-affected.
          */
        private def checkBuiltinListOps(f: SIR, arg: SIR): Unit = {
            import scalus.uplc.DefaultFun
            // Peel Apply chain to find the root builtin
            val (root, allArgs) = peelApplyChain(f, List(arg))
            root match {
                case Some(SIR.Builtin(fn, _, _)) =>
                    fn match {
                        case DefaultFun.MkCons =>
                            // mkCons(element, tail) — first arg is list element
                            if allArgs.nonEmpty then
                                collectTypeVarsFrom(allArgs.head.tp)
                                    .foreach(tv => upgradeKind(tv, CanBeListAffected))
                        case DefaultFun.ConstrData =>
                            // constrData(tag, fields) — second arg is List[Data], fields are list elements
                            if allArgs.size >= 2 then
                                collectTypeVarsFrom(allArgs(1).tp)
                                    .foreach(tv => upgradeKind(tv, CanBeListAffected))
                        case DefaultFun.MapData | DefaultFun.ListData =>
                            // mapData(list) / listData(list) — arg elements are list-affected
                            if allArgs.nonEmpty then
                                collectTypeVarsFrom(allArgs.head.tp)
                                    .foreach(tv => upgradeKind(tv, CanBeListAffected))
                        case DefaultFun.MkPairData =>
                            // mkPairData(fst, snd) — both args become pair elements in a list
                            for a <- allArgs do
                                collectTypeVarsFrom(a.tp)
                                    .foreach(tv => upgradeKind(tv, CanBeListAffected))
                        case _ =>
                    }
                case _ =>
            }
        }

        private def checkFunctionalInterfaceParams(lam: SIR.LamAbs): Unit = {
            var current: SIR = lam
            while current.isInstanceOf[SIR.LamAbs] do {
                val l = current.asInstanceOf[SIR.LamAbs]
                l.param.anns.data.get(FunctionalInterfaceAnnotationKey) match {
                    case Some(SIR.Const(scalus.uplc.Constant.String(traitName), _, _)) =>
                        // FI-annotated parameter: kind depends on trait
                        val kind =
                            if dataRepresentationTraits.contains(traitName) then CanBeListAffected
                            else DefaultRepresentation
                        collectTypeVarsFrom(l.param.tp).foreach(tv => upgradeKind(tv, kind))
                    case _ =>
                        // Non-FI callback: we can't see inside it, so TypeVars
                        // in its type must be at least CanBeListAffected (conservative).
                        // Only applies to function-typed parameters (callbacks).
                        if SIRType.isPolyFunOrFun(l.param.tp) then
                            collectTypeVarsFrom(l.param.tp)
                                .foreach(tv => upgradeKind(tv, CanBeListAffected))
                }
                current = l.term
            }
        }

        private def registerTypeParams(tps: List[SIRType.TypeVar]): Unit =
            for tv <- tps if tv.kind != Transparent do
                allScalaTypeVars += tvKey(tv)

        private def extractTypeParamsAndParams(
            lam: SIR.LamAbs
        ): (List[SIRType.TypeVar], List[SIR.Var]) = {
            val typeParams = lam.typeParams
            val params = mutable.ListBuffer.empty[SIR.Var]
            var current: SIR = lam
            while current.isInstanceOf[SIR.LamAbs] do {
                val l = current.asInstanceOf[SIR.LamAbs]
                params += l.param
                current = l.term
            }
            (typeParams, params.toList)
        }

        // ------- Constraint detection -------

        private def isListLikeDecl(data: DataDecl): Boolean =
            listLikeDeclNames.contains(data.name)

        private def markElementTypeVars(constrType: SIRType): Unit = {
            val typeArgs = extractTypeArgs(constrType)
            if debug then {
                val tvs = typeArgs.flatMap(collectTypeVarsFrom)
                if tvs.nonEmpty then
                    println(s"  markElement: ${constrType.show} → TVs: ${tvs.map(tv => s"${tv.name}#${tv.optId.getOrElse(0)}").mkString(", ")}")
            }
            for arg <- typeArgs do
                collectTypeVarsFrom(arg).foreach(tv => upgradeKind(tv, CanBeListAffected))
        }

        private def checkMatchForList(scrutineeType: SIRType): Unit = {
            extractDataDecl(scrutineeType) match {
                case Some((decl, typeArgs)) if isListLikeDecl(decl) =>
                    if debug then {
                        val tvs = typeArgs.flatMap(collectTypeVarsFrom)
                        if tvs.nonEmpty then
                            println(s"  matchList: ${scrutineeType.show} → TVs: ${tvs.map(tv => s"${tv.name}#${tv.optId.getOrElse(0)}").mkString(", ")}")
                    }
                    for arg <- typeArgs do
                        collectTypeVarsFrom(arg).foreach(tv => upgradeKind(tv, CanBeListAffected))
                case _ =>
            }
        }

        // ------- Call propagation (demand-driven) -------

        private def checkCallPropagation(
            f: SIR,
            arg: SIR,
            scope: Map[String, FunctionInfo]
        ): Unit = {
            val (rootVar, _) = peelApplyChain(f, List(arg))
            val calleeName = rootVar match {
                case Some(v: SIR.Var)         => Some(v.name)
                case Some(v: SIR.ExternalVar) => Some(v.name)
                case _                        => None
            }
            calleeName.foreach { name =>
                scope.get(name) match {
                    case Some(info) =>
                        val id: AnyRef = info.body.asInstanceOf[AnyRef]
                        if !analyzed.contains(id) && !inProgress.contains(id) then {
                            inProgress += id
                            info.body match {
                                case lam: SIR.LamAbs => checkFunctionalInterfaceParams(lam)
                                case _               =>
                            }
                            analyzeSir(info.body, scope)
                            inProgress -= id
                            analyzed += id
                        }
                    case None =>
                }
            }
        }

        private def peelApplyChain(f: SIR, args: List[SIR]): (Option[SIR], List[SIR]) = f match {
            case SIR.Apply(inner, arg, _, _) => peelApplyChain(inner, arg :: args)
            case v: SIR.Var                  => (Some(v), args)
            case v: SIR.ExternalVar          => (Some(v), args)
            case b: SIR.Builtin              => (Some(b), args)
            case _                           => (None, args)
        }

        // ------- Helpers -------

        private def tvKey(tv: SIRType.TypeVar): TVKey = (tv.name, tv.optId)

        private def upgradeKind(tv: SIRType.TypeVar, kind: TypeVarKind): Unit = {
            val key = find(tvKey(tv))
            val current = computedKinds.getOrElse(key, Transparent)
            computedKinds(key) = kindMax(current, kind)
        }

        private def collectTypeVarsFrom(tp: SIRType): Set[SIRType.TypeVar] = {
            val result = mutable.Set.empty[SIRType.TypeVar]
            val visited = new java.util.IdentityHashMap[SIRType.TypeProxy, Boolean]()

            def go(t: SIRType): Unit = t match {
                case tv: SIRType.TypeVar => result += tv
                case SIRType.Fun(in, out) =>
                    go(in); go(out)
                case SIRType.CaseClass(_, typeArgs, parent) =>
                    typeArgs.foreach(go)
                    parent.foreach(go)
                case SIRType.SumCaseClass(_, typeArgs) =>
                    typeArgs.foreach(go)
                case SIRType.TypeLambda(_, body) =>
                    go(body)
                case tp: SIRType.TypeProxy =>
                    if tp.ref != null && !visited.containsKey(tp) then {
                        visited.put(tp, true)
                        go(tp.ref)
                    }
                case _ =>
            }
            go(tp)
            result.toSet
        }

        private def extractTypeArgs(tp: SIRType): List[SIRType] = tp match {
            case SIRType.CaseClass(_, typeArgs, _)        => typeArgs
            case SIRType.SumCaseClass(_, typeArgs)         => typeArgs
            case tp: SIRType.TypeProxy if tp.ref != null   => extractTypeArgs(tp.ref)
            case _                                         => Nil
        }

        private def extractDataDecl(tp: SIRType): Option[(DataDecl, List[SIRType])] = tp match {
            case SIRType.SumCaseClass(decl, typeArgs)  => Some((decl, typeArgs))
            case SIRType.CaseClass(_, _, Some(parent)) => extractDataDecl(parent)
            case tp: SIRType.TypeProxy if tp.ref != null => extractDataDecl(tp.ref)
            case _                                     => None
        }

        // ------- Rebuild -------

        def rebuild(sir: SIR): SIR = sir match {
            case SIR.Decl(data, term) => SIR.Decl(data, rebuild(term))
            case expr: AnnotatedSIR   => rebuildExpr(expr)
        }

        private def rebuildExpr(sir: AnnotatedSIR): AnnotatedSIR = sir match {
            case SIR.LamAbs(param, term, typeParams, anns) =>
                SIR.LamAbs(rebuildVar(param), rebuild(term), typeParams.map(replaceKind), anns)
            case SIR.Let(bindings, body, flags, anns) =>
                SIR.Let(
                  bindings.map(b => Binding(b.name, rebuildType(b.tp), rebuild(b.value))),
                  rebuild(body),
                  flags,
                  anns
                )
            case SIR.Apply(f, arg, tp, anns) =>
                SIR.Apply(rebuildExpr(f), rebuildExpr(arg), rebuildType(tp), anns)
            case SIR.Constr(name, data, args, tp, anns) =>
                SIR.Constr(name, data, args.map(rebuild), rebuildType(tp), anns)
            case SIR.Match(scrutinee, cases, tp, anns) =>
                SIR.Match(
                  rebuildExpr(scrutinee),
                  cases.map(c => SIR.Case(rebuildPattern(c.pattern), rebuild(c.body), c.anns)),
                  rebuildType(tp),
                  anns
                )
            case SIR.Var(name, tp, anns) =>
                SIR.Var(name, rebuildType(tp), anns)
            case SIR.ExternalVar(module, name, tp, anns) =>
                SIR.ExternalVar(module, name, rebuildType(tp), anns)
            case SIR.Select(scrutinee, field, tp, anns) =>
                SIR.Select(rebuild(scrutinee), field, rebuildType(tp), anns)
            case SIR.Cast(term, tp, anns) =>
                SIR.Cast(rebuildExpr(term), rebuildType(tp), anns)
            case SIR.And(a, b, anns) =>
                SIR.And(rebuildExpr(a), rebuildExpr(b), anns)
            case SIR.Or(a, b, anns) =>
                SIR.Or(rebuildExpr(a), rebuildExpr(b), anns)
            case SIR.Not(a, anns) =>
                SIR.Not(rebuildExpr(a), anns)
            case SIR.IfThenElse(cond, t, f, tp, anns) =>
                SIR.IfThenElse(rebuildExpr(cond), rebuildExpr(t), rebuildExpr(f), rebuildType(tp), anns)
            case SIR.Error(msg, anns, cause) =>
                SIR.Error(rebuildExpr(msg), anns, cause)
            case c: SIR.Const   => c
            case b: SIR.Builtin => b
        }

        private def rebuildVar(v: SIR.Var): SIR.Var =
            SIR.Var(v.name, rebuildType(v.tp), v.anns)

        private def rebuildType(tp: SIRType): SIRType =
            rebuildTypeImpl(tp, new java.util.IdentityHashMap())

        private def rebuildTypeImpl(
            tp: SIRType,
            proxyMap: java.util.IdentityHashMap[SIRType.TypeProxy, SIRType.TypeProxy]
        ): SIRType = tp match {
            case tv: SIRType.TypeVar => replaceKind(tv)
            case SIRType.Fun(in, out) =>
                val newIn = rebuildTypeImpl(in, proxyMap)
                val newOut = rebuildTypeImpl(out, proxyMap)
                if (newIn eq in) && (newOut eq out) then tp
                else SIRType.Fun(newIn, newOut)
            case tl @ SIRType.TypeLambda(params, body) =>
                val newParams = params.map(replaceKind)
                val newBody = rebuildTypeImpl(body, proxyMap)
                val paramsChanged = newParams.zip(params).exists((n, o) => !(n eq o))
                if !paramsChanged && (newBody eq body) then tp
                else SIRType.TypeLambda(newParams, newBody)
            case SIRType.CaseClass(cd, typeArgs, parent) =>
                val newArgs = typeArgs.map(rebuildTypeImpl(_, proxyMap))
                val newParent = parent.map(rebuildTypeImpl(_, proxyMap))
                val argsChanged = newArgs.zip(typeArgs).exists((n, o) => !(n eq o))
                val parentChanged = (newParent, parent) match {
                    case (Some(n), Some(o)) => !(n eq o)
                    case _                 => false
                }
                if !argsChanged && !parentChanged then tp
                else SIRType.CaseClass(cd, newArgs, newParent)
            case SIRType.SumCaseClass(decl, typeArgs) =>
                val newArgs = typeArgs.map(rebuildTypeImpl(_, proxyMap))
                val changed = newArgs.zip(typeArgs).exists((n, o) => !(n eq o))
                if !changed then tp
                else SIRType.SumCaseClass(decl, newArgs)
            case tp: SIRType.TypeProxy =>
                if tp.ref == null then tp
                else {
                    val existing = proxyMap.get(tp)
                    if existing != null then existing
                    else {
                        proxyMap.put(tp, tp) // cycle guard
                        val newRef = rebuildTypeImpl(tp.ref, proxyMap)
                        if newRef eq tp.ref then tp
                        else {
                            val newProxy = new SIRType.TypeProxy(null)
                            proxyMap.put(tp, newProxy)
                            newProxy.ref = newRef
                            newProxy
                        }
                    }
                }
            case other => other
        }

        private def replaceKind(tv: SIRType.TypeVar): SIRType.TypeVar = {
            if tv.kind == Transparent then tv // don't change builtins
            else {
                val key = tvKey(tv)
                // Only change TypeVars that were registered as function typeParams
                if !allScalaTypeVars.contains(key) then tv
                else {
                    val canonical = find(key)
                    val newKind = computedKinds.getOrElse(canonical, Transparent)
                    if newKind != tv.kind then {
                        if debug then
                            println(s"  TypeVar ${tv.name}#${tv.optId.getOrElse(0)}: ${tv.kind} → $newKind")
                        SIRType.TypeVar(tv.name, tv.optId, newKind)
                    } else tv
                }
            }
        }

        private def rebuildPattern(p: SIR.Pattern): SIR.Pattern = p match {
            case SIR.Pattern.Constr(constr, bindings, typeParamsBindings) =>
                SIR.Pattern.Constr(constr, bindings, typeParamsBindings.map(rebuildType))
            case other => other
        }

        // ------- Statistics -------

        def printDefaultReprDetails(): Unit = {
            // Find function names for DefaultRepresentation TypeVars
            for key <- allScalaTypeVars do {
                val canonical = find(key)
                computedKinds.get(canonical) match {
                    case Some(DefaultRepresentation) =>
                        val funcName = functionForTypeVar.getOrElse(key, "unknown")
                        println(s"  DefaultRepr: ${key._1}#${key._2.getOrElse(0)} in $funcName")
                    case _ =>
                }
            }
        }

        /** Maps TypeVar key → function name it was registered from. */
        val functionForTypeVar = mutable.Map.empty[TVKey, String]

        def computeStats(): Stats = {
            var transparent = 0
            var defaultRepr = 0
            var listAffected = 0
            for key <- allScalaTypeVars do {
                val canonical = find(key)
                computedKinds.get(canonical) match {
                    case Some(CanBeListAffected)     => listAffected += 1
                    case Some(DefaultRepresentation) => defaultRepr += 1
                    case _                           => transparent += 1
                }
            }
            Stats(allScalaTypeVars.size, transparent, defaultRepr, listAffected)
        }
    }
}
