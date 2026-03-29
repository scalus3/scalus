package scalus.compiler.sir

import scalus.compiler.sir.SIR.*

import java.util.{IdentityHashMap => JIdentityHashMap}

/** SIR→SIR transformation that extracts Nil constants from recursive generic function bodies into
  * extra lambda parameters (prepended as first parameters).
  *
  * UPLC has no polymorphism — an empty list constant must have a concrete element type. Inside a
  * recursive generic function, TypeVars are unresolved, so Nil defaults to `List[Data]`. With
  * `nativeTypeVarRepresentation`, callers pass native values, causing type mismatch.
  *
  * The actual Nil type is determined from the **parent node's expected type** (`tp` field of
  * enclosing IfThenElse, Match, etc.), not from Nil itself (which is always `List[Nothing]`).
  *
  * Two phases:
  *   1. Collect (bottom-up): for each LetRec with TypeVars, find distinct Nil types by tracking
  *      expected types from parent nodes
  *   2. Transform (single top-down pass): add __nil parameters, replace Nil, augment call sites
  */
object ExtractNilParameter {

    private val AllNilNames: Set[String] = Set(
      SIRType.List.NilConstr.name,
      "scalus.uplc.builtin.BuiltinList$.Nil",
      SIRType.PairList.PairNilName
    )

    /** Annotation key placed on Apply nodes created by this transformation (nil injection). */
    val ExtractNilApplyAnnotation: String = "extractNilApply"

    /** Check if an Apply node was created by ExtractNilParameter (nil injection). */
    def isNilInjectionApply(anns: AnnotationsDecl): Boolean =
        anns.data.contains(ExtractNilApplyAnnotation)

    private val nilInjectionAnns: AnnotationsDecl =
        AnnotationsDecl(SIRPosition.empty, data = Map(ExtractNilApplyAnnotation -> SIR.Const(scalus.uplc.Constant.Bool(true), SIRType.Boolean, AnnotationsDecl(SIRPosition.empty))))

    /** Scope entry for a transformed binding. */
    private case class ScopeEntry(
        nilArgs: scala.List[(SIRType, SIR)], // (nilType, argToPass) — Var(__nil) inside rhs, Constr(Nil) externally
        newFunTp: SIRType // function type with nil params prepended
    )

    def apply(sir: SIR): SIR = {
        // Phase 1a: collect direct nil types
        val needsNil = new JIdentityHashMap[Let, scala.List[SIRType]]()
        collect(sir, needsNil, Set.empty)

        // Phase 1b: propagate nil needs transitively — if function A calls
        // function B which needs nil, A also needs nil to forward to B.
        propagateNilNeeds(sir, needsNil)

        needsNil.forEach((k, v) =>
            System.err.println(s"[ExtractNil] collected: ${k.bindings.map(_.name).mkString(",")} nilTypes=${v.map(_.show).mkString(",")}")
        )
        if needsNil.isEmpty then sir
        else
            // Phase 2: single top-down transform
            go(sir, needsNil, Map.empty, None, None)
    }

    // ===================== Phase 1: Collect =====================

    /** Check if a type contains any TypeVar from the given set. */
    private def containsAnyTypeVar(tp: SIRType, tvs: Set[SIRType.TypeVar]): Boolean =
        if tvs.isEmpty then false
        else containsTypeVarFrom(tp, tvs)

    private def containsTypeVarFrom(tp: SIRType, tvs: Set[SIRType.TypeVar]): Boolean = tp match
        case tv: SIRType.TypeVar              => tvs.exists(_.name == tv.name)
        case SIRType.Fun(in, out)             => containsTypeVarFrom(in, tvs) || containsTypeVarFrom(out, tvs)
        case SIRType.TypeLambda(_, body)      => containsTypeVarFrom(body, tvs)
        case SIRType.SumCaseClass(_, tArgs)   => tArgs.exists(containsTypeVarFrom(_, tvs))
        case SIRType.CaseClass(_, tArgs, _)   => tArgs.exists(containsTypeVarFrom(_, tvs))
        case SIRType.TypeProxy(ref) if ref != null => containsTypeVarFrom(ref, tvs)
        case _                                => false

    private def hasTypeParams(rhs: SIR): Boolean = rhs match
        case LamAbs(_, _, tps, _) => tps.nonEmpty
        case _                    => false

    private def getTypeParams(rhs: SIR): scala.List[SIRType.TypeVar] = rhs match
        case LamAbs(_, _, tps, _) => tps
        case _                    => scala.Nil

    /** Collect nil types from a SIR tree, tracking expected types from parent nodes.
      * @param inScopeTVs TypeVars from enclosing functions' type params
      * @return set of function names that need nil (for propagation to callers)
      */
    private def collect(
        sir: SIR,
        needsNil: JIdentityHashMap[Let, scala.List[SIRType]],
        inScopeTVs: Set[SIRType.TypeVar]
    ): Set[String] = sir match

        // Single-binding Let (recursive or not) with TypeVars — check for Nil
        case let @ Let(scala.List(Binding(name, _, rhs)), body, flags, _)
            if hasTypeParams(rhs) =>
            val tps = getTypeParams(rhs)
            val innerTVs = inScopeTVs ++ tps
            val childNamesRhs = collect(rhs, needsNil, innerTVs)
            val childNamesBody = collect(body, needsNil, innerTVs)
            val childNames = childNamesRhs ++ childNamesBody

            val nilTypes = collectNilTypes(rhs, None, innerTVs)
            val calledNilFns = calledNames(rhs).intersect(childNames)

            if nilTypes.nonEmpty || calledNilFns.nonEmpty then
                val allNilTypes = nilTypes.distinct
                if allNilTypes.nonEmpty then
                    needsNil.put(let, allNilTypes)
                childNames + name
            else childNames

        case Let(bindings, body, _, _) =>
            bindings.flatMap(b => collect(b.value, needsNil, inScopeTVs)).toSet ++
                collect(body, needsNil, inScopeTVs)
        case LamAbs(_, term, tps, _) =>
            collect(term, needsNil, inScopeTVs ++ tps)
        case Apply(f, arg, _, _) =>
            collect(f, needsNil, inScopeTVs) ++ collect(arg, needsNil, inScopeTVs)
        case Select(s, _, _, _)        => collect(s, needsNil, inScopeTVs)
        case IfThenElse(c, t, f, _, _) =>
            collect(c, needsNil, inScopeTVs) ++ collect(t, needsNil, inScopeTVs) ++ collect(f, needsNil, inScopeTVs)
        case And(a, b, _)             => collect(a, needsNil, inScopeTVs) ++ collect(b, needsNil, inScopeTVs)
        case Or(a, b, _)              => collect(a, needsNil, inScopeTVs) ++ collect(b, needsNil, inScopeTVs)
        case Not(a, _)                => collect(a, needsNil, inScopeTVs)
        case Match(scr, cases, _, _)  =>
            collect(scr, needsNil, inScopeTVs) ++ cases.flatMap(c => collect(c.body, needsNil, inScopeTVs))
        case Constr(_, _, args, _, _)  => args.flatMap(a => collect(a, needsNil, inScopeTVs)).toSet
        case Cast(expr, _, _)         => collect(expr, needsNil, inScopeTVs)
        case Decl(_, term)            => collect(term, needsNil, inScopeTVs)
        case _                        => Set.empty

    /** Walk a SIR tree tracking expected types from parent nodes. When hitting Nil,
      * if the expected type contains in-scope TypeVars, record it.
      */
    private def collectNilTypes(
        sir: SIR,
        expectedType: Option[SIRType],
        inScopeTVs: Set[SIRType.TypeVar]
    ): scala.List[SIRType] = sir match
        case Constr(name, _, _, _, _) if AllNilNames.contains(name) =>
            expectedType match
                case Some(tp) if containsAnyTypeVar(tp, inScopeTVs) =>
                    scala.List(tp)
                case _ => scala.Nil

        case IfThenElse(c, t, f, tp, _) =>
            collectNilTypes(c, None, inScopeTVs) ++
                collectNilTypes(t, Some(tp), inScopeTVs) ++
                collectNilTypes(f, Some(tp), inScopeTVs)

        case Match(scr, cases, tp, _) =>
            collectNilTypes(scr, None, inScopeTVs) ++
                cases.flatMap(c => collectNilTypes(c.body, Some(tp), inScopeTVs))

        case Cast(expr, tp, _) =>
            collectNilTypes(expr, Some(tp), inScopeTVs)

        case Apply(f, arg, _, _) =>
            // For the arg, expected type comes from f's parameter type
            val argExpected = f.tp match
                case SIRType.Fun(in, _)             => Some(in)
                case SIRType.TypeLambda(_, SIRType.Fun(in, _)) => Some(in)
                case _                              => None
            collectNilTypes(f, None, inScopeTVs) ++
                collectNilTypes(arg, argExpected, inScopeTVs)

        case Let(bindings, body, _, _) =>
            bindings.flatMap(b => collectNilTypes(b.value, None, inScopeTVs)) ++
                collectNilTypes(body, expectedType, inScopeTVs)
        case LamAbs(_, term, tps, _) =>
            collectNilTypes(term, None, inScopeTVs ++ tps)
        case Select(s, _, _, _)   => collectNilTypes(s, None, inScopeTVs)
        case And(a, b, _)         => collectNilTypes(a, None, inScopeTVs) ++ collectNilTypes(b, None, inScopeTVs)
        case Or(a, b, _)          => collectNilTypes(a, None, inScopeTVs) ++ collectNilTypes(b, None, inScopeTVs)
        case Not(a, _)            => collectNilTypes(a, None, inScopeTVs)
        case Constr(cname, data, args, tp, _) =>
            // For Cons(head, tail), the tail arg has the same list type as the Cons result
            val argExpectedTypes = data.constructors
                .find(_.name == cname)
                .map(_.params.map(p => Some(p.tp)))
                .getOrElse(args.map(_ => None))
            args.zip(argExpectedTypes.padTo(args.size, None)).flatMap {
                case (a, exp) => collectNilTypes(a, exp.orElse(expectedType), inScopeTVs)
            }
        case Decl(_, term)        => collectNilTypes(term, None, inScopeTVs)
        case _                    => scala.Nil

    /** Collect names referenced as Var/ExternalVar in a SIR tree. */
    private def calledNames(sir: SIR): Set[String] = {
        var names = Set.empty[String]
        def walk(s: SIR): Unit = s match
            case Var(n, _, _)              => names += n
            case ExternalVar(_, n, _, _)   => names += n
            case Let(bindings, body, _, _) => bindings.foreach(b => walk(b.value)); walk(body)
            case LamAbs(_, term, _, _)     => walk(term)
            case Apply(f, arg, _, _)       => walk(f); walk(arg)
            case Select(s, _, _, _)        => walk(s)
            case IfThenElse(c, t, f, _, _) => walk(c); walk(t); walk(f)
            case And(a, b, _)              => walk(a); walk(b)
            case Or(a, b, _)               => walk(a); walk(b)
            case Not(a, _)                 => walk(a)
            case Match(scr, cases, _, _)   => walk(scr); cases.foreach(c => walk(c.body))
            case Constr(_, _, args, _, _)   => args.foreach(walk)
            case Cast(expr, _, _)          => walk(expr)
            case Decl(_, term)             => walk(term)
            case _                         =>
        walk(sir)
        names
    }

    // ===================== Phase 1b: Propagate nil needs transitively =====================

    /** Build a map from binding name → (Let, nilTypes) for all entries in needsNil,
      * plus a map from binding name → Set[calledName] for all Let bindings with TypeParams.
      * Then iteratively propagate: if A calls B which needs nil, A also needs nil.
      */
    private def propagateNilNeeds(
        sir: SIR,
        needsNil: JIdentityHashMap[Let, scala.List[SIRType]]
    ): Unit = {
        import scala.jdk.CollectionConverters.*

        // Build name → Let index
        val nameToLet = new scala.collection.mutable.HashMap[String, Let]()
        // Build name → set of ExternalVar names called from its rhs
        val callGraph = new scala.collection.mutable.HashMap[String, Set[String]]()

        // Walk the SIR tree to find all Let bindings with TypeParams
        def indexLets(s: SIR): Unit = s match
            case let @ Let(scala.List(Binding(name, _, rhs)), body, _, _) if hasTypeParams(rhs) =>
                nameToLet(name) = let
                callGraph(name) = calledNames(rhs)
                indexLets(rhs)
                indexLets(body)
            case Let(bindings, body, _, _) =>
                bindings.foreach(b => indexLets(b.value))
                indexLets(body)
            case LamAbs(_, term, _, _)     => indexLets(term)
            case Apply(f, arg, _, _)       => indexLets(f); indexLets(arg)
            case IfThenElse(c, t, f, _, _) => indexLets(c); indexLets(t); indexLets(f)
            case Match(scr, cases, _, _)   => indexLets(scr); cases.foreach(c => indexLets(c.body))
            case Constr(_, _, args, _, _)   => args.foreach(indexLets)
            case Cast(expr, _, _)          => indexLets(expr)
            case And(a, b, _)              => indexLets(a); indexLets(b)
            case Or(a, b, _)               => indexLets(a); indexLets(b)
            case Not(a, _)                 => indexLets(a)
            case Select(s, _, _, _)        => indexLets(s)
            case Decl(_, term)             => indexLets(term)
            case _                         =>
        indexLets(sir)

        // Names of functions that currently need nil
        val nilNames = needsNil.entrySet().asScala
            .flatMap(e => e.getKey.bindings.map(_.name))
            .toSet

        // Iterate: propagate nil needs from callees to callers
        var changed = true
        while changed do
            changed = false
            for (callerName, calledFns) <- callGraph do
                val callerLet = nameToLet.get(callerName)
                callerLet match
                    case Some(let) if !needsNil.containsKey(let) =>
                        // Check if caller calls any nil-needing function
                        val calledNilNames = calledFns.intersect(nilNames)
                        if calledNilNames.nonEmpty then
                            // Propagate: collect nil types from called functions,
                            // deduplicate by structural type match (not ==)
                            val raw = calledNilNames.toList.flatMap { cn =>
                                nameToLet.get(cn).flatMap(l => Option(needsNil.get(l))).getOrElse(scala.Nil)
                            }
                            val propagated = raw.foldLeft(scala.List.empty[SIRType]) { (acc, tp) =>
                                if acc.exists(t => typesMatch(t, tp)) then acc
                                else acc :+ tp
                            }
                            if propagated.nonEmpty then
                                needsNil.put(let, propagated)
                                changed = true
                    case _ => // already has nil or not found
    }

    // ===================== Phase 2: Transform (single top-down pass) =====================

    /** Compute result type of applying one argument to a function type. Preserves TypeLambda. */
    private def applyResultType(funTp: SIRType): SIRType = funTp match
        case SIRType.TypeLambda(params, SIRType.Fun(_, out)) => SIRType.TypeLambda(params, out)
        case SIRType.Fun(_, out) => out
        case other               => other

    /** Count curried parameters in a LamAbs chain. */
    private def paramCount(sir: SIR): Int = sir match
        case LamAbs(_, term, _, _) => 1 + paramCount(term)
        case _                     => 0

    /** Single top-down pass. Handles:
      *   - LetRec restructuring (add __nil params)
      *   - Nil replacement (Nil → __nil var, based on expectedType matching)
      *   - Var/ExternalVar injection (Var(f) → Apply(Var(f', newTp), __nil_0, ...))
      *   - Shadowing (LamAbs/Let/Match pattern bindings remove names from scope)
      *
      * @param enclosingEntry scope entry for the immediately enclosing nil-needing function (for Nil replacement)
      * @param expectedType expected type from parent node (for determining Nil's actual type)
      */
    /** Extract TypeVar→ConcreteType substitutions from a function application.
      * Collects from both return type and parameter type vs argument type.
      */
    private def extractSubst(
        funTp: SIRType,
        applyTp: SIRType,
        argTp: Option[SIRType] = None
    ): Map[SIRType.TypeVar, SIRType] = {
        val (paramTp, returnTp) = funTp match
            case SIRType.TypeLambda(_, SIRType.Fun(in, out)) => (Some(in), out)
            case SIRType.Fun(in, out)                        => (Some(in), out)
            case _                                           => return Map.empty
        val returnSubst = collectSubst(returnTp, applyTp)
        val paramSubst = (paramTp, argTp) match
            case (Some(p), Some(a)) => collectSubst(p, a)
            case _                  => Map.empty
        returnSubst ++ paramSubst
    }

    /** Structurally match two types to extract TypeVar bindings. */
    private def collectSubst(
        declared: SIRType,
        actual: SIRType
    ): Map[SIRType.TypeVar, SIRType] = (declared, actual) match
        case (tv: SIRType.TypeVar, concrete) if !concrete.isInstanceOf[SIRType.TypeVar] =>
            Map(tv -> concrete)
        case (SIRType.Fun(dIn, dOut), SIRType.Fun(aIn, aOut)) =>
            collectSubst(dIn, aIn) ++ collectSubst(dOut, aOut)
        case (SIRType.SumCaseClass(dDecl, dArgs), SIRType.SumCaseClass(aDecl, aArgs))
            if dDecl.name == aDecl.name && dArgs.size == aArgs.size =>
            dArgs.zip(aArgs).flatMap((d, a) => collectSubst(d, a)).toMap
        case (SIRType.CaseClass(dC, dArgs, _), SIRType.CaseClass(aC, aArgs, _))
            if dC.name == aC.name && dArgs.size == aArgs.size =>
            dArgs.zip(aArgs).flatMap((d, a) => collectSubst(d, a)).toMap
        case (SIRType.TypeLambda(_, dBody), SIRType.TypeLambda(_, aBody)) =>
            collectSubst(dBody, aBody)
        case _ => Map.empty

    /** Apply type substitutions to a SIRType. */
    private def substituteType(tp: SIRType, subst: Map[SIRType.TypeVar, SIRType]): SIRType =
        if subst.isEmpty then tp
        else tp match
            case tv: SIRType.TypeVar =>
                subst.find((k, _) => k.name == tv.name).map(_._2).getOrElse(tp)
            case SIRType.Fun(in, out) =>
                SIRType.Fun(substituteType(in, subst), substituteType(out, subst))
            case SIRType.SumCaseClass(decl, args) =>
                SIRType.SumCaseClass(decl, args.map(substituteType(_, subst)))
            case SIRType.CaseClass(c, args, parent) =>
                SIRType.CaseClass(c, args.map(substituteType(_, subst)), parent.map(substituteType(_, subst)))
            case SIRType.TypeLambda(params, body) =>
                SIRType.TypeLambda(params, substituteType(body, subst))
            case _ => tp

    private def go(
        sir: SIR,
        needsNil: JIdentityHashMap[Let, scala.List[SIRType]],
        scope: Map[String, ScopeEntry],
        enclosingEntry: Option[ScopeEntry],
        expectedType: Option[SIRType],
        typeSubst: Map[SIRType.TypeVar, SIRType] = Map.empty
    ): SIR = sir match

        // ---- Let (recursive or not) that needs transformation ----
        case let @ Let(scala.List(Binding(name, tp, rhs)), body, flags, anns)
            if needsNil.containsKey(let) =>

            val nilContextTypes = needsNil.get(let) // context types like List[A]
            // Use the context type (e.g. List[A]) for both parameter and call-site Nil.
            // During lowering, when A=BigInt resolves, this becomes List[Integer].
            // Use function name in nil var names to avoid shadowing across scopes.
            val shortName = name.lastIndexOf('.') match
                case -1  => name
                case dot => name.substring(dot + 1)
            val nilVars: scala.List[(SIRType, Var)] = nilContextTypes.zipWithIndex.map {
                case (ctxTp, idx) =>
                    val paramName = s"__nil_${shortName}_$idx"
                    (ctxTp, Var(paramName, ctxTp, AnnotationsDecl.empty))
            }

            // Compute new function type: prepend nil param(s) inside TypeLambda
            val newTp = tp match
                case SIRType.TypeLambda(params, bodyTp) =>
                    val extendedBody = nilVars.foldRight(bodyTp) { case ((ctxTp, _), acc) =>
                        SIRType.Fun(ctxTp, acc)
                    }
                    SIRType.TypeLambda(params, extendedBody)
                case other =>
                    nilVars.foldRight(other) { case ((ctxTp, _), acc) =>
                        SIRType.Fun(ctxTp, acc)
                    }

            // Build Nil constructors for external call sites — use context type.
            // Determine constructor name and data decl from the type (List vs PairList).
            val nilConstrs: scala.List[(SIRType, SIR)] = nilContextTypes.map { ctxTp =>
                val (constrName, dataDecl) = ctxTp match
                    case SIRType.SumCaseClass(decl, _) if decl.name == SIRType.PairList.DataDeclName =>
                        (SIRType.PairList.PairNilName, decl)
                    case SIRType.SumCaseClass(decl, _) =>
                        (SIRType.List.NilConstr.name, decl)
                    case _ =>
                        (SIRType.List.NilConstr.name, SIRType.List.dataDecl)
                (ctxTp, Constr(constrName, dataDecl, scala.Nil,
                    ctxTp, AnnotationsDecl.empty): SIR)
            }

            // Scope entry for inside rhs (self-calls use __nil vars)
            val rhsEntry = ScopeEntry(nilVars.map((tp, v) => (tp, v: SIR)), newTp)
            // Scope entry for let body (external calls use concrete Nil constructors)
            val bodyEntry = ScopeEntry(nilConstrs, newTp)

            val rhsScope = scope + (name -> rhsEntry)
            val bodyScope = scope + (name -> bodyEntry)

            // Transform rhs: wrap with nil LamAbs params, recurse into body
            val transformedRhs = rhs match
                case LamAbs(param, term, tps, lamAnns) =>
                    val transformedBody = go(term, needsNil, rhsScope, Some(rhsEntry), None)
                    // Wrap: LamAbs(__nil_0, LamAbs(__nil_1, ..., LamAbs(param, body)))
                    val innerLam = LamAbs(param, transformedBody, scala.Nil, lamAnns)
                    nilVars.foldRight(innerLam: SIR) { case ((_, nilVar), acc) =>
                        LamAbs(nilVar, acc, scala.Nil, lamAnns)
                    } match
                        // Move typeParams to outermost LamAbs
                        case outerLam: LamAbs =>
                            outerLam.copy(typeParams = tps)
                        case other => other
                case other =>
                    val transformed = go(other, needsNil, rhsScope, Some(rhsEntry), None)
                    nilVars.foldRight(transformed) { case ((_, nilVar), acc) =>
                        LamAbs(nilVar, acc, scala.Nil, anns)
                    }

            // Transform let body (external calls pass concrete Nil)
            val transformedBody = go(body, needsNil, bodyScope, enclosingEntry, expectedType)

            Let(scala.List(Binding(name, newTp, transformedRhs)), transformedBody, flags, anns)

        // ---- Nil constructor: replace with __nil from enclosing entry ----
        case Constr(name, _, _, tp, _) if AllNilNames.contains(name) =>
            enclosingEntry.flatMap { entry =>
                if entry.nilArgs.size == 1 then
                    // Single nil param — always use it
                    Some(entry.nilArgs.head._2)
                else
                    // Multiple nil params — match by expected type
                    expectedType.flatMap { expTp =>
                        entry.nilArgs.collectFirst {
                            case (nilTp, nilArg) if typesMatch(nilTp, expTp) => nilArg
                        }
                    }
            }.getOrElse(sir) // no enclosing entry → leave Nil unchanged

        // ---- Var/ExternalVar in scope: inject nil applications with type substitution ----
        case Var(n, _, vAnns) if scope.contains(n) =>
            val entry = scope(n)
            System.err.println(s"[inject] Var($n) typeSubst=${typeSubst.map((k,v) => s"${k.show}->${v.show}").mkString(", ")} nilArgs=${entry.nilArgs.size}")
            injectNilApplications(Var(n, entry.newFunTp, vAnns), entry, typeSubst)
        case ExternalVar(m, n, _, evAnns) if scope.contains(n) =>
            val entry = scope(n)
            System.err.println(s"[inject] ExternalVar($n) subst=${typeSubst.map((k,v) => s"${k.show}#${k.optId}->${v.show}").mkString(",")} nilArgTps=${entry.nilArgs.map((tp,_) => tp.show).mkString(",")}")
            injectNilApplications(ExternalVar(m, n, entry.newFunTp, evAnns), entry, typeSubst)

        // ---- Apply: extract type substitutions and recurse ----
        case Apply(f, arg, tp, anns) =>
            // Extract TypeVar substitutions from this Apply level
            val newSubst = extractSubst(f.tp, tp, Some(arg.tp))
            if newSubst.nonEmpty || typeSubst.nonEmpty then
                System.err.println(s"[Apply] f.tp=${f.tp.show} tp=${tp.show} newSubst=${newSubst.map((k,v) => s"${k.show}->${v.show}").mkString(",")} inherited=${typeSubst.map((k,v) => s"${k.show}->${v.show}").mkString(",")}")
            val mergedSubst = typeSubst ++ newSubst
            // Recurse with merged substitutions
            val newF = goA(f, needsNil, scope, enclosingEntry, None, mergedSubst)
            val argExpected = f.tp match
                case SIRType.Fun(in, _)                         => Some(in)
                case SIRType.TypeLambda(_, SIRType.Fun(in, _))  => Some(in)
                case _                                          => None
            val newArg = goA(arg, needsNil, scope, enclosingEntry, argExpected, mergedSubst)
            Apply(newF, newArg, tp, anns)

        // ---- IfThenElse/Match: pass tp as expectedType to branches ----
        case IfThenElse(c, t, f, tp, anns) =>
            IfThenElse(
              goA(c, needsNil, scope, enclosingEntry, None),
              goA(t, needsNil, scope, enclosingEntry, Some(tp)),
              goA(f, needsNil, scope, enclosingEntry, Some(tp)),
              tp, anns
            )
        case Match(scr, cases, tp, anns) =>
            Match(
              goA(scr, needsNil, scope, enclosingEntry, None),
              cases.map(c =>
                  val patternNames = c.pattern match
                      case Pattern.Constr(_, bindings, _) => bindings.toSet
                      case _                              => Set.empty[String]
                  val caseScope = patternNames.foldLeft(scope)(_ - _)
                  Case(c.pattern, go(c.body, needsNil, caseScope, enclosingEntry, Some(tp)), c.anns)
              ),
              tp, anns
            )

        // ---- Shadowing ----
        case LamAbs(param, term, tps, anns) =>
            val innerScope = scope - param.name
            LamAbs(param, go(term, needsNil, innerScope, enclosingEntry, None), tps, anns)

        case Let(bindings, body, flags, anns) =>
            val bindingNames = bindings.map(_.name).toSet
            val bodyScope = bindingNames.foldLeft(scope)(_ - _)
            val rhsScope = if flags.isRec then bodyScope else scope
            Let(
              bindings.map(b => Binding(b.name, b.tp, go(b.value, needsNil, rhsScope, enclosingEntry, None))),
              go(body, needsNil, bodyScope, enclosingEntry, expectedType),
              flags, anns
            )

        // ---- Everything else: plain recursion ----
        case Cast(expr, tp, anns) =>
            Cast(goA(expr, needsNil, scope, enclosingEntry, Some(tp)), tp, anns)
        case Select(s, field, tp, anns) =>
            Select(go(s, needsNil, scope, enclosingEntry, None), field, tp, anns)
        case And(a, b, anns) =>
            And(goA(a, needsNil, scope, enclosingEntry, None),
                goA(b, needsNil, scope, enclosingEntry, None), anns)
        case Or(a, b, anns) =>
            Or(goA(a, needsNil, scope, enclosingEntry, None),
               goA(b, needsNil, scope, enclosingEntry, None), anns)
        case Not(a, anns) =>
            Not(goA(a, needsNil, scope, enclosingEntry, None), anns)
        case Constr(cname, data, args, tp, anns) =>
            val argExpectedTypes = data.constructors
                .find(_.name == cname)
                .map(_.params.map(p => Some(p.tp)))
                .getOrElse(args.map(_ => None))
            val newArgs = args.zip(argExpectedTypes.padTo(args.size, None)).map {
                case (a, exp) => go(a, needsNil, scope, enclosingEntry, exp.orElse(expectedType), typeSubst)
            }
            Constr(cname, data, newArgs, tp, anns)
        case Decl(data, term) =>
            Decl(data, go(term, needsNil, scope, enclosingEntry, None))
        case _ => sir

    /** go for AnnotatedSIR */
    private def goA(
        sir: AnnotatedSIR,
        needsNil: JIdentityHashMap[Let, scala.List[SIRType]],
        scope: Map[String, ScopeEntry],
        enclosingEntry: Option[ScopeEntry],
        expectedType: Option[SIRType],
        typeSubst: Map[SIRType.TypeVar, SIRType] = Map.empty
    ): AnnotatedSIR =
        go(sir, needsNil, scope, enclosingEntry, expectedType, typeSubst).asInstanceOf[AnnotatedSIR]

    /** Turn `Var(f, newTp)` into `Apply(...Apply(Var(f, newTp), __nil_0)..., __nil_n)`.
      * Each application peels one nil parameter, so the final result type = original function type.
      */
    private def injectNilApplications(
        base: AnnotatedSIR,
        entry: ScopeEntry,
        typeSubst: Map[SIRType.TypeVar, SIRType]
    ): AnnotatedSIR = {
        var current: AnnotatedSIR = base
        var currentTp = entry.newFunTp
        for (ctxTp, nilArg) <- entry.nilArgs do
            val resultTp = applyResultType(currentTp)
            // Apply type substitution to the Nil's type
            val concreteNilArg = nilArg match
                case c: Constr if typeSubst.nonEmpty =>
                    val concreteTp = substituteType(c.tp, typeSubst)
                    System.err.println(s"[nilSubst] ${c.tp.show} → ${concreteTp.show}")
                    Constr(c.name, c.data, c.args, concreteTp, c.anns)
                case other => other
            current = Apply(current, concreteNilArg.asInstanceOf[AnnotatedSIR], resultTp, nilInjectionAnns)
            currentTp = resultTp
        current
    }

    /** Check if two SIR types match structurally (ignoring TypeVar IDs, comparing by name). */
    private def typesMatch(a: SIRType, b: SIRType): Boolean = (a, b) match
        case (SIRType.TypeVar(n1, _, _), SIRType.TypeVar(n2, _, _)) => n1 == n2
        case (SIRType.Fun(i1, o1), SIRType.Fun(i2, o2)) =>
            typesMatch(i1, i2) && typesMatch(o1, o2)
        case (SIRType.SumCaseClass(d1, a1), SIRType.SumCaseClass(d2, a2)) =>
            d1.name == d2.name && a1.size == a2.size && a1.zip(a2).forall((x, y) => typesMatch(x, y))
        case (SIRType.CaseClass(c1, a1, _), SIRType.CaseClass(c2, a2, _)) =>
            c1.name == c2.name && a1.size == a2.size && a1.zip(a2).forall((x, y) => typesMatch(x, y))
        case (SIRType.TypeLambda(_, b1), SIRType.TypeLambda(_, b2)) =>
            typesMatch(b1, b2)
        case _ => a == b
}
