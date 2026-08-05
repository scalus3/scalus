package scalus.compiler.sir.linking

import scalus.compiler.Options
import scalus.compiler.sir.*

import scala.collection.mutable

case class SIRLinkerOptions(
    useUniversalDataConversion: Boolean,
    /** If true, the linker will print errors to the console, otherwise they are accessible over
      * errorlog
      */
    printErrors: Boolean,
    debugLevel: Int,
)

object SIRLinkerOptions {
    def fromCompilerOptions(compilerOptions: Options): SIRLinkerOptions = {
        SIRLinkerOptions(
          useUniversalDataConversion =
              compilerOptions.targetLoweringBackend == TargetLoweringBackend.SirToUplcV3Lowering,
          printErrors = true,
          debugLevel = compilerOptions.debugLevel
        )
    }
}

/** Links SIR definitions and data declarations into a single SIR module.
  *
  * This class is responsible for linking SIR definitions and data declarations to create a single
  * SIR module.
  *
  * It traverses the SIR tree and links external definitions and data declarations to the global
  * definitions and data declarations.
  */
class SIRLinker(options: SIRLinkerOptions, moduleDefs: Map[String, Module]) {

    import SIRLinker.{LinkingDefState, SIRLinkedBinding}

    private val globalDefs: mutable.LinkedHashMap[String, LinkingDefState] =
        mutable.LinkedHashMap.empty
    private val globalDataDecls: mutable.LinkedHashMap[String, DataDecl] =
        mutable.LinkedHashMap.empty
    private val moduleDefsCache: mutable.Map[String, mutable.LinkedHashMap[String, Binding]] =
        mutable.LinkedHashMap.empty.withDefaultValue(mutable.LinkedHashMap.empty)

    private val debugLevel: Int = if options.debugLevel != 0 then options.debugLevel else 0

    private var errorLog: List[(String, SIRPosition)] = List.empty

    def retrieveErrors: List[(String, SIRPosition)] = {
        errorLog
    }

    // private val sirLoader = new SIRLoader(options.loaderOptions)

    private def error[A](message: String, pos: SIRPosition, defaultValue: A): A = {
        if options.printErrors then println(s"Error: $message at ${pos.show}")
        else errorLog :+= (message, pos)
        defaultValue
    }

    private def isPairListConversion(name: String): Boolean =
        SIRType.PairList.ConversionNames.contains(name)

    def link(sir: SIR, pos: SIRPosition): SIR = {
        if debugLevel > 1 then
            println(
              s"Linking SIR at ${pos.show}, options=$options, modules: ${moduleDefs.keys.mkString(", ")}"
            )
        val processed = traverseAndLink(sir, pos)
        // Preserve today's behavior for the error path: a def still in Linking
        // state means a linking failure; report and degrade like before.
        val states = globalDefs.values.toList
        val stillLinking = states.exists {
            case LinkingDefState.Linking => true
            case _                       => false
        }
        val full: SIR =
            if stillLinking then
                val message = s"Linking in progress at end of linking"
                error(message, pos, SIR.Error(message, AnnotationsDecl.empty.copy(pos = pos)))
            else {
                val linked = states.collect { case LinkingDefState.Linked(b) => b }
                val keys = linked.map(_.name).toSet
                val edges: Map[String, Set[String]] =
                    linked.map(b => b.name -> (collectGlobalRefs(b.body, keys) - b.name)).toMap
                val sccs = stronglyConnectedComponents(linked.map(_.name), edges)
                val groupOf: Map[String, List[String]] =
                    sccs.filter(_.sizeIs >= 2).flatMap(g => g.map(_ -> g)).toMap

                // Completion index: position in `linked`, i.e. the order defs
                // *finished* linking. findAndLinkDefinition fully links every
                // callee (depth-first, synchronously) before its caller
                // completes, so in an ACYCLIC reference graph this is already a
                // valid topological order: every def's dependencies have a
                // strictly smaller completion index than the def itself. Only
                // inside a cycle does that invariant not hold (cycle members can
                // reference each other regardless of index) - which is exactly
                // why a naive "emit in completion order, merge cycles at the
                // earliest member's slot" scheme is unsound: a def outside the
                // cycle but referenced only from a *later*-completing cycle
                // member (e.g. `isEven`'s body calls `isOdd` first, then a
                // plain `positive` helper) completes after that earliest
                // member and lands inside the group's body instead of
                // enclosing it - see the "group member calling a later-linked
                // helper" test.
                val completionIndex: Map[String, Int] =
                    linked.iterator.zipWithIndex.map((b, i) => b.name -> i).toMap

                // Condense the reference graph: every node is either a
                // singleton def or a whole SCC (cyclic group). `nodeRep` names
                // a node by its earliest-completing member - stable no matter
                // which member of the node you start from.
                def nodeMembers(name: String): List[String] = groupOf.getOrElse(name, List(name))
                def nodeRep(name: String): String = nodeMembers(name).minBy(completionIndex)
                val nodeRepOf: Map[String, String] =
                    linked.map(b => b.name -> nodeRep(b.name)).toMap
                val nodeReps: List[String] = linked.map(b => nodeRepOf(b.name)).distinct

                // Condensation edges (dependency direction), dropping
                // intra-SCC references - those are already resolved inside the
                // group's own recursive Let and would otherwise be self-loops.
                val nodeDeps: Map[String, Set[String]] = nodeReps.map { rep =>
                    val members = nodeMembers(rep).toSet
                    val deps = members
                        .flatMap(m => edges.getOrElse(m, Set.empty))
                        .filterNot(members.contains)
                        .map(nodeRepOf)
                    rep -> deps
                }.toMap
                val dependents: Map[String, Set[String]] =
                    nodeDeps.toList
                        .flatMap { case (rep, deps) => deps.map(dep => dep -> rep) }
                        .groupMap(_._1)(_._2)
                        .view
                        .mapValues(_.toSet)
                        .toMap
                val priority: Map[String, Int] =
                    nodeReps.map(rep => rep -> nodeMembers(rep).map(completionIndex).min).toMap

                // Kahn's algorithm: repeatedly emit the available node (all its
                // dependency-nodes already emitted) with the smallest
                // completion-index priority. The emitted order is
                // dependencies-first, i.e. outermost-first for the foldRight
                // below - a node's dependencies must already be in an
                // enclosing Let before the node's own body can reference them.
                //
                // Proof this reproduces today's behavior byte-for-byte on
                // acyclic input: in a fully acyclic graph every node is a
                // singleton, and (per completionIndex above) its dependencies
                // all have a strictly smaller index. By induction on emission
                // step: once every index < k has been emitted, the def with
                // index k has all of its dependencies (indices < k) already
                // emitted, so it is available; every other available def has
                // index >= k, so k is the smallest-priority available node and
                // is emitted next. The loop therefore emits exactly
                // 0, 1, 2, ..., n-1 - plain completion order - whenever the
                // graph has no cycles. Only an actual cycle (which crashed
                // unconditionally before this feature existed) can make the
                // order diverge from completion order.
                val remaining = mutable.Map.from(nodeReps.map(rep => rep -> nodeDeps(rep).size))
                val available = mutable.Set.from(nodeReps.filter(rep => remaining(rep) == 0))
                val order = mutable.ListBuffer.empty[String]
                while available.nonEmpty do
                    val next = available.minBy(rep => (priority(rep), rep))
                    available -= next
                    order += next
                    for dependent <- dependents.getOrElse(next, Set.empty) do
                        remaining(dependent) -= 1
                        if remaining(dependent) == 0 then available += dependent

                val slots: List[List[SIRLinkedBinding]] =
                    order.toList.map(rep => linked.filter(b => nodeRepOf(b.name) == rep))

                def asAnnotated(acc: SIR, name: String): AnnotatedSIR = acc match
                    case annssir: AnnotatedSIR => annssir
                    case _ =>
                        val msg = s"Unexpected Decl. In binding $name in SIRLinker.link"
                        error(msg, pos, SIR.Error(msg, AnnotationsDecl.empty.copy(pos = pos)))
                slots.foldRight(processed) {
                    case (List(b), acc) =>
                        SIR.Let(
                          List(Binding(b.name, b.declaredTp.getOrElse(b.body.tp), b.body)),
                          asAnnotated(acc, b.name),
                          b.flags,
                          AnnotationsDecl.empty.copy(pos = pos)
                        )
                    case (group, acc) =>
                        SIR.Let(
                          group.map(b =>
                              Binding(b.name, b.declaredTp.getOrElse(b.body.tp), b.body)
                          ),
                          asAnnotated(acc, group.head.name),
                          SIR.LetFlags.Recursivity,
                          AnnotationsDecl.empty.copy(pos = pos)
                        )
                }
            }
        val dataDecls = globalDataDecls.foldRight(full: SIR) { case ((_, decl), acc) =>
            SIR.Decl(decl, acc)
        }
        dataDecls
    }

    /** Names of global defs referenced from `sir` (syntactic, no shadow tracking: global names are
      * dot-qualified full names that locals never collide with).
      */
    private def collectGlobalRefs(sir: SIR, keys: Set[String]): Set[String] = {
        val acc = mutable.Set.empty[String]
        def go(s: SIR): Unit = s match
            case SIR.Decl(_, term)                 => go(term)
            case SIR.Var(name, _, _)               => if keys.contains(name) then acc += name
            case SIR.ExternalVar(_, name, _, _)    => if keys.contains(name) then acc += name
            case SIR.Let(bindings, body, _, _)     => bindings.foreach(b => go(b.value)); go(body)
            case SIR.LamAbs(_, term, _, _)         => go(term)
            case SIR.Apply(f, arg, _, _)           => go(f); go(arg)
            case SIR.Select(s1, _, _, _)           => go(s1)
            case SIR.IfThenElse(c, t, f, _, _)     => go(c); go(t); go(f)
            case SIR.And(a, b, _)                  => go(a); go(b)
            case SIR.Or(a, b, _)                   => go(a); go(b)
            case SIR.Not(a, _)                     => go(a)
            case SIR.Match(scrutinee, cases, _, _) => go(scrutinee); cases.foreach(c => go(c.body))
            case SIR.Constr(_, _, args, _, _)      => args.foreach(go)
            case SIR.Cast(expr, _, _)              => go(expr)
            case _: SIR.Builtin | _: SIR.Error | _: SIR.Const => ()
        go(sir)
        acc.toSet
    }

    /** Tarjan strongly connected components; nodes in `nodes` order, edges by name. */
    private def stronglyConnectedComponents(
        nodes: List[String],
        edges: Map[String, Set[String]]
    ): List[List[String]] = {
        val indexOf = mutable.Map.empty[String, Int]
        val lowlink = mutable.Map.empty[String, Int]
        val onStack = mutable.Set.empty[String]
        val stack = mutable.Stack.empty[String]
        val result = mutable.ListBuffer.empty[List[String]]
        var counter = 0

        def strongConnect(v: String): Unit = {
            indexOf(v) = counter
            lowlink(v) = counter
            counter += 1
            stack.push(v)
            onStack += v
            for w <- edges.getOrElse(v, Set.empty) do
                if !indexOf.contains(w) then
                    strongConnect(w)
                    lowlink(v) = math.min(lowlink(v), lowlink(w))
                else if onStack(w) then lowlink(v) = math.min(lowlink(v), indexOf(w))
            if lowlink(v) == indexOf(v) then
                val component = mutable.ListBuffer.empty[String]
                var w = ""
                while {
                    w = stack.pop()
                    onStack -= w
                    component += w
                    w != v
                } do ()
                result += component.toList
        }
        nodes.foreach(v => if !indexOf.contains(v) then strongConnect(v))
        result.toList
    }

    private def traverseAndLink(sir: SIR, pos: SIRPosition): SIR = sir match
        case SIR.Decl(data, term) =>
            SIR.Decl(data, traverseAndLink(term, pos))
        case ans: AnnotatedSIR =>
            traverseAndLinkExpr(ans, pos)

    private def traverseAndLinkExpr(sir: AnnotatedSIR, pos: SIRPosition): AnnotatedSIR = sir match
        case v @ SIR.ExternalVar(moduleName, name, tp, ann) if !globalDefs.contains(name) =>
            if moduleName == "scalus.uplc.builtin.internal.UniversalDataConversion$" then
                if name != "scalus.uplc.builtin.internal.UniversalDataConversion$.fromData" &&
                    name != "scalus.uplc.builtin.internal.UniversalDataConversion$.toData"
                then
                    val msg =
                        s"Unknown external variable in universal data conversion module: ${name}"
                    error(msg, ann.pos, v)
                // For fromData/toData, we allow them as ExternalVar here
                // They will be handled during UPLC lowering in Apply position
            else if isPairListConversion(name) then ()
            // PairList.toList and PairList.toPairList are always noops in UPLC lowering.
            // Skip linking to avoid dead-code let-bindings and their transitive dependencies.
            else linkDefinition(moduleName, name, pos, tp, ann)
            v
        case v @ SIR.Let(bindings, body, flags, anns) =>
            val nBingings =
                bindings.map(b => Binding(b.name, b.tp, traverseAndLink(b.value, pos)))
            val nBody = traverseAndLink(body, pos)
            SIR.Let(nBingings, nBody, flags, anns)
        case SIR.LamAbs(param, term, typeParams, anns) =>
            SIR.LamAbs(param, traverseAndLink(term, pos), typeParams, anns)
        case SIR.Apply(f, arg, tp, anns) =>
            val fReplaced =
                if options.useUniversalDataConversion then
                    anns.data.get("fromData") match
                        case Some(v) =>
                            SIR.ExternalVar(
                              "scalus.uplc.builtin.internal.UniversalDataConversion$",
                              "scalus.uplc.builtin.internal.UniversalDataConversion$.fromData",
                              SIRType.Fun(SIRType.Data.tp, tp),
                              AnnotationsDecl.empty.copy(pos = f.anns.pos)
                            )
                        case None =>
                            anns.data.get("toData") match
                                case Some(v) =>
                                    SIR.ExternalVar(
                                      "scalus.uplc.builtin.internal.UniversalDataConversion$",
                                      "scalus.uplc.builtin.internal.UniversalDataConversion$.toData",
                                      SIRType.Fun(arg.tp, SIRType.Data.tp),
                                      AnnotationsDecl.empty.copy(pos = f.anns.pos)
                                    )
                                case None => f
                else f
            val nF = traverseAndLinkExpr(fReplaced, pos)
            val nArg = traverseAndLinkExpr(arg, pos)
            SIR.Apply(nF, nArg, tp, anns)
        case SIR.And(lhs, rhs, anns) =>
            val nLhs = traverseAndLinkExpr(lhs, pos)
            val nRhs = traverseAndLinkExpr(rhs, pos)
            SIR.And(nLhs, nRhs, anns)
        case SIR.Or(lhs, rhs, anns) =>
            val nLhs = traverseAndLinkExpr(lhs, pos)
            val nRhs = traverseAndLinkExpr(rhs, pos)
            SIR.Or(nLhs, nRhs, anns)
        case SIR.Not(term, anns) => SIR.Not(traverseAndLinkExpr(term, pos), anns)
        case SIR.IfThenElse(cond, t, f, tp, anns) =>
            val nCond = traverseAndLinkExpr(cond, pos)
            val nT = traverseAndLinkExpr(t, pos)
            val nR = traverseAndLinkExpr(f, pos)
            SIR.IfThenElse(nCond, nT, nR, tp, anns)
        case SIR.Constr(name, data, args, tp, anns) =>
            globalDataDecls.put(data.name, data)
            val nArgs = args.map(a => traverseAndLink(a, pos))
            SIR.Constr(name, data, nArgs, tp, anns)
        case SIR.Match(scrutinee, cases, rhsType, anns) =>
            val nScrutinee = traverseAndLinkExpr(scrutinee, pos)
            val nCases =
                cases.map(c => SIR.Case(c.pattern, traverseAndLink(c.body, pos), c.anns))
            SIR.Match(nScrutinee, nCases, rhsType, anns)
        case SIR.Select(scrutinee, field, tp, anns) =>
            val nScrutinee = traverseAndLink(scrutinee, pos)
            SIR.Select(nScrutinee, field, tp, anns)
        case SIR.Cast(term, tp, anns) =>
            SIR.Cast(traverseAndLinkExpr(term, pos), tp, anns)
        case other => other

    private def findAndLinkDefinition(
        defs: collection.Map[String, Binding],
        fullName: String,
        tp: SIRType,
        srcPos: SIRPosition
    ): Boolean = {
        val found = defs.get(fullName)
        for binding <- found do
            globalDefs.update(fullName, LinkingDefState.Linking)
            val nSir = traverseAndLink(binding.value, srcPos)
            // Preserve declared type. Priority:
            //   1. Call-site type if it carries @UplcRepr annotations — those reflect the
            //      specific representation the caller asked for.
            //   2. The module's plugin-computed `Binding.tp` — this is the method's declared
            //      signature. Prefer it over `nSir.tp` because the body's `tp` can diverge
            //      when the body is a `Constr` whose type got wrapped by `prependTypeLambda`
            //      (e.g. for `def f[A]: T = Nothing-typed-body`, where `body.tp` ends up as
            //      `∀A. T[Nothing]` but the binding's declared tp is the correct `∀A. T[A]`).
            //   3. Otherwise fall through to `nSir.tp` at use site via `getOrElse` at line 77.
            def funContainsAnnotated(tp: SIRType): Boolean = tp match
                case _: SIRType.Annotated => true
                case SIRType.Fun(in, out) => funContainsAnnotated(in) || funContainsAnnotated(out)
                case SIRType.TypeLambda(_, body) => funContainsAnnotated(body)
                case _                           => false
            val declTp =
                if funContainsAnnotated(tp) then Some(tp)
                else Some(binding.tp)
            // TODO: research.  removing 'remove' triggers fail of  scalus.CompilerPluginTest. 'compile fieldAsData macro'
            globalDefs.remove(fullName)
            globalDefs.update(
              fullName,
              LinkingDefState.Linked(
                SIRLinkedBinding(fullName, SIR.LetFlags.Recursivity, nSir, declTp)
              )
            )
        found.isDefined
    }

    private def linkDefinition(
        moduleName: String,
        fullName: String,
        pos: SIRPosition,
        tp: SIRType,
        anns: AnnotationsDecl
    ): Unit = {
        // println(s"linkDefinition: ${fullName}")
        retrieveModule(moduleName, pos) match
            case Left(filename) =>
                error(
                  s"Module not found during linking: ${moduleName} , missing filename: ${filename} referenced for name ${fullName} from ${anns.pos.file}: ${anns.pos.startLine + 1}",
                  pos,
                  ()
                )
            case Right(defs) =>
                if !findAndLinkDefinition(defs, fullName, tp, pos) then
                    error(
                      s"Symbol not found during linking: ${fullName} in module ${moduleName} at ${pos.show}",
                      anns.pos,
                      ()
                    )
    }

    private def retrieveModule(
        moduleName: String,
        srcPos: SIRPosition
    ): Either[String, mutable.LinkedHashMap[String, Binding]] = {
        moduleDefsCache.get(moduleName) match
            case Some(defs) => Right(defs)
            case None =>
                moduleDefs.get(moduleName) match
                    case Some(module) =>
                        validateSIRVersion(module, moduleName, srcPos)
                        val defsMap = mutable.LinkedHashMap.from(
                          module.defs.map(d => d.name -> d)
                        )
                        moduleDefsCache.put(moduleName, defsMap)
                        Right(defsMap)
                    case None =>
                        Left(s"Can't find module ${moduleName} in dependenies")
    }

    private def validateSIRVersion(
        module: Module,
        moduleName: String,
        srcPos: SIRPosition
    ): Unit = {
        if (module.version._1 != SIRVersion._1)
            || (module.version._1 == SIRVersion._1
                && SIRVersion._2 < module.version._2)
        then
            error(
              s"""During linking I've found that a module '$moduleName' has an incompatible SIR version: ${module.version} (expected: ${SIRVersion}).
                   |This can happen if you try to link a module compiled with a different version of Scalus.
                   |Please, recompile the module with the version of Scalus that has the SIR version ${SIRVersion}
                   |""".stripMargin,
              srcPos,
              ()
            )
    }

}

object SIRLinker {

    class SIRLinkedBinding(
        val name: String,
        val flags: SIR.LetFlags,
        val body: SIR,
        val declaredTp: Option[SIRType] = None
    )

    enum LinkingDefState {
        case Linking extends LinkingDefState
        case Linked(binding: SIRLinkedBinding) extends LinkingDefState
    }

    def link(
        sir: SIR,
        pos: SIRPosition,
        deps: List[SIRCompiled],
        options: SIRLinkerOptions
    ): SIR = {
        if options.debugLevel > 0 then
            println(s"Linking SIR with deps: ${deps.map(_.sirModule.name).mkString(", ")}")
        val modules = readModules(deps)
        val linker = new SIRLinker(options, modules)
        val linked = linker.link(sir, pos)
        RemoveRecursivity(linked)
    }

    def readModules(deps: List[SIRCompiled]): Map[String, Module] = {
        var retval: Map[String, Module] = Map.empty
        val queue: mutable.Queue[SIRCompiled] = scala.collection.mutable.Queue.empty
        queue.enqueueAll(deps)
        while queue.nonEmpty do
            val dep = queue.dequeue()
            retval.get(dep.sirModule.name) match
                case Some(_) => // already added
                case None =>
                    retval += (dep.sirModule.name -> dep.sirModule)
                    queue.enqueueAll(dep.sirDeps)
        retval
    }

}
