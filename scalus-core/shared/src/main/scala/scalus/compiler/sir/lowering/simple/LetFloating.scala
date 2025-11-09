package scalus.compiler.sir.lowering.simple

import scalus.compiler.sir.*
import scalus.compiler.sir.EnrichedSIR.*

import scala.annotation.tailrec
import scala.collection.mutable

/** Let-floating transformation that moves lazy let bindings closer to their usage points.
  *
  * This transformation implements "floating inward" as described in: Simon Peyton Jones, Will
  * Partain, and André Santos. 1996. Let-floating: moving bindings to give faster programs.
  *
  * The algorithm works in three phases:
  *   1. Enrichment pass (down-traversal): Build enriched SIR with node indices, track variable
  *      usage, lambda context, and dependencies
  *   2. Analysis pass: Find dominating nodes for each lazy let binding using tracked usage
  *      information and transitive dependency propagation
  *   3. Output generation pass (down-traversal): Reconstruct SIR, moving lazy lets to their
  *      computed dominating nodes
  *
  * Lambda barrier: Non-effortless bindings (computations) are prevented from crossing lambda
  * boundaries to avoid duplicating work when the lambda is called multiple times. Effortless
  * bindings (constants, variables, lambdas) can cross freely.
  *
  * TODO: optimization can introduce expressions like App((lambda x, x),y) which can be eliminated.
  * Maybe add a beta-reduction pass after let-floating, or just handle it here.
  */
object LetFloating:

    /** Node information tracked during traversal */
    case class NodeInfo(
        index: Int,
        childIndices: List[Int],
        usedVars: Set[VarRef],
        definedVars: Map[String, VarDef],
        parent: Option[Int] = None, // Parent node index for finding suitable insertion points
        nearestEnclosingLambda: Option[Int] = None // Index of nearest enclosing lambda node
    )

    /** Variable reference: name and defining node index */
    case class VarRef(name: String, definingNode: Int)

    /** Variable definition with dependencies */
    case class VarDef(
        name: String,
        binding: Binding,
        definingNode: Int,
        isLazy: Boolean,
        dependencies: Set[VarRef] // Dependencies with their defining nodes
    )

    /** Tracked information about a variable during let-floating transformation */
    case class TrackedVarInfo(
        var minDirectUsageNode: Option[Int] = None,
        reverseDependencies: mutable.Set[VarRef] = mutable.Set.empty,
        // Minimum node index where this variable is used in multiple children
        var minMultipleChildrenNode: Option[Int] = None
    )

    /** Context during traversal */
    private class Context:
        private var nextIndex = 0
        val nodeInfos = mutable.HashMap.empty[Int, NodeInfo]
        // For each lazy let defined at a node, stores the node where it should be inserted
        val lazyLetPlacement =
            mutable.HashMap.empty[VarRef, Int] // VarRef -> targetInsertionNode
        val localScopes = mutable.ArrayBuffer.empty[Map[String, Int]] // name -> defining node
        // Tracks usage nodes and reverse dependencies for each variable
        val trackedVars = mutable.HashMap.empty[VarRef, TrackedVarInfo]
        // Stack of enclosing lambda node indices
        val lambdaStack = mutable.ArrayBuffer.empty[Int]

        def getNextIndex(): Int =
            val idx = nextIndex
            nextIndex += 1
            idx

        def enterScope(bindings: Map[String, Int]): Unit =
            localScopes += bindings

        def exitScope(): Unit =
            localScopes.remove(localScopes.size - 1)

        def lookupVar(name: String): Option[Int] =
            localScopes.reverseIterator.flatMap(_.get(name)).nextOption()

        def currentEnclosingLambda: Option[Int] =
            lambdaStack.lastOption

        def enterLambda(lambdaIndex: Int): Unit =
            lambdaStack += lambdaIndex

        def exitLambda(): Unit =
            lambdaStack.remove(lambdaStack.size - 1)

        def createNodeInfo(
            index: Int,
            childIndices: List[Int],
            usedVars: Set[VarRef],
            definedVars: Map[String, VarDef]
        ): NodeInfo =
            NodeInfo(index, childIndices, usedVars, definedVars, None, currentEnclosingLambda)

        def trackDirectVarUsage(varRef: VarRef, usageNodeIndex: Int): Unit =
            val info = trackedVars.getOrElseUpdate(varRef, TrackedVarInfo())
            info.minDirectUsageNode match
                case None          => info.minDirectUsageNode = Some(usageNodeIndex)
                case Some(minNode) => info.minDirectUsageNode = Some(minNode.min(usageNodeIndex))

        def trackReverseDependency(dependency: VarRef, dependent: VarRef): Unit =
            trackedVars
                .getOrElseUpdate(dependency, TrackedVarInfo())
                .reverseDependencies += dependent

        def trackMultipleChildrenUsage(nodeIndex: Int, childrenUsedVars: List[Set[VarRef]]): Unit =
            // Find variables that appear in 2+ children
            val varCounts = mutable.HashMap.empty[VarRef, Int]
            for
                childVars <- childrenUsedVars
                varRef <- childVars
            do varCounts(varRef) = varCounts.getOrElse(varRef, 0) + 1

            // Update minMultipleChildrenNode for variables used in multiple children
            for (varRef, count) <- varCounts if count >= 2 do
                val info = trackedVars.getOrElseUpdate(varRef, TrackedVarInfo())
                info.minMultipleChildrenNode match
                    case None => info.minMultipleChildrenNode = Some(nodeIndex)
                    case Some(minNode) =>
                        info.minMultipleChildrenNode = Some(minNode.min(nodeIndex))

    end Context

    /** Phase 1: Down-traversal to build enriched SIR with node indices */
    private def enrichDown(sir: SIR, ctx: Context): EnrichedSIR.Base[Int, NodeInfo] =
        val nodeIndex = ctx.getNextIndex()

        sir match
            case SIR.Decl(data, term) =>
                val enrichedTerm = enrichDown(term, ctx)
                val nodeInfo = enrichedTerm.info
                ctx.nodeInfos(nodeInfo.index) = nodeInfo
                Decl.DeclInfo(data, enrichedTerm, nodeInfo)

            case annSir: AnnotatedSIR =>
                enrichDownAnnotated(annSir, ctx, nodeIndex)

    private def enrichDownAnnotated(
        sir: AnnotatedSIR,
        ctx: Context,
        nodeIndex: Int
    ): AnnotatedBase[Int, NodeInfo] =
        sir match
            case SIR.Var(name, tp, anns) =>
                val defNode = ctx.lookupVar(name).getOrElse(-1)
                val usedVars = if defNode >= 0 then Set(VarRef(name, defNode)) else Set.empty
                // Track direct usage at this Var node
                usedVars.foreach(varRef => ctx.trackDirectVarUsage(varRef, nodeIndex))
                val nodeInfo = ctx.createNodeInfo(nodeIndex, Nil, usedVars, Map.empty)
                ctx.nodeInfos(nodeIndex) = nodeInfo
                Var.VarInfo(name, tp, anns, nodeInfo)

            case SIR.ExternalVar(moduleName, name, tp, anns) =>
                // ExternalVars are not tracked as local variables
                val nodeInfo = ctx.createNodeInfo(nodeIndex, Nil, Set.empty, Map.empty)
                ctx.nodeInfos(nodeIndex) = nodeInfo
                ExternalVar.ExternalVarInfo(moduleName, name, tp, anns, nodeInfo)

            case SIR.Let(bindings, body, flags, anns) =>
                val isLazy = flags.isLazy
                val isRec = flags.isRec

                // Enrich binding values
                val enrichedBindings = bindings.map { b =>
                    val enrichedValue = enrichDown(b.value, ctx)
                    BindingEnriched(b.name, b.tp, enrichedValue)
                }

                // Enter scope with bindings
                val bindingScope = bindings.map(b => b.name -> nodeIndex).toMap
                ctx.enterScope(bindingScope)

                // Enrich body
                val enrichedBody = enrichDown(body, ctx)

                ctx.exitScope()

                // Collect child indices and used vars
                val childIndices = enrichedBindings.map(b => b.value.info.index)
                val bodyInfo = enrichedBody.info
                val allChildIndices = childIndices :+ bodyInfo.index

                val usedVars = enrichedBindings.foldLeft(bodyInfo.usedVars) { (acc, b) =>
                    acc ++ b.value.info.usedVars
                }

                // Track variables used in multiple children (bindings + body)
                val allChildrenVars =
                    enrichedBindings.map(_.value.info.usedVars) :+ bodyInfo.usedVars
                ctx.trackMultipleChildrenUsage(nodeIndex, allChildrenVars)

                // Track dependencies - reuse already computed usedVars from enriched bindings
                val definedVars = bindings
                    .zip(enrichedBindings)
                    .map { case (b, eb) =>
                        val deps = eb.value.info.usedVars
                        b.name -> VarDef(b.name, b, nodeIndex, isLazy, deps)
                    }
                    .toMap

                // Track reverse dependencies in context
                for (name, varDef) <- definedVars do
                    val dependent = VarRef(varDef.name, varDef.definingNode)
                    for dep <- varDef.dependencies do ctx.trackReverseDependency(dep, dependent)

                val nodeInfo = ctx.createNodeInfo(nodeIndex, allChildIndices, usedVars, definedVars)
                ctx.nodeInfos(nodeIndex) = nodeInfo

                Let.LetInfo(enrichedBindings, enrichedBody, flags, anns, nodeInfo)

            case SIR.LamAbs(param, term, typeParams, anns) =>
                ctx.enterScope(Map(param.name -> nodeIndex))
                ctx.enterLambda(nodeIndex)
                val enrichedTerm = enrichDown(term, ctx)
                ctx.exitLambda()
                ctx.exitScope()

                val termInfo = enrichedTerm.info
                val usedVars = termInfo.usedVars.filterNot(_.name == param.name)
                val nodeInfo =
                    ctx.createNodeInfo(nodeIndex, List(termInfo.index), usedVars, Map.empty)
                ctx.nodeInfos(nodeIndex) = nodeInfo

                LamAbs.LamAbsInfo(
                  VarEnriched(param.name, param.tp),
                  enrichedTerm,
                  typeParams,
                  anns,
                  nodeInfo
                )

            case SIR.Apply(f, arg, tp, anns) =>
                val enrichedF = enrichDown(f, ctx).asInstanceOf[AnnotatedBase[Int, NodeInfo]]
                val enrichedArg = enrichDown(arg, ctx).asInstanceOf[AnnotatedBase[Int, NodeInfo]]

                val fInfo = enrichedF.info
                val argInfo = enrichedArg.info
                val usedVars = fInfo.usedVars ++ argInfo.usedVars
                ctx.trackMultipleChildrenUsage(nodeIndex, List(fInfo.usedVars, argInfo.usedVars))
                val nodeInfo =
                    ctx.createNodeInfo(
                      nodeIndex,
                      List(fInfo.index, argInfo.index),
                      usedVars,
                      Map.empty
                    )
                ctx.nodeInfos(nodeIndex) = nodeInfo

                Apply.ApplyInfo(enrichedF, enrichedArg, tp, anns, nodeInfo)

            case SIR.Select(scrutinee, field, tp, anns) =>
                val enrichedScrutinee = enrichDown(scrutinee, ctx)
                val scrutInfo = enrichedScrutinee.info
                val nodeInfo =
                    ctx.createNodeInfo(
                      nodeIndex,
                      List(scrutInfo.index),
                      scrutInfo.usedVars,
                      Map.empty
                    )
                ctx.nodeInfos(nodeIndex) = nodeInfo

                Select.SelectInfo(enrichedScrutinee, field, tp, anns, nodeInfo)

            case SIR.Const(uplcConst, tp, anns) =>
                val nodeInfo = ctx.createNodeInfo(nodeIndex, Nil, Set.empty, Map.empty)
                ctx.nodeInfos(nodeIndex) = nodeInfo
                Const.ConstInfo(uplcConst, tp, anns, nodeInfo)

            case SIR.And(a, b, anns) =>
                val enrichedA = enrichDown(a, ctx).asInstanceOf[AnnotatedBase[Int, NodeInfo]]
                val enrichedB = enrichDown(b, ctx).asInstanceOf[AnnotatedBase[Int, NodeInfo]]
                val aInfo = enrichedA.info
                val bInfo = enrichedB.info
                val usedVars = aInfo.usedVars ++ bInfo.usedVars
                ctx.trackMultipleChildrenUsage(nodeIndex, List(aInfo.usedVars, bInfo.usedVars))
                val nodeInfo =
                    ctx.createNodeInfo(
                      nodeIndex,
                      List(aInfo.index, bInfo.index),
                      usedVars,
                      Map.empty
                    )
                ctx.nodeInfos(nodeIndex) = nodeInfo
                And.AndInfo(enrichedA, enrichedB, anns, nodeInfo)

            case SIR.Or(a, b, anns) =>
                val enrichedA = enrichDown(a, ctx).asInstanceOf[AnnotatedBase[Int, NodeInfo]]
                val enrichedB = enrichDown(b, ctx).asInstanceOf[AnnotatedBase[Int, NodeInfo]]
                val aInfo = enrichedA.info
                val bInfo = enrichedB.info
                val usedVars = aInfo.usedVars ++ bInfo.usedVars
                ctx.trackMultipleChildrenUsage(nodeIndex, List(aInfo.usedVars, bInfo.usedVars))
                val nodeInfo =
                    ctx.createNodeInfo(
                      nodeIndex,
                      List(aInfo.index, bInfo.index),
                      usedVars,
                      Map.empty
                    )
                ctx.nodeInfos(nodeIndex) = nodeInfo
                Or.OrInfo(enrichedA, enrichedB, anns, nodeInfo)

            case SIR.Not(a, anns) =>
                val enrichedA = enrichDown(a, ctx).asInstanceOf[AnnotatedBase[Int, NodeInfo]]
                val aInfo = enrichedA.info
                val nodeInfo =
                    ctx.createNodeInfo(nodeIndex, List(aInfo.index), aInfo.usedVars, Map.empty)
                ctx.nodeInfos(nodeIndex) = nodeInfo
                Not.NotInfo(enrichedA, anns, nodeInfo)

            case SIR.IfThenElse(cond, t, f, tp, anns) =>
                val enrichedCond = enrichDown(cond, ctx).asInstanceOf[AnnotatedBase[Int, NodeInfo]]
                val enrichedT = enrichDown(t, ctx).asInstanceOf[AnnotatedBase[Int, NodeInfo]]
                val enrichedF = enrichDown(f, ctx).asInstanceOf[AnnotatedBase[Int, NodeInfo]]

                val condInfo = enrichedCond.info
                val tInfo = enrichedT.info
                val fInfo = enrichedF.info
                val usedVars = condInfo.usedVars ++ tInfo.usedVars ++ fInfo.usedVars
                ctx.trackMultipleChildrenUsage(
                  nodeIndex,
                  List(condInfo.usedVars, tInfo.usedVars, fInfo.usedVars)
                )
                val nodeInfo = ctx.createNodeInfo(
                  nodeIndex,
                  List(condInfo.index, tInfo.index, fInfo.index),
                  usedVars,
                  Map.empty
                )
                ctx.nodeInfos(nodeIndex) = nodeInfo

                IfThenElse.IfThenElseInfo(enrichedCond, enrichedT, enrichedF, tp, anns, nodeInfo)

            case SIR.Builtin(bn, tp, anns) =>
                val nodeInfo = ctx.createNodeInfo(nodeIndex, Nil, Set.empty, Map.empty)
                ctx.nodeInfos(nodeIndex) = nodeInfo
                Builtin.BuiltinInfo(bn, tp, anns, nodeInfo)

            case SIR.Error(msg, anns, cause) =>
                val enrichedMsg = enrichDown(msg, ctx).asInstanceOf[AnnotatedBase[Int, NodeInfo]]
                val msgInfo = enrichedMsg.info
                val nodeInfo =
                    ctx.createNodeInfo(nodeIndex, List(msgInfo.index), msgInfo.usedVars, Map.empty)
                ctx.nodeInfos(nodeIndex) = nodeInfo
                Error.ErrorInfo(enrichedMsg, anns, cause, nodeInfo)

            case SIR.Constr(name, data, args, tp, anns) =>
                val enrichedArgs = args.map(enrichDown(_, ctx))
                val argInfos = enrichedArgs.map(_.info)
                val usedVars = argInfos.flatMap(_.usedVars).toSet
                val childIndices = argInfos.map(_.index)
                ctx.trackMultipleChildrenUsage(nodeIndex, argInfos.map(_.usedVars).toList)
                val nodeInfo = ctx.createNodeInfo(nodeIndex, childIndices, usedVars, Map.empty)
                ctx.nodeInfos(nodeIndex) = nodeInfo
                Constr.ConstrInfo(name, data, enrichedArgs, tp, anns, nodeInfo)

            case SIR.Match(scrutinee, cases, tp, anns) =>
                val enrichedScrutinee =
                    enrichDown(scrutinee, ctx).asInstanceOf[AnnotatedBase[Int, NodeInfo]]
                val enrichedCases = cases.map { c =>
                    val bindings = c.pattern match
                        case SIR.Pattern.Constr(_, bindings, _) =>
                            bindings.map(_ -> nodeIndex).toMap
                        case _ => Map.empty[String, Int]

                    ctx.enterScope(bindings)
                    val enrichedBody = enrichDown(c.body, ctx)
                    ctx.exitScope()

                    CaseEnriched(c.pattern, enrichedBody, c.anns)
                }

                val scrutInfo = enrichedScrutinee.info
                val caseInfos = enrichedCases.map(c => c.body.info)
                val usedVars = scrutInfo.usedVars ++ caseInfos.flatMap(_.usedVars).toSet
                val childIndices = scrutInfo.index :: caseInfos.map(_.index)
                val allChildrenVars = scrutInfo.usedVars :: caseInfos.map(_.usedVars).toList
                ctx.trackMultipleChildrenUsage(nodeIndex, allChildrenVars)
                val nodeInfo = ctx.createNodeInfo(nodeIndex, childIndices, usedVars, Map.empty)
                ctx.nodeInfos(nodeIndex) = nodeInfo

                Match.MatchInfo(enrichedScrutinee, enrichedCases, tp, anns, nodeInfo)

            case SIR.Cast(term, tp, anns) =>
                val enrichedTerm = enrichDown(term, ctx).asInstanceOf[AnnotatedBase[Int, NodeInfo]]
                val termInfo = enrichedTerm.info
                val nodeInfo =
                    ctx.createNodeInfo(
                      nodeIndex,
                      List(termInfo.index),
                      termInfo.usedVars,
                      Map.empty
                    )
                ctx.nodeInfos(nodeIndex) = nodeInfo
                Cast.CastInfo(enrichedTerm, tp, anns, nodeInfo)

    /** Phase 2: Find dominating nodes for lazy let bindings */
    private def findDominatingNodes(enriched: EnrichedSIR.Base[Int, NodeInfo], ctx: Context): Unit =
        // First pass: compute initial targets for all lazy variables
        def computeInitialTargets(node: EnrichedSIR.Base[Int, NodeInfo]): Unit =
            val info = node.info

            // For each defined lazy variable, compute initial target
            info.definedVars.foreach { case (name, varDef) =>
                if varDef.isLazy then
                    val targetNode = findDominatingNode(varDef, ctx)
                    ctx.lazyLetPlacement(VarRef(varDef.name, varDef.definingNode)) = targetNode
            }

            // Recursively process children
            node match
                case Decl.DeclInfo(_, term, _) => computeInitialTargets(term)
                case Let.LetInfo(bindings, body, _, _, _) =>
                    bindings.foreach(b => computeInitialTargets(b.value))
                    computeInitialTargets(body)
                case LamAbs.LamAbsInfo(_, term, _, _, _) => computeInitialTargets(term)
                case Apply.ApplyInfo(f, arg, _, _, _) =>
                    computeInitialTargets(f)
                    computeInitialTargets(arg)
                case Select.SelectInfo(scrutinee, _, _, _, _) => computeInitialTargets(scrutinee)
                case And.AndInfo(a, b, _, _) =>
                    computeInitialTargets(a)
                    computeInitialTargets(b)
                case Or.OrInfo(a, b, _, _) =>
                    computeInitialTargets(a)
                    computeInitialTargets(b)
                case Not.NotInfo(a, _, _) => computeInitialTargets(a)
                case IfThenElse.IfThenElseInfo(cond, t, f, _, _, _) =>
                    computeInitialTargets(cond)
                    computeInitialTargets(t)
                    computeInitialTargets(f)
                case Error.ErrorInfo(msg, _, _, _)          => computeInitialTargets(msg)
                case Constr.ConstrInfo(_, _, args, _, _, _) => args.foreach(computeInitialTargets)
                case Match.MatchInfo(scrutinee, cases, _, _, _) =>
                    computeInitialTargets(scrutinee)
                    cases.foreach(c => computeInitialTargets(c.body))
                case Cast.CastInfo(term, _, _, _) => computeInitialTargets(term)
                case _                            =>

        // Compute initial targets
        computeInitialTargets(enriched)

        // Second pass: propagate constraints from dependents to dependencies
        propagateTargetConstraints(ctx)

    /** Apply lambda barrier: prevent non-effortless bindings from crossing lambda boundaries
      *
      * Walks backwards from target through the lambda chain to find the outermost lambda that would
      * be crossed when moving from defining node to target node.
      *
      * @param definingNode
      *   The node where the binding is defined
      * @param targetNode
      *   The proposed target node where the binding would be inserted
      * @param ctx
      *   The context containing node information
      * @return
      *   The constrained target node (the outermost lambda boundary if crossing lambdas, or
      *   targetNode if no barrier)
      */
    private def applyLambdaBarrier(definingNode: Int, targetNode: Int, ctx: Context): Int =
        val defLambda = ctx.nodeInfos.get(definingNode).flatMap(_.nearestEnclosingLambda)
        val targetLambda = ctx.nodeInfos.get(targetNode).flatMap(_.nearestEnclosingLambda)

        // If both are in the same lambda context, no barrier
        if defLambda == targetLambda then return targetNode

        // Walk backwards from target to find the outermost lambda we'd cross
        var current = targetLambda
        var outermost = targetLambda

        while current.isDefined && current != defLambda do
            outermost = current
            current = ctx.nodeInfos.get(current.get).flatMap(_.nearestEnclosingLambda)

        // Return the outermost lambda node - we'll insert before it (outside the lambda)
        outermost.getOrElse(targetNode)

    /** Compute initial target based on direct usage only (no transitive dependencies) */
    private def computeInitialTarget(varRef: VarRef, ctx: Context): Int =
        ctx.trackedVars.get(varRef) match
            case None => Int.MaxValue
            case Some(info) =>
                val constraints = mutable.ArrayBuffer.empty[Int]
                info.minDirectUsageNode.foreach(constraints += _)
                info.minMultipleChildrenNode.foreach(constraints += _)

                if constraints.isEmpty then Int.MaxValue
                else
                    val minTarget = constraints.min

                    // Apply lambda barrier: check if the binding is effortless
                    // If not effortless, don't let it cross lambda boundaries
                    ctx.nodeInfos
                        .get(varRef.definingNode)
                        .flatMap { defNodeInfo =>
                            defNodeInfo.definedVars.get(varRef.name).map { varDef =>
                                if !isEffortLest(varDef.binding.value) then
                                    // Non-effortless binding - apply lambda barrier
                                    applyLambdaBarrier(varRef.definingNode, minTarget, ctx)
                                else
                                    // Effortless binding - no barrier
                                    minTarget
                            }
                        }
                        .getOrElse(minTarget)

    /** Propagate target constraints from dependents to dependencies using fixpoint iteration */
    private def propagateTargetConstraints(ctx: Context): Unit =
        var changed = true
        var iteration = 0
        while changed do
            iteration += 1
            changed = false
            ctx.trackedVars.foreach { case (varRef, info) =>
                var currentTarget = ctx.lazyLetPlacement.getOrElse(varRef, Int.MaxValue)
                val oldTarget = currentTarget

                // For each dependent of this variable
                info.reverseDependencies.foreach { depVarRef =>
                    val depTarget = ctx.lazyLetPlacement.getOrElse(depVarRef, Int.MaxValue)
                    // If dependent will be placed at a deeper node, this variable must also be moved there
                    // to be available when the dependent's binding is evaluated
                    if depTarget > currentTarget && depTarget != Int.MaxValue then
                        currentTarget = depTarget
                        changed = true
                }

                if currentTarget != ctx.lazyLetPlacement.getOrElse(varRef, Int.MaxValue) then
                    ctx.lazyLetPlacement(varRef) = currentTarget
            }

    /** Find the dominating node for a lazy variable */
    private def findDominatingNode(varDef: VarDef, ctx: Context): Int =
        val varRef = VarRef(varDef.name, varDef.definingNode)
        // Initial target will be computed first, then propagated
        computeInitialTarget(varRef, ctx)

    /** Generate unique name for floated variable to avoid shadowing */
    private def getFloatedVarName(name: String, definingNode: Int): String =
        s"$name$$$$floated$$$definingNode"

    /** Check if a variable reference refers to a floated binding */
    private def isFloated(varRef: VarRef, ctx: Context): Boolean =
        ctx.lazyLetPlacement
            .get(varRef)
            .exists(target => target > varRef.definingNode && target != Int.MaxValue)

    /** Topologically sort bindings so dependencies come before dependents */
    private def topologicalSort(
        bindings: List[(Binding, Int)],
        ctx: Context
    ): List[(Binding, Int)] =
        // Build dependency map: for each binding, what other bindings does it depend on?
        val bindingMap = bindings.map { case (b, defNode) =>
            (b.name, defNode) -> (b, defNode)
        }.toMap
        val bindingNames = bindings.map { case (b, defNode) => VarRef(b.name, defNode) }.toSet

        val deps = bindings.map { case (binding, defNode) =>
            val varRef = VarRef(binding.name, defNode)
            // Find dependencies of this binding from the tracked info
            val bindingDeps = ctx.trackedVars
                .get(varRef)
                .map { info =>
                    ctx.nodeInfos
                        .get(defNode)
                        .map { nodeInfo =>
                            nodeInfo.definedVars
                                .get(binding.name)
                                .map { varDef =>
                                    // Get dependencies that are also in our binding set
                                    varDef.dependencies.filter(bindingNames.contains)
                                }
                                .getOrElse(Set.empty)
                        }
                        .getOrElse(Set.empty)
                }
                .getOrElse(Set.empty)

            varRef -> bindingDeps
        }.toMap

        // Perform topological sort using Kahn's algorithm
        val result = mutable.ArrayBuffer.empty[(Binding, Int)]
        val remaining = mutable.HashSet.from(bindings.map { case (b, defNode) =>
            VarRef(b.name, defNode)
        })
        val inDegree = mutable.HashMap.from(
          bindings.map { case (b, defNode) =>
              VarRef(b.name, defNode) -> deps(VarRef(b.name, defNode)).size
          }
        )

        while remaining.nonEmpty do
            // Find a binding with no dependencies
            remaining.find(vr => inDegree(vr) == 0) match
                case Some(vr) =>
                    remaining -= vr
                    bindingMap.get((vr.name, vr.definingNode)).foreach(result += _)

                    // Decrease in-degree for dependents
                    remaining.foreach { depVr =>
                        if deps(depVr).contains(vr) then inDegree(depVr) -= 1
                    }

                case None =>
                    // Circular dependency or error - just return remaining in original order
                    remaining.foreach { vr =>
                        bindingMap.get((vr.name, vr.definingNode)).foreach(result += _)
                    }
                    remaining.clear()

        result.toList

    /** Phase 3: Generate output SIR with moved lazy lets */
    private def generateOutput(
        enriched: EnrichedSIR.Base[Int, NodeInfo],
        pendingLets: List[(Binding, Int)], // (Binding, definingNode)
        ctx: Context
    ): SIR =
        enriched match
            case Decl.DeclInfo(data, term, info) =>
                SIR.Decl(data, generateOutput(term, pendingLets, ctx))

            case Decl.DeclPattern(_, _) =>
                throw new IllegalStateException("DeclPattern should not appear in let-floating")

            case Let.LetInfo(bindings, body, flags, anns, info) =>
                val isLazy = flags.isLazy

                if isLazy then
                    // Check if body is a simple Var node for optimization
                    val bodyIsSimpleVar = enriched match
                        case Let.LetInfo(_, Var.VarInfo(_, _, _, _), _, _, _) => true
                        case _                                                => false
                    val bodyNodeIndex = enriched match
                        case Let.LetInfo(_, bodyEnriched, _, _, _) => bodyEnriched.info.index
                        case _                                     => -1

                    // Separate bindings: float (target > current), eliminate (unused), or stay
                    val (floatingBindings, rest) = bindings.partition { b =>
                        ctx.lazyLetPlacement.get(VarRef(b.name, info.index)) match
                            // Optimization: don't float if target is immediate body AND body is just a Var
                            // Example: let lazy x = E in x  (keep in place)
                            case Some(target) if target == bodyNodeIndex && bodyIsSimpleVar => false
                            // Float if target is deeper than current Let
                            case Some(target) if target > info.index && target != Int.MaxValue =>
                                true
                            case _ => false
                    }

                    // Filter out unused bindings (target == Int.MaxValue)
                    val stayingBindings = rest.filter { b =>
                        ctx.lazyLetPlacement.get(VarRef(b.name, info.index)) match
                            case Some(Int.MaxValue) => false // Eliminate unused
                            case _                  => true // Keep (target == current position)
                    }

                    // Only add bindings to pending if they're not unused (target != Int.MaxValue)
                    val validFloatingBindings = floatingBindings.filter { b =>
                        ctx.lazyLetPlacement.get(VarRef(b.name, info.index)) match
                            case Some(Int.MaxValue) => false // Don't add unused bindings to pending
                            case _                  => true
                    }

                    val newPendingLets = pendingLets ++ validFloatingBindings.map(b =>
                        (Binding(b.name, b.tp, generateOutput(b.value, Nil, ctx)), info.index)
                    )

                    if stayingBindings.isEmpty then
                        // All bindings float, just process the body
                        generateOutput(body, newPendingLets, ctx)
                    else
                        // Some bindings stay
                        val newBindings = stayingBindings.map(b =>
                            Binding(
                              b.name,
                              b.tp,
                              generateOutput(b.value, Nil, ctx)
                            )
                        )
                        val newBody = generateOutput(body, newPendingLets, ctx)
                        val newFlags = flags.remove(SIR.LetFlags.Lazy) // Remove lazy flag
                        SIR.Let(newBindings, newBody, newFlags, anns)
                else
                    // Non-lazy let, process normally
                    val newBindings = bindings.map(b =>
                        Binding(
                          b.name,
                          b.tp,
                          generateOutput(b.value, Nil, ctx)
                        )
                    )
                    val newBody = generateOutput(body, pendingLets, ctx)
                    SIR.Let(newBindings, newBody, flags, anns)

            case annEnriched: AnnotatedBase[Int, NodeInfo] =>
                // Insert pending lets if this is their target insertion node
                val currentIndex = enriched.info.index

                val (toInsertHere, toInsertLater) = pendingLets.partition {
                    case (binding, defNode) =>
                        ctx.lazyLetPlacement
                            .get(VarRef(binding.name, defNode))
                            .contains(currentIndex)
                }

                val innerSir = generateOutputAnnotated(annEnriched, toInsertLater, ctx)

                if toInsertHere.nonEmpty then
                    // Sort bindings by dependencies: dependencies must come before dependents
                    val sorted = topologicalSort(toInsertHere, ctx)

                    // Rename floated bindings to avoid shadowing conflicts
                    val renamedBindings = sorted.map { case (binding, defNode) =>
                        val newName = getFloatedVarName(binding.name, defNode)
                        Binding(newName, binding.tp, binding.value)
                    }
                    SIR.Let(
                      renamedBindings,
                      innerSir, // Target node becomes the body of the Let
                      SIR.LetFlags.None, // Insert without lazy flag
                      AnnotationsDecl.empty
                    )
                else innerSir

    private def generateOutputAnnotated(
        enriched: AnnotatedBase[Int, NodeInfo],
        pendingLets: List[(Binding, Int)],
        ctx: Context
    ): AnnotatedSIR =
        enriched match
            case Var.VarInfo(name, tp, anns, info) =>
                // For a Var node, usedVars is either empty (free var) or contains one VarRef with same name
                info.usedVars.headOption match
                    case Some(varRef) if isFloated(varRef, ctx) =>
                        // Variable references a floated binding, use renamed version
                        val newName = getFloatedVarName(name, varRef.definingNode)
                        SIR.Var(newName, tp, anns)
                    case _ =>
                        // Free variable or non-floated binding, keep original name
                        SIR.Var(name, tp, anns)

            case ExternalVar.ExternalVarInfo(moduleName, name, tp, anns, _) =>
                SIR.ExternalVar(moduleName, name, tp, anns)

            case Let.LetInfo(_, _, _, _, _) =>
                // This should be handled in generateOutput
                throw new IllegalStateException("Let should be handled in generateOutput")

            case LamAbs.LamAbsInfo(param, term, typeParams, anns, _) =>
                val newTerm = generateOutput(term, pendingLets, ctx)
                SIR.LamAbs(
                  SIR.Var(param.name, param.tp, AnnotationsDecl.empty),
                  newTerm,
                  typeParams,
                  anns
                )

            case Apply.ApplyInfo(f, arg, tp, anns, _) =>
                val newF = generateOutput(f, pendingLets, ctx).asInstanceOf[AnnotatedSIR]
                val newArg = generateOutput(arg, pendingLets, ctx).asInstanceOf[AnnotatedSIR]
                SIR.Apply(newF, newArg, tp, anns)

            case Select.SelectInfo(scrutinee, field, tp, anns, _) =>
                val newScrutinee = generateOutput(scrutinee, pendingLets, ctx)
                SIR.Select(newScrutinee, field, tp, anns)

            case Const.ConstInfo(uplcConst, tp, anns, _) =>
                SIR.Const(uplcConst, tp, anns)

            case And.AndInfo(a, b, anns, _) =>
                val newA = generateOutput(a, pendingLets, ctx).asInstanceOf[AnnotatedSIR]
                val newB = generateOutput(b, pendingLets, ctx).asInstanceOf[AnnotatedSIR]
                SIR.And(newA, newB, anns)

            case Or.OrInfo(a, b, anns, _) =>
                val newA = generateOutput(a, pendingLets, ctx).asInstanceOf[AnnotatedSIR]
                val newB = generateOutput(b, pendingLets, ctx).asInstanceOf[AnnotatedSIR]
                SIR.Or(newA, newB, anns)

            case Not.NotInfo(a, anns, _) =>
                val newA = generateOutput(a, pendingLets, ctx).asInstanceOf[AnnotatedSIR]
                SIR.Not(newA, anns)

            case IfThenElse.IfThenElseInfo(cond, t, f, tp, anns, _) =>
                val newCond = generateOutput(cond, pendingLets, ctx).asInstanceOf[AnnotatedSIR]
                val newT = generateOutput(t, pendingLets, ctx).asInstanceOf[AnnotatedSIR]
                val newF = generateOutput(f, pendingLets, ctx).asInstanceOf[AnnotatedSIR]
                SIR.IfThenElse(newCond, newT, newF, tp, anns)

            case Builtin.BuiltinInfo(bn, tp, anns, _) =>
                SIR.Builtin(bn, tp, anns)

            case Error.ErrorInfo(msg, anns, cause, _) =>
                val newMsg = generateOutput(msg, pendingLets, ctx).asInstanceOf[AnnotatedSIR]
                SIR.Error(newMsg, anns, cause)

            case Constr.ConstrInfo(name, data, args, tp, anns, _) =>
                val newArgs = args.map(generateOutput(_, pendingLets, ctx))
                SIR.Constr(name, data, newArgs, tp, anns)

            case Match.MatchInfo(scrutinee, cases, tp, anns, _) =>
                val newScrutinee =
                    generateOutput(scrutinee, pendingLets, ctx).asInstanceOf[AnnotatedSIR]
                val newCases = cases.map { c =>
                    val newBody = generateOutput(c.body, pendingLets, ctx)
                    SIR.Case(c.pattern, newBody, c.anns)
                }
                SIR.Match(newScrutinee, newCases, tp, anns)

            case Cast.CastInfo(term, tp, anns, _) =>
                val newTerm = generateOutput(term, pendingLets, ctx).asInstanceOf[AnnotatedSIR]
                SIR.Cast(newTerm, tp, anns)

            case _ =>
                throw new IllegalStateException(
                  s"Impossible enriched node in this optimization: $enriched"
                )

    /** Expression, which not require computations: constants, vars, lamnbdas
      */
    @tailrec
    private def isEffortLest(sir: SIR): Boolean = {
        sir match
            case SIR.Const(_, _, _)          => true
            case SIR.Var(_, _, _)            => true
            case SIR.ExternalVar(_, _, _, _) => true
            case SIR.LamAbs(_, _, _, _)      => true
            case SIR.Decl(data, term)        => isEffortLest(term)
            case _                           => false
    }

    /** Apply let-floating transformation to SIR */
    def apply(sir: SIR): SIR =
        val ctx = new Context()

        // Phase 1: Down-traversal to build enriched SIR
        val enriched = enrichDown(sir, ctx)

        // Phase 2: Find dominating nodes (computes transitive dependencies on-demand)
        findDominatingNodes(enriched, ctx)

        // Phase 3: Generate output
        generateOutput(enriched, Nil, ctx)

end LetFloating
