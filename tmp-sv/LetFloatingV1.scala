package scalus
package sir
package simpleLowering

import scala.collection.mutable

/** Let floating transformation for simple lowering backends.
  *
  * This transformation handles lazy let expressions by reordering them close to their usage sites
  * or eliminating them if unused.
  *
  * Similat oprtimization described in: Simon Peyton Jones et
  * al. "Let-floating: moving bindings to give faster programs" (ICFP '96)
  *
  * The transformation works in three phases:
  *   1. Build an indexed tree with scope-aware variable tracking
  *   2. Compute dependencies and optimal placement for each lazy binding
  *   3. Reconstruct the SIR with floated bindings
  *
  * Only applies to lazy let expressions (with LetFlags.Lazy). Non-lazy and recursive lets are
  * preserved as-is.
  */
object LetFloatingV1:

    /** Unique identifier for a binding, accounting for variable shadowing.
      *
      * @param nodeIndex
      *   The index of the Let node that introduced this binding
      * @param name
      *   The variable name
      */
    case class BindingId(nodeIndex: Int, name: String)

    /** Metadata for each node in the SIR tree.
      *
      * @param index
      *   Unique index for this node
      * @param parentIndex
      *   Index of the parent node (None for root)
      * @param childIndices
      *   Indices of direct children
      * @param varsUsedHere
      *   Variable names referenced at this node (not in subtrees)
      * @param bindingsInScope
      *   Maps variable names to their BindingIds at this point in the tree
      * @param sir
      *   The actual SIR node
      */
    case class NodeMetadata(
        index: Int,
        parentIndex: Option[Int],
        childIndices: List[Int],
        varsUsedHere: Set[String],
        bindingsInScope: Map[String, BindingId],
        sir: SIR
    )

    /** Information about a lazy binding to be floated.
      *
      * @param bindingId
      *   Unique identifier for this binding
      * @param value
      *   The RHS of the binding
      * @param tp
      *   The type of the binding
      * @param dependencies
      *   BindingIds this binding depends on
      * @param usageNodes
      *   Node indices where this binding is used
      * @param minValidNode
      *   Lowest node where all dependencies are in scope
      * @param dominatingNode
      *   Optimal node for placing this binding (LCA of usages)
      */
    case class LazyBindingInfo(
        bindingId: BindingId,
        value: SIR,
        tp: SIRType,
        dependencies: Set[BindingId],
        usageNodes: Set[Int],
        minValidNode: Int,
        dominatingNode: Option[Int]
    )

    /** Apply let floating transformation to a SIR expression.
      *
      * @param sir
      *   The input SIR
      * @return
      *   Transformed SIR with lazy lets floated to optimal positions
      */
    def apply(sir: SIR): SIR =
        // Phase 1: Build indexed tree
        val (nodes, lazyBindings) = buildIndexedTree(sir)

        if lazyBindings.isEmpty then
            // No lazy bindings to float
            return sir

        // Phase 2: Compute dependencies and placement
        val bindingsWithPlacement = computePlacement(nodes, lazyBindings)

        // Phase 3: Reconstruct SIR
        reconstructSIR(nodes(0), nodes, bindingsWithPlacement)

    // ============================================================================
    // Phase 1: Build Indexed Tree
    // ============================================================================

    private def buildIndexedTree(
        sir: SIR
    ): (Map[Int, NodeMetadata], Map[BindingId, LazyBindingInfo]) =
        var nextIndex = 0
        val nodes = mutable.Map.empty[Int, NodeMetadata]
        val lazyBindings = mutable.Map.empty[BindingId, LazyBindingInfo]

        def getNextIndex(): Int =
            val idx = nextIndex
            nextIndex += 1
            idx

        def indexTree(
            sir: SIR,
            parentIndex: Option[Int],
            bindingsInScope: Map[String, BindingId]
        ): Int =
            sir match
                case SIR.Decl(data, term) =>
                    // Decl nodes don't create new scope, just pass through
                    indexTree(term, parentIndex, bindingsInScope)

                case annSir: AnnotatedSIR =>
                    indexAnnotatedTree(annSir, parentIndex, bindingsInScope)

        def indexAnnotatedTree(
            sir: AnnotatedSIR,
            parentIndex: Option[Int],
            bindingsInScope: Map[String, BindingId]
        ): Int =
            val myIndex = getNextIndex()
            val childIndices = mutable.ListBuffer.empty[Int]
            val varsUsedHere = mutable.Set.empty[String]

            sir match
                case SIR.Var(name, _, _) =>
                    // Variable reference
                    varsUsedHere += name

                case SIR.ExternalVar(_, name, _, _) =>
                    // External variable reference
                    varsUsedHere += name

                case SIR.Let(bindings, body, flags, anns) =>
                    if flags.isLazy && !flags.isRec then
                        // Lazy, non-recursive let: candidate for floating
                        var newScope = bindingsInScope

                        bindings.foreach { binding =>
                            val bindingId = BindingId(myIndex, binding.name)

                            // Index the RHS with current scope (binding not in scope for its own RHS)
                            val rhsIndex = indexTree(binding.value, Some(myIndex), bindingsInScope)
                            childIndices += rhsIndex

                            // Collect dependencies from the RHS
                            val deps = collectDependencies(binding.value, bindingsInScope)

                            // Record this lazy binding
                            lazyBindings(bindingId) = LazyBindingInfo(
                              bindingId = bindingId,
                              value = binding.value,
                              tp = binding.tp,
                              dependencies = deps,
                              usageNodes = Set.empty, // Will be populated later
                              minValidNode = myIndex, // Will be refined in phase 2
                              dominatingNode = None // Will be computed in phase 2
                            )

                            // Add to scope for subsequent bindings and body
                            newScope = newScope + (binding.name -> bindingId)
                        }

                        // Index the body with new scope
                        val bodyIndex = indexTree(body, Some(myIndex), newScope)
                        childIndices += bodyIndex
                    else
                        // Non-lazy or recursive let: normal processing
                        var newScope = bindingsInScope

                        if flags.isRec then
                            // For recursive bindings, add to scope before processing RHS
                            bindings.foreach { binding =>
                                val bindingId = BindingId(myIndex, binding.name)
                                newScope = newScope + (binding.name -> bindingId)
                            }

                        bindings.foreach { binding =>
                            val rhsIndex = indexTree(binding.value, Some(myIndex), newScope)
                            childIndices += rhsIndex

                            if !flags.isRec then
                                // For non-recursive, add to scope after processing RHS
                                val bindingId = BindingId(myIndex, binding.name)
                                newScope = newScope + (binding.name -> bindingId)
                        }

                        val bodyIndex = indexTree(body, Some(myIndex), newScope)
                        childIndices += bodyIndex

                case SIR.LamAbs(param, term, typeParams, anns) =>
                    val paramId = BindingId(myIndex, param.name)
                    val newScope = bindingsInScope + (param.name -> paramId)
                    val termIndex = indexTree(term, Some(myIndex), newScope)
                    childIndices += termIndex

                case SIR.Apply(f, arg, tp, anns) =>
                    val fIndex = indexAnnotatedTree(f, Some(myIndex), bindingsInScope)
                    val argIndex = indexAnnotatedTree(arg, Some(myIndex), bindingsInScope)
                    childIndices ++= List(fIndex, argIndex)

                case SIR.Select(scrutinee, field, tp, anns) =>
                    val scrIndex = indexTree(scrutinee, Some(myIndex), bindingsInScope)
                    childIndices += scrIndex

                case SIR.And(a, b, anns) =>
                    val aIndex = indexAnnotatedTree(a, Some(myIndex), bindingsInScope)
                    val bIndex = indexAnnotatedTree(b, Some(myIndex), bindingsInScope)
                    childIndices ++= List(aIndex, bIndex)

                case SIR.Or(a, b, anns) =>
                    val aIndex = indexAnnotatedTree(a, Some(myIndex), bindingsInScope)
                    val bIndex = indexAnnotatedTree(b, Some(myIndex), bindingsInScope)
                    childIndices ++= List(aIndex, bIndex)

                case SIR.Not(a, anns) =>
                    val aIndex = indexAnnotatedTree(a, Some(myIndex), bindingsInScope)
                    childIndices += aIndex

                case SIR.IfThenElse(cond, t, f, tp, anns) =>
                    val condIndex = indexAnnotatedTree(cond, Some(myIndex), bindingsInScope)
                    val tIndex = indexAnnotatedTree(t, Some(myIndex), bindingsInScope)
                    val fIndex = indexAnnotatedTree(f, Some(myIndex), bindingsInScope)
                    childIndices ++= List(condIndex, tIndex, fIndex)

                case SIR.Constr(name, data, args, tp, anns) =>
                    args.foreach { arg =>
                        val argIndex = indexTree(arg, Some(myIndex), bindingsInScope)
                        childIndices += argIndex
                    }

                case SIR.Match(scrutinee, cases, tp, anns) =>
                    val scrIndex = indexAnnotatedTree(scrutinee, Some(myIndex), bindingsInScope)
                    childIndices += scrIndex

                    cases.foreach { case SIR.Case(pattern, body, caseAnns) =>
                        var caseScope = bindingsInScope
                        pattern match
                            case SIR.Pattern.Constr(constr, bindings, typeBindings) =>
                                // Add pattern bindings to scope
                                bindings.foreach { binding =>
                                    val bindingId = BindingId(myIndex, binding)
                                    caseScope = caseScope + (binding -> bindingId)
                                }
                            case _ => // Const or Wildcard: no new bindings
                        val bodyIndex = indexTree(body, Some(myIndex), caseScope)
                        childIndices += bodyIndex
                    }

                case SIR.Cast(term, tp, anns) =>
                    val termIndex = indexAnnotatedTree(term, Some(myIndex), bindingsInScope)
                    childIndices += termIndex

                case SIR.Error(msg, anns, cause) =>
                    val msgIndex = indexAnnotatedTree(msg, Some(myIndex), bindingsInScope)
                    childIndices += msgIndex

                case _: SIR.Const | _: SIR.Builtin =>
                // Leaf nodes, no children

            // Record this node
            nodes(myIndex) = NodeMetadata(
              index = myIndex,
              parentIndex = parentIndex,
              childIndices = childIndices.toList,
              varsUsedHere = varsUsedHere.toSet,
              bindingsInScope = bindingsInScope,
              sir = sir
            )

            myIndex

        // Start indexing from root
        indexTree(sir, None, Map.empty)

        (nodes.toMap, lazyBindings.toMap)

    /** Collect BindingIds that a SIR expression depends on. */
    private def collectDependencies(
        sir: SIR,
        bindingsInScope: Map[String, BindingId]
    ): Set[BindingId] =
        val deps = mutable.Set.empty[BindingId]

        def collectFromSir(sir: SIR, scope: Map[String, BindingId]): Unit =
            sir match
                case SIR.Var(name, _, _) =>
                    scope.get(name).foreach(deps += _)
                case SIR.ExternalVar(_, name, _, _) =>
                    // ExternalVars reference external functions, not local bindings
                    // Never treat them as dependencies
                    ()
                case SIR.Decl(_, term) =>
                    collectFromSir(term, scope)
                case annSir: AnnotatedSIR =>
                    collectFromAnnotated(annSir, scope)

        def collectFromAnnotated(sir: AnnotatedSIR, scope: Map[String, BindingId]): Unit =
            sir match
                case SIR.Var(name, _, _) =>
                    scope.get(name).foreach(deps += _)
                case SIR.ExternalVar(_, name, _, _) =>
                    // ExternalVars reference external functions, not local bindings
                    // Never treat them as dependencies
                    ()
                case SIR.Let(bindings, body, flags, _) =>
                    var newScope = scope
                    if flags.isRec then
                        // Add all bindings first for recursive let
                        bindings.foreach { b =>
                            newScope = newScope + (b.name -> BindingId(-1, b.name))
                        }
                    bindings.foreach { b =>
                        collectFromSir(b.value, newScope)
                        if !flags.isRec then newScope = newScope + (b.name -> BindingId(-1, b.name))
                    }
                    collectFromSir(body, newScope)
                case SIR.LamAbs(param, term, _, _) =>
                    val newScope = scope + (param.name -> BindingId(-1, param.name))
                    collectFromSir(term, newScope)
                case SIR.Apply(f, arg, _, _) =>
                    collectFromAnnotated(f, scope)
                    collectFromAnnotated(arg, scope)
                case SIR.Select(scrutinee, _, _, _) =>
                    collectFromSir(scrutinee, scope)
                case SIR.And(a, b, _) =>
                    collectFromAnnotated(a, scope)
                    collectFromAnnotated(b, scope)
                case SIR.Or(a, b, _) =>
                    collectFromAnnotated(a, scope)
                    collectFromAnnotated(b, scope)
                case SIR.Not(a, _) =>
                    collectFromAnnotated(a, scope)
                case SIR.IfThenElse(cond, t, f, _, _) =>
                    collectFromAnnotated(cond, scope)
                    collectFromAnnotated(t, scope)
                    collectFromAnnotated(f, scope)
                case SIR.Constr(_, _, args, _, _) =>
                    args.foreach(collectFromSir(_, scope))
                case SIR.Match(scrutinee, cases, _, _) =>
                    collectFromAnnotated(scrutinee, scope)
                    cases.foreach { case SIR.Case(pattern, body, _) =>
                        var caseScope = scope
                        pattern match
                            case SIR.Pattern.Constr(_, bindings, _) =>
                                bindings.foreach { b =>
                                    caseScope = caseScope + (b -> BindingId(-1, b))
                                }
                            case _ =>
                        collectFromSir(body, caseScope)
                    }
                case SIR.Cast(term, _, _) =>
                    collectFromAnnotated(term, scope)
                case SIR.Error(msg, _, _) =>
                    collectFromAnnotated(msg, scope)
                case _: SIR.Const | _: SIR.Builtin =>
                // No dependencies

        collectFromSir(sir, bindingsInScope)
        deps.toSet

    // ============================================================================
    // Phase 2: Compute Dependencies and Placement
    // ============================================================================

    private def computePlacement(
        nodes: Map[Int, NodeMetadata],
        lazyBindings: Map[BindingId, LazyBindingInfo]
    ): Map[BindingId, LazyBindingInfo] =
        // First pass: collect usage information
        val usageMap = mutable.Map.empty[BindingId, mutable.Set[Int]]

        nodes.values.foreach { node =>
            node.varsUsedHere.foreach { varName =>
                node.bindingsInScope.get(varName).foreach { bindingId =>
                    usageMap.getOrElseUpdate(bindingId, mutable.Set.empty) += node.index
                }
            }
        }

        // Update lazy bindings with usage information and compute placement
        lazyBindings.map { case (bindingId, info) =>
            val usages = usageMap.getOrElse(bindingId, mutable.Set.empty).toSet

            if usages.isEmpty then
                // Unused binding: will be eliminated
                bindingId -> info.copy(usageNodes = usages, dominatingNode = None)
            else
                // Find the lowest common ancestor (LCA) of all usage nodes
                val lca = findLowestCommonAncestor(usages.toList, nodes)

                // Ensure LCA is valid (all dependencies are in scope)
                val validScopeNode = findMinValidNode(lca, info.dependencies, nodes)

                // Find the nearest valid insertion point (where we can actually insert a Let)
                val insertionPoint = findNearestValidInsertionPoint(validScopeNode, nodes)

                bindingId -> info.copy(usageNodes = usages, dominatingNode = Some(insertionPoint))
        }

    /** Find the lowest common ancestor of a list of node indices. */
    private def findLowestCommonAncestor(
        nodeIndices: List[Int],
        nodes: Map[Int, NodeMetadata]
    ): Int =
        if nodeIndices.isEmpty then 0
        else if nodeIndices.length == 1 then nodeIndices.head
        else
            // Get paths to root for all nodes
            val paths = nodeIndices.map(getPathToRoot(_, nodes))

            // Find common prefix
            val minPathLength = paths.map(_.length).min
            var lcaIndex = 0
            var i = 0
            while i < minPathLength && paths.forall(_(i) == paths.head(i)) do
                lcaIndex = i
                i += 1

            paths.head(lcaIndex)

    /** Get the path from a node to the root. */
    private def getPathToRoot(nodeIndex: Int, nodes: Map[Int, NodeMetadata]): List[Int] =
        var path = List(nodeIndex)
        var current = nodeIndex
        while nodes(current).parentIndex.isDefined do
            current = nodes(current).parentIndex.get
            path = current :: path
        path

    /** Find the minimum valid node where all dependencies are in scope.
      *
      * Starting from the proposed node, move up the tree until all dependencies are in scope.
      */
    private def findMinValidNode(
        proposedNode: Int,
        dependencies: Set[BindingId],
        nodes: Map[Int, NodeMetadata]
    ): Int =
        var current = proposedNode

        while !allDependenciesInScope(current, dependencies, nodes) do
            nodes(current).parentIndex match
                case Some(parent) => current = parent
                case None         => return current // At root, can't go higher

        current

    /** Check if all dependencies are in scope at a given node. */
    private def allDependenciesInScope(
        nodeIndex: Int,
        dependencies: Set[BindingId],
        nodes: Map[Int, NodeMetadata]
    ): Boolean =
        val scope = nodes(nodeIndex).bindingsInScope
        dependencies.forall { dep =>
            scope.get(dep.name).exists(_ == dep)
        }

    /** Check if a node is a valid insertion point for a Let binding.
      *
      * Valid insertion points are places where we can wrap the expression with a Let:
      *   - Root (no parent)
      *   - Body of Let (handled implicitly since we skip lazy lets)
      *   - Body of LamAbs
      *   - Branches of IfThenElse
      *   - Bodies of Match cases
      *
      * Invalid insertion points:
      *   - Leaf nodes (Var, Const, Builtin, ExternalVar)
      *   - Arguments to Apply
      *   - Scrutinees
      */
    private def isValidInsertionPoint(nodeIndex: Int, nodes: Map[Int, NodeMetadata]): Boolean =
        val node = nodes(nodeIndex)
        node.parentIndex match
            case None => true // Root is always valid
            case Some(parentIdx) =>
                val parent = nodes(parentIdx)
                parent.sir match
                    // Can insert at body of lazy let (since we're replacing it)
                    case SIR.Let(_, _, flags, _) if flags.isLazy && !flags.isRec =>
                        // Check if this node is the body (last child)
                        parent.childIndices.lastOption.contains(nodeIndex)
                    // Can insert at body of non-lazy let
                    case SIR.Let(_, _, _, _) =>
                        // Check if this node is the body (last child)
                        parent.childIndices.lastOption.contains(nodeIndex)
                    // Can insert at body of lambda
                    case SIR.LamAbs(_, _, _, _) => true
                    // Can insert at branches of IfThenElse (but not condition)
                    case SIR.IfThenElse(_, _, _, _, _) =>
                        parent.childIndices.indexOf(nodeIndex) != 0 // Not the condition
                    // Can insert at case bodies (not scrutinee)
                    case SIR.Match(_, _, _, _) =>
                        nodeIndex != parent.childIndices.head // Not the scrutinee
                    // Cannot insert at Apply arguments, scrutinees, etc.
                    case _ => false

    /** Find the nearest ancestor that is a valid insertion point. */
    private def findNearestValidInsertionPoint(
        nodeIndex: Int,
        nodes: Map[Int, NodeMetadata]
    ): Int =
        var current = nodeIndex
        while !isValidInsertionPoint(current, nodes) do
            nodes(current).parentIndex match
                case Some(parent) => current = parent
                case None         => return current // At root, must stop

        current

    // ============================================================================
    // Phase 3: Reconstruct SIR
    // ============================================================================

    /** Topologically sort bindings so dependencies come before their dependents.
      *
      * Uses a simple algorithm: repeatedly find bindings with no unmet dependencies.
      */
    private def topologicalSort(bindings: List[LazyBindingInfo]): List[LazyBindingInfo] =
        if bindings.length <= 1 then return bindings

        val bindingIds = bindings.map(_.bindingId).toSet
        val sorted = mutable.ListBuffer.empty[LazyBindingInfo]
        val remaining = mutable.ListBuffer.from(bindings)

        while remaining.nonEmpty do
            // Find a binding whose dependencies are all satisfied (either not in this group or already sorted)
            val nextIdx = remaining.indexWhere { info =>
                val unresolvedDeps = info.dependencies.filter(bindingIds.contains)
                unresolvedDeps.forall(dep => sorted.exists(_.bindingId == dep))
            }

            if nextIdx == -1 then
                // Cycle detected or error - just append remaining in original order
                sorted ++= remaining
                remaining.clear()
            else sorted += remaining.remove(nextIdx)

        sorted.toList

    /** Generate a unique name for a binding to avoid shadowing conflicts.
      *
      * This allows more aggressive floating by eliminating name collisions.
      */
    private def uniquifyName(bindingId: BindingId): String =
        s"${bindingId.name}_${bindingId.nodeIndex}"

    private def reconstructSIR(
        rootNode: NodeMetadata,
        nodes: Map[Int, NodeMetadata],
        lazyBindings: Map[BindingId, LazyBindingInfo]
    ): SIR =
        // Map from node index to bindings that should be inserted there
        val insertionMap = mutable.Map.empty[Int, List[LazyBindingInfo]]

        lazyBindings.values.foreach { info =>
            info.dominatingNode.foreach { targetNode =>
                val existing = insertionMap.getOrElse(targetNode, Nil)
                insertionMap(targetNode) = info :: existing
            }
        }

        // Sort bindings at each insertion point so dependencies come first
        insertionMap.foreach { case (nodeIdx, bindings) =>
            insertionMap(nodeIdx) = topologicalSort(bindings)
        }

        // Build renaming map: BindingId -> original name for all floated bindings
        // NOTE: Variable renaming is disabled for now to avoid scope corruption issues
        // with ExternalVar references. Future enhancement could add selective renaming
        // only when actual shadowing conflicts are detected.
        val renamingMap: Map[BindingId, String] =
            lazyBindings.keys.map { bindingId =>
                bindingId -> bindingId.name // Use original name, no renaming
            }.toMap

        def reconstruct(node: NodeMetadata): SIR =
            val baseSir = node.sir match
                case SIR.Decl(data, _) =>
                    // Reconstruct the term part
                    val childNode = nodes(node.childIndices.head)
                    SIR.Decl(data, reconstruct(childNode))

                case annSir: AnnotatedSIR =>
                    reconstructAnnotated(annSir, node)

            // Insert any lazy bindings that should be placed at this node
            insertionMap.get(node.index) match
                case Some(bindingsToInsert) =>
                    // Wrap with let expressions
                    // Since we're not renaming, just use the original values directly
                    val bindings = bindingsToInsert.map { info =>
                        Binding(info.bindingId.name, info.tp, info.value)
                    }
                    SIR.Let(
                      bindings,
                      baseSir,
                      SIR.LetFlags.None, // Remove lazy flag
                      AnnotationsDecl.empty
                    )
                case None => baseSir

        def reconstructAnnotated(sir: AnnotatedSIR, node: NodeMetadata): AnnotatedSIR =
            sir match
                case SIR.Var(name, tp, anns) =>
                    // Check if this variable refers to a floated binding that was renamed
                    node.bindingsInScope.get(name) match
                        case Some(bindingId) if renamingMap.contains(bindingId) =>
                            SIR.Var(renamingMap(bindingId), tp, anns)
                        case _ => sir

                case extVar: SIR.ExternalVar =>
                    // ExternalVars always reference external functions - never rename them
                    extVar

                case c: SIR.Const   => c
                case b: SIR.Builtin => b

                case SIR.Let(bindings, body, flags, anns) =>
                    if flags.isLazy && !flags.isRec then
                        // This is a lazy let that we're floating
                        // Skip it here, it will be inserted at its dominating node
                        val bodyNode = nodes(node.childIndices.last)
                        reconstruct(bodyNode).asInstanceOf[AnnotatedSIR]
                    else
                        // Non-lazy or recursive let: reconstruct normally
                        val newBindings =
                            bindings.zip(node.childIndices.take(bindings.length)).map {
                                case (binding, childIdx) =>
                                    val childNode = nodes(childIdx)
                                    Binding(binding.name, binding.tp, reconstruct(childNode))
                            }
                        val bodyNode = nodes(node.childIndices.last)
                        val newBody = reconstruct(bodyNode)
                        SIR.Let(newBindings, newBody, flags, anns)

                case SIR.LamAbs(param, term, typeParams, anns) =>
                    val termNode = nodes(node.childIndices.head)
                    SIR.LamAbs(param, reconstruct(termNode), typeParams, anns)

                case SIR.Apply(f, arg, tp, anns) =>
                    val fNode = nodes(node.childIndices(0))
                    val argNode = nodes(node.childIndices(1))
                    SIR.Apply(
                      reconstruct(fNode).asInstanceOf[AnnotatedSIR],
                      reconstruct(argNode).asInstanceOf[AnnotatedSIR],
                      tp,
                      anns
                    )

                case SIR.Select(scrutinee, field, tp, anns) =>
                    val scrNode = nodes(node.childIndices.head)
                    SIR.Select(reconstruct(scrNode), field, tp, anns)

                case SIR.And(a, b, anns) =>
                    val aNode = nodes(node.childIndices(0))
                    val bNode = nodes(node.childIndices(1))
                    SIR.And(
                      reconstruct(aNode).asInstanceOf[AnnotatedSIR],
                      reconstruct(bNode).asInstanceOf[AnnotatedSIR],
                      anns
                    )

                case SIR.Or(a, b, anns) =>
                    val aNode = nodes(node.childIndices(0))
                    val bNode = nodes(node.childIndices(1))
                    SIR.Or(
                      reconstruct(aNode).asInstanceOf[AnnotatedSIR],
                      reconstruct(bNode).asInstanceOf[AnnotatedSIR],
                      anns
                    )

                case SIR.Not(a, anns) =>
                    val aNode = nodes(node.childIndices.head)
                    SIR.Not(reconstruct(aNode).asInstanceOf[AnnotatedSIR], anns)

                case SIR.IfThenElse(cond, t, f, tp, anns) =>
                    val condNode = nodes(node.childIndices(0))
                    val tNode = nodes(node.childIndices(1))
                    val fNode = nodes(node.childIndices(2))
                    SIR.IfThenElse(
                      reconstruct(condNode).asInstanceOf[AnnotatedSIR],
                      reconstruct(tNode).asInstanceOf[AnnotatedSIR],
                      reconstruct(fNode).asInstanceOf[AnnotatedSIR],
                      tp,
                      anns
                    )

                case SIR.Constr(name, data, args, tp, anns) =>
                    val newArgs = node.childIndices.map(idx => reconstruct(nodes(idx)))
                    SIR.Constr(name, data, newArgs, tp, anns)

                case SIR.Match(scrutinee, cases, tp, anns) =>
                    val scrNode = nodes(node.childIndices.head)
                    val newScrutinee = reconstruct(scrNode).asInstanceOf[AnnotatedSIR]

                    val newCases = cases.zip(node.childIndices.tail).map {
                        case (SIR.Case(pattern, body, caseAnns), bodyIdx) =>
                            val bodyNode = nodes(bodyIdx)
                            SIR.Case(pattern, reconstruct(bodyNode), caseAnns)
                    }

                    SIR.Match(newScrutinee, newCases, tp, anns)

                case SIR.Cast(term, tp, anns) =>
                    val termNode = nodes(node.childIndices.head)
                    SIR.Cast(reconstruct(termNode).asInstanceOf[AnnotatedSIR], tp, anns)

                case SIR.Error(msg, anns, cause) =>
                    val msgNode = nodes(node.childIndices.head)
                    SIR.Error(reconstruct(msgNode).asInstanceOf[AnnotatedSIR], anns, cause)

        reconstruct(rootNode)

end LetFloatingV1
