package scalus.sir.simpleLowering

import scalus.sir.*

import java.util

object LetFloatingV2 {

    case class NameBindingId(
        name: String,
        nodeId: Int
    )

    case class UsageVarRecord(
        nEntries: Int,
        nChildsWithUsage: Int,
        usedDirectly: Boolean,
        minDirectOrManyChilds: Int,
    )

    /** Partially created parent node info, used during the construction of the full NodeInfo tree.
      * @param id
      */
    case class TopSideNodeInfo(
        id: Int,
        definedHereVars: Set[String],
        isFloating: Boolean,
        optParentId: Option[Int]
    )

    case class BottomSideNodeInfo(
        id: Int,
        children: List[Int],
        usedVars: Map[NameBindingId, UsageVarRecord],
        sir: Option[SIR]
    )

    object BottomSideNodeInfo {

        def empty(id: Int): BottomSideNodeInfo = {
            BottomSideNodeInfo(
              id = id,
              children = List.empty,
              usedVars = Map.empty,
              sir = None
            )
        }

    }

    def apply(sir: SIR): SIR = {
        val ctx = new BuildContext()
        val rootNodeId = ctx.incrNodeId
        val rootTopSideNodeInfo = TopSideNodeInfo(
          id = rootNodeId,
          definedHereVars = Set.empty,
          isFloating = false,
          optParentId = None
        )
        ctx.addTopSideNode(rootTopSideNodeInfo)
        val rootBottomSideNodeInfo = buildNodeInfo(sir, None, rootNodeId, ctx)
        ctx.addBottomSideNode(rootBottomSideNodeInfo)
        // now find new minimal placement for let bindings

        rootBottomSideNodeInfo.usedVars
        ???

    }

    class BuildContext {
        private var nextNodeId = 0

        // partial nodes are used during the phirst pase, when we traverse down teh sir.
        // then all parent nodes are here and we can use them for name resolving.
        private var topSideNodes: Map[Int, TopSideNodeInfo] = Map.empty

        // full nodes are stored here when we build them going back up the sir tree.
        private var bottomSizeNodes: Map[Int, BottomSideNodeInfo] = Map.empty

        private var dependencyMap: Map[NameBindingId, Set[NameBindingId]] = Map.empty
        private var reverseDependencyMap: Map[NameBindingId, Set[NameBindingId]] = Map.empty

        private var minimumUsage: Map[NameBindingId, Int] = Map.empty

        private val bySirLocator: java.util.IdentityHashMap[SIR, Int] =
            new util.IdentityHashMap()

        def incrNodeId: Int = {
            val current = nextNodeId
            nextNodeId += 1
            current
        }

        def lastNextNodeId: Int = nextNodeId

        def resetNodeId: Unit = {
            nextNodeId = 0
        }

        def getTopSideNode(id: Int): Option[TopSideNodeInfo] = topSideNodes.get(id)

        def addTopSideNode(partialNode: TopSideNodeInfo): Unit = {
            topSideNodes = topSideNodes + (partialNode.id -> partialNode)
        }

        def getBottomSideNode(id: Int): Option[BottomSideNodeInfo] = bottomSizeNodes.get(id)

        def addBottomSideNode(node: BottomSideNodeInfo): Unit = {
            bottomSizeNodes = bottomSizeNodes + (node.id -> node)
            node.sir match
                case Some(sir) => bySirLocator.put(sir, node.id)
                case None      => ()
        }

        def addDependencies(to: NameBindingId, from: Set[NameBindingId]): Unit = {
            dependencyMap.get(to) match {
                case None =>
                    dependencyMap = dependencyMap + (to -> from)
                case Some(existing) =>
                    val updated = existing ++ from
                    dependencyMap = dependencyMap + (to -> updated)
            }
            for fromId <- from do {
                reverseDependencyMap.get(fromId) match {
                    case None =>
                        reverseDependencyMap = reverseDependencyMap + (fromId -> Set(to))
                    case Some(existing) =>
                        val updated = existing + to
                        reverseDependencyMap = reverseDependencyMap + (fromId -> updated)
                }
            }
        }

        def getDependencies(
            bindingId: NameBindingId
        ): Set[NameBindingId] = {
            dependencyMap.get(bindingId) match {
                case None       => Set.empty[NameBindingId]
                case Some(deps) => deps
            }
        }

        def getDependentOn(
            bindingId: NameBindingId
        ): Set[NameBindingId] = {
            reverseDependencyMap.get(bindingId) match {
                case None       => Set.empty[NameBindingId]
                case Some(deps) => deps
            }
        }

        def getMinimumUsage(
            bindingId: NameBindingId
        ): Option[Int] = minimumUsage.get(bindingId)

        def setMinimumUsage(
            bindingId: NameBindingId,
            nodeId: Int
        ): Unit = {
            minimumUsage = minimumUsage + (bindingId -> nodeId)
        }

        def resolve(name: String, fromNodeId: Int): Option[NameBindingId] = {
            topSideNodes.get(fromNodeId).flatMap { node =>
                if node.definedHereVars.contains(name) then {
                    Some(NameBindingId(name, node.id))
                } else {
                    node.optParentId.flatMap { parentId =>
                        resolve(name, parentId)
                    }
                }
            }
        }

        def nodeIndexBySIR(sir: SIR): Option[Int] = {
            val nodeId = bySirLocator.get(sir)
            Option(nodeId)
        }

    }

    /** return partially-constructed NodeInfo for the current node, which later merged.
      */
    private def buildNodeInfo(
        sir: SIR,
        optParentNodeId: Option[Int],
        currentNodeId: Int,
        buildContext: BuildContext
    ): BottomSideNodeInfo = {
        require(buildContext.getTopSideNode(currentNodeId).isDefined)
        sir match {
            case SIR.Decl(data, term) =>
                buildNodeInfo(
                  sir,
                  optParentNodeId,
                  currentNodeId,
                  buildContext
                )
            case SIR.Let(bindings, body, flags, anns) =>
                val definedHereVars = bindings.map(_.name).toSet
                val isFloating = flags.isLazy
                val letNodeId = buildContext.incrNodeId
                val letTopSideNode =
                    TopSideNodeInfo(letNodeId, definedHereVars, isFloating, Some(currentNodeId))
                buildContext.addTopSideNode(letTopSideNode)
                val bindedNodes = bindings.map { b =>
                    val rhsTopSideNode = TopSideNodeInfo(
                      id = buildContext.incrNodeId,
                      definedHereVars = Set.empty,
                      isFloating = false,
                      optParentId = Some(letNodeId)
                    )
                    buildContext.addTopSideNode(rhsTopSideNode)
                    val r = buildNodeInfo(
                      b.value,
                      Some(letNodeId),
                      rhsTopSideNode.id,
                      buildContext
                    )
                    buildContext.addBottomSideNode(r.copy(sir = Some(b.value)))
                    val bindingId = NameBindingId(b.name, letNodeId)
                    val dependencySet = r.usedVars.keySet
                    buildContext.addDependencies(bindingId, dependencySet)
                    r
                }

                val bodyTopSide = TopSideNodeInfo(
                  id = buildContext.incrNodeId,
                  definedHereVars = Set.empty,
                  isFloating = false,
                  optParentId = Some(letNodeId)
                )
                buildContext.addTopSideNode(bodyTopSide)
                val bodyNode = buildNodeInfo(
                  body,
                  Some(letNodeId),
                  bodyTopSide.id,
                  buildContext
                )
                buildContext.addBottomSideNode(bodyNode.copy(sir = Some(body)))

                val letNodeInfo = BottomSideNodeInfo(
                  letNodeId,
                  bodyNode.id +: bindedNodes.map(_.id),
                  absorbUsedVarsFromChildren(
                    currentNodeId,
                    bindedNodes ++ List(bodyNode),
                    buildContext
                  ),
                  Some(sir)
                )
                buildContext.addBottomSideNode(letNodeInfo)

                val retval = BottomSideNodeInfo(
                  currentNodeId,
                  List(letNodeId),
                  absorbUsedVarsFromChildren(currentNodeId, List(letNodeInfo), buildContext),
                  None
                )
                retval
            case SIR.LamAbs(param, term, typeParams, anns) =>
                val newLamNodeId = buildContext.incrNodeId
                val lamTopSideNode = TopSideNodeInfo(
                  id = newLamNodeId,
                  definedHereVars = Set(param.name),
                  isFloating = false,
                  Some(currentNodeId)
                )
                buildContext.addTopSideNode(lamTopSideNode)
                val bodyTopSide = TopSideNodeInfo(
                  id = buildContext.incrNodeId,
                  definedHereVars = Set.empty,
                  isFloating = false,
                  Some(newLamNodeId)
                )
                buildContext.addTopSideNode(bodyTopSide)
                val bodyNode = buildNodeInfo(term, Some(newLamNodeId), bodyTopSide.id, buildContext)
                buildContext.addBottomSideNode(bodyNode.copy(sir = Some(term)))
                val lamNodeInfo = BottomSideNodeInfo(
                  newLamNodeId,
                  List(bodyNode.id),
                  absorbUsedVarsFromChildren(newLamNodeId, List(bodyNode), buildContext),
                  Some(sir)
                )
                buildContext.addBottomSideNode(lamNodeInfo)
                val retval = BottomSideNodeInfo(
                  currentNodeId,
                  List(newLamNodeId),
                  absorbUsedVarsFromChildren(currentNodeId, List(lamNodeInfo), buildContext),
                  None
                )
                retval
            case SIR.Apply(func, arg, tp, anns) =>
                val funcNode = buildNodeInfo(
                  func,
                  optParentNodeId,
                  currentNodeId,
                  buildContext
                )
                val argNode = buildNodeInfo(
                  arg,
                  optParentNodeId,
                  buildContext.incrNodeId,
                  buildContext
                )
                mergeBottomSidesSameLevel(currentNodeId, List(funcNode, argNode), buildContext)
            case SIR.Var(name, tp, anns) =>
                val usedVars = buildContext.resolve(name, currentNodeId) match {
                    case Some(nameBindingId) =>
                        Map(
                          nameBindingId -> UsageVarRecord(
                            nEntries = 1,
                            nChildsWithUsage = 0,
                            usedDirectly = true,
                            minDirectOrManyChilds = currentNodeId
                          )
                        )
                    case None =>
                        Map.empty
                }
                BottomSideNodeInfo(
                  id = currentNodeId,
                  children = List.empty,
                  usedVars = usedVars,
                  sir = Some(sir)
                )
            case SIR.ExternalVar(module, name, tp, anns) =>
                val usedVars = buildContext.resolve(name, currentNodeId) match {
                    case Some(nameBindingId) =>
                        Map(
                          nameBindingId -> UsageVarRecord(
                            nEntries = 1,
                            nChildsWithUsage = 0,
                            usedDirectly = true,
                            minDirectOrManyChilds = currentNodeId
                          )
                        )
                    case None =>
                        Map.empty
                }
                BottomSideNodeInfo(
                  id = currentNodeId,
                  children = List.empty,
                  usedVars = usedVars,
                  sir = Some(sir)
                )
            case SIR.IfThenElse(cond, thenBranch, elseBranch, tp, anns) =>
                val condNode = buildNodeInfo(
                  cond,
                  optParentNodeId,
                  currentNodeId,
                  buildContext
                )
                val thenNode = buildNodeInfo(
                  thenBranch,
                  optParentNodeId,
                  buildContext.incrNodeId,
                  buildContext
                )
                val elseNode = buildNodeInfo(
                  elseBranch,
                  optParentNodeId,
                  buildContext.incrNodeId,
                  buildContext
                )
                mergeBottomSidesSameLevel(
                  currentNodeId,
                  List(condNode, thenNode, elseNode),
                  buildContext
                )
            case SIR.Match(scrutinee, cases, tp, anns) =>
                val scrutineeTopSideNode = TopSideNodeInfo(
                  id = buildContext.incrNodeId,
                  definedHereVars = Set.empty,
                  isFloating = false,
                  optParentNodeId
                )
                buildContext.addTopSideNode(scrutineeTopSideNode)
                val scrutineeNode = buildNodeInfo(
                  scrutinee,
                  optParentNodeId,
                  currentNodeId,
                  buildContext
                )
                buildContext.addBottomSideNode(scrutineeNode)
                val caseNodes = cases.map { c =>
                    val topSideNodeInfo = TopSideNodeInfo(
                      id = buildContext.incrNodeId,
                      definedHereVars = c.pattern match {
                          case SIR.Pattern.Constr(_, bindings, _) => bindings.toSet
                          case SIR.Pattern.Const(_)               => Set.empty
                          case SIR.Pattern.Wildcard               => Set.empty
                      },
                      isFloating = false,
                      optParentNodeId
                    )
                    buildContext.addTopSideNode(topSideNodeInfo)
                    val caseNode = buildNodeInfo(
                      c.body,
                      optParentNodeId,
                      buildContext.incrNodeId,
                      buildContext
                    )
                    buildContext.addBottomSideNode(caseNode.copy(sir = Some(c.body)))
                    caseNode
                }
                val childNodes = scrutineeNode +: caseNodes
                val retval = BottomSideNodeInfo(
                  currentNodeId,
                  childNodes.map(_.id),
                  absorbUsedVarsFromChildren(currentNodeId, childNodes, buildContext),
                  None
                )
                retval
            case SIR.Constr(data, name, args, tp, anns) =>
                val argNodes = args.map { arg =>
                    buildNodeInfo(arg, optParentNodeId, currentNodeId, buildContext)
                }
                mergeBottomSidesSameLevel(currentNodeId, argNodes, buildContext)
            case SIR.Const(value, tp, anns) =>
                BottomSideNodeInfo.empty(currentNodeId)
            case _ =>
                // TODO: implement other SIR nodes
                ???
        }

    }

    // called during bottom-up construction of BottomSideNodeInfo
    private def absorbUsedVarsFromChildren(
        currentNodeId: Int,
        childNodes: List[BottomSideNodeInfo],
        context: BuildContext
    ): Map[NameBindingId, UsageVarRecord] = {
        var usedVars: Map[NameBindingId, UsageVarRecord] = Map.empty

        for child <- childNodes do {
            for (nameBindingId, usageRecord) <- child.usedVars if nameBindingId.nodeId != child.id
            do {
                val updatedRecord = usedVars.get(nameBindingId) match {
                    case Some(existingRecord) =>
                        val newNChildsWithUsage = existingRecord.nChildsWithUsage + 1
                        val newMinDirectOrManyChilds =
                            if newNChildsWithUsage > 1 then currentNodeId
                            else
                                Math.min(
                                  existingRecord.minDirectOrManyChilds,
                                  usageRecord.minDirectOrManyChilds
                                )
                        UsageVarRecord(
                          nEntries = existingRecord.nEntries + usageRecord.nEntries,
                          nChildsWithUsage = newNChildsWithUsage,
                          usedDirectly = false,
                          minDirectOrManyChilds = newMinDirectOrManyChilds
                        )
                    case None =>
                        usageRecord
                }
                usedVars = usedVars + (nameBindingId -> updatedRecord)
            }
        }

        usedVars
    }

    private def mergeBottomSidesSameLevel(
        currentNodeId: Int,
        nodes: List[BottomSideNodeInfo],
        context: BuildContext
    ): BottomSideNodeInfo = {
        val zero = context
            .getBottomSideNode(currentNodeId)
            .getOrElse(
              BottomSideNodeInfo(
                id = currentNodeId,
                children = List.empty,
                usedVars = Map.empty,
                sir = None
              )
            )
        val sameLevel = nodes.foldLeft(zero) { (acc, node) =>
            if node.id != currentNodeId then
                throw IllegalStateException(
                  s"Trying to merge BottomSideNodes on the same level, but node id ${node.id} differs from current node id $currentNodeId"
                )
            val usedVars = mergeUsedVarsInNode(currentNodeId, acc.usedVars, node.usedVars)
            val children = acc.children ++ node.children
            BottomSideNodeInfo(
              id = currentNodeId,
              children = children,
              usedVars = usedVars,
              sir = None
            )
        }
        val childNodes = sameLevel.children.map { childId =>
            context
                .getBottomSideNode(childId)
                .getOrElse(
                  throw IllegalStateException(s"BotomSideNode for ${childId} is missing")
                )
        }
        val usedVarsWithChildsAbsorbed =
            absorbUsedVarsFromChildren(currentNodeId, childNodes, context)
        BottomSideNodeInfo(
          id = currentNodeId,
          children = sameLevel.children,
          usedVars = usedVarsWithChildsAbsorbed,
          sir = None
        )
    }

    private def mergeUsedVarsInNode(
        currentNodeId: Int,
        left: Map[NameBindingId, UsageVarRecord],
        right: Map[NameBindingId, UsageVarRecord]
    ): Map[NameBindingId, UsageVarRecord] = {
        var retval = left
        for (nameBindingId, usageRecord) <- right do {
            val updatedRecord = retval.get(nameBindingId) match {
                case Some(existingRecord) =>
                    val newUsedDirectly =
                        existingRecord.usedDirectly || usageRecord.usedDirectly
                    val nChildsWithUsage =
                        existingRecord.nChildsWithUsage + usageRecord.nChildsWithUsage
                    val newMinDirectOrManyChilds =
                        if newUsedDirectly || nChildsWithUsage > 1 then currentNodeId
                        else
                            Math.min(
                              existingRecord.minDirectOrManyChilds,
                              usageRecord.minDirectOrManyChilds
                            )
                    UsageVarRecord(
                      nEntries = existingRecord.nEntries + usageRecord.nEntries,
                      nChildsWithUsage =
                          existingRecord.nChildsWithUsage + usageRecord.nChildsWithUsage,
                      usedDirectly = existingRecord.usedDirectly || usageRecord.usedDirectly,
                      minDirectOrManyChilds = Math.min(
                        existingRecord.minDirectOrManyChilds,
                        usageRecord.minDirectOrManyChilds
                      )
                    )
                case None =>
                    usageRecord
            }
            retval = retval + (nameBindingId -> updatedRecord)
        }
        retval
    }

    private def calculatePlacementNode(
        bindingId: NameBindingId,
        buildContext: BuildContext
    ): Int = {
        buildContext.getMinimumUsage(bindingId) match
            case Some(nodeId) =>
                if nodeId < 0 then
                    throw IllegalStateException(
                      s"Minimum usage node id for binding $bindingId is invalid: $nodeId, looping dependencies?"
                    )
                else nodeId
            case None =>
                buildContext.setMinimumUsage(bindingId, -1)
                val ownTopNode = buildContext
                    .getTopSideNode(bindingId.nodeId)
                    .getOrElse(
                      throw IllegalStateException(
                        s"TopSideNode for binding $bindingId not found"
                      )
                    )
                if ownTopNode.isFloating then
                    val ownBottomNode = buildContext
                        .getBottomSideNode(bindingId.nodeId)
                        .getOrElse(
                          throw IllegalStateException(
                            s"BottomSideNode for binding $bindingId not found"
                          )
                        )
                    val ownMin = ownBottomNode.usedVars.get(bindingId) match
                        case Some(usageRecord) => usageRecord.minDirectOrManyChilds
                        case None              => Int.MaxValue
                    val dependent = buildContext.getDependentOn(bindingId)
                    val depPlacementNodeId = dependent.foldLeft(Int.MaxValue) {
                        (acc, depBindingId) =>
                            calculatePlacementNode(depBindingId, buildContext) min acc
                    }
                    val finalPlacementNodeId = ownMin min depPlacementNodeId
                    buildContext.setMinimumUsage(bindingId, finalPlacementNodeId)
                    finalPlacementNodeId
                else
                    buildContext.setMinimumUsage(bindingId, bindingId.nodeId)
                    bindingId.nodeId
    }

    // structure of recursion here should be exactly as in buildNodeInfo, to ensure we rebuild the same SIR structure
    // with the same node ids.
    private def rebuildSIR(
        sir: SIR,
        optParentNodeId: Option[Int],
        unallocatedLets: Map[Int, Map[NameBindingId, SIR]],
        buildContext: BuildContext
    ): SIR = {

        sir match
            case SIR.Decl(data, tem) =>
                val newTerm =
                    rebuildSIR(tem, currentNodeId, optParentNodeId, unallocatedLets, buildContext)
                SIR.Decl(data, newTerm)
            case SIR.Let(bindings, body, flags, anns) =>
                val letNodeId = buildContext.incrNodeId
                val newUnallocatedLets =
                    if flags.isLazy then
                        val thisUnalloctedLets = bindings
                            .map(b => {
                                val nameBindingId = NameBindingId(b.name, letNodeId)
                                val newPlacement =
                                    calculatePlacementNode(nameBindingId, buildContext)
                            })
                        unallocatedLets ++ thisUnalloctedLets
                    else unallocatedLets

                val bindingNodeIds = bindings.map { b =>
                    val rhsNodeId = buildContext.incrNodeId
                    val newRhs = rebuildSIR(
                      b.value,
                      letNodeId,
                      Some(currentNodeId),
                      unallocatedLets,
                      buildContext
                    )
                    (b.name, newRhs, rhsNodeId)
                }
                ???
    }

}
