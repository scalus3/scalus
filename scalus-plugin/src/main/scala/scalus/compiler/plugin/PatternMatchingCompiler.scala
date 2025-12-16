package scalus.compiler.plugin

import dotty.tools.dotc.*
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.*
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.core.Types.*
import dotty.tools.dotc.util.{SourcePosition, SrcPos}
import scalus.compiler.plugin.SirCaseDecisionTree.SplitStrategy.{DuplicateChecks, DuplicateRows}
import scalus.compiler.plugin.SirParsedCase.ActionRef.FailMatch
import scalus.compiler.plugin.SirParsedCase.{ActionRef, BindingNameInfo, GroupedTuples, Pattern}
import scalus.compiler.sir.*

import scala.annotation.{tailrec, unused}
import scala.collection.mutable.ListBuffer

class PatternMatchingContext(
    val globalPrefix: String,
    var env: SIRCompiler.Env,
    val topLevelPos: SrcPos,
    // should be set after parsing.
    var parsedActions: IndexedSeq[SirParsedAction] = IndexedSeq.empty,
    var parsedGuards: IndexedSeq[Option[SirParsedGuard]] = IndexedSeq.empty,
    var isUnchecked: Boolean = false,
    var decisionTreeRefs: IndexedSeq[SirCaseDecisionTree] = IndexedSeq.empty
) {

    private var tmpNameCounter: Int = 1
    private var scopeDepth: Int = 0 // Track nesting depth for scoped variable names

    private var _actionsUsageCount: Map[Int, Int] = Map.empty
    private var _guardsUsageCount: Map[Int, Int] = Map.empty
    private var _scrutineeName: String = ""

    private var _decisionTreeRefsUsageCount: Map[Int, Int] = Map.empty

    def freshName(prefix: String = ""): String = {
        val localPrefix = prefix
        val name = s"${globalPrefix}${localPrefix}_$tmpNameCounter"
        tmpNameCounter += 1
        name
    }

    def scrutineeName: String = {
        if _scrutineeName.isEmpty then _scrutineeName = freshName("scrutinee")
        _scrutineeName
    }

    def setScrutineeName(name: String): Unit = {
        _scrutineeName = name
    }

    def actionsUsageCount: Map[Int, Int] = _actionsUsageCount

    def countActionUsage(actionIndex: Int): Int = {
        _actionsUsageCount.get(actionIndex) match
            case Some(c) =>
                _actionsUsageCount = _actionsUsageCount + (actionIndex -> (c + 1))
                c + 1
            case None =>
                _actionsUsageCount = _actionsUsageCount + (actionIndex -> 1)
                1
    }

    def guardsUsageCount: Map[Int, Int] = _guardsUsageCount

    def countGuardUsage(guardIndex: Int): Int = {
        _guardsUsageCount.get(guardIndex) match
            case Some(c) =>
                _guardsUsageCount = _guardsUsageCount + (guardIndex -> (c + 1))
                c + 1
            case None =>
                _guardsUsageCount = _guardsUsageCount + (guardIndex -> 1)
                1
    }

    def decisionTreeRefUsageCount: Map[Int, Int] = _decisionTreeRefsUsageCount

    def countDecisionTreeRefUsage(refIndex: Int): Int = {
        _decisionTreeRefsUsageCount.get(refIndex) match
            case Some(c) =>
                _decisionTreeRefsUsageCount = _decisionTreeRefsUsageCount + (refIndex -> (c + 1))
                c + 1
            case None =>
                _decisionTreeRefsUsageCount = _decisionTreeRefsUsageCount + (refIndex -> 1)
                1
    }

    def createLeaf(
        columnBinding: IndexedSeq[SirParsedCase.BindingNameInfo],
        row: SirParsedCase.GroupedTupleRow
    ): SirCaseDecisionTree.Leaf = {
        if columnBinding.size != row.patterns.size then
            throw IllegalStateException("columnBinding.size != row.patterns.size")
        row.actionRef match {
            case SirParsedCase.ActionRef.Origin(i) =>
                countActionUsage(i)
            case _ =>
        }
        val binding = row.patterns.zip(columnBinding).foldLeft(Map.empty[VariableKey, String]) {
            case (m, (p, cb)) =>
                p.optNameInfo match {
                    case None => m
                    case Some(nameInfo) =>
                        val key = VariableKey(nameInfo.name, nameInfo.symbol)
                        m + (key -> cb.name)
                }
        }
        SirCaseDecisionTree.Leaf(binding, row.actionRef, row.pos)
    }

}

/** representation, suitable to optimization, simular to
  *
  * http://moscova.inria.fr/~maranget/papers/ml05e-maranget.pdf
  */
case class SirParsedCase(
    pattern: SirParsedCase.Pattern,
    guardRef: Option[Int],
    actionRef: SirParsedCase.ActionRef,
    pos: SrcPos
)

object SirParsedCase:

    sealed trait Pattern:

        def pos: SrcPos

        def withAlias(nameInfo: BindingNameInfo): Pattern

        def withOptAlias(optNameInfo: Option[BindingNameInfo]): Pattern =
            optNameInfo match
                case None           => this
                case Some(nameInfo) => this.withAlias(nameInfo)

        def optNameInfo: Option[BindingNameInfo]

        def collectBindingInfos: Map[VariableKey, BindingNameInfo]

        // def collectNames: Map[String, SIRType]

        /*
        def collectSymbols: Map[String, Option[Symbol]] = {
            def collectFromNameInfo(info: BindingNameInfo): Map[String, Option[Symbol]] =
                Map(info.name -> info.symbol)

            this match {
                case ts: Pattern.TypeSelector =>
                    ts.optNameInfo
                        .map(collectFromNameInfo)
                        .getOrElse(Map.empty) ++ ts.innerPattern.collectSymbols
                case c: Pattern.Constructor =>
                    val subcasesSymbols = c.subcases.flatMap(_.collectSymbols).toMap
                    c.optNameInfo.map(collectFromNameInfo).getOrElse(Map.empty) ++ subcasesSymbols
                case w: Pattern.Wildcard =>
                    w.optNameInfo.map(collectFromNameInfo).getOrElse(Map.empty)
                case _: Pattern.PrimitiveConstant =>
                    Map.empty
                case or: Pattern.OrPattern =>
                    // OrPattern doesn't have bindings, collect from sub-patterns
                    or.patterns.flatMap(_.collectSymbols).toMap
                case _: Pattern.ErrorPattern =>
                    Map.empty
            }
        }
        
         */

        def show: String

    end Pattern

    object Pattern:

        case class TypeSelector(
            tp: SIRType,
            optNameInfo: Option[BindingNameInfo],
            innerPattern: Pattern,
            pos: SrcPos
        )(using Context)
            extends Pattern {

            optNameInfo match {
                case Some(nameInfo) =>
                    SIRUnify.topLevelUnifyType(tp, nameInfo.tp, SIRUnify.Env.empty) match {
                        case SIRUnify.UnificationSuccess(emv, u) =>
                        case SIRUnify.UnificationFailure(path, l, r) =>
                            throw IllegalStateException(
                              s"TypeSelector: type selector type ${tp.show} is not compatible with binding type ${nameInfo.tp.show} at ${pos.sourcePos.show} in path ${path}"
                            )
                    }
                case _ =>
            }

            def withAlias(alias: BindingNameInfo): Pattern = {
                optNameInfo match {
                    case None           => this.copy(optNameInfo = Some(alias))
                    case Some(nameInfo) => this.copy(optNameInfo = Some(nameInfo.withAlias(alias)))
                }
            }

            def collectBindingInfos = optNameInfo match {
                case None =>
                    innerPattern.collectBindingInfos
                case Some(nameInfo) =>
                    innerPattern.collectBindingInfos + (VariableKey(
                      nameInfo.name,
                      nameInfo.symbol
                    ) -> nameInfo)
            }

            def show: String = s"(${innerPattern.show} : ${tp.show})"

        }

        case class Constructor(
            tp: SIRType.CaseClass,
            freeTypeParams: List[SIRType.TypeVar],
            // filledTypeParams: Map[Symbol, SIRType],
            optNameInfo: Option[BindingNameInfo],
            // constructorSymbol: Symbol,
            subcases: IndexedSeq[Pattern],
            pos: SrcPos
        ) extends Pattern {

            def withAlias(alias: BindingNameInfo): Pattern = {
                optNameInfo match {
                    case None => this.copy(optNameInfo = Some(alias))
                    case Some(nameInfo) =>
                        this.copy(optNameInfo = Some(nameInfo.withAlias(alias)))
                }
            }

            override def collectBindingInfos: Map[VariableKey, BindingNameInfo] = {
                val subcasesNames = subcases.foldLeft(Map.empty[VariableKey, BindingNameInfo]) {
                    (m, p) =>
                        m ++ p.collectBindingInfos
                }
                optNameInfo match {
                    case None => subcasesNames
                    case Some(nameInfo) =>
                        subcasesNames + (VariableKey(nameInfo.name, nameInfo.symbol) -> nameInfo)
                }
            }

            def show: String =
                val constructorName = tp.constrDecl.name
                val subcasesStr = subcases.map(_.show).mkString(", ")
                s"$constructorName($subcasesStr)"

        }

        case class Wildcard(
            tp: SIRType,
            optNameInfo: Option[BindingNameInfo],
            pos: SrcPos
        ) extends Pattern {

            def withAlias(alias: BindingNameInfo): Pattern = {
                this.optNameInfo match {
                    case None           => this.copy(optNameInfo = Some(alias))
                    case Some(nameInfo) => this.copy(optNameInfo = Some(nameInfo.withAlias(alias)))
                }
            }

            override def collectBindingInfos: Map[VariableKey, BindingNameInfo] = {
                optNameInfo match {
                    case None           => Map.empty
                    case Some(nameInfo) => Map(nameInfo.variableKey -> nameInfo)
                }
            }

            override def show: String = s"*: ${tp.show}"

        }

        case class PrimitiveConstant(
            value: SIR.Const,
            optNameInfo: Option[BindingNameInfo] = None,
            pos: SourcePosition
        ) extends Pattern {

            def withAlias(alias: BindingNameInfo): Pattern =
                optNameInfo match
                    case Some(nameInfo) => this.copy(optNameInfo = Some(nameInfo.withAlias(alias)))
                    case None           => this.copy(optNameInfo = Some(alias))

            override def collectBindingInfos: Map[VariableKey, BindingNameInfo] =
                optNameInfo match
                    case None => Map.empty
                    case Some(nameInfo) =>
                        Map(nameInfo.variableKey -> nameInfo.copy(tp = value.tp))

            override def show: String = {
                value.uplcConst.toString
            }

        }

        case class OrPattern(patterns: List[Pattern], pos: SourcePosition) extends Pattern {
            def withAlias(alias: BindingNameInfo): Pattern =
                this.copy(patterns = this.patterns.map(_.withAlias(alias)))

            override def optNameInfo: Option[BindingNameInfo] = None

            override def collectBindingInfos: Map[VariableKey, BindingNameInfo] = Map.empty

            override def show: String = patterns.map(_.show).mkString(" | ")

        }

        class ErrorPattern private (val msg: String, val pos: SrcPos) extends Pattern {
            def withAlias(alias: BindingNameInfo): Pattern = this

            override def optNameInfo: Option[BindingNameInfo] = None

            override def collectBindingInfos: Map[VariableKey, BindingNameInfo] = Map.empty

            override def show: String = s"<ErrorPattern: $msg>"
        }

        object ErrorPattern {
            def apply(msg: String, pos: SrcPos, ctx: PatternMatchingContext)(using
                Context
            ): ErrorPattern = {
                if ctx.env.debug then
                    println(s"SirParsedCase.PatternError: $msg at ${pos.sourcePos.show}")
                report.error(msg, pos)
                new ErrorPattern(msg, pos)
            }

            def unapply(p: ErrorPattern): Option[(String, SrcPos)] = Some((p.msg, p.pos))

        }

    end Pattern

    enum ActionRef:
        case Origin(i: Int)
        case FailMatch
    end ActionRef

    case class BindingNameInfo(
        name: String,
        scalaName: Option[String],
        tp: SIRType,
        symbol: Option[Symbol],
        aliases: Set[BindingNameInfo] = Set.empty
    ) {

        def variableKey: VariableKey = VariableKey(name, symbol)

        def withAlias(alias: BindingNameInfo): BindingNameInfo =
            this.copy(aliases = this.aliases + alias)

        def show: String = {
            val symbolStr = symbol match {
                case Some(sym) => s"#${sym.id}"
                case None      => ""
            }
            s"$name$symbolStr: ${tp.show}"
        }
    }

    case class GroupedTupleRow(
        patterns: IndexedSeq[Pattern],
        guardRef: Option[Int],
        actionRef: ActionRef,
        pos: SrcPos
    )

    case class GroupedTuples(
        columnBindings: IndexedSeq[BindingNameInfo],
        activeColumns: Set[Int],
        rows: List[GroupedTupleRow]
    )

    case class GroupedTupleRowWithGuard(
        row: GroupedTupleRow,
        optGuard: Option[AnnotatedSIR],
    )

    case class JoinedConstructorCase(
        tp: SIRType.CaseClass,
        tuples: IndexedSeq[GroupedTupleRow]
    )

end SirParsedCase

case class SirParsedAction(
    sir: AnnotatedSIR,
    bindedVariables: Map[String, BindingNameInfo],
    pos: SrcPos
) {

    lazy val typeBindings: List[TypeBinding] =
        bindedVariables.map { case (name, info) => TypeBinding(name, info.tp) }.toList

}

case class SirParsedGuard(
    sir: AnnotatedSIR,
    bindedVariables: Map[String, BindingNameInfo],
    pos: SrcPos
) {

    lazy val typeBindings: List[TypeBinding] =
        bindedVariables.map { case (name, info) => TypeBinding(name, info.tp) }.toList

}

case class SirParsedMatch(
    scrutinee: AnnotatedSIR,
    cases: IndexedSeq[SirParsedCase],
    compiledActions: IndexedSeq[SirParsedAction],
    comliledGuards: IndexedSeq[Option[SirParsedGuard]],
    scrutineeTp: SIRType,
    resTp: SIRType,
    pos: SourcePosition
)

sealed trait SirCaseDecisionTree

object SirCaseDecisionTree:

    case class ConstructorsChoice(
        columnName: String,
        scrutineeTp: SIRType,
        byConstructors: Map[String, ConstructorEntry],
        defaultBranch: Option[SirCaseDecisionTree],
        pos: SrcPos
    ) extends SirCaseDecisionTree

    case class ConstructorEntry(
        caseClass: SIRType.CaseClass,
        names: IndexedSeq[SirParsedCase.BindingNameInfo],
        tree: SirCaseDecisionTree,
        pos: SrcPos
    )

    case class ConstantChoice(
        columnName: String,
        tp: SIRType.Primitive,
        byConstants: Map[SIR.Const, SirCaseDecisionTree],
        defaultBranch: Option[SirCaseDecisionTree],
        pos: SrcPos
    ) extends SirCaseDecisionTree

    case class CheckGuard(
        bingingMap: Map[VariableKey, String], // Use VariableKey to distinguish shadowed variables
        guard: Int,
        pos: SrcPos,
        nextTrue: SirCaseDecisionTree,
        nextFalse: Option[SirCaseDecisionTree]
    ) extends SirCaseDecisionTree

    case class Leaf(
        binding: Map[VariableKey, String], // Use VariableKey to distinguish shadowed variables
        action: SirParsedCase.ActionRef,
        pos: SrcPos
    ) extends SirCaseDecisionTree

    /** Index of decision tree in the decision tree references array, which builds during building
      * decisionn tree. When compiling decison tree,we decide - embed reference or embed tree
      * inline.
      * @param indexß
      */
    case class Reference(
        index: Int
    ) extends SirCaseDecisionTree

    /** let wehave situation
      * ```
      * case 1, r1tail => action1
      * case *, r2tail => action2
      * case 1, r3tail => action3
      * ```
      *
      * we have to variant to choose: A) split rows futher only when next row is also have constant
      * check. After first wildcard enty we return to full check, because the cost of duplicatong
      * row can be more . then the cost of yet one constant check.
      *
      * B) the alternative will be transform this to .
      * ```
      *  case 1, r1tail => action1 .
      *  case 1, r2tail => action2 .
      *  case 1, r3tail => action3 .
      *  --------
      *  . in rest: case *, r2tail => action2 .
      * ```
      * as in original article.
      */
    enum SplitStrategy:
        case DuplicateChecks
        case DuplicateRows

    /** Whe we compile decision tree, we can embed action or guard in two ways:
      *   - inline (when we copy action each time)
      *   - byReference (when we at first create a let-binding for action, and then use it by name)
      */
    enum EmbeddingType:
        case Inline
        case ByReference

    def show(tree: SirCaseDecisionTree)(using Context): String = {
        val sb: StringBuilder = new StringBuilder()

        def print(level: Int, tree: SirCaseDecisionTree): Unit = {
            sb.append(" " * level)
            tree match {
                case leaf: Leaf =>
                    sb.append(s"Leaf(${leaf.binding} ${leaf.action}, ${leaf.pos.sourcePos.show})")
                        .append("\n")
                case cc: ConstructorsChoice =>
                    sb.append(s"ConstructorsChoice(${cc.columnName}, ${cc.scrutineeTp.show})\n")
                    cc.byConstructors.foreach { case (name, entry) =>
                        sb.append(" " * (level + 2)).append(s"Constructor: $name\n")
                        print(level + 4, entry.tree)
                    }
                    cc.defaultBranch match {
                        case Some(defBr) =>
                            sb.append(" " * (level + 2)).append(s"DefaultBranch:\n")
                            print(level + 4, defBr)
                        case None =>
                    }
                case cc: ConstantChoice =>
                    sb.append(s"ConstantChoice(${cc.columnName}, ${cc.tp.show})\n")
                    cc.byConstants.foreach { case (const, tree) =>
                        sb.append(" " * (level + 2)).append(s"Constant: ${const}\n")
                        print(level + 4, tree)
                    }
                    sb.append(" " * (level + 2)).append(s"DefaultBranch:\n")
                    cc.defaultBranch match {
                        case Some(defaultBranch) => print(level + 4, defaultBranch)
                        case None                =>
                    }
                case cg: CheckGuard =>
                    sb.append(
                      s"CheckGuard(guard=${cg.guard}, binding=${cg.bingingMap}, ${cg.pos.sourcePos.show})\n"
                    )
                    sb.append(" " * (level + 2)).append(s"TrueBranch:\n")
                    print(level + 4, cg.nextTrue)
                    sb.append(" " * (level + 2)).append(s"FalseBranch:\n")
                    cg.nextFalse match {
                        case Some(nextFalse) => print(level + 4, nextFalse)
                        case None            => sb.append(" " * (level + 4)).append("None\n")
                    }
                case ref: Reference =>
                    sb.append(s"Reference(index=${ref.index})\n")
            }
        }

        print(0, tree)
        sb.toString()
    }

end SirCaseDecisionTree

case class SirCaseDecisionTreeWithRefs(
    tree: SirCaseDecisionTree,
    references: IndexedSeq[SirCaseDecisionTree]
)

object SirCaseDecisionTreeWithRefs {

    def show(treeWithRefs: SirCaseDecisionTreeWithRefs)(using Context): String = {
        if treeWithRefs.references.isEmpty then SirCaseDecisionTree.show(treeWithRefs.tree)
        else
            val sb: StringBuilder = new StringBuilder()
            sb.append("DecisionTreeWithRefs:\n")
            sb.append("Main Tree:\n")
            sb.append(SirCaseDecisionTree.show(treeWithRefs.tree))
            if treeWithRefs.references.nonEmpty then
                sb.append("References:\n")
                treeWithRefs.references.zipWithIndex.foreach { case (refTree, index) =>
                    sb.append(s"Reference $index:\n")
                    sb.append(SirCaseDecisionTree.show(refTree))
                }
            sb.toString()
    }

}

/*
    enum A:
      case C0
      case C1(a)
      case C2(b, c)
      case C3(a, b, c)
    compiles to:
    Decl(DataDecl("A", List(ConstrDecl("C0", List()), ConstrDecl("C1", List("a")), ConstrDecl("C2", List("b", "c")))), ...)

    c ==> Constr("C0", constrDecl, List())

    c match
      case C0 => 0
      case C1(a) => 1
      case C2(b, c) => 2
      case _ => 3
    compiles to:
    Match(c, List(
      Case(C0, Nil, 0),
      Case(C1, List(a), 1),
      Case(C2, List(b, c), 2),
      Case(C3, List(_, _, _), 3)
    )
 */
class PatternMatchingCompiler(val compiler: SIRCompiler)(using Context) {

    import tpd.*

    private val trueConst = SIR.Const.bool(true, AnnotationsDecl.empty)

    // Helper to create annotations with inline stack from context
    private def mkAnns(pos: SrcPos, ctx: PatternMatchingContext): AnnotationsDecl =
        compiler.mkAnns(pos, ctx.env)

    def compileMatch(
        tree: Match,
        env: SIRCompiler.Env,
        isUnchecked: Boolean = false
    ): AnnotatedSIR = {
        if env.debug then println(s"compileMatch: ${tree.show}")
        val bPrefix = s"_${tree.srcPos.startPos.source.name}_${tree.srcPos.line}_match_"
        val ctx = PatternMatchingContext(bPrefix, env, tree.srcPos)
        if isUnchecked then ctx.isUnchecked = true
        val parsedMatch = parseMatch(ctx, tree, env)

        // Optimization: if scrutinee is already a variable, use it directly instead of creating a let binding
        val needsScrutineeLet = parsedMatch.scrutinee match {
            case SIR.Var(name, _, _) =>
                // Set the scrutinee name to the variable name to avoid creating a redundant let binding
                ctx.setScrutineeName(name)
                false
            case _ =>
                true
        }

        val decisionTree = buildDecisionTree(ctx, parsedMatch, env)
        if env.debug then
            println(
              s"compileMath, decisionTree:\n${SirCaseDecisionTreeWithRefs.show(decisionTree)}"
            )
        val sir = compileDecisionTree(ctx, decisionTree, parsedMatch, needsScrutineeLet)
        if env.debug then println(s"end of compileMartch: ${sir}")
        sir
    }

    def parseConstructorPattern(
        ctx: PatternMatchingContext,
        unapply: UnApply,
        optBindingNameInfo: Option[SirParsedCase.BindingNameInfo],
        pos: SourcePosition,
        scrutineeType: SIRType
    ): SirParsedCase.Pattern = {
        if ctx.env.debug then
            println(s"parseConstructorPattern: ${unapply.show} ${unapply.tpe.show}")
        retrieveUnapplyType(unapply) match
            case Left(err) =>
                SirParsedCase.Pattern.ErrorPattern(err, unapply.srcPos, ctx)
            case Right(cType) =>
                if ctx.env.debug then println(s"parseConstructorPattern: cType = ${cType.show} ")
                val sirConstrType = compiler.sirTypeInEnv(cType, unapply.srcPos, ctx.env)
                if ctx.env.debug then
                    println(
                      s"parseConstructorPattern: sirConstrType = ${sirConstrType.show} [${SIRType.unrollTypeProxy(sirConstrType).show}] "
                    )
                val (typeParams, constrDecl, typeArgs) = SIRType
                    .collectProd(sirConstrType)
                    .getOrElse {
                        println(s"unapply=${unapply.show}")
                        println(s"unapply.tpe=${unapply.tpe.show}")
                        println(s"unapply.fun.tpe=${unapply.fun.tpe.show}")
                        println(s"cType = ${cType.show}")
                        println(s"scroutineType=${scrutineeType.show}")
                        throw TypingException(
                          cType,
                          unapply.srcPos,
                          s"constructor pattern type is not case class or enum: ${sirConstrType.show}"
                        )
                    }
                val optParent = SIRType.prodParent(sirConstrType)
                val typeParamsMap = constrDecl.typeParams.zip(typeArgs).toMap
                if ctx.env.debug then
                    println(
                      s"parseConstructorPattern: typeParamsMap = ${typeParamsMap
                              .map { (k, v) => s"${k.name} -> ${v.show}" }}"
                    )
                val paramTypes =
                    constrDecl.params.map(b => SIRType.substitute(b.tp, typeParamsMap, Map.empty))

                val subpatterns = unapply.patterns
                    .zip(paramTypes)
                    .map { case (p, tp) =>
                        parsePattern(ctx, p, p.sourcePos, tp)
                    }
                    .toIndexedSeq
                SirParsedCase.Pattern.Constructor(
                  SIRType.CaseClass(constrDecl, typeArgs, optParent),
                  typeParams,
                  optBindingNameInfo,
                  // constrSymbol,
                  subpatterns,
                  pos
                )
    }

    def retrieveUnapplyType(unapply: UnApply): Either[String, Type] = {
        val cType = unapply.tpe.dealias.widen
        if cType == defn.NothingType then {
            unapply.fun.tpe.widen match {
                case mt: MethodType =>
                    Right(mt.paramInfos.head.dealias)
                case pt: PolyType =>
                    pt.resType match {
                        case mt: MethodType =>
                            mt.paramInfos.head.dealias
                            Right(mt.paramInfos.head.dealias)
                        case _ =>
                            Left(
                              s"Invalid unapply polymethod type: expected methodType, have: ${unapply.fun.tpe.show}"
                            )
                    }
                case _ =>
                    Left(
                      s"Unapply.fun is not a mwthod or polymethod: ${unapply.fun.tpe.show}"
                    )
            }
        } else {
            Right(cType)
        }
    }

    def parsePatternInOptBind(
        ctx: PatternMatchingContext,
        pat: Tree,
        optBindingNameInfo: Option[SirParsedCase.BindingNameInfo],
        pos: SourcePosition,
        sirScrutineeType: SIRType
    ): SirParsedCase.Pattern = {

        pat match
            case u @ UnApply(fun, _, _) =>
                parseConstructorPattern(
                  ctx,
                  u,
                  optBindingNameInfo,
                  pos,
                  sirScrutineeType
                )
            case Ident(nme.WILDCARD) =>
                SirParsedCase.Pattern.Wildcard(
                  sirScrutineeType,
                  optBindingNameInfo,
                  pos
                )
            case Literal(const) =>
                val sirExpr = compiler.compileExpr(ctx.env, pat)
                sirExpr match
                    case c: SIR.Const =>
                        SirParsedCase.Pattern.PrimitiveConstant(c, None, pat.sourcePos)
                    case _ =>
                        val err = GenericError("const pattern should be a constant", pat.srcPos)
                        compiler.error(
                          err,
                          SirParsedCase.Pattern
                              .Wildcard(sirScrutineeType, optBindingNameInfo, pos)
                        )
            case Typed(inner, tpt) =>
                val tptSirType = compiler.sirTypeInEnv(tpt.tpe, pos, ctx.env)
                if tptSirType ~=~ sirScrutineeType then {
                    parsePatternInOptBind(
                      ctx,
                      inner,
                      optBindingNameInfo,
                      inner.sourcePos,
                      tptSirType
                    )
                } else
                    val innerPattern = parsePatternInOptBind(
                      ctx,
                      inner,
                      optBindingNameInfo,
                      inner.sourcePos,
                      tptSirType
                    )
                    SirParsedCase.Pattern.TypeSelector(
                      tptSirType,
                      optBindingNameInfo,
                      innerPattern,
                      pos
                    )
            case Alternative(trees) =>
                val patterns = trees.map { p =>
                    parsePatternInOptBind(
                      ctx,
                      p,
                      optBindingNameInfo,
                      p.sourcePos,
                      sirScrutineeType
                    )
                }
                SirParsedCase.Pattern.OrPattern(patterns.toList, pat.sourcePos)
            case _ =>
                if pat.symbol.is(Flags.Case) then {
                    val sirPatTp = compiler.sirTypeInEnv(pat.tpe, pat.srcPos, ctx.env)
                    val constrDecl = compiler.makeConstrDecl(ctx.env, pat.srcPos, pat.symbol)
                    SIRType.collectSumCaseClass(sirPatTp) match {
                        case Some((typeVars, sumCaseClass)) =>
                            val sirConstrTp = sumCaseClass.decl.constrType(constrDecl.name)
                            SIRType.collectProdCaseClass(sirConstrTp) match
                                case Some((freeTypeVars, constrCaseClass)) =>
                                    SirParsedCase.Pattern.Constructor(
                                      constrCaseClass,
                                      freeTypeVars,
                                      optBindingNameInfo,
                                      // pat.symbol,
                                      IndexedSeq.empty,
                                      pos
                                    )
                                case None =>
                                    // Impossible, because we just found it as constructor of sum type
                                    SirParsedCase.Pattern.ErrorPattern(
                                      s"Constructor type is not a product type: $sirConstrTp",
                                      pat.srcPos,
                                      ctx
                                    )
                        case None =>
                            // constructor without parent
                            SIRType.collectProdCaseClass(sirPatTp) match
                                case Some((freeTypeVars, constrCaseClass)) =>
                                    SirParsedCase.Pattern.Constructor(
                                      constrCaseClass,
                                      freeTypeVars,
                                      optBindingNameInfo,
                                      // pat.symbol,
                                      IndexedSeq.empty,
                                      pos
                                    )
                                case None =>
                                    SirParsedCase.Pattern.ErrorPattern(
                                      s"Strange pattern: type is not a aum or product type: ${sirPatTp.show}",
                                      pat.srcPos,
                                      ctx
                                    )
                    }
                } else
                    println(s"Unsupported pattern: " + pat.show)
                    println(s"tree: $pat")
                    compiler.error(
                      UnsupportedMatchExpression(pat, pat.srcPos),
                      SirParsedCase.Pattern
                          .Wildcard(sirScrutineeType, optBindingNameInfo, pos)
                    )

    }

    def parsePattern(
        ctx: PatternMatchingContext,
        pat: Tree,
        pos: SrcPos,
        sirScrutineeType: SIRType
    ): SirParsedCase.Pattern = {
        if ctx.env.debug then
            println(
              s"parsePattern: ${pat.show} ${pat.tpe.show} ${sirScrutineeType.show} [${SIRType.unrollTypeProxy(sirScrutineeType).show}]"
            )
        pat match
            // this is case Constr(name @ _) or Constr(name)
            case b @ Bind(name, pattern) =>
                val tp = compiler.sirTypeInEnv(b.tpe.widen, b.srcPos, ctx.env)
                val nameInfo =
                    SirParsedCase.BindingNameInfo(
                      name.show,
                      Some(name.show),
                      tp,
                      Some(b.symbol),
                    )
                parsePatternInOptBind(
                  ctx,
                  pattern,
                  Some(nameInfo),
                  pat.srcPos.sourcePos,
                  sirScrutineeType
                )
            case _ =>
                parsePatternInOptBind(
                  ctx,
                  pat,
                  None,
                  pat.srcPos.sourcePos,
                  sirScrutineeType
                )
    }

    def parseSIRCase(
        ctx: PatternMatchingContext,
        c: CaseDef,
        i: Int,
        scrutineeType: SIRType
    ): (SirParsedCase, Option[SirParsedGuard], SirParsedAction) = {
        c match
            case CaseDef(pat, guard, rhs) =>
                val optGuard = if guard.isEmpty then None else Some(guard)
                val sirPat = parsePattern(ctx, pat, c.srcPos, scrutineeType)
                val sirCase = SirParsedCase(
                  sirPat,
                  optGuard.map(_ => i),
                  SirParsedCase.ActionRef.Origin(i),
                  c.srcPos.sourcePos
                )
                val patBindings = sirPat.collectBindingInfos
                val patBindingsByName =
                    patBindings.groupBy { case (k, v) => k.varName }.map { case (name, bindings) =>
                        if bindings.size > 1 then
                            // multiple bindings with the same name -
                            report.error(
                              s"Multiple bindings with the same name ${name} detected in pattern matching. Consider renaming variables to avoid shadowing.",
                              c.srcPos
                            )
                        name -> bindings.head._2
                    }
                val patNames = patBindings.map { case (k, v) => k.varName -> v.tp }
                val nEnv = ctx.env.copy(vars = ctx.env.vars ++ patBindings.map { case (k, v) =>
                    k -> v.tp
                })

                val optCompiledGuard = optGuard.map { g =>
                    val sirG = compiler.compileExpr(nEnv, g)
                    val (usedNames, unusedNames) =
                        SIR.partitionUsedFreeVarsFrom(sirG, patNames.keys.toSet)
                    val bindings = usedNames.map { name =>
                        val bindingInfo = patBindingsByName(name)
                        name -> bindingInfo
                    }.toMap
                    SirParsedGuard(sirG, bindings, g.srcPos)
                }
                val compiledAction = parseCaseAction(ctx, patBindingsByName, nEnv, rhs)
                (sirCase, optCompiledGuard, compiledAction)
    }

    def parseMatch(
        ctx: PatternMatchingContext,
        tree: Match,
        env: SIRCompiler.Env
    ): SirParsedMatch = {
        if env.debug then println(s"parsedMatch: ${tree.show}")
        val Match(scrutineeMbUnchecked, cases) = tree
        // val typeSymbol = matchTree.tpe.widen.dealias.typeSymbol
        // report.echo(s"Match: ${typeSymbol} ${typeSymbol.children} $adtInfo", tree.srcPos)
        // Check for @unchecked on the scrutinee
        val (scrutinee, isUnchecked) = scrutineeMbUnchecked match
            case Typed(selectorExpr, tp) =>
                tp.tpe match
                    case AnnotatedType(_, ann) =>
                        if ann.symbol == defn.UncheckedAnnot then (selectorExpr, true)
                        else (scrutineeMbUnchecked, false)
                    case _ => (scrutineeMbUnchecked, false)
            case _ => (scrutineeMbUnchecked, false)
        if isUnchecked then ctx.isUnchecked = true
        val scrutineeType = scrutinee.tpe.dealias.widen
        val scrutineeSirType = compiler.sirTypeInEnv(scrutineeType, tree.srcPos, env)
        val sirCasesGuardActions = cases.zipWithIndex.map { case (cs, i) =>
            parseSIRCase(ctx, cs, i, scrutineeSirType)
        }.toIndexedSeq
        val sirCases = sirCasesGuardActions.map(_._1)
        val actions = sirCasesGuardActions.map(_._3)
        ctx.parsedActions = actions
        val optGuards = sirCasesGuardActions.map(_._2)
        ctx.parsedGuards = optGuards
        val matchSirType = compiler.sirTypeInEnv(tree.tpe.dealias.widen, tree.srcPos, env)
        val scrutineeSir = compiler.compileExpr(env, scrutinee)
        SirParsedMatch(
          scrutineeSir,
          sirCases,
          actions,
          optGuards,
          scrutineeSirType,
          matchSirType,
          tree.srcPos.sourcePos
        )
    }

    private def parseCaseAction(
        ctx: PatternMatchingContext,
        patNames: Map[String, BindingNameInfo],
        env: SIRCompiler.Env,
        rhs: Tree,
    ): SirParsedAction = {
        if ctx.env.debug then println(s"parseCaseAction: ${rhs.show}, patNames = ${patNames}")
        val sirRhs = compiler.compileExpr(env, rhs)
        val (usedNames, unusedNames) =
            SIR.partitionUsedFreeVarsFrom(sirRhs, patNames.keys.toSet)
        val bindings = usedNames.map { name =>
            name -> patNames(name)
        }.toMap
        SirParsedAction(sirRhs, bindings, rhs.srcPos)
    }

    def buildDecisionTree(
        ctx: PatternMatchingContext,
        parsedMatch: SirParsedMatch,
        env: SIRCompiler.Env
    ): SirCaseDecisionTreeWithRefs = {

        if ctx.env.debug then println(s"buildDecisionTree: nCases=${parsedMatch.cases.length}")

        val tupleRows = parsedMatch.cases.zipWithIndex.map { case (c, i) =>
            SirParsedCase.GroupedTupleRow(
              IndexedSeq(c.pattern),
              c.guardRef,
              SirParsedCase.ActionRef.Origin(i),
              c.pos
            )
        }

        val scrutineeName = ctx.scrutineeName

        // TODOP: maybe we should keep in ctx BingingNameInfo (and store symbol if it exists)
        val globalBindNameInfo = BindingNameInfo(scrutineeName, None, parsedMatch.scrutineeTp, None)

        val groupedTuples = SirParsedCase.GroupedTuples(
          IndexedSeq(globalBindNameInfo),
          Set(0),
          tupleRows.toList
        )

        if ctx.env.debug then
            println(s"globalBindNameInfo: ${globalBindNameInfo}")
            println(s"scrutineeTp: ${parsedMatch.scrutineeTp.show}")
            println(s"groupedTuples: ${groupedTuples}")
        val tree = buildGroupedTuplesDecisionTree(ctx, groupedTuples, ctx.topLevelPos, None)
        SirCaseDecisionTreeWithRefs(tree, ctx.decisionTreeRefs)
    }

    /*
    def buildConstructorDecisionTree(
        ctx: PatternMatchingContext,
        constructorCases: SirParsedCase.JoinedConstructorCase,
        scrutineeTp: SIRType,
        topLevelPos: SrcPos
    ): SirCaseDecisionTree.ConstructorEntry = {

        val nParams = constructorCases.tp.constrDecl.params.length
        if nParams == 0 then
            val firstCase = constructorCases.tuples.head
            val leaf = ctx.createLeaf( firstCase.actionRef, firstCase.pos)
            val entry = SirCaseDecisionTree.ConstructorEntry(
              constructorCases.tp,
              IndexedSeq.empty,
              leaf,
              topLevelPos
            )
            entry
        else
            val columnBinding = constructorCases.tp.constrDecl.params
                .map(b =>
                    BindingNameInfo(
                      ctx.freshName(b.name),
                      None,
                      b.tp,
                      Symbols.NoSymbol,
                      Set.empty
                    )
                )
                .toIndexedSeq

            val group = SirParsedCase.GroupedTuples(
              columnBinding,
              (0 until nParams).toSet,
              constructorCases.tuples.toList
            )
            val nextTree = buildGroupedTuplesDecisionTree(ctx, group)
            SirCaseDecisionTree.ConstructorEntry(
              constructorCases.tp,
              columnBinding,
              nextTree,
              topLevelPos
            )
    }
    
     */

    def buildGroupedTuplesDecisionTree(
        ctx: PatternMatchingContext,
        group: SirParsedCase.GroupedTuples,
        pos: SrcPos,
        optNextReference: Option[SirCaseDecisionTree.Reference]
    ): SirCaseDecisionTree = {
        val group1 = eliminateAlternativesInGroup(group)
        val group2 = eliminateTypeSelectorsInGroup(ctx, group1)

        if ctx.env.debug then
            println(
              s"buildGroupedTuplesDecisionTree: group1.rows.length=${group1.rows.length}, group2.rows.length = ${group2.rows.length}"
            )

        if group2.rows.isEmpty then {
            if ctx.env.debug then println(s"buildGroupedTuplesDecisionTree: empty group")
            optNextReference.foreach(ref => ctx.countDecisionTreeRefUsage(ref.index))
            optNextReference getOrElse {
                SirCaseDecisionTree.Leaf(
                  Map.empty,
                  SirParsedCase.ActionRef.FailMatch,
                  ctx.topLevelPos
                )
            }
        } else
            val firstRow = group2.rows.head
            val nonWildcards = firstRow.patterns.zipWithIndex.filter { case (p, i) =>
                group2.activeColumns.contains(i) && {
                    p match {
                        case SirParsedCase.Pattern.Wildcard(_, _, _) => false
                        case _                                       => true
                    }
                }
            }
            if nonWildcards.isEmpty then {
                if firstRow.guardRef.isDefined then
                    buildSpecializedGuard(
                      ctx,
                      firstRow,
                      group2,
                      optNextReference
                    )
                else
                    val retval = ctx.createLeaf(
                      group2.columnBindings,
                      firstRow
                    )
                    if group2.rows.length > 1 then {
                        group2.rows.tail.foreach(r =>
                            if r.actionRef != FailMatch then
                                report.warning("Unreachable case in pattern matching", r.pos)
                        )
                    }
                    retval
            } else {
                val prioritized = prioritizePatterns(group2, firstRow, patterns = nonWildcards)
                val (p, i) = prioritized.head
                p match
                    case constr: SirParsedCase.Pattern.Constructor =>
                        val stp = group2.columnBindings(i).tp
                        buildSpecializedConstr(ctx, group2, firstRow, i, stp, optNextReference)
                    case const: SirParsedCase.Pattern.PrimitiveConstant =>
                        const.value.tp match
                            case tp: SIRType.Primitive =>
                                buildSpecializedConst(
                                  ctx,
                                  group2,
                                  i,
                                  tp,
                                  p.pos,
                                  optNextReference
                                )
                            case _ =>
                                report.error("Constant pattern should be primitive", const.pos)
                                SirCaseDecisionTree.Leaf(
                                  Map.empty,
                                  SirParsedCase.ActionRef.FailMatch,
                                  p.pos
                                )
                    case _ =>
                        compiler.error(
                          GenericError(s"Impossible pattern in specialization: ${p}", p.pos),
                          SirCaseDecisionTree.Leaf(
                            Map.empty,
                            SirParsedCase.ActionRef.FailMatch,
                            p.pos
                          )
                        )
            }
    }

    private def buildSpecializedGuard(
        ctx: PatternMatchingContext,
        currentRow: SirParsedCase.GroupedTupleRow,
        group: SirParsedCase.GroupedTuples,
        optNextDecisionTree: Option[SirCaseDecisionTree.Reference]
    ): SirCaseDecisionTree = {
        val guardRef = currentRow.guardRef.get
        val optIfFalseTree = group.rows match {
            case head :: tail =>
                Some(
                  buildGroupedTuplesDecisionTree(
                    ctx,
                    group.copy(rows = tail),
                    head.pos,
                    optNextDecisionTree
                  )
                )
            case Nil =>
                optNextDecisionTree
        }
        val actionTree =
            ctx.createLeaf(group.columnBindings, currentRow)
        ctx.countGuardUsage(guardRef)
        SirCaseDecisionTree.CheckGuard(
          actionTree.binding,
          guardRef,
          currentRow.pos,
          actionTree,
          optIfFalseTree
        )
    }

    private def buildSpecializedConst(
        ctx: PatternMatchingContext,
        group: SirParsedCase.GroupedTuples,
        col: Int,
        tp: SIRType.Primitive,
        pos: SrcPos,
        optNextDecisionTree: Option[SirCaseDecisionTree.Reference]
    ): SirCaseDecisionTree = {

        def checkConstantPattern(
            p: SirParsedCase.Pattern
        ): Option[(SirParsedCase.Pattern.PrimitiveConstant, SIR.Const, SIR.Const)] = {
            p match
                case c: SirParsedCase.Pattern.PrimitiveConstant =>
                    Some((c, c.value, c.value))
                case _ => None
        }

        val splitPolicy = DuplicateChecks

        val (withConstants, splittedRest) = collectSpecialized(
          group.rows,
          col,
          checkConstantPattern,
          splitPolicy
        )

        val optNextSubtree =
            if splittedRest.isEmpty then optNextDecisionTree
            else {
                val nextActiveColumns = splitPolicy match {
                    case DuplicateChecks => group.activeColumns
                    case DuplicateRows   => group.activeColumns - col
                }
                val newGroup = SirParsedCase.GroupedTuples(
                  group.columnBindings,
                  nextActiveColumns,
                  splittedRest
                )
                val nextSubtree = buildGroupedTuplesDecisionTree(
                  ctx,
                  newGroup,
                  splittedRest.head.pos,
                  optNextDecisionTree
                )
                val nextIndex = ctx.decisionTreeRefs.length
                ctx.decisionTreeRefs = ctx.decisionTreeRefs.appended(nextSubtree)
                Some(SirCaseDecisionTree.Reference(nextIndex))
            }

        val constEntries = withConstants.map { case (const, (_, rows)) =>
            val newGroup = SirParsedCase.GroupedTuples(
              group.columnBindings,
              group.activeColumns - col,
              rows
            )
            val subtree = buildGroupedTuplesDecisionTree(ctx, newGroup, pos, optNextSubtree)
            (const, subtree)
        }.toMap
        val columnName = group.columnBindings(col).name
        if !(tp == SIRType.Boolean && constEntries.size == 2) then
            optNextSubtree.foreach(ref => ctx.countDecisionTreeRefUsage(ref.index))
        SirCaseDecisionTree.ConstantChoice(columnName, tp, constEntries, optNextSubtree, pos)
    }

    /** Collect rows with specialized pattern in the given column according to checkPattern function
      * and splitStrategy. Return - Map[Key, (Value, List[Rows with this key])] and List of rest
      * rows (i,e. default specialized) Key and Value are extracted from pattern by checkPattern
      * function.
      */
    private def collectSpecialized[P <: SirParsedCase.Pattern, K, V](
        rows: List[SirParsedCase.GroupedTupleRow],
        colIndex: Int,
        checkPattern: SirParsedCase.Pattern => Option[(P, K, V)],
        splitStrategy: SirCaseDecisionTree.SplitStrategy
    ): (Map[K, (V, List[SirParsedCase.GroupedTupleRow])], List[SirParsedCase.GroupedTupleRow]) = {
        var groupedRows: Map[K, (V, ListBuffer[SirParsedCase.GroupedTupleRow])] = Map.empty
        val restPrefix: ListBuffer[SirParsedCase.GroupedTupleRow] = ListBuffer.empty
        var cursor = rows
        var done = false
        while cursor.nonEmpty && !done do {
            val r = cursor.head
            checkPattern(r.patterns(colIndex)) match
                case Some((pattern, key, value)) =>
                    groupedRows.get(key) match
                        case Some((v, buf)) =>
                            buf.append(r)
                        case None =>
                            val buf = ListBuffer.empty[SirParsedCase.GroupedTupleRow]
                            buf.append(r)
                            groupedRows = groupedRows + (key -> (value, buf))
                    if splitStrategy == SirCaseDecisionTree.SplitStrategy.DuplicateRows then
                        if restPrefix.nonEmpty then
                            groupedRows.foreach:
                                case (k, (v, rows)) => rows.appendAll(restPrefix)
                            restPrefix.clear()
                case None =>
                    if splitStrategy == SirCaseDecisionTree.SplitStrategy.DuplicateRows then
                        // we should duplicate this row in all groups, and continue
                        restPrefix += r
                    else
                        // we should stop collecting, and put all next rows to rest
                        done = true
            if !done then cursor = cursor.tail
        }
        val grouped = groupedRows.map { case (k, (v, buf)) =>
            (k, (v, buf.toList))
        }
        splitStrategy match {
            case SirCaseDecisionTree.SplitStrategy.DuplicateRows =>
                (grouped, restPrefix.toList)
            case SirCaseDecisionTree.SplitStrategy.DuplicateChecks =>
                (grouped, cursor)
        }

    }

    private def buildSpecializedConstr(
        ctx: PatternMatchingContext,
        group: SirParsedCase.GroupedTuples,
        row: SirParsedCase.GroupedTupleRow,
        colIndex: Int,
        scrutineeTp: SIRType,
        optNextDecisionTree: Option[SirCaseDecisionTree.Reference]
    ): SirCaseDecisionTree = {

        if ctx.env.debug then
            println(
              s"buildSpecializedConstr: colIndex=${colIndex}, scrutineeTp=${scrutineeTp.show}"
            )

        def checkPattern(
            p: SirParsedCase.Pattern
        ): Option[
          (SirParsedCase.Pattern.Constructor, String, (SIRType.CaseClass, List[SIRType.TypeVar]))
        ] = {
            p match
                case c: SirParsedCase.Pattern.Constructor =>
                    val v = (c.tp, c.freeTypeParams)
                    Some((c, c.tp.constrDecl.name, v))
                case _ => None
        }

        val (constructorCases, tail) = this.collectSpecialized(
          group.rows,
          colIndex,
          checkPattern,
          SirCaseDecisionTree.SplitStrategy.DuplicateRows
        )

        val optNextSubtree =
            if tail.isEmpty then optNextDecisionTree
            else
                val newGroup = SirParsedCase.GroupedTuples(
                  group.columnBindings,
                  group.activeColumns - colIndex,
                  tail
                )
                val nextSubtree = buildGroupedTuplesDecisionTree(
                  ctx,
                  newGroup,
                  tail.head.pos,
                  optNextDecisionTree
                )
                val nextIndex = ctx.decisionTreeRefs.length
                ctx.decisionTreeRefs = ctx.decisionTreeRefs.appended(nextSubtree)
                Some(SirCaseDecisionTree.Reference(nextIndex))

        val filledConstructorEntries = constructorCases.map {
            case (constrName, ((sirCaseClass, freeTypeParams), rows)) =>
                if ctx.env.debug then
                    println(
                      s"buildSpecializedConstr: constrName=${constrName}, sirCaseClass=${sirCaseClass.show} nRows=${rows.length}"
                    )
                val newGroup = insertConstructorPatternsIntoGroup(
                  ctx,
                  sirCaseClass,
                  freeTypeParams,
                  group,
                  colIndex,
                  rows
                )
                val constrPos = rows match
                    case head :: _ => head.pos
                    case Nil       => ctx.topLevelPos
                val subtree =
                    buildGroupedTuplesDecisionTree(ctx, newGroup, constrPos, optNextSubtree)
                val newGroupBindings = newGroup.columnBindings.drop(group.columnBindings.size)
                val entry = SirCaseDecisionTree.ConstructorEntry(
                  sirCaseClass,
                  newGroupBindings,
                  subtree,
                  constrPos
                )
                (constrName, entry)
        }.toMap

        optNextSubtree match {
            case None =>
            case Some(refTree) =>
                SIRType.collectSumCaseClass(scrutineeTp) match
                    case Some((typeParams, sumCaseClass)) =>
                        if sumCaseClass.decl.constructors.length != filledConstructorEntries.size
                        then {
                            //  btw, for some backends refTree may be compiled for each constructor,
                            //  for some - only once for all missing constructors.
                            // We now count omce ans minimal heurisric.
                            ctx.countDecisionTreeRefUsage(refTree.index)
                        }
                    case None =>
                        // Non-sum type pattern matching (e.g., case classes, tuples)
                        // No need to count reference usage
                        ()
        }

        val columnName = group.columnBindings(colIndex).name
        SirCaseDecisionTree.ConstructorsChoice(
          columnName,
          scrutineeTp,
          filledConstructorEntries,
          optNextSubtree,
          row.pos
        )
    }

    private def insertConstructorPatternsIntoGroup(
        ctx: PatternMatchingContext,
        cc: SIRType.CaseClass,
        ccFreeTypeParams: List[SIRType.TypeVar],
        prevGroup: SirParsedCase.GroupedTuples,
        colIndex: Int,
        rows: List[SirParsedCase.GroupedTupleRow]
    ): SirParsedCase.GroupedTuples = {
        val nextIndex = prevGroup.columnBindings.length
        if ctx.env.debug then
            println(
              s"insertConstructorPatternsIntoGroup: cc=${cc.constrDecl.name}, colIndex=${colIndex}, nextIndex=${nextIndex}"
            )
        val constructorBindings = cc.constrDecl.params.map(b =>
            val tp =
                SIRType.substitute(b.tp, cc.constrDecl.typeParams.zip(cc.typeArgs).toMap, Map.empty)
            BindingNameInfo(
              ctx.freshName(b.name),
              None,
              tp,
              None, // No symbol information for generated bindings
              Set.empty
            )
        )
        val newColumnBindings = prevGroup.columnBindings ++ constructorBindings
        val newActiveColumns =
            (prevGroup.activeColumns - colIndex) ++ (nextIndex until nextIndex + constructorBindings.length)
        val newRows = rows.map { r =>
            val constructorPattern = r.patterns(colIndex) match
                case c: SirParsedCase.Pattern.Constructor => c
                case w: SirParsedCase.Pattern.Wildcard =>
                    createWildcardConstructorPattern(cc, ccFreeTypeParams, w.pos)
                case _ =>
                    throw IllegalStateException(
                      "Only constructor or wildcard patterns are expected here"
                    )
            val newPatterns = r.patterns ++ constructorPattern.subcases
            SirParsedCase.GroupedTupleRow(
              newPatterns,
              r.guardRef,
              r.actionRef,
              r.pos
            )
        }
        GroupedTuples(
          newColumnBindings,
          newActiveColumns,
          newRows
        )
    }

    private def createWildcardConstructorPattern(
        cc: SIRType.CaseClass,
        ccFreeTypeParams: List[SIRType.TypeVar],
        pos: SrcPos
    ): SirParsedCase.Pattern.Constructor = {
        val subcases = cc.constrDecl.params
            .map(b =>
                val tp = SIRType
                    .substitute(b.tp, cc.constrDecl.typeParams.zip(cc.typeArgs).toMap, Map.empty)
                SirParsedCase.Pattern.Wildcard(tp, None, pos)
            )
            .toIndexedSeq
        SirParsedCase.Pattern.Constructor(cc, ccFreeTypeParams, None, subcases, pos)
    }

    private def eliminateAlternativesInGroup(
        group: SirParsedCase.GroupedTuples,
    ): SirParsedCase.GroupedTuples = {
        group.copy(rows =
            group.rows.flatMap(r => eliminateAlternativesInRow(r, group.activeColumns))
        )
    }

    private def eliminateAlternativesInRow(
        row: SirParsedCase.GroupedTupleRow,
        columnsToExpand: Set[Int]
    ): List[SirParsedCase.GroupedTupleRow] = {
        val alternatives = row.patterns.zipWithIndex.flatMap { case (p, i) =>
            if columnsToExpand.contains(i) then
                p match
                    case pa @ SirParsedCase.Pattern.OrPattern(alts, pos) => Some((pa, i))
                    case _                                               => None
            else None
        }
        if alternatives.isEmpty then List(row)
        else
            val newRows = alternatives.flatMap { case (pa, i) =>
                pa.patterns.map(alt =>
                    SirParsedCase.GroupedTupleRow(
                      row.patterns.updated(i, alt),
                      row.guardRef,
                      row.actionRef,
                      row.pos
                    )
                )
            }
            newRows.toList
    }

    private def eliminateTypeSelectorsInGroup(
        ctx: PatternMatchingContext,
        group: SirParsedCase.GroupedTuples
    ): SirParsedCase.GroupedTuples = {
        val newRows =
            group.rows.flatMap(row => eliminateTypeSelectorsInRaw(ctx, group.columnBindings, row))
        group.copy(rows = newRows)
    }

    private def eliminateTypeSelectorsInRaw(
        ctx: PatternMatchingContext,
        columnBindings: IndexedSeq[BindingNameInfo],
        row: SirParsedCase.GroupedTupleRow
    ) = {
        val typeSelectors = row.patterns.zipWithIndex.flatMap { case (p, i) =>
            p match
                case ts @ SirParsedCase.Pattern.TypeSelector(
                      tp,
                      optNameInfo,
                      innerPattern,
                      pos
                    ) =>
                    Some((ts, i))
                case _ => None
        }
        if typeSelectors.isEmpty then List(row)
        else {
            // make new rows with all possible unrollings of type selectors
            val newRows = typeSelectors.map { case (ts, i) =>
                val scrutineeTp = columnBindings(i).tp
                val unrolledPattern = unrollTypeSelector(ctx, ts, ts.pos, scrutineeTp)
                SirParsedCase.GroupedTupleRow(
                  row.patterns.updated(i, unrolledPattern),
                  row.guardRef,
                  row.actionRef,
                  row.pos
                )
            }
            newRows
        }
    }

    private def prioritizePatterns(
        @unused group: SirParsedCase.GroupedTuples,
        @unused row: SirParsedCase.GroupedTupleRow,
        patterns: IndexedSeq[(SirParsedCase.Pattern, Int)]
    ): IndexedSeq[(SirParsedCase.Pattern, Int)] = {
        patterns.sortBy { case (p, i) =>
            p match
                case SirParsedCase.Pattern.Constructor(_, _, _, _, _) => 0
                case SirParsedCase.Pattern.PrimitiveConstant(_, _, _) => 1
                case SirParsedCase.Pattern.TypeSelector(_, _, _, _)   => 2
                case SirParsedCase.Pattern.OrPattern(_, _)            => 3
                case _                                                => 4
        }
    }

    private def unrollTypeSelector(
        ctx: PatternMatchingContext,
        ts: SirParsedCase.Pattern.TypeSelector,
        pos: SrcPos,
        scrutineeTp: SIRType
    ): SirParsedCase.Pattern = {

        @tailrec
        def injectInnerPattern(
            top: SirParsedCase.Pattern,
            ts: SirParsedCase.Pattern.TypeSelector
        ): SirParsedCase.Pattern = {
            top match
                case SirParsedCase.Pattern.Constructor(
                      tp,
                      typeParams,
                      optNameInfo,
                      subcases,
                      pos
                    ) =>
                    if SIRType.isSynteticNarrowConstrDeclName(tp.constrDecl.name) then {
                        injectInnerPattern(subcases.head, ts)
                    } else {
                        SirParsedCase.Pattern.ErrorPattern(
                          s"Cannot inject inner pattern of typeSelector ${ts.tp.show} into $top",
                          pos,
                          ctx
                        )
                    }
                case SirParsedCase.Pattern.Wildcard(tp, optNameInfo, pos) =>
                    SirParsedCase.Pattern.Wildcard(
                      tp,
                      ts.optNameInfo.orElse(optNameInfo),
                      pos
                    )
                case _ =>
                    SirParsedCase.Pattern.ErrorPattern(
                      s"Type selector inner pattern can be only constructor or wildcard, but got ${top}",
                      top.pos,
                      ctx
                    )
        }

        ts.tp match
            case p: SIRType.Primitive =>
                if p == scrutineeTp then ts.innerPattern
                else {
                    report.error(
                      s"Type selector pattern  ${ts.tp.show} does not match scrutinee type ${scrutineeTp.show}",
                      pos
                    )
                    SirParsedCase.Pattern.Wildcard(scrutineeTp, ts.optNameInfo, pos)
                }
            case SIRType.FreeUnificator =>
                ts.innerPattern
            case _ =>
                SIRType.collectProdCaseClass(ts.tp) match {
                    case Some((patTypeVars, patCaseClass)) =>
                        SIRType.collectProdCaseClass(scrutineeTp) match
                            case Some((scrutineeTypeVars, scrutineeCaseClass)) =>
                                if patCaseClass.constrDecl.name == scrutineeCaseClass.constrDecl.name
                                then ts.innerPattern
                                else
                                    SirParsedCase.Pattern.ErrorPattern(
                                      s"Type selector pattern  ${ts.tp.show} does not match scrutinee type ${scrutineeTp.show}",
                                      pos,
                                      ctx
                                    )
                            case None =>
                                SIRType.collectSumCaseClass(scrutineeTp) match
                                    case Some((srcurineeTypeVars, scrutineeSumClass)) =>
                                        if scrutineeSumClass.decl.constructors.exists(
                                              _.name == patCaseClass.constrDecl.name
                                            )
                                        then

                                            ts.innerPattern match {
                                                case SirParsedCase.Pattern.Wildcard(_, _, _) =>
                                                    val constrPattern =
                                                        createWildcardConstructorPattern(
                                                          patCaseClass,
                                                          patTypeVars,
                                                          pos
                                                        )
                                                    constrPattern
                                                        .withOptAlias(ts.optNameInfo)
                                                        .withOptAlias(ts.innerPattern.optNameInfo)
                                                case innerConstr: SirParsedCase.Pattern.Constructor =>
                                                    if innerConstr.tp.constrDecl.name == patCaseClass.constrDecl.name
                                                    then innerConstr.withOptAlias(ts.optNameInfo)
                                                    else
                                                        SirParsedCase.Pattern.ErrorPattern(
                                                          s"Type selector inner pattern must be constructor of type ${patCaseClass.constrDecl.name}, but got ${innerConstr.tp.show}",
                                                          pos,
                                                          ctx
                                                        )
                                                case _ =>
                                                    println(s"scrutineeTp: ${scrutineeTp.show}")
                                                    println(s"patCaseClass: ${patCaseClass.show}")
                                                    println(
                                                      s"ts.innerPattern: ${ts.innerPattern.show}"
                                                    )
                                                    throw new RuntimeException(
                                                      "NonWildcardInnerPattern"
                                                    )
                                                    report.error(
                                                      s"type selector inner pattern must be wildcard or constructor, we have ${ts.innerPattern.show}",
                                                      pos
                                                    )
                                                    ???
                                            }
                                        else
                                            patCaseClass.parent match
                                                case Some(parentPatCaseClass) =>
                                                    val top = unrollTypeSelector(
                                                      ctx,
                                                      ts.copy(
                                                        tp = parentPatCaseClass,
                                                        optNameInfo = None
                                                      ),
                                                      pos,
                                                      scrutineeTp
                                                    )
                                                    injectInnerPattern(top, ts)
                                                case None =>
                                                    SirParsedCase.Pattern.ErrorPattern(
                                                      s"Type selector pattern  ${ts.tp.show} does not match scrutinee type ${scrutineeTp.show}",
                                                      pos,
                                                      ctx
                                                    )
                                    case None =>
                                        SirParsedCase.Pattern.ErrorPattern(
                                          s"scrutinee type ${scrutineeTp.show} is not a sum case class and can't be mathed with type selector pattern ${ts.tp.show}",
                                          pos,
                                          ctx
                                        )
                    case None =>
                        // SIRtype is not prod,
                        SIRType.collectSumCaseClass(ts.tp) match {
                            case Some(typeParams, patSumClass) =>
                                SIRType.collectSumCaseClass(scrutineeTp) match
                                    case Some((scrutineeTypeVars, scrutineeSumClass)) =>
                                        if patSumClass.decl.name == scrutineeSumClass.decl.name
                                        then ts.innerPattern.withOptAlias(ts.optNameInfo)
                                        else {
                                            val constrName = SIRType.syntheticNarrowConstrDeclName(
                                              patSumClass.decl.name
                                            )
                                            scrutineeSumClass.decl.constructors.find(
                                              _.name == constrName
                                            ) match {
                                                case Some(narrowConstr) =>
                                                    val narrowConstrTp =
                                                        scrutineeSumClass.decl.constrType(
                                                          narrowConstr.name
                                                        )
                                                    SIRType.collectProdCaseClass(
                                                      narrowConstrTp
                                                    ) match {
                                                        case Some(
                                                              (typeParams, narrowConstrCaseClass)
                                                            ) =>
                                                            val constrPattern =
                                                                createWildcardConstructorPattern(
                                                                  narrowConstrCaseClass,
                                                                  typeParams,
                                                                  pos
                                                                )
                                                            injectInnerPattern(
                                                              constrPattern,
                                                              ts
                                                            )
                                                        case None =>
                                                            SirParsedCase.Pattern.ErrorPattern(
                                                              s"Type selector pattern  ${ts.tp.show} is not a product type",
                                                              pos,
                                                              ctx
                                                            )
                                                    }

                                                case None =>
                                                    // in principle, we can find matching constructors navigating from the scrutineeSumClass don,
                                                    //  but multilevel selaed hoerarchies is not officially supported.
                                                    SirParsedCase.Pattern.ErrorPattern(
                                                      s"Type selector pattern  ${ts.tp.show} does not match scrutinee type ${scrutineeTp.show}",
                                                      pos,
                                                      ctx
                                                    )
                                            }
                                        }
                                    case None =>
                                        SirParsedCase.Pattern.ErrorPattern(
                                          s"scrutinee type ${scrutineeTp.show} is not a sum case class and can't be mathed with type selector pattern ${ts.tp.show}",
                                          pos,
                                          ctx
                                        )
                            case None =>
                                SirParsedCase.Pattern.ErrorPattern(
                                  s"Type selector pattern  ${ts.tp.show} is not supported: shoule be prod or sum or primitive type",
                                  pos,
                                  ctx
                                )
                        }

                }

    }

    private def compileDecisionTree(
        ctx: PatternMatchingContext,
        treeWithRefs: SirCaseDecisionTreeWithRefs,
        parsedMatch: SirParsedMatch,
        needsScrutineeLet: Boolean
    ): AnnotatedSIR = {
        // now we build SIR in form:
        // Let (action = compile(action)
        //  in ......
        //   in Let( = compile(action2)
        val actionsRecords = ctx.parsedActions.zipWithIndex.map((a, i) =>
            val count = ctx.actionsUsageCount.getOrElse(i, 0)
            val embedding =
                if count <= 1 then SirCaseDecisionTree.EmbeddingType.Inline
                else
                    val actionSize = SIR.size(a.sir)
                    if actionSize * i <= 3 * a.bindedVariables.size + 4 then
                        // ApplyN(LamN(x1...xn, action(x1,..xn)) b1...bn),
                        SirCaseDecisionTree.EmbeddingType.Inline
                    else SirCaseDecisionTree.EmbeddingType.ByReference
            val name = ctx.freshName(s"caseAction$i")
            (a.sir, embedding, name)
        )
        val guardsRecords = ctx.parsedGuards.zipWithIndex.map { (og, i) =>
            val count = ctx.guardsUsageCount.getOrElse(i, 0)
            val embedding = og match {
                case None => SirCaseDecisionTree.EmbeddingType.Inline
                case Some(g) =>
                    if count <= 1 then SirCaseDecisionTree.EmbeddingType.Inline
                    else
                        val guardSize = SIR.size(g.sir)
                        if guardSize * i <= 3 * g.bindedVariables.size + 4 then
                            // ApplyN(LamN(x1...xn, guard(x1,..xn)) b1...bn),
                            SirCaseDecisionTree.EmbeddingType.Inline
                        else SirCaseDecisionTree.EmbeddingType.ByReference
            }
            og.map(g => (g, embedding, ctx.freshName(s"caseGuard${i}_")))
        }
        val dcRefs = ctx.decisionTreeRefs.zipWithIndex.foldLeft(
          IndexedSeq.empty[(AnnotatedSIR, SirCaseDecisionTree.EmbeddingType, String)]
        ) { (prevDc, treeIndex) =>
            val (decisionTree, index) = treeIndex
            val compiledDecisionTree = compileDecisions(
              ctx,
              decisionTree,
              parsedMatch,
              actionsRecords,
              guardsRecords,
              prevDc
            )
            val treeSize = SIR.size(compiledDecisionTree)
            // val count = ctx.decisionTreeRefUsageCount.getOrElse(index, 0)
            //  TODO: better estimation of treshold
            //   lambda u (dtree u)
            val embedding =
                if treeSize >= 10 then SirCaseDecisionTree.EmbeddingType.ByReference
                else SirCaseDecisionTree.EmbeddingType.Inline
            val name = ctx.freshName(s"caseDTRef${index}_")
            prevDc.appended((compiledDecisionTree, embedding, name))
        }
        val decisions =
            compileDecisions(
              ctx,
              treeWithRefs.tree,
              parsedMatch,
              actionsRecords,
              guardsRecords,
              dcRefs
            )
        val decisionsWithScrutinee =
            if needsScrutineeLet then
                // For non-variable scrutinees, create a let binding
                SIR.Let(
                  List(
                    Binding(
                      ctx.scrutineeName,
                      parsedMatch.scrutineeTp,
                      parsedMatch.scrutinee
                    )
                  ),
                  decisions,
                  SIR.LetFlags.Lazy,
                  AnnotationsDecl.fromSrcPos(ctx.topLevelPos)
                )
            else
                // For variable scrutinees, no let binding needed
                decisions
        val sir = addActionsGuardsDecRefsLet(
          ctx,
          decisionsWithScrutinee,
          actionsRecords,
          guardsRecords,
          dcRefs
        )
        sir
    }

    private def compileDecisions(
        ctx: PatternMatchingContext,
        tree: SirCaseDecisionTree,
        parsedMatch: SirParsedMatch,
        actions: IndexedSeq[(AnnotatedSIR, SirCaseDecisionTree.EmbeddingType, String)],
        guards: IndexedSeq[Option[(SirParsedGuard, SirCaseDecisionTree.EmbeddingType, String)]],
        dcRefs: IndexedSeq[(AnnotatedSIR, SirCaseDecisionTree.EmbeddingType, String)]
    ): AnnotatedSIR = {
        tree match {
            case SirCaseDecisionTree.Leaf(bindingMap, actionRef, pos) =>
                if ctx.env.debug then
                    println(s"compileDecisions: leaf ${SirCaseDecisionTree.show(tree)}")
                actionRef match {
                    case SirParsedCase.ActionRef.FailMatch =>
                        val anns = AnnotationsDecl
                            .fromSrcPos(pos) + ("sir.FailMatch" -> trueConst)
                        SIR.Error("Match failure", anns)
                    case SirParsedCase.ActionRef.Origin(index) =>
                        val action = parsedMatch.compiledActions(index)
                        val (sir, embedding, name) = actions(index)
                        val applied = embedding match {
                            case SirCaseDecisionTree.EmbeddingType.Inline =>
                                // Filter bindingMap to only include variables with matching symbols
                                // This prevents shadowed inner variables from overwriting outer scope variables
                                val filteredBindingMap = bindingMap
                                    .filter { case (varKey, _) =>
                                        action.bindedVariables.get(varKey.varName) match {
                                            case Some(actionNameInfo) =>
                                                // Only include if symbols match
                                                varKey.symbolId == actionNameInfo.symbol.map(_.id)
                                            case None =>
                                                // Variable not in action, don't include
                                                false
                                        }
                                    }
                                    .map { case (varKey, value) => (varKey.varName, value) }

                                val renamed = SIR.renameFreeVarsInExpr(
                                  sir,
                                  filteredBindingMap
                                )
                                renamed
                            case SirCaseDecisionTree.EmbeddingType.ByReference =>
                                generateActionRefApply(
                                  name,
                                  action,
                                  pos
                                )
                        }
                        applied
                }
            case SirCaseDecisionTree.CheckGuard(bindingMap, guardRef, pos, ifTrue, optIfFalse) =>
                val cond: AnnotatedSIR =
                    generateGuardCondition(ctx, bindingMap, guardRef, guards, pos)
                val thenBranch = compileDecisions(ctx, ifTrue, parsedMatch, actions, guards, dcRefs)
                val ifFalse = optIfFalse.getOrElse(
                  SirCaseDecisionTree.Leaf(
                    bindingMap,
                    SirParsedCase.ActionRef.FailMatch,
                    pos
                  )
                )
                val elseBranch =
                    compileDecisions(ctx, ifFalse, parsedMatch, actions, guards, dcRefs)
                SIR.IfThenElse(
                  cond,
                  thenBranch,
                  elseBranch,
                  parsedMatch.resTp,
                  AnnotationsDecl.fromSrcPos(pos)
                )
            case SirCaseDecisionTree.ConstantChoice(
                  columnName,
                  scrutineeTp,
                  constEntries,
                  optNext,
                  pos
                ) =>
                // match on promitives will be available on plutus 4.
                val caseDefs = constEntries.map { case (const, subtree) =>
                    val caseBody =
                        compileDecisions(ctx, subtree, parsedMatch, actions, guards, dcRefs)
                    SIR.Case(SIR.Pattern.Const(const), caseBody, AnnotationsDecl.fromSrcPos(pos))
                }.toList
                val defaultCase = {
                    val next = optNext.getOrElse(
                      SirCaseDecisionTree.Leaf(
                        Map.empty,
                        SirParsedCase.ActionRef.FailMatch,
                        pos
                      )
                    )
                    val defaultBody =
                        compileDecisions(ctx, next, parsedMatch, actions, guards, dcRefs)
                    SIR.Case(
                      SIR.Pattern.Wildcard,
                      defaultBody,
                      AnnotationsDecl.fromSrcPos(pos) + ("sir.DefaultCase" -> SIR.Const
                          .bool(true, AnnotationsDecl.empty))
                    )
                }
                val scrutinee = SIR.Var(columnName, scrutineeTp, mkAnns(pos, ctx))
                val matchAnns = mkAnns(
                  pos,
                  ctx
                ) ++ (if ctx.isUnchecked then Map("unchecked" -> trueConst) else Map.empty)
                SIR.Match(
                  scrutinee,
                  caseDefs :+ defaultCase,
                  parsedMatch.resTp,
                  matchAnns
                )
            case SirCaseDecisionTree.ConstructorsChoice(
                  columnName,
                  scrutineeTp,
                  constructorEntries,
                  optNext,
                  pos
                ) =>
                val caseDefs = constructorEntries.map { case (_, entry) =>
                    val pattern = SIR.Pattern.Constr(
                      entry.caseClass.constrDecl,
                      entry.names.map(b => b.name).toList,
                      entry.caseClass.typeArgs,
                    )
                    val caseBody =
                        compileDecisions(ctx, entry.tree, parsedMatch, actions, guards, dcRefs)
                    SIR.Case(pattern, caseBody, AnnotationsDecl.fromSrcPos(entry.pos))
                }.toList
                val scrutinee = SIR.Var(columnName, scrutineeTp, mkAnns(pos, ctx))
                // Check if this is a product type (single constructor) that's already matched
                // CaseClass is always a product type (single constructor)
                // SumCaseClass with 1 constructor is also a product type
                val isProductTypeFullyMatched = scrutineeTp match {
                    case _: SIRType.CaseClass =>
                        // CaseClass is always a single constructor, check if we have one entry
                        constructorEntries.size == 1
                    case _ =>
                        SIRType.collectSumCaseClass(scrutineeTp) match {
                            case Some((_, sumCaseClass)) =>
                                sumCaseClass.decl.constructors.length == 1 && constructorEntries.size == 1
                            case None => false
                        }
                }
                val optDefaultCase = optNext.flatMap { nextTree =>
                    // Skip default case for product types - the single constructor always matches
                    if isProductTypeFullyMatched then None
                    else {
                        val defaultBody =
                            compileDecisions(ctx, nextTree, parsedMatch, actions, guards, dcRefs)
                        Some(
                          SIR.Case(
                            SIR.Pattern.Wildcard,
                            defaultBody,
                            mkAnns(pos, ctx) + ("sir.DefaultCase" -> SIR.Const
                                .bool(true, AnnotationsDecl.empty))
                          )
                        )
                    }
                } orElse {
                    SIRType.collectSumCaseClass(scrutineeTp) match {
                        case Some((typeParams, sumCaseClass)) =>
                            if sumCaseClass.decl.constructors.length != constructorEntries.size
                            then
                                val failMatch = SirCaseDecisionTree.Leaf(
                                  Map.empty,
                                  SirParsedCase.ActionRef.FailMatch,
                                  pos
                                )
                                Some(
                                  SIR.Case(
                                    SIR.Pattern.Wildcard,
                                    compileDecisions(
                                      ctx,
                                      failMatch,
                                      parsedMatch,
                                      actions,
                                      guards,
                                      dcRefs
                                    ),
                                    mkAnns(
                                      pos,
                                      ctx
                                    ) + ("sir.DefaultCase" -> SIR.Const
                                        .bool(true, AnnotationsDecl.empty))
                                  )
                                )
                            else None
                        case None => None
                    }
                }
                val matchAnns = mkAnns(
                  pos,
                  ctx
                ) ++ (if ctx.isUnchecked then Map("unchecked" -> trueConst) else Map.empty)
                SIR.Match(
                  scrutinee,
                  caseDefs ++ optDefaultCase,
                  parsedMatch.resTp,
                  matchAnns
                )
            case SirCaseDecisionTree.Reference(index) =>
                val (sir, embedding, name) = dcRefs(index)
                embedding match {
                    case SirCaseDecisionTree.EmbeddingType.Inline =>
                        if ctx.env.debug then
                            println(s"compileDecisions: inlining decision tree ref $index")
                        sir
                    case SirCaseDecisionTree.EmbeddingType.ByReference =>
                        if ctx.env.debug then
                            println(
                              s"compileDecisions: referencing decision tree ref $index by name"
                            )
                        val posAnns = sir.anns
                        SIR.Apply(
                          SIR.Var(name, SIRType.Fun(SIRType.Unit, sir.tp), posAnns),
                          SIR.Const.unit(posAnns),
                          sir.tp,
                          posAnns
                        )
                }
        }
    }

    private def addActionsGuardsDecRefsLet(
        ctx: PatternMatchingContext,
        decisions: AnnotatedSIR,
        actions: IndexedSeq[(AnnotatedSIR, SirCaseDecisionTree.EmbeddingType, String)],
        guards: IndexedSeq[Option[(SirParsedGuard, SirCaseDecisionTree.EmbeddingType, String)]],
        dcRefs: IndexedSeq[(AnnotatedSIR, SirCaseDecisionTree.EmbeddingType, String)]
    ): AnnotatedSIR = {
        val s0 = decisions
        val s1 = dcRefs.zipWithIndex.foldLeft(s0) { case (acc, ((sir, embedding, name), i)) =>
            embedding match {
                case SirCaseDecisionTree.EmbeddingType.Inline => acc
                case SirCaseDecisionTree.EmbeddingType.ByReference =>
                    val tp = SIRType.Fun(SIRType.Unit, decisions.tp)
                    val posAnns = AnnotationsDecl.apply(pos = sir.anns.pos)
                    val dtreeLambda = SIR.LamAbs(
                      SIR.Var("u", SIRType.Unit, posAnns),
                      sir,
                      List.empty,
                      posAnns
                    )
                    val dtreeLet = SIR.Let(
                      List(Binding(name, tp, dtreeLambda)),
                      acc,
                      SIR.LetFlags.Lazy,
                      posAnns
                    )
                    dtreeLet
            }
        }
        val s2 = actions.zipWithIndex.foldRight(s1) { case (((sir, embedding, name), i), acc) =>
            embedding match {
                case SirCaseDecisionTree.EmbeddingType.Inline => acc
                case SirCaseDecisionTree.EmbeddingType.ByReference =>
                    val action = ctx.parsedActions(i)
                    val tp = generateActionSirType(action)
                    val posAnns = AnnotationsDecl.apply(pos = sir.anns.pos)
                    val actionLet = SIR.Let(
                      List(Binding(name, tp, sir)),
                      acc,
                      SIR.LetFlags.Lazy,
                      posAnns
                    )
                    actionLet
            }
        }
        val s3 = guards.foldRight(s2) { case (og, acc) =>
            og match
                case None => acc
                case Some((guard, embedding, name)) =>
                    embedding match {
                        case SirCaseDecisionTree.EmbeddingType.Inline => acc
                        case SirCaseDecisionTree.EmbeddingType.ByReference =>
                            val tp = buildGuardType(guard.typeBindings)
                            val posAnns = AnnotationsDecl.apply(pos = guard.sir.anns.pos)

                            val guardLet = SIR.Let(
                              List(Binding(name, tp, guard.sir)),
                              acc,
                              SIR.LetFlags.Lazy,
                              posAnns
                            )
                            guardLet
                    }
        }
        s3
    }

    /** generatee ''' Apply(....(Apply(name,b1), .. bName) '''
      */
    private def generateActionRefApply(
        name: String,
        action: SirParsedAction,
        pos: SrcPos
    ): AnnotatedSIR = {
        if action.bindedVariables.isEmpty then
            val actionVar = SIR.Var(
              name,
              SIRType.Fun(SIRType.Unit, action.sir.tp),
              AnnotationsDecl.fromSrcPos(pos)
            )
            SIR.Apply(
              actionVar,
              SIR.Const.unit(AnnotationsDecl.fromSrcPos(pos)),
              action.sir.tp,
              AnnotationsDecl.fromSrcPos(pos)
            )
        else
            val actionType = generateActionSirType(action)
            val actionVar = SIR.Var(name, actionType, AnnotationsDecl.fromSrcPos(pos))
            val typeBinding = action.bindedVariables.map { case (n, b) =>
                TypeBinding(n, b.tp)
            }.toList
            val applySir = buildApplySeq(actionVar, typeBinding, pos)
            applySir
    }

    private def generateGuardCondition(
        context: PatternMatchingContext,
        bindingMap: Map[VariableKey, String],
        guardIndex: Int,
        guardRecords: IndexedSeq[
          Option[(SirParsedGuard, SirCaseDecisionTree.EmbeddingType, String)]
        ],
        pos: SrcPos
    ): AnnotatedSIR = {
        guardRecords(guardIndex) match
            case None =>
                report.error("Internal error: guard reference is None", pos)
                if context.env.debug then
                    println(s"generateGuardCondition: guard is None at index $guardIndex")
                SIR.Const.bool(true, AnnotationsDecl.fromSrcPos(pos))
            case Some((guard, embedding, name)) =>
                embedding match {
                    case SirCaseDecisionTree.EmbeddingType.Inline =>
                        val renameMap = guard.bindedVariables.map { case (n, b) =>
                            (n, bindingMap(b.variableKey))
                        }.toMap
                        SIR.renameFreeVars(
                          guard.sir,
                          renameMap
                        ).asInstanceOf[AnnotatedSIR]
                    case SirCaseDecisionTree.EmbeddingType.ByReference =>
                        generateGuardApply(name, guard, pos)
                }
    }

    private def generateGuardApply(
        name: String,
        guard: SirParsedGuard,
        pos: SrcPos
    ): AnnotatedSIR = {
        if guard.bindedVariables.isEmpty then
            val guardVar = SIR.Var(
              name,
              SIRType.Fun(SIRType.Unit, SIRType.Boolean),
              AnnotationsDecl.fromSrcPos(pos)
            )
            SIR.Apply(
              guardVar,
              SIR.Const.unit(AnnotationsDecl.fromSrcPos(pos)),
              SIRType.Boolean,
              AnnotationsDecl.fromSrcPos(pos)
            )
        else
            val guardType = buildGuardType(guard.typeBindings)
            val guardVar = SIR.Var(name, guardType, AnnotationsDecl.fromSrcPos(pos))
            val applySir = buildApplySeq(guardVar, guard.typeBindings, pos)
            applySir
    }

    private def buildGuardType(bindedVariables: List[TypeBinding]): SIRType = {
        if bindedVariables.isEmpty then SIRType.Fun(SIRType.Unit, SIRType.Boolean)
        else
            bindedVariables.foldRight(SIRType.Boolean: SIRType) { (b, acc) =>
                SIRType.Fun(b.tp, acc)
            }
    }

    private def buildApplySeq(
        s0: AnnotatedSIR,
        args: List[TypeBinding],
        pos: SrcPos
    ): AnnotatedSIR = {
        args.foldLeft(s0) { (acc, b) =>
            val arg = SIR.Var(b.name, b.tp, AnnotationsDecl.fromSrcPos(pos))
            SIR.Apply(
              acc,
              arg,
              acc.tp match {
                  case SIRType.Fun(_, retTp) => retTp
                  case _ =>
                      throw IllegalStateException(
                        s"Expected function type in application, but got ${acc.tp.show}"
                      )
              },
              AnnotationsDecl.fromSrcPos(pos)
            )
        }
    }

    private def generateActionSirType(
        action: SirParsedAction
    ): SIRType = {
        if action.bindedVariables.isEmpty then SIRType.Fun(SIRType.Unit, action.sir.tp)
        else
            action.typeBindings.foldRight(action.sir.tp) { (b, acc) =>
                SIRType.Fun(b.tp, acc)
            }
    }

}
