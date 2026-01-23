package scalus.compiler.sir

//import scalus.uplc.Constant.Integer
import scalus.uplc.{Constant, DefaultFun}

val SIRVersion: (Int, Int) = (4, 0)

case class Module(
    version: (Int, Int),
    name: String,
    linked: Boolean,
    requireBackend: Option[String],
    defs: List[Binding]
)

case class Binding(name: String, tp: SIRType, value: SIR) {

    override def toString: String = {
        s"Binding(\"$name\" [${tp.show}] : $value)"
    }

}

case class TypeBinding(name: String, tp: SIRType) {
    override def toString: String = s"TypeBinding(\"$name\" : ${tp.show})"
}

/** SIR Position - position in Scala source code.
  * @param file
  *   -- fiel of position. Empty file ("") means that position is unknown.
  * @param startLine
  * @param startColumn
  * @param endLine
  * @param endColumn
  */
case class SIRPosition(
    file: String,
    startLine: Int,
    startColumn: Int,
    endLine: Int,
    endColumn: Int,
    inlinedFrom: List[SIRPosition] = Nil
) {

    def show: String = {
        val basePos = s"$file:${startLine + 1}:${startColumn} - ${endLine + 1}:${endColumn}"
        if inlinedFrom.isEmpty then basePos
        else s"$basePos\n  inlined from: ${inlinedFrom.map(_.showSingle).mkString(" -> ")}"
    }

    def showSingle: String = s"$file:${startLine + 1}:${startColumn}"

    def isEmpty: Boolean = file.isEmpty && startLine == 0 && startColumn == 0 &&
        endLine == 0 && endColumn == 0

}

object SIRPosition {

    val empty: SIRPosition = SIRPosition("", 0, 0, 0, 0, Nil)

}

case class AnnotationsDecl(
    pos: SIRPosition,
    comment: Option[String] = None,
    data: Map[String, SIR] = Map.empty
) {

    def ++(data: Map[String, SIR]): AnnotationsDecl = {
        AnnotationsDecl(
          pos = pos,
          comment = comment,
          data = this.data ++ data
        )
    }

    def +(keyValue: (String, SIR)): AnnotationsDecl = {
        AnnotationsDecl(
          pos = pos,
          comment = comment,
          data = this.data + keyValue
        )
    }

}

case object AnnotationsDecl {
    import scala.quoted.*

    inline def empty: AnnotationsDecl = ${ SIRMacro.emptyAnnotationsDeclImpl }

}

case class ConstrDecl(
    /** Name (usually - full name of symbol, i.s. scalus.cardano.onchain.plutus.prelude.List$.Nit )
      */
    name: String,

    /** Parameters of the constructor.
      */
    params: List[TypeBinding],

    /** Type parameters of this type.
      */
    typeParams: List[SIRType.TypeVar],

    /** Type parameters of the parent constructor.
      */
    parentTypeArgs: List[SIRType],

    /** Annotations of the constructor.
      */
    annotations: AnnotationsDecl
) {

    if name.contains(" ") || name.contains("\u0021") then {
        throw new RuntimeException("Invalid name in constructor: " + name)
    }

}

//  Data~ Lost(Const)

//  A -> B -> C1, C2
//    -> B1 -> D

//f:  B => Int   B = Data(C1,C2):     \lambda (c1,c2) => c2(x)
//f:  A => Int   A = Data(C1, C2, D)

case class DataDecl(
    /** Name of the data type. (full name of the symbol)
      */
    name: String,
    constructors: List[ConstrDecl],
    typeParams: List[SIRType.TypeVar],
    annotations: AnnotationsDecl
) {

    if constructors.isEmpty then
        throw new RuntimeException(
          s"Data declaration must have at least one constructor. (name=$name)"
        )
    // if (name == "")

    private var _tp: SIRType = null
    private var _constrs: Map[String, SIRType] = null

    def tp: SIRType =
        if _tp == null then
            _tp = typeParams match
                case Nil => SIRType.SumCaseClass(this, Nil)
                case _   => SIRType.TypeLambda(typeParams, SIRType.SumCaseClass(this, typeParams))
        _tp

    def constrType(constrName: String): SIRType = {
        if _constrs == null then {
            val nConstrs = constructors.length
            _constrs = constructors
                .map { c =>
                    val parent = typeParams match
                        case Nil => tp
                        case typeParams =>
                            val tpEnv = typeParams.zip(c.parentTypeArgs).toMap
                            SIRType.substitute(tp, tpEnv, Map.empty)
                    // Determine if this constructor should have a parent:
                    // - Case class: single constructor with same name as DataDecl -> no parent
                    // - Enum case: constructor name differs from DataDecl, or multiple constructors -> has parent
                    val isCaseClass = nConstrs == 1 && c.name == name
                    val optParent = if isCaseClass then None else Some(parent)
                    val sirType = c.typeParams match
                        case Nil => SIRType.CaseClass(c, Nil, optParent)
                        case targs =>
                            SIRType.TypeLambda(
                              c.typeParams,
                              SIRType.CaseClass(c, c.typeParams, optParent)
                            )
                    c.name -> sirType
                }
                .toMap[String, SIRType]
        }
        _constrs.getOrElse(
          constrName,
          throw new IllegalArgumentException(
            s"Constructor for $constrName not found in $name"
          )
        )

    }
}

//case class ExternalDataDecl(module: String, name: String)

sealed trait SIR {

    def tp: SIRType

    def ~=~(that: SIR): Boolean =
        SIRUnify.unifySIR(this, that, SIRUnify.Env.empty) match {
            case SIRUnify.UnificationSuccess(_, _)    => true
            case SIRUnify.UnificationFailure(_, _, _) => false
        }

    /** Applies this SIR term to an argument, creating an Apply node.
      *
      * This method handles function application by:
      *   - Stripping any Decl wrappers from the function term
      *   - Extracting the result type from the function's type signature
      *   - Creating an Apply node with the unwrapped function and argument
      *   - Re-wrapping the result with the original Decl nodes
      *
      * The method supports both simple function types (`SIRType.Fun`) and polymorphic functions
      * wrapped in type lambdas (`SIRType.TypeLambda`).
      *
      * @param arg
      *   the argument to apply to this function term
      * @return
      *   a SIR expression representing the function application, potentially wrapped in Decl nodes
      * @throws RuntimeException
      *   if the term's type is not a function type
      *
      * @example
      *   {{{
      * val f: SIR = ... // a function of type A -> B
      * val arg: AnnotatedSIR = ... // an argument of type A
      * val result: SIR = f $ arg // Apply(f, arg, B, anns)
      *   }}}
      */
    infix def $(arg: AnnotatedSIR): SIR = {
        // Helper to strip Decls and collect them
        @scala.annotation.tailrec
        def stripDecls(sir: SIR, decls: List[DataDecl] = Nil): (AnnotatedSIR, List[DataDecl]) =
            sir match
                case SIR.Decl(data, term)    => stripDecls(term, data :: decls)
                case annotated: AnnotatedSIR => (annotated, decls)

        val (term, decls) = stripDecls(this)

        // Compute result type from function type
        // Handle both direct Fun types and TypeLambda-wrapped Fun types
        val resultType = term.tp match
            case SIRType.Fun(_, out) => out
            case SIRType.TypeLambda(_, body) =>
                body match
                    case SIRType.Fun(_, out) => out
                    case _ =>
                        throw new RuntimeException(
                          s"Cannot apply non-function type ${term.tp.show} at ${term.anns.pos.show}"
                        )
            case _ =>
                throw new RuntimeException(
                  s"Cannot apply non-function type ${term.tp.show} at ${term.anns.pos.show}"
                )

        // Create Apply node
        val applyNode = SIR.Apply(term, arg, resultType, term.anns)

        // Re-wrap with Decls (in reverse order)
        decls.foldLeft(applyNode: SIR)((acc, data) => SIR.Decl(data, acc))
    }

}

sealed trait AnnotatedSIR extends SIR {

    def anns: AnnotationsDecl

}

object SIR:

    //  Module A:
    //   case class AClass(x:Int, y: String)
    //  Module b:
    //    val x = A.AClass(1, "hello")
    //    val y = A.AClass(2, "world")

    // Mole b SIR:  (var1)
    //  Var(x,  SumType("AClass", List(("x", Int), ("y", String))))
    //  Var(y,  SumType("AClass", List(("x", Int), ("y", String))))

    // Module b SIR:
    //  TypeAlias(1, SumType("AClass", List(("x", Int), ("y", String))))
    //  Var(x,  TypeRef(1))

    case class Var(name: String, tp: SIRType, anns: AnnotationsDecl) extends AnnotatedSIR {

        override def toString: String = s"Var($name, ${tp.show})"
    }

    case class ExternalVar(moduleName: String, name: String, tp: SIRType, anns: AnnotationsDecl)
        extends AnnotatedSIR {

        override def toString: String = s"ExternalVar($moduleName, $name, ${tp.show})"

    }

    opaque type LetFlags = Int

    extension (flags: LetFlags) {
        def isRec: Boolean = LetFlags.isRec(flags)
        def isLazy: Boolean = LetFlags.isLazy(flags)
        def |(other: LetFlags): LetFlags = flags | other
        def &(other: LetFlags): LetFlags = flags & other
        def remove(other: LetFlags): LetFlags = flags & ~other
    }

    object LetFlags {
        val None: LetFlags = 0
        val Recursivity: LetFlags = 1 << 0
        val Lazy: LetFlags = 1 << 1

        def is(flags: LetFlags): Boolean = flags != 0
        def isRec(flags: LetFlags): Boolean = (flags & Recursivity) != 0
        def isLazy(flags: LetFlags): Boolean = (flags & Lazy) != 0
    }

    case class Let(
        bindings: List[Binding],
        body: SIR,
        flags: LetFlags,
        anns: AnnotationsDecl
    ) extends AnnotatedSIR {

        bindings match {
            case List(Binding("args", tp, value)) =>
                throw RuntimeException("QQQ")
            case _ =>
        }

        override def tp: SIRType = body.tp
    }

    case class LamAbs(
        param: Var,
        term: SIR,
        typeParams: List[SIRType.TypeVar],
        anns: AnnotationsDecl
    ) extends AnnotatedSIR {

        override def tp: SIRType = {
            if typeParams.isEmpty then SIRType.Fun(param.tp, term.tp)
            else SIRType.TypeLambda(typeParams, SIRType.Fun(param.tp, term.tp))
        }

    }

    case class Apply(f: AnnotatedSIR, arg: AnnotatedSIR, tp: SIRType, anns: AnnotationsDecl)
        extends AnnotatedSIR {

        if f.tp == SIRType.Unit then
            throw new RuntimeException(
              s"Apply: f is Unit, cannot apply to Unit at ${anns.pos.show}.\n" +
                  s"f: $f"
            )

        // TODO: make tp computable, not stored.  (implement subst at first).
        /*
        def recheckTp: SIRType = f.tp match {
            case SIRType.Fun(in, out) => out
            case SIRType.TypeLambda(tp1, tpexpr) =>
                val sftp = tpexpr.subst(tp1,arg.tp)
                sftp match {
                    case SIRType.Fun(in, out) => out
                    case _ => throw new Exception("Apply: f is not a function")
                }
            case _ => throw new Exception("Apply: f is not a function")
        }
         */

    }

    case class Select(scrutinee: SIR, field: String, tp: SIRType, anns: AnnotationsDecl)
        extends AnnotatedSIR

    case class Const(uplcConst: Constant, tp: SIRType, anns: AnnotationsDecl) extends AnnotatedSIR

    object Const {
        def boolean(value: Boolean, anns: AnnotationsDecl = AnnotationsDecl.empty): Const = {
            Const(Constant.Bool(value), SIRType.Boolean, anns)
        }

        def bool(value: Boolean, anns: AnnotationsDecl = AnnotationsDecl.empty): Const = {
            boolean(value, anns)
        }

        def integer(value: BigInt, anns: AnnotationsDecl = AnnotationsDecl.empty): Const = {
            Const(Constant.Integer(value), SIRType.Integer, anns)
        }

        def string(value: String, anns: AnnotationsDecl = AnnotationsDecl.empty): Const = {
            Const(Constant.String(value), SIRType.String, anns)
        }

        def unit(anns: AnnotationsDecl = AnnotationsDecl.empty): Const = {
            Const(Constant.Unit, SIRType.Unit, anns)
        }
    }

    case class And(a: AnnotatedSIR, b: AnnotatedSIR, anns: AnnotationsDecl) extends AnnotatedSIR {
        override def tp: SIRType = SIRType.Boolean
    }

    case class Or(a: AnnotatedSIR, b: AnnotatedSIR, anns: AnnotationsDecl) extends AnnotatedSIR {
        override def tp: SIRType = SIRType.Boolean
    }

    case class Not(a: AnnotatedSIR, anns: AnnotationsDecl) extends AnnotatedSIR {
        override def tp: SIRType = SIRType.Boolean
    }

    case class IfThenElse(
        cond: AnnotatedSIR,
        t: AnnotatedSIR,
        f: AnnotatedSIR,
        tp: SIRType,
        anns: AnnotationsDecl
    ) extends AnnotatedSIR

    case class Builtin(bn: DefaultFun, tp: SIRType, anns: AnnotationsDecl) extends AnnotatedSIR

    case class Error(msg: AnnotatedSIR, anns: AnnotationsDecl, cause: Throwable | Null = null)
        extends AnnotatedSIR {
        override def tp: SIRType = SIRType.TypeNothing
    }

    object Error {

        def apply(msg: String, anns: AnnotationsDecl): Error =
            Error(SIR.Const(Constant.String(msg), SIRType.String, anns), anns)

    }

    // TODO: unify data-decl.
    case class Constr(
        name: String,
        data: DataDecl,
        args: List[SIR],
        tp: SIRType,
        anns: AnnotationsDecl
    ) extends AnnotatedSIR

    sealed trait Pattern {
        def show: String = this match {
            case Pattern.Constr(constr, bindings, typeParamsBindings) =>
                s"${constr.name}, ${bindings.mkString(", ")}"
            case Pattern.Const(value) => s"${value.uplcConst}"
            case Pattern.Wildcard     => "_"
        }
    }

    object Pattern:
        case class Constr(
            constr: ConstrDecl,
            bindings: List[String],
            typeParamsBindings: List[SIRType]
        ) extends Pattern
        case class Const(value: SIR.Const) extends Pattern
        case object Wildcard extends Pattern

    case class Case(
        pattern: Pattern,
        body: SIR,
        anns: AnnotationsDecl
    )

    /** Match expression.
      * @param scrutinee
      * @param cases
      * @param tp
      *   \- resulting type of Match expression, can be calculated as max(tp of all cases)
      */
    case class Match(scrutinee: AnnotatedSIR, cases: List[Case], tp: SIRType, anns: AnnotationsDecl)
        extends AnnotatedSIR

    case class Cast(term: AnnotatedSIR, tp: SIRType, anns: AnnotationsDecl) extends AnnotatedSIR {

        if anns.pos.file.contains("GeneratingUnscopedVarsTest") then
            tp match
                case SIRType.Fun(SIRType.Integer, tuple2Constr) =>
                    tuple2Constr match
                        case SIRType.CaseClass(constr, List(SIRType.Integer, SIRType.Integer), _)
                            if constr.name == "scala.Tuple2" =>
                            throw RuntimeException("QQQ")
                        case _ =>
                case _ =>

    }

    case class Decl(data: DataDecl, term: SIR) extends SIR {

        override def tp: SIRType = term.tp

    }

    def partitionUsedFreeVarsFrom(sir: SIR, vars: Set[String]): (Set[String], Set[String]) = {

        scala.util.boundary[(Set[String], Set[String])] {

            def f(
                sir: SIR,
                localNames: Set[String],
                acc: (Set[String], Set[String])
            ): (Set[String], Set[String]) =
                sir match {
                    case Var(name, _, _) =>
                        if localNames.contains(name) then acc
                        else if vars.contains(name) then {
                            val nUsed = acc._1 + name
                            val nUnused = acc._2 - name
                            if nUnused.isEmpty then scala.util.boundary.break((nUsed, nUnused))
                            else (nUsed, nUnused)
                        } else acc
                    case _ => acc
                }

            accumulate[(Set[String], Set[String])](
              sir,
              (Set.empty, vars),
              Set.empty,
              f
            )

        }

    }

    def renameFreeVars(sir: SIR, renames: Map[String, String]): SIR = {

        sir match
            case SIR.Decl(data, term) =>
                SIR.Decl(data, renameFreeVars(term, renames))
            case annSir: AnnotatedSIR =>
                renameFreeVarsInExpr(annSir, renames)

    }

    def renameFreeVarsInExpr(sir: AnnotatedSIR, renames: Map[String, String]): AnnotatedSIR = {

        def processSir(sir: SIR, localNames: Set[String]): SIR = {
            sir match {
                case SIR.Decl(data, term) =>
                    SIR.Decl(data, processSir(term, localNames))
                case annSir: AnnotatedSIR =>
                    processAnnotated(annSir, localNames)
            }
        }

        def processAnnotated(sir: AnnotatedSIR, localNames: Set[String]): AnnotatedSIR = {
            sir match {
                case Var(name, tp, anns) =>
                    if localNames.contains(name) then sir
                    else
                        renames.get(name) match {
                            case Some(newName) => Var(newName, tp, anns)
                            case None          => sir
                        }
                case ExternalVar(moduleName, name, tp, anns) => sir
                case Let(bindings, body, flags, anns) =>
                    val initBindings: IndexedSeq[Binding] = IndexedSeq.empty
                    val (newBindings, newLocalNames) =
                        bindings.foldLeft((initBindings, localNames)) {
                            case ((accBindings, ln), b) =>
                                // For recursive lets, the binding name is in scope when processing the value
                                // For non-recursive lets, the binding name is NOT in scope when processing the value
                                if flags.isRec then
                                    val nLn = ln + b.name
                                    val nValue = processSir(b.value, nLn)
                                    (accBindings :+ Binding(b.name, b.tp, nValue), nLn)
                                else
                                    val nValue = processSir(b.value, ln)
                                    val nLn = ln + b.name
                                    (accBindings :+ Binding(b.name, b.tp, nValue), nLn)
                        }
                    val newBody = processSir(body, newLocalNames)
                    Let(newBindings.toList, newBody, flags, anns)
                case LamAbs(param, term, typeParams, anns) =>
                    val nLn = localNames + param.name
                    val nTerm = processSir(term, nLn)
                    LamAbs(param, nTerm, typeParams, anns)
                case Apply(f, arg, tp, anns) =>
                    val nF = processSir(f, localNames).asInstanceOf[AnnotatedSIR]
                    val nArg = processSir(arg, localNames).asInstanceOf[AnnotatedSIR]
                    Apply(nF, nArg, tp, anns)
                case Select(scrutinee, field, tp, anns) =>
                    val nScrutinee = processSir(scrutinee, localNames).asInstanceOf[AnnotatedSIR]
                    Select(nScrutinee, field, tp, anns)
                case Const(_, _, _) => sir
                case And(a, b, anns) =>
                    val nA = processSir(a, localNames).asInstanceOf[AnnotatedSIR]
                    val nB = processSir(b, localNames).asInstanceOf[AnnotatedSIR]
                    And(nA, nB, anns)
                case Or(a, b, anns) =>
                    val nA = processSir(a, localNames).asInstanceOf[AnnotatedSIR]
                    val nB = processSir(b, localNames).asInstanceOf[AnnotatedSIR]
                    Or(nA, nB, anns)
                case Not(a, anns) =>
                    val nA = processSir(a, localNames).asInstanceOf[AnnotatedSIR]
                    Not(nA, anns)
                case IfThenElse(cond, t, f, tp, anns) =>
                    val nCond = processSir(cond, localNames).asInstanceOf[AnnotatedSIR]
                    val nT = processSir(t, localNames).asInstanceOf[AnnotatedSIR]
                    val nF = processSir(f, localNames).asInstanceOf[AnnotatedSIR]
                    IfThenElse(nCond, nT, nF, tp, anns)
                case Builtin(_, _, _) => sir
                case Error(msg, anns, cause) =>
                    val nMsg = processSir(msg, localNames).asInstanceOf[AnnotatedSIR]
                    Error(nMsg, anns, cause)
                case Constr(name, data, args, tp, anns) =>
                    val nArgs = args.map(a => processSir(a, localNames))
                    Constr(name, data, nArgs, tp, anns)
                case Match(scrutinee, cases, tp, anns) =>
                    val nScrutinee = processSir(scrutinee, localNames).asInstanceOf[AnnotatedSIR]
                    val nCases = cases.map { c =>
                        val nLn = c.pattern match {
                            case Pattern.Wildcard               => localNames
                            case Pattern.Constr(_, bindings, _) => localNames ++ bindings
                            case Pattern.Const(_)               => localNames
                        }
                        val nBody = processSir(c.body, nLn)
                        Case(c.pattern, nBody, c.anns)
                    }
                    Match(nScrutinee, nCases, tp, anns)
                case Cast(term, tp, anns) =>
                    val nTerm = processAnnotated(term, localNames)
                    Cast(nTerm, tp, anns)
            }
        }

        processAnnotated(sir, Set.empty)

    }

    def accumulate[A](
        sir: SIR,
        a0: A,
        localNames: Set[String],
        f: (SIR, Set[String], A) => A
    ): A = {

        sir match {
            case Var(_, _, anns)            => f(sir, localNames, a0)
            case ExternalVar(_, _, _, anns) => f(sir, localNames, a0)
            case Let(bindings, body, _, anns) =>
                val (aN, lnN) = bindings.foldLeft((a0, localNames)) { case ((a, ln), b) =>
                    val a1 = accumulate(b.value, a, ln, f)
                    val ln1 = ln + b.name
                    (a1, ln1)
                }
                val aBody = accumulate(body, aN, lnN, f)
                f(sir, lnN, aBody)
            case LamAbs(param, term, _, anns) =>
                val ln1 = localNames + param.name
                val a1 = accumulate(term, a0, ln1, f)
                f(sir, ln1, a1)
            case Apply(fun, arg, _, anns) =>
                val a1 = accumulate(fun, a0, localNames, f)
                val a2 = accumulate(arg, a1, localNames, f)
                f(sir, localNames, a2)
            case Select(scrutinee, _, _, anns) =>
                val a1 = accumulate(scrutinee, a0, localNames, f)
                f(sir, localNames, a1)
            case Const(_, _, anns) => f(sir, localNames, a0)
            case And(a, b, anns) =>
                val a1 = accumulate(a, a0, localNames, f)
                val a2 = accumulate(b, a1, localNames, f)
                f(sir, localNames, a2)
            case Or(a, b, anns) =>
                val a1 = accumulate(a, a0, localNames, f)
                val a2 = accumulate(b, a1, localNames, f)
                f(sir, localNames, a2)
            case Not(a, anns) =>
                val a1 = accumulate(a, a0, localNames, f)
                f(sir, localNames, a1)
            case IfThenElse(cond, t, f1, _, anns) =>
                val a1 = accumulate(cond, a0, localNames, f)
                val a2 = accumulate(t, a1, localNames, f)
                val a3 = accumulate(f1, a2, localNames, f)
                f(sir, localNames, a3)
            case Builtin(_, _, anns) => f(sir, localNames, a0)
            case Error(msg, anns, _) =>
                val a1 = accumulate(msg, a0, localNames, f)
                f(sir, localNames, a1)
            case Constr(_, _, args, _, anns) =>
                val a1 = args.foldLeft(a0)((a, b) => accumulate(b, a, localNames, f))
                f(sir, localNames, a1)
            case Match(scrutinee, cases, _, anns) =>
                val a1 = accumulate(scrutinee, a0, localNames, f)
                val (a2, _) = cases.foldLeft((a1, localNames)) { case ((a, ln), c) =>
                    val ln1 = c.pattern match {
                        case Pattern.Wildcard               => ln
                        case Pattern.Constr(_, bindings, _) => ln ++ bindings
                        case Pattern.Const(_)               => ln
                    }
                    val a1 = accumulate(c.body, a, ln1, f)
                    (a1, ln)
                }
                f(sir, localNames, a2)
            case Cast(term, _, anns) =>
                val a1 = accumulate(term, a0, localNames, f)
                f(sir, localNames, a1)
            case Decl(_, term) =>
                val a1 = accumulate(term, a0, localNames, f)
                f(sir, localNames, a1)
        }
    }

    // Compute size of SIR expression (number of nodes in the tree)
    def size(sir: SIR): Int = sir match
        case Var(_, _, _)                  => 1
        case ExternalVar(_, _, _, _)       => 1
        case Let(bindings, body, _, _)     => 1 + bindings.map(b => size(b.value)).sum + size(body)
        case LamAbs(_, term, _, _)         => 1 + size(term)
        case Apply(f, arg, _, _)           => 1 + size(f) + size(arg)
        case Select(scrutinee, _, _, _)    => 1 + size(scrutinee)
        case Const(_, _, _)                => 1
        case And(a, b, _)                  => 1 + size(a) + size(b)
        case Or(a, b, _)                   => 1 + size(a) + size(b)
        case Not(a, _)                     => 1 + size(a)
        case IfThenElse(cond, t, f, _, _)  => 1 + size(cond) + size(t) + size(f)
        case Builtin(_, _, _)              => 1
        case Error(msg, _, _)              => 1 + size(msg)
        case Constr(_, _, args, _, _)      => 1 + args.map(a => size(a)).sum
        case Match(scrutinee, cases, _, _) => 1 + size(scrutinee) + cases.map(c => size(c.body)).sum
        case Cast(term, _, _)              => 1 + size(term)
        case Decl(data, term)              => 1 + size(term)

end SIR

case class Program(version: (Int, Int, Int), term: SIR)

object SIRChecker {

    case class CheckException(msg: String, cause: Throwable | Null = null)
        extends RuntimeException(msg, cause)

    def checkAndThrow(SIR: SIR, throwOnFirst: Boolean = true): Unit = {
        val errors = checkSIR(SIR, throwOnFirst)
        if errors.nonEmpty then throw new CheckException(errors.mkString("\n"))
    }

    def checkSIR(sir: SIR, throwOnFirst: Boolean): Seq[String] = {
        checkExpr(sir, throwOnFirst)
    }

    def checkData(data: DataDecl, throwOnFirst: Boolean): Seq[String] = {
        val checkConstrs = data.constructors.flatMap(c => checkConstr(c, throwOnFirst))
        val checkTp = checkType(data.tp, throwOnFirst)
        checkConstrs ++ checkTp
    }

    def checkConstr(constr: ConstrDecl, throwOnFirst: Boolean): Seq[String] = {
        val checkParams = constr.params.flatMap(b => checkTypeBinding(b, throwOnFirst))
        checkParams
    }

    def checkTypeBinding(tpb: TypeBinding, throwOnFirst: Boolean): Seq[String] =
        checkType(tpb.tp, throwOnFirst)

    def checkAnnotations(decl: AnnotationsDecl, throwOnFirst: Boolean): Seq[String] = {
        decl.data.flatMap { case (k, v) => checkExpr(v, throwOnFirst) }.toSeq
    }

    def checkExpr(expr: SIR, throwOnFirst: Boolean): Seq[String] = {
        val checkTp = checkType(expr.tp, throwOnFirst)
        expr match {
            case SIR.Var(_, tp, anns)            => checkTp ++ checkAnnotations(anns, throwOnFirst)
            case SIR.ExternalVar(_, _, tp, anns) => checkTp ++ checkAnnotations(anns, throwOnFirst)
            case SIR.Let(bindings, body, flags, anns) =>
                val checkBindings = bindings.flatMap(b => checkExpr(b.value, throwOnFirst))
                checkBindings ++ checkSIR(body, throwOnFirst) ++ checkTp ++ checkAnnotations(
                  anns,
                  throwOnFirst
                )
            case SIR.LamAbs(param, term, typeParams, anns) =>
                checkType(param.tp, throwOnFirst) ++ checkSIR(
                  term,
                  throwOnFirst
                ) ++ checkTp ++ checkAnnotations(anns, throwOnFirst)
            case SIR.Apply(f, arg, tp, anns) =>
                checkSIR(f, throwOnFirst) ++ checkSIR(
                  arg,
                  throwOnFirst
                ) ++ checkTp ++ checkAnnotations(anns, throwOnFirst)
            case SIR.Select(scrutinee, _, _, anns) =>
                checkSIR(scrutinee, throwOnFirst) ++ checkTp ++ checkAnnotations(anns, throwOnFirst)
            case SIR.Const(_, tp, anns) => checkTp ++ checkAnnotations(anns, throwOnFirst)
            case SIR.And(a, b, anns) =>
                checkSIR(a, throwOnFirst) ++ checkSIR(b, throwOnFirst) ++ checkTp
            case SIR.Or(a, b, anns) =>
                checkSIR(a, throwOnFirst) ++ checkSIR(b, throwOnFirst) ++ checkTp
            case SIR.Not(a, anns) => checkSIR(a, throwOnFirst) ++ checkTp
            case SIR.IfThenElse(cond, t, f, tp, anns) =>
                checkSIR(cond, throwOnFirst) ++ checkSIR(t, throwOnFirst) ++ checkSIR(
                  f,
                  throwOnFirst
                ) ++ checkTp ++ checkAnnotations(anns, throwOnFirst)
            case SIR.Builtin(_, tp, anns) => checkTp ++ checkAnnotations(anns, throwOnFirst)
            case SIR.Error(_, anns, _)    => checkTp ++ checkAnnotations(anns, throwOnFirst)
            case SIR.Constr(_, data, args, tp1, anns) =>
                val checkArgs = args.flatMap(a => checkSIR(a, throwOnFirst))
                val checkTp1 = checkType(tp1, throwOnFirst)
                checkData(data, throwOnFirst) ++ checkArgs ++ checkTp ++ checkTp1 ++
                    checkAnnotations(anns, throwOnFirst)
            case SIR.Match(scrutinee, cases, tp, anns) =>
                val checkCases = cases.flatMap(c => checkCase(c, throwOnFirst))
                checkSIR(scrutinee, throwOnFirst) ++ checkCases ++ checkTp ++ checkAnnotations(
                  anns,
                  throwOnFirst
                )
            case SIR.Cast(term, tp, anns) =>
                checkSIR(term, throwOnFirst) ++ checkType(tp, throwOnFirst) ++
                    checkAnnotations(anns, throwOnFirst)
            case SIR.Decl(data, term) =>
                checkData(data, throwOnFirst) ++ checkSIR(term, throwOnFirst)
        }
    }

    def checkCase(c: SIR.Case, throwOnFirst: Boolean): Seq[String] = {
        c.pattern match
            case SIR.Pattern.Constr(constr, bindings, typeBindings) =>
                typeBindings.flatMap(x => checkType(x, throwOnFirst)) ++
                    checkConstr(constr, throwOnFirst) ++ checkSIR(c.body, throwOnFirst)
            case SIR.Pattern.Const(value) =>
                checkSIR(value, throwOnFirst) ++ checkSIR(c.body, throwOnFirst)
            case SIR.Pattern.Wildcard => checkSIR(c.body, throwOnFirst)
    }

    def checkType(tp: SIRType, throwOnFirst: Boolean): Seq[String] =
        if SIRType.checkAllProxiesFilled(tp) then Nil
        else
            val msg = s"Type has unfilled proxies. (tp=${tp})"
            if throwOnFirst then throw CheckException(msg)
            else Seq(msg)

}
