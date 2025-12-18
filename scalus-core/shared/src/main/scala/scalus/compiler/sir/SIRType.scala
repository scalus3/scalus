package scalus.compiler.sir

import scalus.compiler.sir.SIRType.TypeVar
import scalus.uplc.{DefaultUni, TypeScheme as UplcTypeScheme}

import java.util
import scala.annotation.tailrec

sealed trait SIRType {

    def show: String

    def showDebug: String

    infix def ~=~(that: SIRType): Boolean =
        SIRUnify.topLevelUnifyType(this, that, SIRUnify.Env.empty).isSuccess

    infix def ->:(that: SIRType): SIRType.Fun =
        SIRType.Fun(that, this)

    inline def =>>:(that: SIRType): SIRType.TypeLambda =
        this match
            case x: SIRType.TypeVar => SIRType.TypeLambda(scala.List(x), that)
            case other =>
                throw new IllegalArgumentException(
                  s"Expected type variable at the left of =>>:, got $other"
                )

}

sealed trait SIRVarStorage

object SIRVarStorage {
    case object Data extends SIRVarStorage
    case object ScottEncoding extends SIRVarStorage
    case object Cases extends SIRVarStorage

    val DEFAULT = ScottEncoding
}

object SIRType {

    sealed trait Primitive extends SIRType {
        def uplcType: DefaultUni
        override def showDebug: String = show
    }

    case object ByteString extends Primitive {
        override def uplcType: DefaultUni = DefaultUni.ByteString
        override def show: String = "ByteString"
    }
    given ByteString.type = ByteString

    case object Integer extends Primitive {
        override def uplcType: DefaultUni = DefaultUni.Integer
        override def show: String = "Int"
    }
    given Integer.type = Integer

    case object String extends Primitive {
        override def uplcType: DefaultUni = DefaultUni.String
        override def show: String = "String"
    }
    given String.type = String

    case object Boolean extends Primitive {
        override def uplcType: DefaultUni = DefaultUni.Bool
        override def show: String = "Boolean"
    }
    given Boolean.type = Boolean
    case object Unit extends Primitive {
        override def uplcType: DefaultUni = DefaultUni.Unit
        override def show: String = "Unit"
    }

    // sealed trait MappedBuiltin[T] extends Lifted[T] with ULPCMapped

    object Data {
        val name = "scalus.builtin.Data"

        object Constr {
            val name = "scalus.builtin.Data$.Constr"

            def buildConstrDecl(dataType: SIRType): ConstrDecl = {
                ConstrDecl(
                  name,
                  scala.List(
                    TypeBinding("constr", Integer),
                    TypeBinding("args", SIRType.List(dataType))
                  ),
                  scala.Nil,
                  scala.Nil,
                  AnnotationsDecl.empty
                )
            }
        }

        object Map {
            val name = "scalus.builtin.Data$.Map"

            def buildConstrDecl(dataType: SIRType): ConstrDecl = {
                ConstrDecl(
                  name,
                  scala.List(
                    TypeBinding("values", SIRType.List(BuiltinPair(dataType, dataType)))
                  ),
                  scala.Nil,
                  scala.Nil,
                  AnnotationsDecl.empty
                )
            }
        }

        object List {
            val name = "scalus.builtin.Data$.List"

            def buildConstrDecl(dataType: SIRType): ConstrDecl = {
                ConstrDecl(
                  name,
                  scala.List(TypeBinding("values", SIRType.List(dataType))),
                  scala.Nil,
                  scala.Nil,
                  AnnotationsDecl.empty
                )
            }
        }

        object I {
            val name = "scalus.builtin.Data$.I"

            val constrDecl: ConstrDecl = {
                ConstrDecl(
                  name,
                  scala.List(TypeBinding("value", Integer)),
                  scala.Nil,
                  scala.Nil,
                  AnnotationsDecl.empty
                )
            }
        }

        object B {
            val name = "scalus.builtin.Data$.B"

            val constrDecl: ConstrDecl = {
                ConstrDecl(
                  name,
                  scala.List(TypeBinding("value", ByteString)),
                  scala.Nil,
                  scala.Nil,
                  AnnotationsDecl.empty
                )
            }
        }

        lazy val dataDecl: DataDecl = {
            val proxy = new TypeProxy(null)
            val retval = DataDecl(
              name,
              scala.List(
                Constr.buildConstrDecl(proxy),
                Map.buildConstrDecl(proxy),
                List.buildConstrDecl(proxy),
                I.constrDecl,
                B.constrDecl
              ),
              scala.Nil,
              AnnotationsDecl.empty
            )
            proxy.ref = SumCaseClass(retval, scala.Nil)
            retval
        }

        def apply(): SumCaseClass = SumCaseClass(dataDecl, scala.Nil)

        lazy val tp: SIRType = dataDecl.tp

        def unapply(tp: SIRType): Boolean = {
            tp match
                case SumCaseClass(decl, scala.Nil) if decl.name == name => true
                case CaseClass(constrDecl, _, _)
                    if constrDecl.name == Constr.name ||
                        constrDecl.name == Map.name ||
                        constrDecl.name == List.name ||
                        constrDecl.name == I.name ||
                        constrDecl.name == B.name =>
                    true
                case _ => false
        }
    }

    case object BLS12_381_G1_Element extends Primitive {
        override def uplcType: DefaultUni = DefaultUni.BLS12_381_G1_Element
        override def show: String = "BLS12_381_G1_Element"
    }

    case object BLS12_381_G2_Element extends Primitive {

        override def uplcType: DefaultUni = DefaultUni.BLS12_381_G2_Element
        override def show: String = "BLS12_381_G2_Element"
    }

    case object BLS12_381_MlResult extends Primitive {

        override def uplcType: DefaultUni = DefaultUni.BLS12_381_MlResult
        override def show: String = "BLS12_381_MlResult"
    }

    case class CaseClass(
        constrDecl: ConstrDecl,
        typeArgs: scala.List[SIRType],
        parent: Option[SIRType]
    ) extends SIRType {

        if constrDecl.name == "scala.Tuple3" then
            if parent.nonEmpty then
                throw new IllegalArgumentException(
                  s"Tuple3 cannot have parent, got $parent"
                )

        // Debug check: enum case classes should have a parent
        if constrDecl.name.contains(
              "scalus.testing.regression.cosmex20251107.minimized.Action$.Close"
            )
        then
            if parent.isEmpty then
                val stackTrace = new Exception().getStackTrace.take(10).mkString("\n  ")
                throw new RuntimeException(
                  s"[DEBUG] Enum case class ${constrDecl.name} was created without parent!\nStack trace:\n  $stackTrace"
                )

        override def show: String =
            if typeArgs.isEmpty then constrDecl.name
            else s"${constrDecl.name}[${typeArgs.map(_.show).mkString(", ")}]"

        override def showDebug: String =
            if typeArgs.isEmpty then constrDecl.name
            else s"${constrDecl.name}[${typeArgs.map(_.showDebug).mkString(", ")}]"

    }

    case class SumCaseClass(decl: DataDecl, typeArgs: scala.List[SIRType]) extends SIRType {

        override def show: String =
            if typeArgs.isEmpty then decl.name
            else s"${decl.name}[${typeArgs.map(_.show).mkString(", ")}]"

        override def showDebug: String =
            if typeArgs.isEmpty then decl.name
            else s"${decl.name}[${typeArgs.map(_.showDebug).mkString(", ")}]"

    }

    case class Fun(in: SIRType, out: SIRType) extends SIRType {

        override def show: String = {
            in match {
                case inF: Fun =>
                    s"(${in.show}) -> ${out.show}"
                case SIRType.TypeLambda(params, body: Fun) =>
                    s"(${in.show}) -> ${body.show}"
                case _ =>
                    s"${in.show} -> ${out.show}"
            }

        }

        override def showDebug: String = {
            in match {
                case inF: Fun =>
                    s"(${in.showDebug}) -> ${out.showDebug}"
                case SIRType.TypeLambda(params, body: Fun) =>
                    s"(${in.showDebug}) -> ${body.showDebug}"
                case _ =>
                    s"${in.showDebug} -> ${out.showDebug}"
            }

        }

    }

    /** Type variable have two forms: when id is not set, that means that for each instantiation of
      * type-lambda, a new set of type-variables with fresh id-s are created. when id is set, that
      * means that computations are situated in the process of instantiation of some type-lambda,
      * @param name
      * @param id
      * @param isBuiltin - if true, then this type variable is a type variable of a builtin uplc function, if false, then
      *                  this is Scala type variable, which can be represented in uplc in a special form.
      *                   Note, that builtin type variables can come only from Builtin Functions.
      */
    case class TypeVar(name: String, optId: Option[Long] = None, isBuiltin: Boolean)
        extends SIRType {

        override def show: String = name

        override def showDebug: String = s"${name}#${optId.getOrElse(0L)}"

        def :=>>(body: SIRType): TypeLambda = TypeLambda(scala.List(this), body)

    }

    trait TypeVarGenerationContext {

        def contains(tv: TypeVar): Boolean

        def freshCopy(tv: TypeVar): TypeVar

    }

    /** Type lambda (always curried).
      */
    case class TypeLambda(params: scala.List[TypeVar], body: SIRType) extends SIRType {

        override def show: String = s" [${params.map(_.show).mkString(",")}] =>> ${body.show}"

        override def showDebug: String =
            s" [${params.map(_.showDebug).mkString(",")}] =>> ${body.showDebug}"

    }

    object TypeLambda {
        def apply(param: String, body: TypeVar => SIRType, isBuiltin: Boolean): TypeLambda = {
            val tv = TypeVar(param, None, isBuiltin)
            TypeLambda(scala.List(tv), body(tv))
        }
    }

    object TypeLambda2 {
        def apply(
            param1: String,
            param2: String,
            body: (TypeVar, TypeVar) => SIRType,
            isBuiltin: Boolean
        ): TypeLambda = {
            val tv1 = TypeVar(param1, None, isBuiltin)
            val tv2 = TypeVar(param2, None, isBuiltin)
            TypeLambda(scala.List(tv1, tv2), body(tv1, tv2))
        }
    }

    case object FreeUnificator extends SIRType {
        override def show: String = "*"
        override def showDebug: String = show
    }

    class TypeProxy(
        private var _ref: SIRType | Null,
        private var _freezed: Boolean = false,
        private var callbacks: scala.List[TypeProxy => Unit] = Nil
    ) extends SIRType {

        val createEx = new RuntimeException("type-proxy-created")

        override def hashCode(): Int = {
            if ref == null then 0
            else
                // do not call ref.hashCode because it will be recursively called
                //  TODO: actually this breaks case-class contract
                //    (equal recursive base-classes become unequal)
                //    (maybe pass some hashcode to the constructor)

                // in principle now we can change this to summon[SIRHashCodeInRec[TypeProxy]].newRec(ref)
                //  but let's wait
                java.lang.System.identityHashCode(ref)

        }

        override def show: String = {
            if ref eq null then "Proxy(unresolved)"
            else "Proxy"
        }

        override def showDebug: String = {
            val internal =
                if ref eq null then "null" else java.lang.System.identityHashCode(ref).toString
            s"Proxy($internal)"
        }

        def ref: SIRType | Null = _ref

        def ref_=(value: SIRType | Null): Unit = {
            value match
                case tp: TypeProxy =>
                    throw new IllegalArgumentException(
                      s"TypeProxy cannot be assigned to TypeProxy: $tp, tp.ref=${tp.ref}"
                    )
                case _ =>
            if _ref != null && _freezed then
                throw new IllegalStateException(
                  s"TypeProxy is freezed, cannot change ref from ${_ref} to $value",
                  createEx
                )
            _ref = value
            callbacks.foreach(f => f(this))
        }

        def freeze: Unit = {
            _freezed = true
        }

        def setCallback(f: TypeProxy => Unit): Unit = {
            callbacks = f :: callbacks
        }

    }

    object TypeProxy {

        def apply(ref: SIRType | Null): TypeProxy = {
            ref match
                case TypeProxy(ref1) =>
                    throw new IllegalArgumentException(
                      s"TypeProxy cannot be created from another TypeProxy: $ref1"
                    )
                case _: Primitive =>
                    throw new IllegalArgumentException(
                      s"TypeProxy cannot be created from Primitive: $ref"
                    )
                case _ =>
            val proxy = new TypeProxy(ref)
            ref match {
                case tp: TypeProxy =>
                    proxy.ref = tp.ref
                case _ =>
            }
            proxy
        }

        def unapply(tp: SIRType): Option[SIRType | Null] = tp match {
            case tp: TypeProxy => Some(tp.ref)
            case _             => None
        }

    }

    /** Represents a package or companion object of a case class. Note, that case objects going to
      * case-class.
      * @param name
      */
    case class TypeNonCaseModule(name: String) extends SIRType {
        override def show: String = s"PackageOrEnclosingObject($name)"
        override def showDebug: String = show
    }

    case object TypeNothing extends SIRType {
        override def show: String = "Nothing"
        override def showDebug: String = show
    }

    object List {

        lazy val dataDecl: DataDecl = {
            val proxy = new TypeProxy(null)
            val aInCons = TypeVar("A", Some(1), false)
            val retval = DataDecl(
              "scalus.prelude.List",
              scala.List(NilConstr, Cons.buildConstr(aInCons, proxy)),
              scala.List(TypeVar("A", Some(2), false)),
              AnnotationsDecl.empty
            )
            proxy.ref = SumCaseClass(retval, scala.List(TypeVar("A", Some(1), false)))
            if !checkAllProxiesFilled(retval.tp) then
                throw new IllegalStateException(s"List dataDecl has unfilled proxies: ${retval.tp}")
            retval
        }

        def apply(a: SIRType): SumCaseClass =
            SumCaseClass(dataDecl, scala.List(a))

        def unapply(l: SIRType): Option[SIRType] = l match {
            case SumCaseClass(dataDecl, scala.List(a)) =>
                if dataDecl.name == "scalus.prelude.List" then Some(a)
                else None
            case this.Cons(a) => Some(a)
            case this.Nil     => Some(Unit)
            case _            => None
        }

        object Cons {

            val name = "scalus.prelude.List$.Cons"

            def buildConstr(a: TypeVar, listSum: SIRType): ConstrDecl = {
                ConstrDecl(
                  name,
                  scala.List(TypeBinding("head", a), TypeBinding("tail", listSum)),
                  scala.List(a),
                  scala.List(a),
                  AnnotationsDecl.empty
                )
            }

            // TODO:  remove duplication via cache
            lazy val constr = {
                dataDecl.constructors
                    .find(_.name == "scalus.prelude.List$.Cons")
                    .getOrElse(
                      throw new IllegalStateException("Cons constructor not found in List.dataDecl")
                    )
            }

            def apply(a: SIRType) =
                CaseClass(constr, scala.List(a), Some(List.apply(a)))

            def unapply(x: SIRType): Option[SIRType] = x match {
                case CaseClass(constr, scala.List(a), _) =>
                    if constr.name == "scalus.prelude.List$.Cons" then Some(a)
                    else None
                case _ => None
            }

        }

        // val NilConstr = ConstrDecl("Nil", SIRVarStorage.DEFAULT, scala.Nil, scala.Nil, scala.Nil)
        val NilConstr =
            ConstrDecl(
              "scalus.prelude.List$.Nil",
              scala.Nil,
              scala.Nil,
              scala.List(SIRType.TypeNothing),
              AnnotationsDecl.empty
            )

        val Nil = CaseClass(NilConstr, scala.Nil, Some(List.apply(SIRType.TypeNothing)))

    }

    object BuiltinPair {

        val name = "scalus.builtin.BuiltinPair"

        val constrDecl = {
            val A = TypeVar("A", None, true)
            val B = TypeVar("B", None, true)
            ConstrDecl(
              name,
              scala.List(TypeBinding("fst", A), TypeBinding("snd", B)),
              scala.List(A, B),
              scala.Nil,
              AnnotationsDecl.empty
            )
        }

        def apply(a: SIRType, b: SIRType): SIRType =
            CaseClass(constrDecl, scala.List(a, b), None)

        def unapply(x: SIRType): Option[(SIRType, SIRType)] = x match {
            case CaseClass(`constrDecl`, scala.List(a, b), None) => Some((a, b))
            case _                                               => None
        }

    }

    object Tuple2 {

        val name = "scala.Tuple2"

        val constrDecl = {
            val tuple2Hash = name.hashCode
            val A = TypeVar("A", Some(tuple2Hash), true)
            val B = TypeVar("B", Some(tuple2Hash + 1), true)
            ConstrDecl(
              name,
              scala.List(TypeBinding("_1", A), TypeBinding("_2", B)),
              scala.List(A, B),
              scala.Nil,
              AnnotationsDecl.empty
            )
        }

        def apply(a: SIRType, b: SIRType): SIRType =
            CaseClass(constrDecl, scala.List(a, b), None)

        def unapply(x: SIRType): Option[(SIRType, SIRType)] = x match {
            case CaseClass(`constrDecl`, scala.List(a, b), None) => Some((a, b))
            case _                                               => None
        }

    }

    object Varargs {
        val name = "scalus.prelude.Varargs"

        val constrDecl = {
            val hash = name.hashCode
            val a = TypeVar("A", Some(hash), true)
            ConstrDecl(
              name,
              scala.List(TypeBinding("list", SIRType.List(a))),
              scala.List(a),
              scala.Nil,
              AnnotationsDecl.empty
            )
        }

        val dataDecl = {
            DataDecl(name, scala.List(constrDecl), scala.Nil, AnnotationsDecl.empty)
        }

        def apply(a: SIRType): SIRType =
            CaseClass(constrDecl, scala.List(a), None)

        def unapply(x: SIRType): Option[SIRType] = {
            x match
                case CaseClass(constrDecl, scala.List(a), scala.None) if constrDecl.name == name =>
                    Some(a)
                case _ => scala.None
        }

    }

    object BuiltinArray {
        val name = "scalus.builtin.BuiltinArray"

        val constrDecl = {
            val A = TypeVar("A", None, true)
            ConstrDecl(
              name,
              scala.List(TypeBinding("A", A)),
              scala.List(A),
              scala.Nil,
              AnnotationsDecl.empty
            )
        }

        def apply(a: SIRType): SIRType =
            CaseClass(constrDecl, scala.List(a), None)

        def unapply(x: SIRType): Option[SIRType] = x match {
            case CaseClass(`constrDecl`, scala.List(a), None) => Some(a)
            case _                                            => None
        }

    }

    object BuiltinList {
        val name = "scalus.builtin.BuiltinList"

        object Nil {
            val name = "scalus.builtin.BuiltinList$.Nil"
            lazy val constrDecl: ConstrDecl = {
                ConstrDecl(
                  name,
                  scala.Nil,
                  scala.Nil,
                  scala.List(SIRType.TypeNothing),
                  AnnotationsDecl.empty
                )
            }
            def tp = BuiltinList.dataDecl.constrType(name)
            def apply(): SIRType = tp
        }

        object Cons {
            val name = "scalus.builtin.BuiltinList$.Cons"

            def buildConstrDecl(tv: TypeVar, listType: SIRType) = {
                ConstrDecl(
                  name,
                  scala.List(TypeBinding("h", tv), TypeBinding("tl", listType)),
                  scala.List(tv),
                  scala.List(tv),
                  AnnotationsDecl.empty
                )
            }

        }

        lazy val dataDecl: DataDecl = {
            val consProxy = new TypeProxy(null)
            val a = TypeVar("A", Some(2), false)
            val consA: SIRType.TypeVar = TypeVar("A", Some(1), false)
            val retval = DataDecl(
              name,
              scala.List(
                BuiltinList.Nil.constrDecl,
                BuiltinList.Cons.buildConstrDecl(consA, consProxy)
              ),
              scala.List(a),
              AnnotationsDecl.empty
            )
            consProxy.ref = typeApply(retval.tp, scala.List(consA))
            retval
        }

        def apply(elemType: SIRType) =
            SumCaseClass(dataDecl, scala.List(elemType))

        def unapply(tp: SIRType): Option[SIRType] = {
            tp match
                case SumCaseClass(dataDecl, scala.List(elemType)) if dataDecl.name == name =>
                    Some(elemType)
                case CaseClass(constrDecl, typeArgs, optParent) =>
                    if constrDecl.name == Cons.name then Some(typeArgs.head)
                    else if constrDecl.name == Nil.name then Some(SIRType.TypeNothing)
                    else None
                case _ => None
        }

    }

    /** Check if the type is a function or a polymorphic function without unfolding type arguments.
      * (i.e. isPolyFunOrFun(SIRType.Fun(SIRType.Unit, SIRType.Unit)) == true,
      * isPolyFunOrFun(SIRType.TypeVar("A")) == false even if A is a needed function)
      * @param tp - type to check
      * @param trace - trace for recursive entries
      * @return
      */
    @scala.annotation.tailrec
    def isPolyFunOrFun(
        tp: SIRType,
        trace: java.util.IdentityHashMap[SIRType, SIRType] = new util.IdentityHashMap()
    ): Boolean = {
        if trace.containsKey(tp) then false
        else
            trace.put(tp, tp)
            tp match {
                case SIRType.Fun(_, _)           => true
                case SIRType.TypeLambda(_, body) => isPolyFunOrFun(body, trace)
                case SIRType.TypeProxy(ref) =>
                    if ref == null then false
                    else isPolyFunOrFun(ref, trace)
                case _ => false
            }
    }

    def collectPolyOrFun(tp: SIRType): Option[(scala.List[TypeVar], SIRType, SIRType)] = {

        @tailrec
        def collect(
            tp: SIRType,
            acc: scala.List[TypeVar]
        ): Option[(scala.List[TypeVar], SIRType, SIRType)] = {
            tp match {
                case SIRType.Fun(in, out) =>
                    Some((acc, in, out))
                case SIRType.TypeLambda(params, body) =>
                    collect(body, acc ++ params)
                case SIRType.TypeProxy(ref) =>
                    collect(ref, acc)
                case _ =>
                    None
            }
        }

        collect(tp, scala.List.empty)

    }

    @tailrec
    def isSum(tp: SIRType): Boolean = {
        tp match {
            case SIRType.SumCaseClass(_, _)  => true
            case SIRType.TypeLambda(_, body) => isSum(body)
            case SIRType.TypeProxy(ref) =>
                if ref == null then false
                else isSum(ref)
            case _ => false
        }
    }

    def collectSumCaseClass(tp: SIRType): Option[(scala.List[SIRType.TypeVar], SumCaseClass)] = {

        @tailrec
        def go(
            tp: SIRType,
            acc: scala.List[SIRType.TypeVar]
        ): Option[(scala.List[SIRType.TypeVar], SumCaseClass)] = {
            tp match {
                case cc: SumCaseClass                 => Some((acc, cc))
                case SIRType.TypeLambda(params, body) => go(body, acc ++ params)
                case SIRType.TypeProxy(ref) =>
                    if ref == null then None
                    else go(ref, acc)
                case _ => None
            }
        }

        go(tp, scala.List.empty)

    }

    @tailrec
    def isProd(tp: SIRType): Boolean = {
        tp match {
            case SIRType.CaseClass(_, _, _)  => true
            case SIRType.TypeLambda(_, body) => isProd(body)
            case SIRType.TypeProxy(ref) =>
                if ref == null then false
                else isProd(ref)
            case _ => false
        }
    }

    /** Return CaseClass, leave free type-vars in type-lambda
      * @param tp
      * @return
      */
    def collectProdCaseClass(tp: SIRType): Option[(scala.List[SIRType.TypeVar], CaseClass)] = {

        @tailrec
        def go(
            tp: SIRType,
            acc: scala.List[SIRType.TypeVar]
        ): Option[(scala.List[SIRType.TypeVar], CaseClass)] = {
            tp match {
                case cc: CaseClass                    => Some((acc, cc))
                case SIRType.TypeLambda(params, body) => go(body, acc ++ params)
                case SIRType.TypeProxy(ref) =>
                    if ref == null then None
                    else go(ref, acc)
                case _ => None
            }
        }

        go(tp, scala.List.empty)

    }

    /** Check if the type is a function or a polymorphic function  from unit without unfolding type arguments.
      * (i.e. isPolyFunOrFunUnit(SIRType.Fun(SIRType.Unit, SIRType.Unit)) == true,
      *       isPolyFunOrFunInut(SIRType.Fun(SIRType.Integer, SIRType.Unit)) == false,
      * isPolyFunOrFunUnt(SIRType.TypeVar("A")) == false even if A is a needed function)
      *
      * @param tp    - type to check
      * @param trace - trace for recursive entries
      * @return
      */
    @tailrec
    def isPolyFunOrFunUnit(
        tp: SIRType,
        trace: java.util.IdentityHashMap[SIRType, SIRType] = new util.IdentityHashMap()
    ): Boolean = {
        if trace.containsKey(tp) then false
        else
            trace.put(tp, tp)
            tp match
                case SIRType.Fun(SIRType.Unit, _) => true
                case SIRType.TypeLambda(_, body)  => isPolyFunOrFunUnit(body, trace)
                case SIRType.TypeProxy(ref) =>
                    if ref == null then false
                    else isPolyFunOrFunUnit(ref, trace)
                case _ => false
    }

    case class TypeApplyException(msg: String, cause: Throwable | Null = null)
        extends RuntimeException(msg, cause)

    @tailrec
    def typeApply(tpl: SIRType, args: List[SIRType]): SIRType =
        if args.isEmpty then tpl
        else
            tpl match
                case TypeLambda(params, body) =>
                    if params.length != args.length then
                        throw TypeApplyException(
                          s"length of type-lambda parameter list is differ then args list. tpl=${tpl.show}, args=${args
                                  .map(_.show)}"
                        )
                    else
                        val env = params.zip(args).toMap
                        substitute(body, env, Map.empty)
                case TypeProxy(ref) =>
                    typeApply(ref, args)
                case _ =>
                    throw TypeApplyException(
                      s"type-lambda should be the first argument of typeApply,  we have ${tpl.show}"
                    )

    case class CaclulateApplyTypeException(msg: String, cause: Throwable | Null = null)
        extends RuntimeException(msg, cause)

    def calculateApplyType(
        f: SIRType,
        arg: SIRType,
        env: Map[TypeVar, SIRType],
        debug: Boolean = false
    ): SIRType = {
        val maxTypeVarIdInEnv = env.keys
            .map(_.optId.getOrElse(0L))
            .maxOption
            .getOrElse(0L)
        calculateApplyTypeWithUnifyEnv(
          f,
          arg,
          new CalculateApplyTypeContext(
            SIRUnify.Env.empty.copy(filledTypes = env),
            createMinimalTypeVarGenerationContext(maxTypeVarIdInEnv, scala.List(f, arg)),
            reportUngrounded = debug,
            debug = debug
          )
        )
    }

    class CalculateApplyTypeContext(
        var env: SIRUnify.Env,
        val tvGen: SetBasedTypeVarGenerationContext,
        var tvSubst: Map[SIRType.TypeVar, SIRType.TypeVar] = Map.empty,
        val tvAlreadyUnique: Boolean = false,
        val reportUngrounded: Boolean = false,
        val debug: Boolean = false
    )

    /** Calculate the type of the application of function f to argument arg.
      *
      * @param f
      * @param arg
      * @param ctx
      * @return
      */
    @tailrec
    def calculateApplyTypeWithUnifyEnv(
        f: SIRType,
        arg: SIRType,
        ctx: CalculateApplyTypeContext
    ): SIRType = {

        def freshTypeLambda(tl: TypeLambda): (SIRType, scala.List[TypeVar]) = {
            val newMappingPairs = tl.params.map(tv => (tv, ctx.tvGen.freshCopy(tv)))
            val newMapping = newMappingPairs.toMap
            ctx.tvSubst = ctx.tvSubst ++ newMapping
            val renamingContext = RenamingTypeVars.makeContext(newMapping, ctx.tvGen)
            (RenamingTypeVars.inType(tl.body, renamingContext), newMappingPairs.map(_._2).toList)
        }

        def unrollTypeLambda(
            x: SIRType,
            freshCondition: => Boolean
        ): (SIRType, scala.List[TypeVar]) =
            x match {
                case tl: SIRType.TypeLambda =>
                    if freshCondition then freshTypeLambda(tl)
                    else (tl.body, tl.params)
                case TypeProxy(ref) =>
                    unrollTypeLambda(ref, freshCondition)
                case _ =>
                    (x, scala.List.empty)
            }

        val argOverlapp = ctx.tvGen.importSetFromType(arg)
        val fOverlapp = ctx.tvGen.importSetFromType(f)

        val (uniqueArg, argTps) = unrollTypeLambda(arg, argOverlapp)
        val (uniqueF, fTps) = unrollTypeLambda(f, fOverlapp)

        if ctx.debug then
            println(
              s"calculateApplyTypeWithUnifyEnv: f=${uniqueF.show}, arg=${uniqueArg.show}, " +
                  s"argTps=${argTps.map(_.show).mkString(",")}, fTps=${fTps.map(_.show).mkString(",")}"
            )

        uniqueF match
            case SIRType.Fun(in, out) =>
                val (uniqueIn, inTps) = unrollTypeLambda(in, true)
                val (uniqueOut, outTps) = unrollTypeLambda(out, true)
                SIRUnify.topLevelUnifyType(
                  uniqueIn,
                  uniqueArg,
                  ctx.env.withUpcasting.setDebug(ctx.debug)
                ) match
                    case SIRUnify.UnificationSuccess(env, unificator) =>
                        ctx.env = env
                        val argTpsRest = argTps.filterNot(ctx.env.filledTypes.contains)
                        val inTvSubst = inTps.flatMap { tv =>
                            ctx.env.eqTypes.get(tv) match {
                                case Some(eqTypes) =>
                                    eqTypes.find(etv => argTps.contains(etv)).map((tv, _))
                                case None => None
                            }
                        }.toMap
                        val inTpsRest = inTps.filterNot(x =>
                            inTvSubst.contains(x) || env.filledTypes.contains(x)
                        )
                        val fTvSubst = fTps.flatMap { tv =>
                            ctx.env.eqTypes.get(tv) match {
                                case Some(eqTypes) =>
                                    eqTypes
                                        .find(etv => argTps.contains(etv))
                                        .orElse(eqTypes.find(etv => inTps.contains(etv)))
                                        .map((tv, _))
                                case None => None
                            }
                        }
                        val fTvRest =
                            fTps.filterNot(x => fTvSubst.contains(x) || env.filledTypes.contains(x))
                        val resBody = substitute(
                          uniqueOut,
                          env.filledTypes ++ inTvSubst ++ fTvSubst,
                          Map.empty
                        )
                        val resParams0 = argTpsRest ++ inTpsRest ++ fTvRest ++ outTps
                        val (ground, unground) = partitionGround(resParams0, resBody)
                        if unground.nonEmpty && ctx.reportUngrounded then
                            println(
                              "type parameters are not ground in the result type: " +
                                  s"${unground.map(_.show).mkString(", ")} in ${resBody.show}"
                            )
                        val resParams = ground
                        if resParams.isEmpty then resBody
                        else SIRType.TypeLambda(resParams, resBody)
                    case SIRUnify.UnificationFailure(path, left, right) =>
                        if ctx.debug then {
                            println(
                              s"Failed unification ${uniqueIn.show} and ${uniqueArg.show}, path= ${path.mkString(".")}"
                            )
                            println(s"left = ${left}")
                            println(s"right= ${right}")
                        }
                        throw CaclulateApplyTypeException(
                          s"Cannot calculate apply type for ${f.show} to ${arg.show}, function type does not match argument type.\n" +
                              s"Failed unification ${uniqueIn.show} and ${uniqueArg.show}, path= ${path.mkString(".")}, "
                        )
            case tv: TypeVar =>
                ctx.env.filledTypes.get(tv) match
                    case Some(filledType) =>
                        calculateApplyTypeWithUnifyEnv(filledType, arg, ctx)
                    case None =>
                        throw CaclulateApplyTypeException(
                          s"Cannot calculate apply type for $f to $arg, type variable $tv is not filled in the environment"
                        )
            case _ =>
                throw CaclulateApplyTypeException(
                  s"Cannot calculate apply type for $f to $arg, expected a function type, got ${f.show}"
                )

    }

    def substitute(
        rType: SIRType,
        env: Map[SIRType.TypeVar, SIRType],
        proxyEnv: Map[SIRType.TypeProxy, SIRType.TypeProxy]
    ): SIRType = {
        rType match
            case tv: TypeVar =>
                env.get(tv) match
                    case Some(t) => t
                    case None    => tv
            case TypeLambda(params, body) =>
                val intersected = params.filter(tv => env.contains(tv))
                if intersected.isEmpty then TypeLambda(params, substitute(body, env, proxyEnv))
                else TypeLambda(params, substitute(body, env -- intersected, proxyEnv))
            case CaseClass(constrDecl, typeArgs, optParent) =>
                CaseClass(
                  constrDecl,
                  typeArgs.map(substitute(_, env, proxyEnv)),
                  optParent.map(c => substitute(c, env, proxyEnv))
                )
            case SumCaseClass(decl, typeArgs) =>
                SumCaseClass(decl, typeArgs.map(substitute(_, env, proxyEnv)))
            case Fun(in, out) =>
                Fun(substitute(in, env, proxyEnv), substitute(out, env, proxyEnv))
            case tp: TypeProxy =>
                proxyEnv.get(tp) match
                    case Some(t) => t
                    case None =>
                        val newProxy = new TypeProxy(null)
                        newProxy.ref = substitute(tp.ref, env, proxyEnv.updated(tp, newProxy))
                        newProxy
            case other =>
                other
    }

    def fromDefaultUni(uplcType: DefaultUni): SIRType = {
        uplcType match
            case DefaultUni.ByteString           => ByteString
            case DefaultUni.Integer              => Integer
            case DefaultUni.String               => String
            case DefaultUni.Bool                 => Boolean
            case DefaultUni.Unit                 => Unit
            case DefaultUni.Data                 => Data.tp
            case DefaultUni.BLS12_381_G1_Element => BLS12_381_G1_Element
            case DefaultUni.BLS12_381_G2_Element => BLS12_381_G2_Element
            case DefaultUni.BLS12_381_MlResult   => BLS12_381_MlResult
            case DefaultUni.ProtoList =>
                val a = TypeVar("A", Some(DefaultUni.ProtoList.hashCode()), true)
                TypeLambda(scala.List(a), SumCaseClass(BuiltinList.dataDecl, scala.List(a)))
            case DefaultUni.ProtoPair =>
                val a = TypeVar("A", Some(DefaultUni.ProtoPair.hashCode()), true)
                val b = TypeVar("B", Some(DefaultUni.ProtoPair.hashCode() + 1), true)
                TypeLambda(scala.List(a, b), BuiltinPair(a, b))
            case DefaultUni.ProtoArray =>
                val a = TypeVar("A", Some(DefaultUni.ProtoArray.hashCode()), true)
                TypeLambda(scala.List(a), BuiltinArray(a))
            case DefaultUni.Apply(f, arg) =>
                f match
                    case DefaultUni.ProtoList =>
                        BuiltinList(fromDefaultUni(arg))
                    case DefaultUni.ProtoArray =>
                        BuiltinArray(fromDefaultUni(arg))
                    case DefaultUni.Apply(DefaultUni.ProtoPair, a) =>
                        BuiltinPair(fromDefaultUni(a), fromDefaultUni(arg))
                    case DefaultUni.ProtoPair =>
                        val a = TypeVar("A", Some(DefaultUni.ProtoPair.hashCode()), true)
                        TypeLambda(scala.List(a), BuiltinPair(a, fromDefaultUni(arg)))
                    case _ =>
                        SIRType.Fun(fromDefaultUni(f), fromDefaultUni(arg))
    }

    def fromUplcTypeScheme(uplcTypeSchema: UplcTypeScheme): SIRType = {
        uplcTypeSchema match
            case UplcTypeScheme.Type(argType) => fromDefaultUni(argType)
            case UplcTypeScheme.Arrow(argType, resType) =>
                Fun(fromUplcTypeScheme(argType), fromUplcTypeScheme(resType))
            case UplcTypeScheme.All(typeVar, body) =>
                TypeLambda(scala.List(TypeVar(typeVar, None, true)), fromUplcTypeScheme(body))
            case UplcTypeScheme.App(f, arg) =>
                calculateApplyType(fromUplcTypeScheme(f), fromUplcTypeScheme(arg), Map.empty)
            case UplcTypeScheme.TVar(name) => TypeVar(name, None, true)
    }

    def parentsEqSeq(input: SIRType, parent: SIRType): List[SIRType] = {
        SIRUnify.subtypeSeq(input, parent, SIRUnify.Env.empty)
    }

    def parentsNoEqSeq(input: SIRType, parent: SIRType): List[SIRType] = {
        parentsEqSeq(input, parent) match
            case Nil => Nil
            case x :: xs =>
                if x ~=~ parent then xs
                else x :: xs
    }

    def leastUpperBound(left: SIRType, right: SIRType): SIRType = {
        SIRUnify.topLevelUnifyType(left, right, SIRUnify.Env.empty.withUpcasting) match {
            case SIRUnify.UnificationSuccess(env, res) => res
            case SIRUnify.UnificationFailure(_, _, _) =>
                SIRType.FreeUnificator
        }
    }

    def leastUpperBoundSeq(types: scala.List[SIRType]): SIRType = {
        types match {
            case Nil => SIRType.TypeNothing
            case x :: xs =>
                xs.foldLeft(x) { (acc, tp) =>
                    leastUpperBound(acc, tp)
                }
        }
    }

    @scala.annotation.tailrec
    def retrieveDataDecl(tp: SIRType): Either[String, DataDecl] = {
        tp match {
            case tp: SumCaseClass => Right(tp.decl)
            case TypeProxy(ref) =>
                if ref == null then Left("TypeProxy is not resolved")
                else retrieveDataDecl(ref)
            case TypeLambda(_, body) => retrieveDataDecl(body)
            case _                   => Left(s"Expected SumCaseClass, got $tp")
        }
    }

    @scala.annotation.tailrec
    def retrieveConstrDecl(tp: SIRType): Either[String, ConstrDecl] = {
        tp match {
            case tp: CaseClass => Right(tp.constrDecl)
            case TypeProxy(ref) =>
                if ref == null then Left("TypeProxy is not resolved")
                else retrieveConstrDecl(ref)
            case TypeLambda(_, body) => retrieveConstrDecl(body)
            case _ => Left(s"Expected CaseClass, got ${tp.show} (${tp.getClass.getSimpleName}")
        }
    }

    def collectProd(tp: SIRType): Option[(scala.List[TypeVar], ConstrDecl, scala.List[SIRType])] = {
        tp match {
            case CaseClass(constrDecl, typeArgs, _) =>
                Some(
                  (
                    scala.List.empty,
                    constrDecl,
                    typeArgs
                  )
                )
            case TypeProxy(ref) =>
                if ref == null then None
                else collectProd(ref)
            case TypeLambda(tps, body) =>
                collectProd(body) match
                    case Some((accTps, constrDecl, typeArgs)) =>
                        Some((tps ++ accTps, constrDecl, typeArgs))
                    case None => None
            case _ => None
        }
    }

    @tailrec
    def prodParent(tp: SIRType): Option[SIRType] = {
        tp match {
            case CaseClass(_, _, Some(parent)) => Some(parent)
            case TypeProxy(ref) =>
                if ref == null then None
                else prodParent(ref)
            case TypeLambda(_, body) => prodParent(body)
            case _                   => None
        }
    }

    def collectSum(tp: SIRType): Option[(scala.List[TypeVar], DataDecl, scala.List[SIRType])] = {
        tp match {
            case SumCaseClass(decl, typeArgs) =>
                Some((scala.List.empty, decl, typeArgs))
            case TypeProxy(ref) =>
                if ref == null then None
                else collectSum(ref)
            case TypeLambda(tps, body) =>
                collectSum(body) match
                    case Some((accTps, decl, typeArgs)) =>
                        Some((tps ++ accTps, decl, typeArgs))
                    case None => None
            case _ => None
        }
    }

    def checkAllProxiesFilled(tp: SIRType): Boolean = {
        checkAllProxiesFilledTraced(tp, new util.IdentityHashMap[SIRType, SIRType], Nil)
    }

    def checkAllProxiesFilledTraced(
        tp: SIRType,
        traceloops: java.util.IdentityHashMap[SIRType, SIRType],
        log: List[String]
    ): Boolean = {
        if traceloops.get(tp) != null then true
        else
            traceloops.put(tp, tp)
            tp match
                case tp: TypeProxy =>
                    if tp.ref == null then
                        println("Found null typeproxe, created at: ")
                        tp.createEx.printStackTrace()
                        false
                    else if traceloops.get(tp.ref) != null then
                        checkAllProxiesFilledTraced(tp.ref, traceloops, log)
                    else true
                case Fun(in, out) =>
                    checkAllProxiesFilledTraced(in, traceloops, "in" :: log) &&
                    checkAllProxiesFilledTraced(out, traceloops, "out" :: log)
                case CaseClass(constrDecl, typeArgs, optParent) =>
                    constrDecl.params.forall(x =>
                        checkAllProxiesFilledTraced(
                          x.tp,
                          traceloops,
                          s"${constrDecl.name} param" :: log
                        )
                    ) &&
                    typeArgs.forall(x =>
                        checkAllProxiesFilledTraced(x, traceloops, "typeArgs" :: log)
                    )
                case SumCaseClass(dataDecl: DataDecl, typeArgs) =>
                    checkAllProxiesFilledTraced(
                      dataDecl.tp,
                      traceloops,
                      s"dataDecl ${dataDecl.name}" :: log
                    ) &&
                    dataDecl.constructors.forall { constrDecl =>
                        constrDecl.params.forall(x =>
                            checkAllProxiesFilledTraced(
                              x.tp,
                              traceloops,
                              s"${constrDecl.name}" :: log
                            )
                        )
                    } &&
                    typeArgs.forall(x =>
                        checkAllProxiesFilledTraced(x, traceloops, "typeArgs" :: log)
                    )
                case TypeLambda(params, body) =>
                    checkAllProxiesFilledTraced(body, traceloops, "type-lambda bofy" :: log)
                case _ => true
    }

    /** Unroll TypeProxy to get the actual type it references
      *
      * Follows TypeProxy.ref chain until a non-proxy type is found. Returns the original type if
      * it's not a TypeProxy.
      *
      * @param tp
      *   The type to unroll
      * @return
      *   The unrolled type (non-proxy)
      */
    def unrollTypeProxy(tp: SIRType): SIRType = tp match {
        case proxy: TypeProxy =>
            if proxy.ref == null then tp
            else unrollTypeProxy(proxy.ref)
        case _ => tp
    }

    def syntheticNarrowConstrDeclName(childDataDeclName: String): String = {
        s"z_narrow$$${childDataDeclName}"
    }

    def isSynteticNarrowConstrDeclName(name: String): Boolean = {
        name.startsWith("z_narrow$$")
    }

    class SetBasedTypeVarGenerationContext(
        var typeVars: Set[TypeVar],
        var maxCounter: Long,
    ) extends TypeVarGenerationContext {

        override def contains(tv: TypeVar): Boolean =
            typeVars.contains(tv)

        override def freshCopy(tv: TypeVar): TypeVar = {
            if maxCounter == -1 then
                throw new IllegalStateException(
                  "Cannot create fresh copy of type variable when maxCounter is -1"
                )
            maxCounter += 1
            val freshTypeVar = TypeVar(tv.name, Some(maxCounter), tv.isBuiltin)
            typeVars += freshTypeVar
            freshTypeVar
        }

        def updateMaxCounter(id: Long): Unit = {
            if id > maxCounter then maxCounter = id
        }

        def importSetFromType(tp: SIRType): Boolean = {
            val proxySet = new util.IdentityHashMap[SIRType, SIRType]()

            val prevTypeVars = typeVars
            var foundCollision = false

            def accept(tp: SIRType): Unit = {
                tp match {
                    case TypeLambda(tps, body) =>
                        tps.foreach { tv =>
                            if !contains(tv) then
                                typeVars += tv
                                updateMaxCounter(tv.optId.getOrElse(0L))
                            else if !foundCollision && prevTypeVars.contains(tv) then
                                foundCollision = true
                        }
                        accept(body)
                    case tv: TypeVar =>
                        if !contains(tv) then
                            typeVars += tv
                            updateMaxCounter(tv.optId.getOrElse(0L))
                        else if !foundCollision && prevTypeVars.contains(tv) then
                            foundCollision = true
                    case Fun(in, out) =>
                        accept(in)
                        accept(out)
                    case CaseClass(constrDecl, typeArgs, optParent) =>
                        typeArgs.foreach(accept)
                        optParent.foreach(accept)
                        proxySet.put(tp, tp)
                    case SumCaseClass(dataDecl, typeArgs) =>
                        typeArgs.foreach(accept)
                        proxySet.put(tp, tp)
                    case proxy: TypeProxy =>
                        Option(proxySet.get(proxy.ref)) match {
                            case Some(visited) =>
                            // do nothing, already visited
                            case None =>
                                proxySet.put(proxy.ref, proxy.ref)
                                accept(proxy.ref)
                        }
                    case TypeNonCaseModule(_) =>
                    case FreeUnificator | TypeNothing | Unit | Integer | String | Boolean |
                        ByteString | BLS12_381_G1_Element | BLS12_381_G2_Element |
                        BLS12_381_MlResult =>
                    // do nothing, these types are not type variables
                }
            }

            accept(tp)
            foundCollision
        }

    }

    def createMinimalTypeVarGenerationContext(
        initCounter: Long,
        initTypes: scala.List[SIRType]
    ): SetBasedTypeVarGenerationContext = {

        @tailrec
        def advance(
            acc: SetBasedTypeVarGenerationContext,
            dataDeclNames: Set[String],
            constrDeclNames: Set[String],
            proxiedRefs: util.IdentityHashMap[SIRType, SIRType],
            initTypes: LazyList[SIRType]
        ): SetBasedTypeVarGenerationContext = {
            if initTypes.isEmpty then acc
            else
                initTypes.head match {
                    case TypeVar(name, Some(id), isBuiltin) =>
                        if id > acc.maxCounter then acc.maxCounter = id
                        acc.typeVars += TypeVar(name, Some(id), isBuiltin)
                        acc
                    case TypeLambda(params, body) =>
                        params.foreach { tv =>
                            if !acc.contains(tv) then
                                acc.typeVars += tv
                                if tv.optId.isDefined && tv.optId.get > acc.maxCounter then
                                    acc.maxCounter = tv.optId.get
                        }
                        advance(
                          acc,
                          dataDeclNames,
                          constrDeclNames,
                          proxiedRefs,
                          initTypes.tail.prepended(body)
                        )
                    case CaseClass(constrDecl, typeArgs, optParent) =>
                        if !constrDeclNames.contains(constrDecl.name) then {
                            val nConstrDeclNames = constrDeclNames + constrDecl.name
                            advance(
                              acc,
                              dataDeclNames,
                              nConstrDeclNames,
                              proxiedRefs,
                              LazyList[SIRType](typeArgs*) ++
                                  optParent.to(LazyList) ++
                                  constrDecl.typeParams ++
                                  constrDecl.params.map(_.tp).to(LazyList) ++ initTypes.tail
                            )
                        } else acc
                    case SumCaseClass(dataDecl, typeArgs) =>
                        if !dataDeclNames.contains(dataDecl.name) then {
                            val nDataDeclNames = dataDeclNames + dataDecl.name
                            val nConstrDeclNames =
                                dataDecl.constructors.map(_.name).toSet ++ constrDeclNames
                            advance(
                              acc,
                              nDataDeclNames,
                              nConstrDeclNames,
                              proxiedRefs,
                              typeArgs.to(LazyList) ++ dataDecl.typeParams ++
                                  dataDecl.constructors
                                      .flatMap { constrDecl =>
                                          constrDecl.params.map(_.tp) ++ constrDecl.typeParams
                                      }
                                      .to(LazyList) ++ initTypes.tail
                            )
                        } else acc
                    case Fun(in, out) =>
                        advance(
                          acc,
                          dataDeclNames,
                          constrDeclNames,
                          proxiedRefs,
                          LazyList(in) ++ LazyList(out) ++ initTypes.tail
                        )
                    case TypeProxy(ref) =>
                        Option(proxiedRefs.get(ref)) match
                            case Some(visited) => acc
                            case None =>
                                proxiedRefs.put(ref, ref)
                                advance(
                                  acc,
                                  dataDeclNames,
                                  constrDeclNames,
                                  proxiedRefs,
                                  LazyList(ref) ++ initTypes.tail
                                )
                    case _ =>
                        advance(acc, dataDeclNames, constrDeclNames, proxiedRefs, initTypes.tail)
                }

        }

        advance(
          new SetBasedTypeVarGenerationContext(Set.empty, initCounter),
          Set.empty,
          Set.empty,
          new util.IdentityHashMap[SIRType, SIRType](),
          initTypes.to(LazyList)
        )

    }

    def isGround(tv: TypeVar, tp: SIRType): Boolean = {
        var found = false
        val typeProxies = new util.IdentityHashMap[SIRType, SIRType]()
        val stack = scala.collection.mutable.Stack[SIRType](tp)

        @scala.annotation.tailrec
        def advance(tp: SIRType): Unit = {
            if !found then
                tp match {
                    case tv1: SIRType.TypeVar =>
                        if tv1 == tv then found = true
                    case TypeLambda(params, body) =>
                        if !params.contains(tv) then advance(body)
                    case TypeProxy(ref) =>
                        Option(typeProxies.get(ref)) match {
                            case Some(visited) =>
                            // do nothing, already visited
                            case None =>
                                typeProxies.put(ref, ref)
                                advance(ref)
                        }
                    case Fun(in, out) =>
                        stack.push(out)
                        advance(in)
                    case CaseClass(constrDecl, typeArgs, optParent) =>
                        typeArgs.foreach { arg =>
                            stack.push(arg)
                        }
                        optParent.foreach { parent =>
                            stack.push(parent)
                        }
                    case SumCaseClass(dataDecl, typeArgs) =>
                        typeArgs.foreach { arg =>
                            stack.push(arg)
                        }
                    case _ =>
                }
        }

        stack.push(tp)
        while stack.nonEmpty && !found do
            val tp = stack.pop()
            advance(tp)
        found
    }

    def partitionGround(tvs: List[TypeVar], tp: SIRType): (List[TypeVar], List[TypeVar]) = {
        var grounded: Set[TypeVar] = Set.empty
        var ungrounded: Set[TypeVar] = tvs.toSet

        val typeProxies = new util.IdentityHashMap[SIRType, SIRType]()

        val stack = scala.collection.mutable.Stack[(SIRType, Set[TypeVar])]()

        @tailrec
        def advance(tp: SIRType, unshadowedSet: Set[TypeVar]): Unit = {
            if false then
                println(
                  s"advance: ${tp.show}, unshadowedSet=${unshadowedSet.map(_.show).mkString(", ")}"
                )
            if unshadowedSet.nonEmpty && ungrounded.nonEmpty then
                tp match
                    case TypeLambda(params, body) =>
                        val unshadowed = unshadowedSet -- params
                        advance(body, unshadowed)
                    case tv: TypeVar =>
                        if unshadowedSet.contains(tv) then
                            if ungrounded.contains(tv) then
                                grounded += tv
                                ungrounded -= tv
                    case TypeProxy(ref) =>
                        Option(typeProxies.get(ref)) match {
                            case Some(visited) =>
                            // do nothing
                            case None =>
                                typeProxies.put(ref, ref)
                                advance(ref, unshadowedSet)
                        }
                    case Fun(in, out) =>
                        stack.push((out, unshadowedSet))
                        advance(in, unshadowedSet)
                    case CaseClass(constrDecl, typeArgs, optParent) =>
                        typeArgs.foreach { arg =>
                            stack.push((arg, unshadowedSet))
                        }
                        optParent.foreach { parent =>
                            stack.push((parent, unshadowedSet))
                        }
                    case SumCaseClass(dataDecl, typeArgs) =>
                        typeArgs.foreach { arg =>
                            stack.push((arg, unshadowedSet))
                        }
                    case other =>
        }

        stack.push((tp, ungrounded))
        while stack.nonEmpty && ungrounded.nonEmpty do
            val (tp, unshadowedSet) = stack.pop()
            advance(tp, unshadowedSet)
        (grounded.toList, ungrounded.toList)

    }

    def isTypeVarsUsedIn(tvs: List[SIRType.TypeVar], tp: SIRType): Boolean = {
        partitionGround(tvs, tp)._1.nonEmpty
    }

}
