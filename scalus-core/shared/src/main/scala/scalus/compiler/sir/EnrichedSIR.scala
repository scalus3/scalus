package scalus.compiler.sir

import scalus.compiler.sir.{AnnotationsDecl, DataDecl, SIR, SIRType}
import scalus.uplc.{Constant, DefaultFun}

/** EnrichedSIR[P,T] mirrors the structure of SIR trait but allows parameterization with pattern
  * type P and info type T.
  *
  * For each SIR case class Xxx, there is a sealed trait Xxx[P,T] with two cases:
  *   - XxxPattern[P,T](pattern: P, info: T) representing a pattern node
  *   - XxxInfo[P,T] with the same parameters as origin SIR, but using appropriate traits from
  *     EnrichedSIR[P,T] and with additional info: T
  */
object EnrichedSIR:

    sealed trait Base[P, T]:
        def info: T

    sealed trait AnnotatedBase[P, T] extends Base[P, T]

    // Var
    sealed trait Var[P, T] extends AnnotatedBase[P, T]

    object Var:
        case class VarPattern[P, T](pattern: P, info: T) extends Var[P, T]
        case class VarInfo[P, T](name: String, tp: SIRType, anns: AnnotationsDecl, info: T)
            extends Var[P, T]

    // ExternalVar
    sealed trait ExternalVar[P, T] extends AnnotatedBase[P, T]

    object ExternalVar:
        case class ExternalVarPattern[P, T](pattern: P, info: T) extends ExternalVar[P, T]
        case class ExternalVarInfo[P, T](
            moduleName: String,
            name: String,
            tp: SIRType,
            anns: AnnotationsDecl,
            info: T
        ) extends ExternalVar[P, T]

    // Let
    sealed trait Let[P, T] extends AnnotatedBase[P, T]

    object Let:
        case class LetPattern[P, T](pattern: P, info: T) extends Let[P, T]
        case class LetInfo[P, T](
            bindings: List[BindingEnriched[P, T]],
            body: Base[P, T],
            flags: SIR.LetFlags,
            anns: AnnotationsDecl,
            info: T
        ) extends Let[P, T]

    case class BindingEnriched[P, T](name: String, tp: SIRType, value: Base[P, T])

    // LamAbs
    sealed trait LamAbs[P, T] extends AnnotatedBase[P, T]

    object LamAbs:
        case class LamAbsPattern[P, T](pattern: P, info: T) extends LamAbs[P, T]
        case class LamAbsInfo[P, T](
            param: VarEnriched[P, T],
            term: Base[P, T],
            typeParams: List[SIRType.TypeVar],
            anns: AnnotationsDecl,
            info: T
        ) extends LamAbs[P, T]

    case class VarEnriched[P, T](name: String, tp: SIRType)

    // Apply
    sealed trait Apply[P, T] extends AnnotatedBase[P, T]

    object Apply:
        case class ApplyPattern[P, T](pattern: P, info: T) extends Apply[P, T]
        case class ApplyInfo[P, T](
            f: AnnotatedBase[P, T],
            arg: AnnotatedBase[P, T],
            tp: SIRType,
            anns: AnnotationsDecl,
            info: T
        ) extends Apply[P, T]

    // Select
    sealed trait Select[P, T] extends AnnotatedBase[P, T]

    object Select:
        case class SelectPattern[P, T](pattern: P, info: T) extends Select[P, T]
        case class SelectInfo[P, T](
            scrutinee: Base[P, T],
            field: String,
            tp: SIRType,
            anns: AnnotationsDecl,
            info: T
        ) extends Select[P, T]

    // Const
    sealed trait Const[P, T] extends AnnotatedBase[P, T]

    object Const:
        case class ConstPattern[P, T](pattern: P, info: T) extends Const[P, T]
        case class ConstInfo[P, T](
            uplcConst: Constant,
            tp: SIRType,
            anns: AnnotationsDecl,
            info: T
        ) extends Const[P, T]

    // And
    sealed trait And[P, T] extends AnnotatedBase[P, T]

    object And:
        case class AndPattern[P, T](pattern: P, info: T) extends And[P, T]
        case class AndInfo[P, T](
            a: AnnotatedBase[P, T],
            b: AnnotatedBase[P, T],
            anns: AnnotationsDecl,
            info: T
        ) extends And[P, T]

    // Or
    sealed trait Or[P, T] extends AnnotatedBase[P, T]

    object Or:
        case class OrPattern[P, T](pattern: P, info: T) extends Or[P, T]
        case class OrInfo[P, T](
            a: AnnotatedBase[P, T],
            b: AnnotatedBase[P, T],
            anns: AnnotationsDecl,
            info: T
        ) extends Or[P, T]

    // Not
    sealed trait Not[P, T] extends AnnotatedBase[P, T]

    object Not:
        case class NotPattern[P, T](pattern: P, info: T) extends Not[P, T]
        case class NotInfo[P, T](a: AnnotatedBase[P, T], anns: AnnotationsDecl, info: T)
            extends Not[P, T]

    // IfThenElse
    sealed trait IfThenElse[P, T] extends AnnotatedBase[P, T]

    object IfThenElse:
        case class IfThenElsePattern[P, T](pattern: P, info: T) extends IfThenElse[P, T]
        case class IfThenElseInfo[P, T](
            cond: AnnotatedBase[P, T],
            t: AnnotatedBase[P, T],
            f: AnnotatedBase[P, T],
            tp: SIRType,
            anns: AnnotationsDecl,
            info: T
        ) extends IfThenElse[P, T]

    // Builtin
    sealed trait Builtin[P, T] extends AnnotatedBase[P, T]

    object Builtin:
        case class BuiltinPattern[P, T](pattern: P, info: T) extends Builtin[P, T]
        case class BuiltinInfo[P, T](bn: DefaultFun, tp: SIRType, anns: AnnotationsDecl, info: T)
            extends Builtin[P, T]

    // Error
    sealed trait Error[P, T] extends AnnotatedBase[P, T]

    object Error:
        case class ErrorPattern[P, T](pattern: P, info: T) extends Error[P, T]
        case class ErrorInfo[P, T](
            msg: AnnotatedBase[P, T],
            anns: AnnotationsDecl,
            cause: Throwable | Null,
            info: T
        ) extends Error[P, T]

    // Constr
    sealed trait Constr[P, T] extends AnnotatedBase[P, T]

    object Constr:
        case class ConstrPattern[P, T](pattern: P, info: T) extends Constr[P, T]
        case class ConstrInfo[P, T](
            name: String,
            data: DataDecl,
            args: List[Base[P, T]],
            tp: SIRType,
            anns: AnnotationsDecl,
            info: T
        ) extends Constr[P, T]

    // Match
    sealed trait Match[P, T] extends AnnotatedBase[P, T]

    object Match:
        case class MatchPattern[P, T](pattern: P, info: T) extends Match[P, T]
        case class MatchInfo[P, T](
            scrutinee: AnnotatedBase[P, T],
            cases: List[CaseEnriched[P, T]],
            tp: SIRType,
            anns: AnnotationsDecl,
            info: T
        ) extends Match[P, T]

    case class CaseEnriched[P, T](pattern: SIR.Pattern, body: Base[P, T], anns: AnnotationsDecl)

    // Cast
    sealed trait Cast[P, T] extends AnnotatedBase[P, T]

    object Cast:
        case class CastPattern[P, T](pattern: P, info: T) extends Cast[P, T]
        case class CastInfo[P, T](
            term: AnnotatedBase[P, T],
            tp: SIRType,
            anns: AnnotationsDecl,
            info: T
        ) extends Cast[P, T]

    // Decl
    sealed trait Decl[P, T] extends Base[P, T]

    object Decl:
        case class DeclPattern[P, T](pattern: P, info: T) extends Decl[P, T]
        case class DeclInfo[P, T](data: DataDecl, term: Base[P, T], info: T) extends Decl[P, T]

end EnrichedSIR
