package scalus.compiler.sir

import scalus.uplc.Constant as UplcConstant

sealed trait SIRPattern

object SIRPattern {

    case class Var(name: Option[String], tp: Option[SIRType]) extends SIRPattern
    case class ExternalVar(moduleName: Option[String], name: Option[String], tp: Option[SIRType])
        extends SIRPattern
    case class Binding(name: Option[String], tp: Option[SIRType], value: Option[SIR])
    case class Let(binding: Option[Binding], body: Option[SIRPattern]) extends SIRPattern
    case class LamAbs(
        param: Option[Binding],
        term: Option[SIRPattern],
        typeParams: Option[List[SIRType]]
    ) extends SIRPattern
    case class Apply(f: Option[SIRPattern], arg: Option[SIRPattern], tp: Option[SIRType])
        extends SIRPattern
    case class Select(scrutinee: Option[SIRPattern], field: Option[String], tp: Option[SIRType])
        extends SIRPattern
    case class Const(value: Option[UplcConstant], tp: Option[SIRType]) extends SIRPattern
    case class And(left: Option[SIRPattern], right: Option[SIRPattern]) extends SIRPattern
    case class Or(left: Option[SIRPattern], right: Option[SIRPattern]) extends SIRPattern
    case class Not(term: Option[SIRPattern]) extends SIRPattern
    case class IfThenElse(
        cond: Option[SIRPattern],
        thenp: Option[SIRPattern],
        elsep: Option[SIRPattern]
    ) extends SIRPattern
    case class Builtin(name: Option[String], tp: Option[SIRType]) extends SIRPattern
    case class Error(msg: Option[SIRPattern], tp: Option[SIRType]) extends SIRPattern
    case class Constr(
        name: Option[String],
        dataDeclName: Option[String],
        args: Option[List[SIRPattern]],
        tp: Option[SIRType]
    ) extends SIRPattern

    sealed trait CasePattern
    object CasePattern {
        case class Constr(
            constrDeclName: Option[String],
            args: Option[List[Option[String]]],
            tp: Option[List[Option[SIRType]]]
        ) extends CasePattern
        case class Const(value: Option[UplcConstant], tp: Option[SIRType]) extends CasePattern
        case class Wildcard(tp: Option[SIRType]) extends CasePattern
        case class Bind(name: String, pattern: Option[CasePattern])
    }
    case class Case(
        pattern: Option[CasePattern],
        body: Option[SIRPattern]
    )
    case class Match(
        scrutinee: Option[SIRPattern],
        cases: Option[List[Case]],
        tp: Option[SIRType]
    ) extends SIRPattern

    case class Cast(term: Option[SIRPattern], tp: Option[SIRType]) extends SIRPattern

    case class Bind(name: String, pattern: Option[SIRPattern])

}
