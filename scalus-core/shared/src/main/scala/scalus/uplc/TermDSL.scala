package scalus.uplc

import scalus.*
import scalus.uplc.builtin.Data

object TermDSL:
    given Conversion[DefaultFun, Term] with
        def apply(bn: DefaultFun): Term = Term.Builtin(bn)

    given constantAsTerm[A: Constant.LiftValue]: Conversion[A, Term] with
        def apply(c: A): Term = Term.Const(summon[Constant.LiftValue[A]].lift(c))

    given Conversion[Constant, Term] with
        def apply(c: Constant): Term = Term.Const(c)

    given constantAsData[A: Data.ToData]: Conversion[A, Data] with
        def apply(c: A): Data = summon[Data.ToData[A]](c)
