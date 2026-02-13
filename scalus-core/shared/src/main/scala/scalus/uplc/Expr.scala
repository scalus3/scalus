package scalus.uplc

import scalus.*
import scalus.uplc.builtin.{ByteString, Data}
import scalus.uplc.Constant.LiftValue
import scalus.utils.Macros

import scala.annotation.targetName
import scala.language.implicitConversions

trait Delayed[+A]
case class Expr[+A](term: Term)

object ExprBuilder:
    import Term.*

    trait Unlift[A]:
        def unlift: Expr[Data => A]

    given Unlift[BigInt] with {
        def unlift = unIData
    }

    given Unlift[ByteString] with {
        def unlift = unBData
    }

    given liftableToExpr[A: LiftValue]: Conversion[A, Expr[A]] = const

    def const[A: LiftValue](a: A): Expr[A] = Expr(Term.Const(summon[LiftValue[A]].lift(a)))
    def vr[A](name: String): Expr[A] = Expr(Term.Var(NamedDeBruijn(name)))
    def app[A, B](f: Expr[A => B], x: Expr[A]): Expr[B] = Expr(Term.Apply(f.term, x.term))
    def lam[A](name: String): [B] => (Expr[A] => Expr[B]) => Expr[A => B] = [B] =>
        (f: Expr[A] => Expr[B]) => Expr(Term.LamAbs(name, f(vr(name)).term))
    inline def lam[A, B](inline f: Expr[A] => Expr[B]): Expr[A => B] = ${ Macros.lamMacro('f) }
    def delay[A](x: Expr[A]): Expr[Delayed[A]] = Expr(Term.Delay(x.term))
    def force[A](x: Expr[Delayed[A]]): Expr[A] = Expr(Term.Force(x.term))
    def error(msg: String): Expr[Delayed[Nothing]] = Expr(
      Term.Delay(Term.Error())
    ) // TODO: add trace
    def err(msg: String): Expr[Nothing] = Expr(Term.Error()) // TODO: add trace
    def let[A, B](expr: Expr[A])(f: Expr[A] => Expr[B]): Expr[B] = lam[A]("let")[B](f)(expr)

    // Z Combinator
    // (lam ff [(lam xx [ff (lam vv [xx xx vv])]) (lam xx [ff (lam vv [xx xx vv])])])
    val ZTerm: Term = λ { ff =>
        val zz = λ(xx => ff $ λ(vv => xx $ xx $ vv))
        zz $ zz
    }
    def Z[A, B]: Expr[((A => B) => A => B) => A => B] = Expr(ZTerm)
    def z[A, B](f: Expr[(A => B) => A => B]): Expr[A => B] =
        Expr(ZTerm $ f.term)

    def rec[A, B](f: Expr[A => B] => Expr[A => B]): Expr[A => B] =
        Z(lam[A => B]("self")(self => f.apply(self)))

    def ifThenElse[A](cond: Expr[Boolean])(t: Expr[Delayed[A]])(
        f: Expr[Delayed[A]]
    ): Expr[Delayed[A]] =
        Expr(Term.Force(Term.Builtin(DefaultFun.IfThenElse)) $ cond.term $ t.term $ f.term)

    def ifThenElse2[A](cond: Expr[Boolean])(t: Expr[A])(
        f: Expr[A]
    ): Expr[A] = !ifThenElse(cond)(delay(t))(delay(f))
    val unConstrData: Expr[Data => (BigInt, List[Data])] = Expr(
      Term.Builtin(DefaultFun.UnConstrData)
    )
    val unListData: Expr[Data => List[Data]] = Expr(Term.Builtin(DefaultFun.UnListData))
    val unBData: Expr[Data => ByteString] = Expr(Term.Builtin(DefaultFun.UnBData))
    val unIData: Expr[Data => BigInt] = Expr(Term.Builtin(DefaultFun.UnIData))

    def fstPair[A, B](x: Expr[(A, B)]): Expr[A] = Expr(
      Term.Apply(Term.Force(Term.Force(Term.Builtin(DefaultFun.FstPair))), x.term)
    )
    def sndPair[A, B](x: Expr[(A, B)]): Expr[B] = Expr(
      Term.Apply(Term.Force(Term.Force(Term.Builtin(DefaultFun.SndPair))), x.term)
    )

    val headList: Expr[List[Data] => Data] = Expr(
      Term.Force(Term.Builtin(DefaultFun.HeadList))
    )

    val tailList: Expr[List[Data] => List[Data]] = Expr(
      Term.Force(Term.Builtin(DefaultFun.TailList))
    )

    val nullList: Expr[List[Data] => Boolean] = Expr(
      Term.Force(Term.Builtin(DefaultFun.NullList))
    )

    def chooseList[A, B](ls: Expr[List[A]])(e: Expr[B])(ne: Expr[B]): Expr[B] = Expr(
      Term.Force(Term.Force(Term.Builtin(DefaultFun.ChooseList))) $ ls.term $ e.term $ ne.term
    )

    val addInteger: Expr[BigInt => BigInt => BigInt] = Expr(
      Term.Builtin(DefaultFun.AddInteger)
    )

    val substractInteger: Expr[BigInt => BigInt => BigInt] = Expr(
      Term.Builtin(DefaultFun.SubtractInteger)
    )

    val lessThanEqualsInteger: Expr[BigInt => BigInt => Boolean] = Expr(
      Term.Builtin(DefaultFun.LessThanEqualsInteger)
    )

    val lessThanInteger: Expr[BigInt => BigInt => Boolean] = Expr(
      Term.Builtin(DefaultFun.LessThanInteger)
    )

    val equalsInteger: Expr[BigInt => BigInt => Boolean] = Expr(
      Term.Builtin(DefaultFun.EqualsInteger)
    )

    def equalsByteString(lhs: Expr[ByteString])(rhs: Expr[ByteString]): Expr[Boolean] = Expr(
      Term.Builtin(DefaultFun.EqualsByteString) $ lhs.term $ rhs.term
    )

    inline def fieldAsData[A: Data.ToData](inline expr: A => Any): Expr[Data] => Expr[Data] = ${
        Macros.fieldAsExprDataMacro('expr)
    }

    extension (lhs: Expr[BigInt])
        @targetName("plus")
        def |+|(rhs: Expr[BigInt]): Expr[BigInt] = addInteger(lhs)(rhs)
        def |-|(rhs: Expr[BigInt]): Expr[BigInt] = substractInteger(lhs)(rhs)
        def ===(rhs: Expr[BigInt]): Expr[Boolean] = equalsInteger(lhs)(rhs)
        def <=(rhs: Expr[BigInt]): Expr[Boolean] = lessThanEqualsInteger(lhs)(rhs)
        def <(rhs: Expr[BigInt]): Expr[Boolean] = lessThanInteger(lhs)(rhs)

    extension (lhs: Expr[ByteString])
        infix def =*=(rhs: Expr[ByteString]): Expr[Boolean] = equalsByteString(lhs)(rhs)

    extension [A, B](lhs: Expr[A => B]) def apply(rhs: Expr[A]): Expr[B] = app(lhs, rhs)
    extension [B, C](lhs: Expr[B => C])
        def compose[A](rhs: Expr[A => B]): Expr[A => C] = lam(a => app(lhs, app(rhs, a)))
    extension [A](lhs: Expr[A]) def unary_~ : Expr[Delayed[A]] = delay(lhs)
    extension [A](lhs: Expr[Delayed[A]]) def unary_! : Expr[A] = force(lhs)
