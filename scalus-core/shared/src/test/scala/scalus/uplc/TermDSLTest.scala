package scalus.uplc

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.*
import scalus.builtin.ByteString
import scalus.uplc.DefaultUni.asConstant
import scalus.uplc.Term.*
import scalus.uplc.Term.asTerm
import scalus.uplc.TermDSL.given
import scalus.uplc.test.ArbitraryInstances

import scala.language.implicitConversions

class TermDSLTest extends AnyFunSuite with ScalaCheckPropertyChecks with ArbitraryInstances:
    test("constant as Constant") {
        val int: Constant = asConstant(2)
        assert(int == Constant.Integer(2))
        val bigint: Constant = asConstant(BigInt(2))
        assert(bigint == Constant.Integer(2))
        val bool: Constant = asConstant(true)
        assert(bool == Constant.Bool(true))
        val ba: Constant = asConstant(ByteString(2, 3))
        assert(ba == Constant.ByteString(ByteString(2, 3)))
        val s: Constant = asConstant("Hello")
        assert(s == Constant.String("Hello"))
        val u: Constant = asConstant(())
        assert(u == Constant.Unit)
        val li: Constant = asConstant(Seq(1, 2))
        assert(
          li ==
              Constant.List(DefaultUni.Integer, Constant.Integer(1) :: Constant.Integer(2) :: Nil)
        )
        val p: Constant = asConstant((1, false))
        assert(
          p ==
              Constant.Pair(
                Constant.Integer(1),
                Constant.Bool(false)
              )
        )
    }
    test("constant as Term") {
        val int: Term = 2
        assert(int == 2.asTerm)
        val bigint: Term = BigInt(2)
        assert(bigint == BigInt(2).asTerm)
        val bool: Term = true
        assert(bool == true.asTerm)
        val ba: Term = ByteString(2, 3)
        assert(ba == ByteString(2, 3).asTerm)
        val s: Term = "Hello"
        assert(s == "Hello".asTerm)
        val u: Term = ()
        assert(u == ().asTerm)
        val li: Term = Seq(1, 2)
        assert(
          li == Seq(1, 2).asTerm
        )
        val p: Term = (1, false)
        assert(
          p == (1, false).asTerm
        )
    }

    test("exists an implicit conversion from Constant to Term") {
        val int: Term = Constant.Integer(2)
        assert(int == 2.asTerm)
    }

    test("force/delay/λ/apply/vr") {
        forAll { (t: Term) =>
            assert(vr"a" == Term.Var(NamedDeBruijn("a", 0)))
            assert(!t == Term.Force(t))
            assert(~t == Term.Delay(t))
            assert((t $ t) == Term.Apply(t, t))
            assert(λ("x")(t) == Term.LamAbs("x", t))
            assert(lam("x")(t) == Term.LamAbs("x", t))
            assert(lam("x", "y")(t) == Term.LamAbs("x", Term.LamAbs("y", t)))
        }
    }

    test("pretty Apply") {
        val t = vr"f" $ vr"x" $ vr"y" $ vr"z"
        assert(t.show == "[f x y z]")
    }
