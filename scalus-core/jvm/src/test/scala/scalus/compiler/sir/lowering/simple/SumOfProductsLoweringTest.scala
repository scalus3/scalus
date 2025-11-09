package scalus.compiler.sir.lowering.simple

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.*
import scalus.Compiler.compile
import scalus.builtin.ByteString.*
import scalus.cardano.ledger.Word64
import scalus.ledger.api.v3.TxId
import scalus.sir.*
import scalus.uplc.DefaultFun.*
import scalus.uplc.DefaultUni.asConstant
import scalus.uplc.Term.*
import scalus.uplc.TermDSL.given
import scalus.uplc.test.ArbitraryInstances
import scalus.uplc.{Constant, DeBruijn, Term}

import scala.language.implicitConversions

class SumOfProductsLoweringTest extends AnyFunSuite, ScalaCheckPropertyChecks, ArbitraryInstances {
    extension (sir: SIR)

        infix def lowersTo(r: Term): Unit = {
            val r1 = SumOfProductsLowering(sir, generateErrorTraces = false).lower()
            val deBruijnR1 = DeBruijn.deBruijnTerm(r1)
            val deBruijnR = DeBruijn.deBruijnTerm(r)
            assert(deBruijnR1 α_== deBruijnR)
        }

    private val ae = AnnotationsDecl.empty

    test("lower constant") {
        forAll { (c: Constant) =>
            SIR.Const(c, SIRType.Integer, ae) lowersTo Term.Const(c)
        }
    }

    test("lower error") {
        SIR.Error("error", ae) lowersTo Term.Error
        assert(
          SIR.Error("error", ae)
              .toUplc(generateErrorTraces = true) == !(!Trace $ "error" $ ~Term.Error)
        )
    }

    // have no sence -- var is not scoped, will be error
    // test("lower Var") {
    //    SIR.Var("x", SIRType.ByteString, ae) lowersTo vr"x"
    // }

    test("lower Lam/Apply") {
        import SIRType.{TypeLambda, TypeVar, Unit}
        val idType = TypeLambda(List(TypeVar("A", Some(1), false)), TypeVar("A", Some(1), false))
        val x = SIR.Var("x", TypeVar("X", Some(2), false), ae)

        SIR.Apply(
          SIR.LamAbs(x, x, List.empty, ae),
          SIR.Const(Constant.Unit, Unit, ae),
          Unit,
          ae
        ) lowersTo (lam("x")(vr"x") $ Constant.Unit)

    }

    test("lower builtins") {
        SIRBuiltins.addInteger lowersTo AddInteger
        SIRBuiltins.headList lowersTo !HeadList
        SIRBuiltins.fstPair lowersTo !(!FstPair)
    }

    test("lower let") {
        import SIRType.{Fun, Integer}
        /* let x = 1 in
       let y = 2 in x + y
       lowers to (\x -> (\y -> x + y) 2) 1
         */
        SIR.Let(
          Binding("x", Integer, SIR.Const(asConstant(1), Integer, ae)) :: Binding(
            "y",
            Integer,
            SIR.Const(asConstant(2), Integer, ae)
          ) :: Nil,
          SIR.Apply(
            SIR.Apply(
              SIRBuiltins.addInteger,
              SIR.Var("x", Integer, ae),
              Fun(Integer, Integer),
              ae
            ),
            SIR.Var("y", Integer, ae),
            Integer,
            ae
          ),
          SIR.LetFlags.None,
          ae
        ) lowersTo (lam("x")(lam("y")(AddInteger $ vr"x" $ vr"y") $ 2) $ 1)
    }

    test("lower Constr") {
        val sir = compile { prelude.List.Nil: prelude.List[BigInt] }
        val uplc = SumOfProductsLowering(sir, generateErrorTraces = false).lower()
        // println("compiled:" + uplc.pretty.render(100))
        val expected = Term.Constr(Word64.Zero, List.empty)
        // println("expected:" + expected.pretty.render(100))
        sir lowersTo expected
    }

    test("lower newtype Constr") {
        val sir = compile { TxId(hex"DEADBEEF") }
        sir lowersTo hex"DEADBEEF".asTerm
    }

    test("lower And, Or, Not") {
        /* And True False
       lowers to (\True False -> And True False) True False
         */
        val a = SIR.Var("a", SIRType.Boolean, ae)
        val b = SIR.Var("b", SIRType.Boolean, ae)
        SIR.And(a, b, ae) lowersTo !(!IfThenElse $ vr"a" $ ~vr"b" $ ~false)
        SIR.Or(a, b, ae) lowersTo !(!IfThenElse $ vr"a" $ ~true $ ~vr"b")
        SIR.Not(a, ae) lowersTo !(!IfThenElse $ vr"a" $ ~false $ ~true)
    }

    test("lower Match") {
        /* list match
            case Nil -> error
            case Cons(h, tl) -> 2

            lowers to (case list [error, \h tl -> 2])

            newtype match
                case Newtype(a) -> error

            lowers to (\a -> error) newtype
         */

        val sir = compile {
            (prelude.List.Nil: prelude.List[BigInt]) match
                case prelude.List.Nil         => BigInt(1)
                case prelude.List.Cons(h, tl) => BigInt(2)
        }

        // With LetFloating optimization, the scrutinee lazy let is floated into the case
        // This becomes (lam scrutinee scrutinee) $ (constr 0) in the case scrutinee position
        val expected = Term.Case(
          lam("scrutinee")(vr"scrutinee") $ Term.Constr(Word64.Zero, List.empty),
          List(BigInt(1), λ("h", "tl")(BigInt(2)))
        )
        val compiled = SumOfProductsLowering(sir, generateErrorTraces = false).lower()

        val djExpected = DeBruijn.deBruijnTerm(expected)
        val djCompiled = DeBruijn.deBruijnTerm(compiled)

        val isEq = djCompiled α_== djExpected

        if !isEq then
            println(s"Expected: ${expected.pretty.render(100)}")
            println(s"Compiled: ${compiled.pretty.render(100)}")

        assert(isEq, "lowered term is not equal to expected term")
    }

    test("lower newtype Match") {
        /*
            newtype match
                case Newtype(a) -> 1

            lowers to (\a -> 1) newtype
         */
        val sir = compile {
            TxId(hex"DEADBEEF") match
                case TxId(id) => BigInt(1)
        }

        // With LetFloating optimization, the scrutinee lazy let is floated
        // The newtype unwrapping creates: (lam id BODY) ((lam scrutinee scrutinee) VALUE)
        val expected = lam("id")(BigInt(1)) $ (lam("scrutinee")(vr"scrutinee") $ hex"DEADBEEF")
        val compiled = SumOfProductsLowering(sir, generateErrorTraces = false).lower()

        val djExpected = DeBruijn.deBruijnTerm(expected)
        val djCompiled = DeBruijn.deBruijnTerm(compiled)

        val isEq = djCompiled α_== djExpected

        if !isEq then
            println(s"Expected: ${expected.pretty.render(100)}")
            println(s"Compiled: ${compiled.pretty.render(100)}")

        assert(isEq, "lowered term is not equal to expected term")
    }

    test("lower newtype Match with wildcard pattern") {
        val sir = compile {
            TxId(hex"DEADBEEF") match
                case _ => BigInt(1)
        }

        // With let-floating enabled, the unused scrutinee binding is eliminated
        sir lowersTo BigInt(1)
    }

    test("lower Select") {
        /*  x.field2
            lowers to
            (case x [\f1 f2 ... -> f2])

            newtype.field1
            lowers to
            newtype
         */
        val sir = compile {
            (hex"DEADBEEF", true)._2
        }
        sir lowersTo Term.Case(
          Term.Constr(Word64.Zero, List(hex"DEADBEEF", true)),
          List(λ("_1", "_2")(vr"_2"))
        )
    }

    test("lower newtype Select") {
        /*  x.field2
            lowers to
            (case x [\f1 f2 ... -> f2])

            newtype.field1
            lowers to
            newtype
         */
        val sir = compile {
            TxId(hex"DEADBEEF").hash
        }
        sir lowersTo hex"DEADBEEF".asTerm
    }

}
