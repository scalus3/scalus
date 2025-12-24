package scalus.builtin

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.Compiler.compile
import scalus.builtin.ByteString.*
import scalus.builtin.{ByteString, Data}
import scalus.prelude.List as PList
import scalus.uplc.{Constant, Term}
import scalus.uplc.eval.{PlutusVM, Result}

/** Tests for Data constructor compilation.
  *
  * Tests that Data.unit, Data.I, Data.B, Data.Constr, Data.List, and Data.Map compile correctly to
  * their corresponding UPLC builtins.
  */
class DataConstructorCompileTest extends AnyFunSuite {

    given PlutusVM = PlutusVM.makePlutusV3VM()

    // Helper to extract Data from evaluation result
    def evalToData(term: Term)(using vm: PlutusVM): Data = {
        term.evaluateDebug match {
            case Result.Success(Term.Const(Constant.Data(d)), _, _, _) => d
            case Result.Success(other, _, _, _) =>
                fail(s"Expected Const(Data), got: $other")
            case Result.Failure(err, _, _, _) =>
                fail(s"Evaluation failed: $err")
        }
    }

    // ==================== Data.unit tests ====================

    test("Data.unit compiles to Data constant") {
        val sir = compile { Data.unit }
        val uplc = sir.toUplc()
        // Should compile to a constant Data value
        val expected = Term.Const(Constant.Data(Data.Constr(0, PList.Nil)))
        assert(uplc == expected)
    }

    test("Data.unit evaluates correctly") {
        val sir = compile { Data.unit }
        val result = evalToData(sir.toUplc())
        assert(result == Data.Constr(0, PList.Nil))
    }

    // ==================== Data.I tests ====================

    test("Data.I compiles to iData builtin") {
        val sir = compile { Data.I(BigInt(42)) }
        val result = evalToData(sir.toUplc())
        assert(result == Data.I(BigInt(42)))
    }

    test("Data.I with variable compiles correctly") {
        val sir = compile { (n: BigInt) => Data.I(n) }
        val uplc = sir.toUplc()
        val applied = Term.Apply(uplc, Term.Const(Constant.Integer(BigInt(123))))
        val result = evalToData(applied)
        assert(result == Data.I(BigInt(123)))
    }

    // ==================== Data.B tests ====================

    test("Data.B compiles to bData builtin") {
        val sir = compile { Data.B(hex"deadbeef") }
        val result = evalToData(sir.toUplc())
        assert(result == Data.B(hex"deadbeef"))
    }

    test("Data.B with variable compiles correctly") {
        val sir = compile { (bs: ByteString) => Data.B(bs) }
        val uplc = sir.toUplc()
        val applied = Term.Apply(uplc, Term.Const(Constant.ByteString(hex"cafe")))
        val result = evalToData(applied)
        assert(result == Data.B(hex"cafe"))
    }

    // ==================== Combined usage tests ====================

    test("Data constructors can be used in expressions") {
        val sir = compile { (x: BigInt) =>
            if x > 0 then Data.I(x)
            else Data.unit
        }
        val uplc = sir.toUplc()

        val resultPositive = evalToData(Term.Apply(uplc, Term.Const(Constant.Integer(BigInt(5)))))
        assert(resultPositive == Data.I(BigInt(5)))

        val resultZero = evalToData(Term.Apply(uplc, Term.Const(Constant.Integer(BigInt(0)))))
        assert(resultZero == Data.Constr(0, PList.Nil))
    }

    // ==================== Data.Constr tests ====================

    test("Data.Constr compiles to constrData builtin") {
        val sir = compile { Data.Constr(BigInt(0), PList.Nil) }
        val result = evalToData(sir.toUplc())
        assert(result == Data.Constr(0, PList.Nil))
    }

    test("Data.Constr with arguments compiles correctly") {
        val sir = compile {
            Data.Constr(BigInt(1), PList(Data.I(BigInt(42)), Data.I(BigInt(43))))
        }
        val result = evalToData(sir.toUplc())
        assert(result == Data.Constr(1, PList(Data.I(42), Data.I(43))))
    }

    test("Data.Constr with variable tag compiles correctly") {
        val sir = compile { (tag: BigInt) =>
            Data.Constr(tag, PList.Nil)
        }
        val uplc = sir.toUplc()
        val applied = Term.Apply(uplc, Term.Const(Constant.Integer(BigInt(5))))
        val result = evalToData(applied)
        assert(result == Data.Constr(5, PList.Nil))
    }

    // ==================== Data.List tests ====================

    test("Data.List with empty list compiles correctly") {
        val sir = compile { Data.List(PList.Nil) }
        val result = evalToData(sir.toUplc())
        assert(result == Data.List(PList.Nil))
    }

    test("Data.List with elements compiles to listData builtin") {
        val sir = compile {
            Data.List(PList(Data.I(BigInt(1)), Data.I(BigInt(2)), Data.I(BigInt(3))))
        }
        val result = evalToData(sir.toUplc())
        assert(result == Data.List(PList(Data.I(1), Data.I(2), Data.I(3))))
    }

    test("Data.List with variable compiles correctly") {
        val sir = compile { (d: Data) =>
            Data.List(PList(d))
        }
        val uplc = sir.toUplc()
        val applied = Term.Apply(uplc, Term.Const(Constant.Data(Data.I(100))))
        val result = evalToData(applied)
        assert(result == Data.List(PList(Data.I(100))))
    }

    // ==================== Data.Map tests ====================

    test("Data.Map with empty map compiles correctly") {
        val sir = compile { Data.Map(PList.Nil) }
        val result = evalToData(sir.toUplc())
        assert(result == Data.Map(PList.Nil))
    }

    test("Data.Map with entries compiles to mapData builtin") {
        val sir = compile {
            Data.Map(PList((Data.I(BigInt(1)), Data.I(BigInt(10)))))
        }
        val result = evalToData(sir.toUplc())
        assert(result == Data.Map(PList((Data.I(1), Data.I(10)))))
    }

    test("Data.Map with multiple entries compiles correctly") {
        val sir = compile {
            Data.Map(
              PList(
                (Data.I(BigInt(1)), Data.B(hex"aa")),
                (Data.I(BigInt(2)), Data.B(hex"bb"))
              )
            )
        }
        val result = evalToData(sir.toUplc())
        assert(
          result == Data.Map(
            PList(
              (Data.I(1), Data.B(hex"aa")),
              (Data.I(2), Data.B(hex"bb"))
            )
          )
        )
    }
}
