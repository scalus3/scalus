package scalus.uplc.eval

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.Compiler.compile
import scalus.builtin.{BuiltinValue, Data}
import scalus.prelude.List as PList
import scalus.uplc.{Constant, DefaultFun, Term}

/** Tests for BuiltinValue compilation to UPLC. */
class BuiltinValueCompileTest extends AnyFunSuite {

    given PlutusVM = PlutusVM.makePlutusV3VM()

    test("BuiltinValue.empty compiles to unValueData builtin call") {
        val sir = compile { BuiltinValue.empty }
        val uplc = sir.toUplc()
        // Should compile to: unValueData(emptyDataMap)
        val expected = Term.Apply(
          Term.Builtin(DefaultFun.UnValueData),
          Term.Const(Constant.Data(Data.Map(PList.Nil)))
        )
        assert(uplc == expected)
    }

    test("BuiltinValue.empty evaluates to empty value") {
        val sir = compile { BuiltinValue.empty }
        val uplc = sir.toUplc()
        val result = uplc.evaluateDebug
        result match {
            case Result.Success(term, _, _, _) =>
                term match {
                    case Term.Const(Constant.BuiltinValue(v)) =>
                        assert(v == BuiltinValue.empty)
                    case other =>
                        fail(s"Expected Const(BuiltinValue), got: $other")
                }
            case Result.Failure(err, _, _, _) =>
                fail(s"Expected success but got failure: $err")
        }
    }
}
