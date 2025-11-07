package scalus.prelude

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.onchain.{ImpossibleLedgerStateError, OnchainError, RequirementError}
import scalus.uplc.Constant.toValue
import scalus.uplc.eval.PlutusVM
import scalus.uplc.Term

import scala.reflect.ClassTag

class PreludeTest extends AnyFunSuite {

    private given PlutusVM = PlutusVM.makePlutusV3VM()
    test("fail() should throw an exception") {
        assertEvalFails[OnchainError] {
            scalus.prelude.fail()
        }
        assertEvalFails[OnchainError] {
            scalus.prelude.fail("This is a failure message")
        }
    }

    test("require() should throw an exception when condition is false") {
        assertEvalFails[RequirementError] {
            require(false, "Condition failed")
        }

    }

    test("require() should return () when condition is true") {
        assertEvalEq(require(true, "This should not fail"), ())
    }

    test("impossible() should throw an exception") {
        assertEvalFails[ImpossibleLedgerStateError] {
            impossible()
        }
    }

    test("??? should throw an exception") {
        assertEvalFails[NotImplementedError] {
            ???
        }
    }

    private inline def assertEvalFails[E <: Throwable: ClassTag](inline code: Any): Unit = {
        import scalus.*
        assertThrows[E](code)
        val result = Compiler.compileInline(code).toUplc(true).evaluateDebug
        assert(result.isFailure)
    }

    private inline def assertEvalEq(inline code: Any, expected: Any): Unit = {
        import scalus.*
        assert(code == expected)
        val term = Compiler.compileInline(code).toUplc(true).evaluate
        term match
            case Term.Const(const) => assert(toValue(const) == expected)
            case _                 => fail(s"Unexpected term: $term")
    }

}
