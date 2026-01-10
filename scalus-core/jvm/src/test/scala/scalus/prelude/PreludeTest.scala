package scalus.prelude

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.onchain.{ImpossibleLedgerStateError, OnchainError, RequirementError}
import scalus.compiler.compileInline
import scalus.uplc.Constant.toValue
import scalus.uplc.Term
import scalus.uplc.eval.PlutusVM

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

    // Simple assertion that checks:
    // 1. The code throws the expected exception at runtime
    // 2. The compiled UPLC evaluates to failure
    private inline def assertEvalFails[E <: Throwable: ClassTag](inline code: Any): Unit = {
        import scalus.*
        assertThrows[E](code)
        val result = compileInline(code).toUplc(true).evaluateDebug
        assert(result.isFailure, s"Expected UPLC evaluation to fail, but got: $result")
    }

    private inline def assertEvalEq(inline code: Any, expected: Any): Unit = {
        import scalus.*
        assert(code == expected)
        val term = compileInline(code).toUplc(true).evaluate
        term match
            case Term.Const(const) => assert(toValue(const) == expected)
            case _                 => fail(s"Unexpected term: $term")
    }
}
