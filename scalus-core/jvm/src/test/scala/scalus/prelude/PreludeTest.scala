package scalus.prelude

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.onchain.{ImpossibleLedgerStateError, OnchainError, RequirementError}
import scalus.testing.kit.EvalTestKit

class PreludeTest extends AnyFunSuite with EvalTestKit {

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
}
