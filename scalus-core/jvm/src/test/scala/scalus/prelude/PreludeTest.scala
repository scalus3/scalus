package scalus.prelude

import scalus.cardano.onchain.plutus.prelude.`???`

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.onchain.{ImpossibleLedgerStateError, OnchainError, RequirementError}
import scalus.testing.kit.EvalTestKit

class PreludeTest extends AnyFunSuite with EvalTestKit {

    test("fail() should throw an exception") {
        assertEvalFails[OnchainError] {
            scalus.cardano.onchain.plutus.prelude.fail()
        }
        assertEvalFails[OnchainError] {
            scalus.cardano.onchain.plutus.prelude.fail("This is a failure message")
        }
    }

    test("require() should throw an exception when condition is false") {
        assertEvalFails[RequirementError] {
            scalus.cardano.onchain.plutus.prelude.require(false, "Condition failed")
        }
    }

    test("require() should return () when condition is true") {
        assertEvalEq(
          scalus.cardano.onchain.plutus.prelude.require(true, "This should not fail"),
          ()
        )
    }

    test("impossible() should throw an exception") {
        assertEvalFails[ImpossibleLedgerStateError] {
            scalus.cardano.onchain.plutus.prelude.impossible()
        }
    }

    test("??? should throw an exception") {
        assertEvalFails[NotImplementedError] {
            ???
        }
    }
}
