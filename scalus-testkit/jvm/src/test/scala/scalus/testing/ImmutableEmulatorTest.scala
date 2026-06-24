package scalus.testing

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.*
import scalus.cardano.ledger.rules.Context
import scalus.cardano.node.Emulator

class ImmutableEmulatorTest extends AnyFunSuite {

    test("fromEmulator carries over evaluatorMode (issue #314)") {
        val emulator = Emulator(
          initialContext =
              Context.testMainnet().copy(evaluatorMode = EvaluatorMode.EvaluateAndComputeCost)
        )

        val immutable = ImmutableEmulator.fromEmulator(emulator)

        assert(
          immutable.evaluatorMode == EvaluatorMode.EvaluateAndComputeCost,
          "fromEmulator must not silently revert evaluatorMode to Validate"
        )
    }

    test("fromEmulator preserves evaluatorMode after a slot advance (issue #314)") {
        val emulator = Emulator(
          initialContext =
              Context.testMainnet().copy(evaluatorMode = EvaluatorMode.EvaluateAndComputeCost)
        )
        emulator.setSlot(100L)

        val immutable = ImmutableEmulator.fromEmulator(emulator)

        assert(immutable.currentSlot == 100L)
        assert(immutable.evaluatorMode == EvaluatorMode.EvaluateAndComputeCost)
    }
}
