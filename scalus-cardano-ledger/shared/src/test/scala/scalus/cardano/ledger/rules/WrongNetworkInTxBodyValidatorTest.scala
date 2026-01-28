package scalus.cardano.ledger.rules

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.Network.{Mainnet, Testnet}
import scalus.cardano.ledger.*
import scalus.cardano.node.TestEmulatorFactory

class WrongNetworkInTxBodyValidatorTest extends AnyFunSuite with ArbitraryInstances {

    test("WrongNetworkInTxBody - success for matching network") {
        val transaction = randomTransactionWithIsValidField.withNetwork(Testnet)

        val emulator = TestEmulatorFactory.create(
          validators = Seq(WrongNetworkInTxBodyValidator),
          mutators = Seq.empty
        )
        val result = emulator.submitSync(transaction)
        assert(result.isRight, result)
    }

    test("WrongNetworkInTxBody - success for missing network") {
        val transaction = Transaction(
          body = TransactionBody(
            inputs = TaggedSortedSet.empty,
            outputs = IndexedSeq.empty,
            fee = Coin.zero,
            networkId = None
          ),
          witnessSet = TransactionWitnessSet.empty
        )

        val emulator = TestEmulatorFactory.create(
          validators = Seq(WrongNetworkInTxBodyValidator),
          mutators = Seq.empty
        )
        val result = emulator.submitSync(transaction)
        assert(result.isRight, result)
    }

    test("WrongNetworkInTxBody - failure for non-matching network") {
        val transaction = randomTransactionWithIsValidField.withNetwork(Mainnet)

        val emulator = TestEmulatorFactory.create(
          validators = Seq(WrongNetworkInTxBodyValidator),
          mutators = Seq.empty
        )
        val result = emulator.submitSync(transaction)
        assert(result.isLeft, result)
    }
}
