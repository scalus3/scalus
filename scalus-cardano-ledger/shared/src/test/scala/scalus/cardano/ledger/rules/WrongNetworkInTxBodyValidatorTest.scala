package scalus.cardano.ledger
package rules

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.Network.{Mainnet, Testnet}

class WrongNetworkInTxBodyValidatorTest extends AnyFunSuite, ValidatorRulesTestKit {

    test("WrongNetworkInTxBodyValidator rule success for matching network") {
        val transaction = randomTransactionWithIsValidField.withNetwork(Testnet)
        val result = WrongNetworkInTxBodyValidator.validate(Context(), State(), transaction)
        assert(result.isRight, result)
    }

    test("WrongNetworkInTxBodyValidator rule success for missing network") {
        val transaction = Transaction(
          body = TransactionBody(
            inputs = TaggedSortedSet.empty,
            outputs = IndexedSeq.empty,
            fee = Coin.zero,
            networkId = None
          ),
          witnessSet = TransactionWitnessSet.empty
        )
        val result = WrongNetworkInTxBodyValidator.validate(Context(), State(), transaction)
        assert(result.isRight, result)
    }

    test("WrongNetworkInTxBodyValidator rule failure for non-matching network") {
        val transaction = randomTransactionWithIsValidField.withNetwork(Mainnet)
        val result = WrongNetworkInTxBodyValidator.validate(Context(), State(), transaction)
        assert(result.isLeft, result)
        assert(result.left.exists(_.isInstanceOf[TransactionException.WrongNetworkInTxBody]))
    }

}
