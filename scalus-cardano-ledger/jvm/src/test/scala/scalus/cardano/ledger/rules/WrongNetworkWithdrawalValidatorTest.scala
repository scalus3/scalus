package scalus.cardano.ledger
package rules

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.Network.{Mainnet, Testnet}

class WrongNetworkWithdrawalValidatorTest extends AnyFunSuite, ValidatorRulesTestKit {

    test("WrongNetworkWithdrawalValidator rule success") {
        val transaction = randomTransactionWithIsValidField.withNetwork(Testnet)
        val result = WrongNetworkWithdrawalValidator.validate(Context(), State(), transaction)
        assert(result.isRight, result)
    }

    test("WrongNetworkWithdrawalValidator rule failure") {
        val transaction = randomTransactionWithIsValidField.withNetwork(Mainnet)
        val result = WrongNetworkWithdrawalValidator.validate(Context(), State(), transaction)
        assert(result.isLeft, result)
        assert(result.left.exists(_.isInstanceOf[TransactionException.WrongNetworkWithdrawal]))
    }

}
