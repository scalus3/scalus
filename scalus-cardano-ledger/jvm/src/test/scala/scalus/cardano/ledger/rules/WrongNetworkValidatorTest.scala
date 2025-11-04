package scalus.cardano.ledger
package rules

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.Network.{Mainnet, Testnet}

class WrongNetworkValidatorTest extends AnyFunSuite, ValidatorRulesTestKit {

    test("WrongNetworkValidator rule success") {
        val transaction = randomTransactionWithIsValidField.withNetwork(Testnet)
        val result = WrongNetworkValidator.validate(Context(), State(), transaction)
        assert(result.isRight, result)
    }

    test("WrongNetworkValidator rule failure") {
        val transaction = randomTransactionWithIsValidField.withNetwork(Mainnet)
        val result = WrongNetworkValidator.validate(Context(), State(), transaction)
        assert(result.isLeft, result)
        assert(result.left.exists(_.isInstanceOf[TransactionException.WrongNetworkAddress]))
    }

}
