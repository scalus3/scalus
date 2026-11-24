package scalus.cardano.ledger
package rules

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.Network
import scalus.cardano.address.Network.{Mainnet, Testnet}

class WrongNetworkInTxBodyValidatorTest extends AnyFunSuite, ValidatorRulesTestKit {

    // The rule reads only the network id of the body.
    private def txOn(network: Option[Network]): Transaction =
        txSpending(input(0))(TransactionWitnessSet.empty)
            .withBody(_.copy(networkId = network.map(_.networkId.toInt)))

    test("WrongNetworkInTxBodyValidator rule success for matching network") {
        val transaction = txOn(Some(Testnet))
        val result = validate(WrongNetworkInTxBodyValidator, Map.empty, transaction)
        assert(result.isRight, result)
    }

    test("WrongNetworkInTxBodyValidator rule success for missing network") {
        val transaction = Transaction(emptyBody, TransactionWitnessSet.empty)
        val result = validate(WrongNetworkInTxBodyValidator, Map.empty, transaction)
        assert(result.isRight, result)
    }

    test("WrongNetworkInTxBodyValidator rule failure for non-matching network") {
        val transaction = txOn(Some(Mainnet))
        val result = validate(WrongNetworkInTxBodyValidator, Map.empty, transaction)
        assert(result.isLeft, result)
        assert(result.left.exists(_.isInstanceOf[TransactionException.WrongNetworkInTxBody]))
    }

}
