package scalus.cardano.ledger
package rules

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.Network.Mainnet

import scala.collection.immutable.SortedMap

class WrongNetworkWithdrawalValidatorTest extends AnyFunSuite, ValidatorRulesTestKit {

    private def withdrawing(account: RewardAccount): Transaction =
        Transaction(
          emptyBody.copy(withdrawals = Some(Withdrawals(SortedMap(account -> Coin(0L))))),
          TransactionWitnessSet.empty
        )

    test("WrongNetworkWithdrawalValidator rule success with matching network") {
        // rewardAccount(n) is on Testnet, the network of Context().
        val transaction = withdrawing(rewardAccount(1))
        val result = validate(WrongNetworkWithdrawalValidator, Map.empty, transaction)
        assert(result.isRight, result)
    }

    test("WrongNetworkWithdrawalValidator rule failure with non-matching network") {
        val transaction = withdrawing(rewardAccount(1, Mainnet))
        val result = validate(WrongNetworkWithdrawalValidator, Map.empty, transaction)
        assert(result.isLeft, result)
    }

    test("WrongNetworkWithdrawalValidator rule success with no withdrawals") {
        val transaction = Transaction(emptyBody, TransactionWitnessSet.empty)
        val result = validate(WrongNetworkWithdrawalValidator, Map.empty, transaction)
        assert(result.isRight, result)
    }
}
