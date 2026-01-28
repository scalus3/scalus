package scalus.cardano.ledger
package rules

import org.scalacheck.Arbitrary
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.*
import scalus.cardano.address.Network.{Mainnet, Testnet}

import scala.collection.immutable.SortedMap

class WrongNetworkWithdrawalValidatorTest extends AnyFunSuite, ValidatorRulesTestKit {
    test("WrongNetworkWithdrawalValidator rule success with matching network") {
        val transaction = Transaction(
          body = TransactionBody(
            inputs = TaggedSortedSet.empty,
            outputs = IndexedSeq.empty,
            fee = Coin(0),
            withdrawals = Some(
              Withdrawals(
                SortedMap(
                  RewardAccount(
                    StakeAddress(
                      Testnet,
                      Arbitrary.arbitrary[StakePayload].sample.get
                    )
                  ) -> Coin(0L)
                )
              )
            )
          ),
          witnessSet = TransactionWitnessSet.empty
        )
        val result = WrongNetworkWithdrawalValidator.validate(Context(), State(), transaction)
        assert(result.isRight, result)
    }

    test("WrongNetworkWithdrawalValidator rule failure with non-matching network") {
        val transaction = Transaction(
          body = TransactionBody(
            inputs = TaggedSortedSet.empty,
            outputs = IndexedSeq.empty,
            fee = Coin(0),
            withdrawals = Some(
              Withdrawals(
                SortedMap(
                  RewardAccount(
                    StakeAddress(
                      Mainnet,
                      Arbitrary.arbitrary[StakePayload].sample.get
                    )
                  ) -> Coin(0L)
                )
              )
            )
          ),
          witnessSet = TransactionWitnessSet.empty
        )
        val result = WrongNetworkWithdrawalValidator.validate(Context(), State(), transaction)
        assert(result.isLeft, result)
    }

    test("WrongNetworkWithdrawalValidator rule success with no withdrawals") {
        val transaction = Transaction(
          body = TransactionBody(
            inputs = TaggedSortedSet.empty,
            outputs = IndexedSeq.empty,
            fee = Coin(0),
            withdrawals = None
          ),
          witnessSet = TransactionWitnessSet.empty
        )
        val result = WrongNetworkWithdrawalValidator.validate(Context(), State(), transaction)
        assert(result.isRight, result)
    }
}
