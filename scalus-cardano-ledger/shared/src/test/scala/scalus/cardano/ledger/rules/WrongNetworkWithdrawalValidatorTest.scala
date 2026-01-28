package scalus.cardano.ledger.rules

import org.scalacheck.Arbitrary
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.Network.{Mainnet, Testnet}
import scalus.cardano.address.{StakeAddress, StakePayload}
import scalus.cardano.ledger.*
import scalus.cardano.node.TestEmulatorFactory

import scala.collection.immutable.SortedMap

class WrongNetworkWithdrawalValidatorTest extends AnyFunSuite with ArbitraryInstances {

    test("WrongNetworkWithdrawal - success with matching network") {
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

        val emulator = TestEmulatorFactory.create(
          validators = Seq(WrongNetworkWithdrawalValidator),
          mutators = Seq.empty
        )
        val result = emulator.submitSync(transaction)
        assert(result.isRight, result)
    }

    test("WrongNetworkWithdrawal - failure with non-matching network") {
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

        val emulator = TestEmulatorFactory.create(
          validators = Seq(WrongNetworkWithdrawalValidator),
          mutators = Seq.empty
        )
        val result = emulator.submitSync(transaction)
        assert(result.isLeft, result)
    }

    test("WrongNetworkWithdrawal - success with no withdrawals") {
        val transaction = Transaction(
          body = TransactionBody(
            inputs = TaggedSortedSet.empty,
            outputs = IndexedSeq.empty,
            fee = Coin(0),
            withdrawals = None
          ),
          witnessSet = TransactionWitnessSet.empty
        )

        val emulator = TestEmulatorFactory.create(
          validators = Seq(WrongNetworkWithdrawalValidator),
          mutators = Seq.empty
        )
        val result = emulator.submitSync(transaction)
        assert(result.isRight, result)
    }
}
