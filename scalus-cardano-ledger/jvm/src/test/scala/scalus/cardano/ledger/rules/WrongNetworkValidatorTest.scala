package scalus.cardano.ledger
package rules

import org.scalacheck.Arbitrary
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.Network.{Mainnet, Testnet}
import scalus.cardano.address.*

class WrongNetworkValidatorTest extends AnyFunSuite, ValidatorRulesTestKit {
    test(
      "WrongNetworkValidator rule success for outputs for ShelleyAddress with matching network"
    ) {
        val transaction = Transaction(
          body = TransactionBody(
            inputs = TaggedSortedSet.empty,
            outputs = IndexedSeq(
              Sized(
                TransactionOutput(
                  ShelleyAddress(
                    Testnet,
                    Arbitrary.arbitrary[ShelleyPaymentPart].sample.get,
                    Arbitrary.arbitrary[ShelleyDelegationPart].sample.get
                  ),
                  Value.zero
                )
              )
            ),
            fee = Coin.zero
          ),
          witnessSet = TransactionWitnessSet.empty
        )
        val result = WrongNetworkValidator.validate(Context(), State(), transaction)
        assert(result.isRight, result)
    }

    test(
      "WrongNetworkValidator rule failure for outputs for ShelleyAddress with non-matching network"
    ) {
        val transaction = Transaction(
          body = TransactionBody(
            inputs = TaggedSortedSet.empty,
            outputs = IndexedSeq(
              Sized(
                TransactionOutput(
                  ShelleyAddress(
                    Mainnet,
                    Arbitrary.arbitrary[ShelleyPaymentPart].sample.get,
                    Arbitrary.arbitrary[ShelleyDelegationPart].sample.get
                  ),
                  Value.zero
                )
              )
            ),
            fee = Coin.zero
          ),
          witnessSet = TransactionWitnessSet.empty
        )
        val result = WrongNetworkValidator.validate(Context(), State(), transaction)
        assert(result.isLeft, result)
    }

    test("WrongNetworkValidator rule success for outputs for StakeAddress with matching network") {
        val transaction = Transaction(
          body = TransactionBody(
            inputs = TaggedSortedSet.empty,
            outputs = IndexedSeq(
              Sized(
                TransactionOutput(
                  StakeAddress(
                    Testnet,
                    Arbitrary.arbitrary[StakePayload].sample.get
                  ),
                  Value.zero
                )
              )
            ),
            fee = Coin.zero
          ),
          witnessSet = TransactionWitnessSet.empty
        )
        val result = WrongNetworkValidator.validate(Context(), State(), transaction)
        assert(result.isRight, result)
    }

    test(
      "WrongNetworkValidator rule failure for outputs for StakeAddress with non-matching network"
    ) {
        val transaction = Transaction(
          body = TransactionBody(
            inputs = TaggedSortedSet.empty,
            outputs = IndexedSeq(
              Sized(
                TransactionOutput(
                  StakeAddress(
                    Mainnet,
                    Arbitrary.arbitrary[StakePayload].sample.get
                  ),
                  Value.zero
                )
              )
            ),
            fee = Coin.zero
          ),
          witnessSet = TransactionWitnessSet.empty
        )
        val result = WrongNetworkValidator.validate(Context(), State(), transaction)
        assert(result.isLeft, result)
    }

    test("WrongNetworkValidator rule failure for outputs for ByronAddress") {
        val transaction = Transaction(
          body = TransactionBody(
            inputs = TaggedSortedSet.empty,
            outputs = IndexedSeq(
              Sized(
                TransactionOutput(
                  Arbitrary.arbitrary[ByronAddress].sample.get,
                  Value.zero
                )
              )
            ),
            fee = Coin.zero
          ),
          witnessSet = TransactionWitnessSet.empty
        )
        val result = WrongNetworkValidator.validate(Context(), State(), transaction)
        assert(result.isLeft, result)
    }

    test(
      "WrongNetworkValidator rule success for collateralReturnOutput for ShelleyAddress with matching network"
    ) {
        val transaction = Transaction(
          body = TransactionBody(
            inputs = TaggedSortedSet.empty,
            outputs = IndexedSeq.empty,
            fee = Coin.zero,
            collateralReturnOutput = Some(
              Sized(
                TransactionOutput(
                  ShelleyAddress(
                    Testnet,
                    Arbitrary.arbitrary[ShelleyPaymentPart].sample.get,
                    Arbitrary.arbitrary[ShelleyDelegationPart].sample.get
                  ),
                  Value.zero
                )
              )
            )
          ),
          witnessSet = TransactionWitnessSet.empty
        )
        val result = WrongNetworkValidator.validate(Context(), State(), transaction)
        assert(result.isRight, result)
    }

    test(
      "WrongNetworkValidator rule failure for collateralReturnOutput for ShelleyAddress with non-matching network"
    ) {
        val transaction = Transaction(
          body = TransactionBody(
            inputs = TaggedSortedSet.empty,
            outputs = IndexedSeq.empty,
            fee = Coin.zero,
            collateralReturnOutput = Some(
              Sized(
                TransactionOutput(
                  ShelleyAddress(
                    Mainnet,
                    Arbitrary.arbitrary[ShelleyPaymentPart].sample.get,
                    Arbitrary.arbitrary[ShelleyDelegationPart].sample.get
                  ),
                  Value.zero
                )
              )
            )
          ),
          witnessSet = TransactionWitnessSet.empty
        )
        val result = WrongNetworkValidator.validate(Context(), State(), transaction)
        assert(result.isLeft, result)
    }

    test(
      "WrongNetworkValidator rule success for collateralReturnOutput for StakeAddress with matching network"
    ) {
        val transaction = Transaction(
          body = TransactionBody(
            inputs = TaggedSortedSet.empty,
            outputs = IndexedSeq.empty,
            fee = Coin.zero,
            collateralReturnOutput = Some(
              Sized(
                TransactionOutput(
                  StakeAddress(
                    Testnet,
                    Arbitrary.arbitrary[StakePayload].sample.get
                  ),
                  Value.zero
                )
              )
            )
          ),
          witnessSet = TransactionWitnessSet.empty
        )
        val result = WrongNetworkValidator.validate(Context(), State(), transaction)
        assert(result.isRight, result)
    }

    test(
      "WrongNetworkValidator rule failure for collateralReturnOutput for StakeAddress with non-matching network"
    ) {
        val transaction = Transaction(
          body = TransactionBody(
            inputs = TaggedSortedSet.empty,
            outputs = IndexedSeq.empty,
            fee = Coin.zero,
            collateralReturnOutput = Some(
              Sized(
                TransactionOutput(
                  StakeAddress(
                    Mainnet,
                    Arbitrary.arbitrary[StakePayload].sample.get
                  ),
                  Value.zero
                )
              )
            )
          ),
          witnessSet = TransactionWitnessSet.empty
        )
        val result = WrongNetworkValidator.validate(Context(), State(), transaction)
        assert(result.isLeft, result)
    }

    test("WrongNetworkValidator rule failure for collateralReturnOutput for ByronAddress") {
        val transaction = Transaction(
          body = TransactionBody(
            inputs = TaggedSortedSet.empty,
            outputs = IndexedSeq.empty,
            fee = Coin.zero,
            collateralReturnOutput = Some(
              Sized(
                TransactionOutput(
                  Arbitrary.arbitrary[ByronAddress].sample.get,
                  Value.zero
                )
              )
            )
          ),
          witnessSet = TransactionWitnessSet.empty
        )
        val result = WrongNetworkValidator.validate(Context(), State(), transaction)
        assert(result.isLeft, result)
    }
}
