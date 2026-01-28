package scalus.cardano.ledger
package rules

import org.scalacheck.Arbitrary
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.*
import scalus.cardano.address.Network.{Mainnet, Testnet}
import scalus.uplc.builtin.ByteString

class WrongNetworkValidatorTest extends AnyFunSuite, ValidatorRulesTestKit {
    test(
      "WrongNetworkValidator rule success for outputs for ShelleyAddress with matching network"
    ) {
        val transaction = Transaction(
          body = TransactionBody(
            inputs = TaggedSortedSet.empty,
            outputs = IndexedSeq(
              Sized(
                Output(
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
                Output(
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
                Output(
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
                Output(
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
        // Create a mainnet Byron address (no network magic = mainnet)
        // Context uses Network.Testnet by default, so this address should fail validation
        val mainnetByronAddress = ByronAddress.create(
          addrRoot = ByteString.fromHex("aa5372095aaa680d19d4ca496983a145709c3be18b0d4c83cb7bdc5e"),
          addrType = 0,
          networkMagic = None // No magic = mainnet
        )
        val transaction = Transaction(
          body = TransactionBody(
            inputs = TaggedSortedSet.empty,
            outputs = IndexedSeq(
              Sized(
                Output(
                  mainnetByronAddress,
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
                Output(
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
                Output(
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
                Output(
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
                Output(
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
        // Create a mainnet Byron address (no network magic = mainnet)
        // Context uses Network.Testnet by default, so this address should fail validation
        val mainnetByronAddress = ByronAddress.create(
          addrRoot = ByteString.fromHex("aa5372095aaa680d19d4ca496983a145709c3be18b0d4c83cb7bdc5e"),
          addrType = 0,
          networkMagic = None // No magic = mainnet
        )
        val transaction = Transaction(
          body = TransactionBody(
            inputs = TaggedSortedSet.empty,
            outputs = IndexedSeq.empty,
            fee = Coin.zero,
            collateralReturnOutput = Some(
              Sized(
                Output(
                  mainnetByronAddress,
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
