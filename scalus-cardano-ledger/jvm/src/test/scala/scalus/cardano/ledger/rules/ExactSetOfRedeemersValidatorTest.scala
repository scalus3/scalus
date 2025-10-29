package scalus.cardano.ledger
package rules

import org.scalacheck.Arbitrary
import org.scalatest.funsuite.AnyFunSuite
import scalus.builtin.Data
import scalus.cardano.address.{Address, Network, ShelleyAddress}
import TransactionWitnessSet.given
import scala.collection.immutable.SortedMap

class ExactSetOfRedeemersValidatorTest extends AnyFunSuite, ValidatorRulesTestKit {

    test("ExactSetOfRedeemersValidator success with no scripts") {
        val input = Arbitrary.arbitrary[TransactionInput].sample.get
        val utxo = Map(
          input -> TransactionOutput(
            Arbitrary.arbitrary[ShelleyAddress].sample.get,
            Value(Coin(1000000L))
          )
        )
        val transaction = Transaction(
          body = TransactionBody(
            inputs = TaggedSortedSet.from(Set(input)),
            outputs = IndexedSeq.empty,
            fee = Coin.zero
          ),
          witnessSet = TransactionWitnessSet()
        )
        val context = Context()
        val state = State(utxos = utxo)
        val result = ExactSetOfRedeemersValidator.validate(context, state, transaction)
        assert(result.isRight)
    }

    test("ExactSetOfRedeemersValidator success with matching spend redeemer") {
        val plutusScript = Arbitrary.arbitrary[Script.PlutusV1].sample.get
        val input = Arbitrary.arbitrary[TransactionInput].sample.get
        val utxo = Map(
          input -> TransactionOutput(
            Address(Network.Testnet, Credential.ScriptHash(plutusScript.scriptHash)),
            Value(Coin(1000000L))
          )
        )
        val dummyData = Arbitrary.arbitrary[Data].sample.get
        val dummyExUnits = Arbitrary.arbitrary[ExUnits].sample.get
        val transaction = Transaction(
          TransactionBody(
            inputs = TaggedSortedSet.from(Set(input)),
            outputs = IndexedSeq.empty,
            fee = Coin.zero,
          ),
          TransactionWitnessSet(
            scripts = Seq(plutusScript),
            redeemers = Redeemers(Redeemer(RedeemerTag.Spend, 0, dummyData, dummyExUnits)),
            vkeyWitnesses = Set.empty,
            plutusData = Seq.empty
          )
        )
        val context = Context()
        val state = State(utxos = utxo)
        val result = ExactSetOfRedeemersValidator.validate(context, state, transaction)
        assert(result.isRight)
    }

    test("ExactSetOfRedeemersValidator success with matching mint redeemer") {
        val plutusScript = Arbitrary.arbitrary[Script.PlutusV2].sample.get
        val input = Arbitrary.arbitrary[TransactionInput].sample.get
        val utxo = Map(
          input -> TransactionOutput(
            Arbitrary.arbitrary[ShelleyAddress].sample.get,
            Value(Coin(1000000L))
          )
        )
        val dummyData = Arbitrary.arbitrary[Data].sample.get
        val dummyExUnits = Arbitrary.arbitrary[ExUnits].sample.get
        val transaction = Transaction(
          TransactionBody(
            inputs = TaggedSortedSet.from(Set(input)),
            outputs = IndexedSeq.empty,
            fee = Coin.zero,
            mint = Some(
              Mint(
                MultiAsset(
                  SortedMap(
                    plutusScript.scriptHash -> SortedMap(AssetName.empty -> 1)
                  )
                )
              )
            ),
          ),
          TransactionWitnessSet(
            scripts = Seq(plutusScript),
            redeemers = Redeemers(Redeemer(RedeemerTag.Mint, 0, dummyData, dummyExUnits)),
            vkeyWitnesses = Set.empty,
            plutusData = Seq.empty
          )
        )
        val context = Context()
        val state = State(utxos = utxo)

        val result = ExactSetOfRedeemersValidator.validate(context, state, transaction)
        assert(result.isRight)
    }

    test("ExactSetOfRedeemersValidator failure with extra redeemers") {
        val plutusScript = Arbitrary.arbitrary[Script.PlutusV1].sample.get
        val input = Arbitrary.arbitrary[TransactionInput].sample.get
        val utxo = Map(
          input -> TransactionOutput(
            Address(Network.Testnet, Credential.ScriptHash(plutusScript.scriptHash)),
            Value(Coin(1000000L))
          )
        )
        val dummyData = Arbitrary.arbitrary[Data].sample.get
        val dummyExUnits = Arbitrary.arbitrary[ExUnits].sample.get
        val transaction = Transaction(
          TransactionBody(
            inputs = TaggedSortedSet.from(Set(input)),
            outputs = IndexedSeq.empty,
            fee = Coin.zero,
          ),
          TransactionWitnessSet(
            scripts = Seq(plutusScript),
            redeemers = Redeemers(
              Redeemer(RedeemerTag.Spend, 0, dummyData, dummyExUnits),
              Redeemer(RedeemerTag.Mint, 0, dummyData, dummyExUnits)
            ),
            vkeyWitnesses = Set.empty,
            plutusData = Seq.empty
          )
        )
        val context = Context()
        val state = State(utxos = utxo)

        val result = ExactSetOfRedeemersValidator.validate(context, state, transaction)

        assert(result.isLeft)
    }

    test("ExactSetOfRedeemersValidator failure with missing redeemers") {
        val plutusScript = Arbitrary.arbitrary[Script.PlutusV2].sample.get
        val input = Arbitrary.arbitrary[TransactionInput].sample.get
        val utxo = Map(
          input -> TransactionOutput(
            Arbitrary.arbitrary[ShelleyAddress].sample.get,
            Value(Coin(1000000L))
          )
        )
        val dummyData = Arbitrary.arbitrary[Data].sample.get
        val dummyExUnits = Arbitrary.arbitrary[ExUnits].sample.get
        val transaction = Transaction(
          TransactionBody(
            inputs = TaggedSortedSet.from(Set(input)),
            outputs = IndexedSeq.empty,
            fee = Coin.zero,
            mint = Some(
              Mint(
                MultiAsset(
                  SortedMap(
                    plutusScript.scriptHash -> SortedMap(AssetName.empty -> 1)
                  )
                )
              )
            ),
          ),
          TransactionWitnessSet(
            plutusV2Scripts = TaggedSortedSet(plutusScript),
            redeemers = None // no redeemer
          )
        )
        val context = Context()
        val state = State(utxos = utxo)

        val result = ExactSetOfRedeemersValidator.validate(context, state, transaction)

        assert(result.isLeft)
    }

    test("ExactSetOfRedeemersValidator success with native scripts (no redeemers needed)") {
        val (privateKey, publicKey) = generateKeyPair()
        val nativeScript = Timelock.Signature(Hash(scalus.builtin.platform.blake2b_224(publicKey)))
        val input = Arbitrary.arbitrary[TransactionInput].sample.get
        val utxo = Map(
          input -> TransactionOutput(
            Address(Network.Testnet, Credential.ScriptHash(nativeScript.scriptHash)),
            Value(Coin(1000000L))
          )
        )
        val transaction = Transaction(
          TransactionBody(
            inputs = TaggedSortedSet.from(Set(input)),
            outputs = IndexedSeq.empty,
            fee = Coin.zero,
            mint = Some(
              Mint(
                MultiAsset(
                  SortedMap(
                    nativeScript.scriptHash -> SortedMap(AssetName.empty -> 1)
                  )
                )
              )
            ),
          ),
          TransactionWitnessSet(
            nativeScripts = TaggedSortedSet(Script.Native(nativeScript)),
            redeemers = None
          ),
        )
        val context = Context()
        val state = State(utxos = utxo)

        val result = ExactSetOfRedeemersValidator.validate(context, state, transaction)
        assert(result.isRight)
    }

    test("ExactSetOfRedeemersValidator success with mixed native and Plutus scripts") {
        val (privateKey, publicKey) = generateKeyPair()
        val nativeScript = Timelock.Signature(Hash(scalus.builtin.platform.blake2b_224(publicKey)))
        val plutusScript = Arbitrary.arbitrary[Script.PlutusV2].sample.get

        val input1 = TransactionInput(
          TransactionHash.fromHex(
            "a000000000000000000000000000000000000000000000000000000000000000"
          ),
          0
        )
        val input2 = TransactionInput(
          TransactionHash.fromHex(
            "b000000000000000000000000000000000000000000000000000000000000000"
          ),
          0
        )

        val utxo = Map(
          input1 -> TransactionOutput(
            Address(Network.Testnet, Credential.ScriptHash(nativeScript.scriptHash)),
            Value(Coin(1000000L))
          ),
          input2 -> TransactionOutput(
            Address(Network.Testnet, Credential.ScriptHash(plutusScript.scriptHash)),
            Value(Coin(1000000L))
          )
        )

        val dummyData = Arbitrary.arbitrary[Data].sample.get
        val dummyExUnits = Arbitrary.arbitrary[ExUnits].sample.get

        // Create inputs set and determine the actual index of the Plutus script input
        val inputsSet = Set(input1, input2)
        val sortedInputs = inputsSet.toSeq.sorted
        val plutusInputIndex = sortedInputs.zipWithIndex
            .find { case (input, _) =>
                utxo.get(input).exists(_.address.scriptHashOption.contains(plutusScript.scriptHash))
            }
            .get
            ._2

        val transaction = Transaction(
          TransactionBody(
            inputs = TaggedSortedSet.from(inputsSet),
            outputs = IndexedSeq.empty,
            fee = Coin.zero,
          ),
          TransactionWitnessSet(
            scripts = Seq(Script.Native(nativeScript), plutusScript),
            redeemers = Redeemers(
              // calculate the index manually for a tx where n of inputs > 1
              Redeemer(RedeemerTag.Spend, plutusInputIndex, dummyData, dummyExUnits)
            ),
            vkeyWitnesses = Set.empty,
            plutusData = Seq.empty
          )
        )
        val context = Context()
        val state = State(utxos = utxo)

        val result = ExactSetOfRedeemersValidator.validate(context, state, transaction)
        assert(result.isRight)
    }
}
