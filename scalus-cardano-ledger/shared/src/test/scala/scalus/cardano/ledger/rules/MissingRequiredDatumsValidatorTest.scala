package scalus.cardano.ledger.rules

import org.scalacheck.Arbitrary
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.{Address, Network, ShelleyAddress}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.TransactionWitnessSet.given
import scalus.cardano.node.TestEmulatorFactory
import scalus.uplc.builtin.Data

class MissingRequiredDatumsValidatorTest extends AnyFunSuite with ArbitraryInstances {
    test("MissingRequiredDatumsValidator success with no scripts") {
        val input = Arbitrary.arbitrary[TransactionInput].sample.get
        val utxo = Map(
          input -> Output(
            Arbitrary.arbitrary[ShelleyAddress].sample.get,
            Value(Coin(1000000L))
          )
        )
        val transaction = Transaction(
          TransactionBody(
            inputs = TaggedSortedSet.from(Set(input)),
            outputs = IndexedSeq.empty,
            fee = Coin.zero
          )
        )

        val emulator = TestEmulatorFactory.create(
          utxos = utxo,
          validators = Seq(MissingRequiredDatumsValidator),
          mutators = Seq.empty
        )
        val result = emulator.submitSync(transaction)
        assert(result.isRight)
    }

    test("MissingRequiredDatumsValidator success with matching datum") {
        val plutusScript = Arbitrary.arbitrary[Script.PlutusV3].sample.get
        val input = Arbitrary.arbitrary[TransactionInput].sample.get
        val datum = Arbitrary.arbitrary[Data].sample.get
        val datumHash = DataHash.fromByteString(datum.dataHash)
        val utxo = Map(
          input -> Output(
            Address(Network.Testnet, Credential.ScriptHash(plutusScript.scriptHash)),
            Value(Coin(1000000L)),
            datumHash
          )
        )
        val transaction = Transaction(
          TransactionBody(
            inputs = TaggedSortedSet.from(Set(input)),
            outputs = IndexedSeq.empty,
            fee = Coin.zero
          ),
          TransactionWitnessSet(
            plutusV3Scripts = TaggedSortedStrictMap(plutusScript),
            plutusData = KeepRaw(
              TaggedSortedMap.from(
                Set(KeepRaw(datum))
              )
            )
          )
        )

        val emulator = TestEmulatorFactory.create(
          utxos = utxo,
          validators = Seq(MissingRequiredDatumsValidator),
          mutators = Seq.empty
        )
        val result = emulator.submitSync(transaction)
        assert(result.isRight)
    }

    test("MissingRequiredDatumsValidator failure with missing datum") {
        val plutusScript = Arbitrary.arbitrary[Script.PlutusV1].sample.get
        val input = Arbitrary.arbitrary[TransactionInput].sample.get
        val datumHash = Arbitrary.arbitrary[DataHash].sample.get
        val utxo = Map(
          input -> Output(
            Address(Network.Testnet, Credential.ScriptHash(plutusScript.scriptHash)),
            Value(Coin(1000000L)),
            datumHash
          )
        )
        val transaction = Transaction(
          TransactionBody(
            inputs = TaggedSortedSet.from(Set(input)),
            outputs = IndexedSeq.empty,
            fee = Coin.zero
          ),
          TransactionWitnessSet(
            plutusV1Scripts = TaggedSortedStrictMap(plutusScript)
          )
        )

        val emulator = TestEmulatorFactory.create(
          utxos = utxo,
          validators = Seq(MissingRequiredDatumsValidator),
          mutators = Seq.empty
        )
        val result = emulator.submitSync(transaction)
        assert(result.isLeft)
    }

    test("MissingRequiredDatumsValidator failure with unspendable UTxO no datum hash") {
        val plutusScript = Arbitrary.arbitrary[Script.PlutusV1].sample.get
        val input = Arbitrary.arbitrary[TransactionInput].sample.get
        val utxo = Map(
          input -> Output(
            Address(Network.Testnet, Credential.ScriptHash(plutusScript.scriptHash)),
            Value(Coin(1000000L))
          )
        )
        val transaction = Transaction(
          TransactionBody(
            inputs = TaggedSortedSet.from(Set(input)),
            outputs = IndexedSeq.empty,
            fee = Coin.zero
          ),
          TransactionWitnessSet(
            plutusV1Scripts = TaggedSortedStrictMap(plutusScript)
          )
        )

        val emulator = TestEmulatorFactory.create(
          utxos = utxo,
          validators = Seq(MissingRequiredDatumsValidator),
          mutators = Seq.empty
        )
        val result = emulator.submitSync(transaction)
        assert(result.isLeft)
    }

    test("MissingRequiredDatumsValidator success with no datum hash for PlutusV3") {
        val plutusScript = Arbitrary.arbitrary[Script.PlutusV3].sample.get
        val input = Arbitrary.arbitrary[TransactionInput].sample.get
        val utxo = Map(
          input -> Output(
            Address(Network.Testnet, Credential.ScriptHash(plutusScript.scriptHash)),
            Value(Coin(1000000L))
          )
        )
        val transaction = Transaction(
          TransactionBody(
            inputs = TaggedSortedSet.from(Set(input)),
            outputs = IndexedSeq.empty,
            fee = Coin.zero
          ),
          TransactionWitnessSet(
            plutusV3Scripts = TaggedSortedStrictMap(plutusScript)
          )
        )

        val emulator = TestEmulatorFactory.create(
          utxos = utxo,
          validators = Seq(MissingRequiredDatumsValidator),
          mutators = Seq.empty
        )
        val result = emulator.submitSync(transaction)
        assert(result.isRight)
    }

    test("MissingRequiredDatumsValidator success with supplemental datum from outputs") {
        val input = Arbitrary.arbitrary[TransactionInput].sample.get
        val datum = Arbitrary.arbitrary[Data].sample.get
        val datumHash = DataHash.fromByteString(datum.dataHash)
        val utxo = Map(
          input -> Output(
            Arbitrary.arbitrary[ShelleyAddress].sample.get,
            Value(Coin(1000000L))
          )
        )
        val transaction = Transaction(
          TransactionBody(
            inputs = TaggedSortedSet.from(Set(input)),
            outputs = IndexedSeq(
              Sized(
                Output(
                  Arbitrary.arbitrary[ShelleyAddress].sample.get,
                  Value(Coin(500000L)),
                  datumHash
                )
              )
            ),
            fee = Coin.zero
          ),
          TransactionWitnessSet(
            plutusData = KeepRaw(
              TaggedSortedMap.from(
                Set(KeepRaw(datum))
              )
            )
          )
        )

        val emulator = TestEmulatorFactory.create(
          utxos = utxo,
          validators = Seq(MissingRequiredDatumsValidator),
          mutators = Seq.empty
        )
        val result = emulator.submitSync(transaction)
        assert(result.isRight)
    }

    test("MissingRequiredDatumsValidator success with supplemental datum from reference inputs") {
        val input = Arbitrary.arbitrary[TransactionInput].sample.get
        val referenceInput = Arbitrary.arbitrary[TransactionInput].sample.get
        val datum = Arbitrary.arbitrary[Data].sample.get
        val datumHash = DataHash.fromByteString(datum.dataHash)
        val utxo = Map(
          input -> Output(
            Arbitrary.arbitrary[ShelleyAddress].sample.get,
            Value(Coin(1000000L))
          ),
          referenceInput -> Output(
            Arbitrary.arbitrary[ShelleyAddress].sample.get,
            Value(Coin(500000L)),
            datumHash
          )
        )
        val transaction = Transaction(
          TransactionBody(
            inputs = TaggedSortedSet.from(Set(input)),
            outputs = IndexedSeq.empty,
            fee = Coin.zero,
            referenceInputs = TaggedSortedSet.from(Set(referenceInput))
          ),
          TransactionWitnessSet(
            plutusData = KeepRaw(
              TaggedSortedMap.from(
                Set(KeepRaw(datum))
              )
            )
          )
        )

        val emulator = TestEmulatorFactory.create(
          utxos = utxo,
          validators = Seq(MissingRequiredDatumsValidator),
          mutators = Seq.empty
        )
        val result = emulator.submitSync(transaction)
        assert(result.isRight)
    }

    test(
      "MissingRequiredDatumsValidator success with supplemental datum from collateralReturnOutput"
    ) {
        val input = Arbitrary.arbitrary[TransactionInput].sample.get
        val datum = Arbitrary.arbitrary[Data].sample.get
        val datumHash = DataHash.fromByteString(datum.dataHash)
        val utxo = Map(
          input -> Output(
            Arbitrary.arbitrary[ShelleyAddress].sample.get,
            Value(Coin(1000000L))
          )
        )
        val collateralReturn = Sized(
          Output(
            Arbitrary.arbitrary[ShelleyAddress].sample.get,
            Value(Coin(500000L)),
            datumHash
          )
        )
        val transaction = Transaction(
          TransactionBody(
            inputs = TaggedSortedSet.from(Set(input)),
            outputs = IndexedSeq.empty,
            fee = Coin.zero,
            collateralReturnOutput = Some(collateralReturn)
          ),
          TransactionWitnessSet(
            plutusData = KeepRaw(
              TaggedSortedMap.from(
                Set(KeepRaw(datum))
              )
            )
          )
        )

        val emulator = TestEmulatorFactory.create(
          utxos = utxo,
          validators = Seq(MissingRequiredDatumsValidator),
          mutators = Seq.empty
        )
        val result = emulator.submitSync(transaction)
        assert(result.isRight)
    }

    test(
      "MissingRequiredDatumsValidator success when datum is used in outputs, reference inputs and collateral return but skipped in witness"
    ) {
        val input = Arbitrary.arbitrary[TransactionInput].sample.get
        val referenceInput = Arbitrary.arbitrary[TransactionInput].sample.get
        val datum = Arbitrary.arbitrary[Data].sample.get
        val datumHash = DataHash.fromByteString(datum.dataHash)
        val utxo = Map(
          input -> Output(
            Arbitrary.arbitrary[ShelleyAddress].sample.get,
            Value(Coin(1000000L))
          ),
          referenceInput -> Output(
            Arbitrary.arbitrary[ShelleyAddress].sample.get,
            Value(Coin(500000L)),
            datumHash
          )
        )
        val collateralReturn = Sized(
          Output(
            Arbitrary.arbitrary[ShelleyAddress].sample.get,
            Value(Coin(300000L)),
            datumHash
          )
        )
        val transaction = Transaction(
          TransactionBody(
            inputs = TaggedSortedSet.from(Set(input)),
            outputs = IndexedSeq(
              Sized(
                Output(
                  Arbitrary.arbitrary[ShelleyAddress].sample.get,
                  Value(Coin(500000L)),
                  datumHash
                )
              )
            ),
            fee = Coin.zero,
            referenceInputs = TaggedSortedSet.from(Set(referenceInput)),
            collateralReturnOutput = Some(collateralReturn)
          )
        )

        val emulator = TestEmulatorFactory.create(
          utxos = utxo,
          validators = Seq(MissingRequiredDatumsValidator),
          mutators = Seq.empty
        )
        val result = emulator.submitSync(transaction)
        assert(result.isRight)
    }

    test("MissingRequiredDatumsValidator failure with not allowed supplemental datum") {
        val input = Arbitrary.arbitrary[TransactionInput].sample.get
        val datum = Arbitrary.arbitrary[Data].sample.get
        val utxo = Map(
          input -> Output(
            Arbitrary.arbitrary[ShelleyAddress].sample.get,
            Value(Coin(1000000L))
          )
        )
        val transaction = Transaction(
          TransactionBody(
            inputs = TaggedSortedSet.from(Set(input)),
            outputs = IndexedSeq.empty,
            fee = Coin.zero
          ),
          TransactionWitnessSet(
            plutusData = KeepRaw(
              TaggedSortedMap.from(
                Set(KeepRaw(datum))
              )
            )
          )
        )

        val emulator = TestEmulatorFactory.create(
          utxos = utxo,
          validators = Seq(MissingRequiredDatumsValidator),
          mutators = Seq.empty
        )
        val result = emulator.submitSync(transaction)
        assert(result.isLeft)
    }

    test("MissingRequiredDatumsValidator success with native script") {
        val (_, publicKey) = TestEmulatorFactory.generateKeyPair()
        val nativeScript =
            Timelock.Signature(Hash(scalus.uplc.builtin.platform.blake2b_224(publicKey)))
        val input = Arbitrary.arbitrary[TransactionInput].sample.get
        val utxo = Map(
          input -> Output(
            Address(Network.Testnet, Credential.ScriptHash(nativeScript.scriptHash)),
            Value(Coin(1000000L))
          )
        )
        val transaction = Transaction(
          TransactionBody(
            inputs = TaggedSortedSet.from(Set(input)),
            outputs = IndexedSeq.empty,
            fee = Coin.zero
          ),
          TransactionWitnessSet(
            nativeScripts = TaggedSortedMap(Script.Native(nativeScript))
          )
        )

        val emulator = TestEmulatorFactory.create(
          utxos = utxo,
          validators = Seq(MissingRequiredDatumsValidator),
          mutators = Seq.empty
        )
        val result = emulator.submitSync(transaction)
        assert(result.isRight)
    }

    // Tests with inline datums
    test("MissingRequiredDatumsValidator success with inline datum in inputs") {
        val plutusScript = Arbitrary.arbitrary[Script.PlutusV3].sample.get
        val input = Arbitrary.arbitrary[TransactionInput].sample.get
        val datum = Arbitrary.arbitrary[Data].sample.get
        val utxo = Map(
          input -> TransactionOutput.Babbage(
            Address(Network.Testnet, Credential.ScriptHash(plutusScript.scriptHash)),
            Value(Coin(1000000L)),
            datumOption = Some(DatumOption.Inline(datum)),
            scriptRef = None
          )
        )
        val transaction = Transaction(
          TransactionBody(
            inputs = TaggedSortedSet.from(Set(input)),
            outputs = IndexedSeq.empty,
            fee = Coin.zero
          ),
          TransactionWitnessSet(
            plutusV3Scripts = TaggedSortedStrictMap(plutusScript)
          )
        )

        val emulator = TestEmulatorFactory.create(
          utxos = utxo,
          validators = Seq(MissingRequiredDatumsValidator),
          mutators = Seq.empty
        )
        val result = emulator.submitSync(transaction)
        assert(result.isRight)
    }
}
