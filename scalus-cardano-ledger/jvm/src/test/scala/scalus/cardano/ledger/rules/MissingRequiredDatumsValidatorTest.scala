package scalus.cardano.ledger
package rules

import org.scalacheck.Arbitrary
import org.scalatest.funsuite.AnyFunSuite
import scalus.builtin.Data
import scalus.cardano.address.{Address, Network, ShelleyAddress}
import TransactionWitnessSet.given

class MissingRequiredDatumsValidatorTest extends AnyFunSuite, ValidatorRulesTestKit {
    test("MissingRequiredDatumsValidator success with no scripts") {
        val input = Arbitrary.arbitrary[TransactionInput].sample.get
        val utxo = Map(
          input -> TransactionOutput(
            Arbitrary.arbitrary[ShelleyAddress].sample.get,
            Value(Coin(1000000L))
          )
        )
        val transaction = Transaction(
          body = KeepRaw(
            TransactionBody(
              inputs = TaggedSortedSet.from(Set(input)),
              outputs = IndexedSeq.empty,
              fee = Coin.zero
            )
          ),
          witnessSet = TransactionWitnessSet()
        )
        val context = Context()
        val state = State(utxos = utxo)

        val result = MissingRequiredDatumsValidator.validate(context, state, transaction)

        assert(result.isRight)
    }

    test("MissingRequiredDatumsValidator success with matching datum") {
        val plutusScript = Arbitrary.arbitrary[Script.PlutusV3].sample.get
        val input = Arbitrary.arbitrary[TransactionInput].sample.get
        val datum = Arbitrary.arbitrary[Data].sample.get
        val datumHash = DataHash.fromByteString(datum.dataHash)
        val utxo = Map(
          input -> TransactionOutput(
            Address(Network.Testnet, Credential.ScriptHash(plutusScript.scriptHash)),
            Value(Coin(1000000L)),
            datumHash
          )
        )
        val transaction = Transaction(
          body = KeepRaw(
            TransactionBody(
              inputs = TaggedSortedSet.from(Set(input)),
              outputs = IndexedSeq.empty,
              fee = Coin.zero
            )
          ),
          witnessSet = TransactionWitnessSet(
            plutusV3Scripts = TaggedSortedStrictMap(plutusScript),
            plutusData = KeepRaw(
              TaggedSortedMap.from(
                Set(KeepRaw(datum))
              )
            )
          )
        )
        val context = Context()
        val state = State(utxos = utxo)

        val result = MissingRequiredDatumsValidator.validate(context, state, transaction)
        assert(result.isRight)
    }

    test("MissingRequiredDatumsValidator failure with missing datum") {
        val plutusScript = Arbitrary.arbitrary[Script.PlutusV1].sample.get
        val input = Arbitrary.arbitrary[TransactionInput].sample.get
        val datumHash = Arbitrary.arbitrary[DataHash].sample.get
        val utxo = Map(
          input -> TransactionOutput(
            Address(Network.Testnet, Credential.ScriptHash(plutusScript.scriptHash)),
            Value(Coin(1000000L)),
            datumHash
          )
        )
        val transaction = Transaction(
          body = KeepRaw(
            TransactionBody(
              inputs = TaggedSortedSet.from(Set(input)),
              outputs = IndexedSeq.empty,
              fee = Coin.zero
            )
          ),
          witnessSet = TransactionWitnessSet(
            plutusV1Scripts = TaggedSortedStrictMap(plutusScript)
          )
        )
        val context = Context()
        val state = State(utxos = utxo)

        val result = MissingRequiredDatumsValidator.validate(context, state, transaction)
        assert(result.isLeft)
    }

    test("MissingRequiredDatumsValidator failure with unspendable UTxO no datum hash") {
        val plutusScript = Arbitrary.arbitrary[Script.PlutusV1].sample.get
        val input = Arbitrary.arbitrary[TransactionInput].sample.get
        val utxo = Map(
          input -> TransactionOutput(
            Address(Network.Testnet, Credential.ScriptHash(plutusScript.scriptHash)),
            Value(Coin(1000000L))
          )
        )
        val transaction = Transaction(
          body = KeepRaw(
            TransactionBody(
              inputs = TaggedSortedSet.from(Set(input)),
              outputs = IndexedSeq.empty,
              fee = Coin.zero
            )
          ),
          witnessSet = TransactionWitnessSet(
            plutusV1Scripts = TaggedSortedStrictMap(plutusScript)
          )
        )
        val context = Context()
        val state = State(utxos = utxo)

        val result = MissingRequiredDatumsValidator.validate(context, state, transaction)
        assert(result.isLeft)
    }

    test("MissingRequiredDatumsValidator success with no datum hash for PlutusV3") {
        val plutusScript = Arbitrary.arbitrary[Script.PlutusV3].sample.get
        val input = Arbitrary.arbitrary[TransactionInput].sample.get
        val utxo = Map(
          input -> TransactionOutput(
            Address(Network.Testnet, Credential.ScriptHash(plutusScript.scriptHash)),
            Value(Coin(1000000L))
          )
        )
        val transaction = Transaction(
          body = KeepRaw(
            TransactionBody(
              inputs = TaggedSortedSet.from(Set(input)),
              outputs = IndexedSeq.empty,
              fee = Coin.zero
            )
          ),
          witnessSet = TransactionWitnessSet(
            plutusV3Scripts = TaggedSortedStrictMap(plutusScript)
          )
        )
        val context = Context()
        val state = State(utxos = utxo)

        val result = MissingRequiredDatumsValidator.validate(context, state, transaction)
        assert(result.isRight)
    }

    test("MissingRequiredDatumsValidator success with supplemental datum from outputs") {
        val input = Arbitrary.arbitrary[TransactionInput].sample.get
        val datum = Arbitrary.arbitrary[Data].sample.get
        val datumHash = DataHash.fromByteString(datum.dataHash)
        val utxo = Map(
          input -> TransactionOutput(
            Arbitrary.arbitrary[ShelleyAddress].sample.get,
            Value(Coin(1000000L))
          )
        )
        val transaction = Transaction(
          body = KeepRaw(
            TransactionBody(
              inputs = TaggedSortedSet.from(Set(input)),
              outputs = IndexedSeq(
                Sized(
                  TransactionOutput(
                    Arbitrary.arbitrary[ShelleyAddress].sample.get,
                    Value(Coin(500000L)),
                    datumHash
                  )
                )
              ),
              fee = Coin.zero
            )
          ),
          witnessSet = TransactionWitnessSet(
            plutusData = KeepRaw(
              TaggedSortedMap.from(
                Set(KeepRaw(datum))
              )
            )
          )
        )
        val context = Context()
        val state = State(utxos = utxo)

        val result = MissingRequiredDatumsValidator.validate(context, state, transaction)

        assert(result.isRight)
    }

    test("MissingRequiredDatumsValidator success with supplemental datum from reference inputs") {
        val input = Arbitrary.arbitrary[TransactionInput].sample.get
        val referenceInput = Arbitrary.arbitrary[TransactionInput].sample.get
        val datum = Arbitrary.arbitrary[Data].sample.get
        val datumHash = DataHash.fromByteString(datum.dataHash)
        val utxo = Map(
          input -> TransactionOutput(
            Arbitrary.arbitrary[ShelleyAddress].sample.get,
            Value(Coin(1000000L))
          ),
          referenceInput -> TransactionOutput(
            Arbitrary.arbitrary[ShelleyAddress].sample.get,
            Value(Coin(500000L)),
            datumHash
          )
        )
        val transaction = Transaction(
          body = KeepRaw(
            TransactionBody(
              inputs = TaggedSortedSet.from(Set(input)),
              outputs = IndexedSeq.empty,
              fee = Coin.zero,
              referenceInputs = TaggedSortedSet.from(Set(referenceInput))
            )
          ),
          witnessSet = TransactionWitnessSet(
            plutusData = KeepRaw(
              TaggedSortedMap.from(
                Set(KeepRaw(datum))
              )
            )
          )
        )
        val context = Context()
        val state = State(utxos = utxo)

        val result = MissingRequiredDatumsValidator.validate(context, state, transaction)

        assert(result.isRight)
    }

    test(
      "MissingRequiredDatumsValidator success when datum is used in both outputs and reference inputs but skipped in witness"
    ) {
        val input = Arbitrary.arbitrary[TransactionInput].sample.get
        val referenceInput = Arbitrary.arbitrary[TransactionInput].sample.get
        val datum = Arbitrary.arbitrary[Data].sample.get
        val datumHash = DataHash.fromByteString(datum.dataHash)
        val utxo = Map(
          input -> TransactionOutput(
            Arbitrary.arbitrary[ShelleyAddress].sample.get,
            Value(Coin(1000000L))
          ),
          referenceInput -> TransactionOutput(
            Arbitrary.arbitrary[ShelleyAddress].sample.get,
            Value(Coin(500000L)),
            datumHash
          )
        )
        val transaction = Transaction(
          body = KeepRaw(
            TransactionBody(
              inputs = TaggedSortedSet.from(Set(input)),
              outputs = IndexedSeq(
                Sized(
                  TransactionOutput(
                    Arbitrary.arbitrary[ShelleyAddress].sample.get,
                    Value(Coin(500000L)),
                    datumHash
                  )
                )
              ),
              fee = Coin.zero,
              referenceInputs = TaggedSortedSet.from(Set(referenceInput))
            )
          ),
          witnessSet = TransactionWitnessSet.empty
        )
        val context = Context()
        val state = State(utxos = utxo)

        val result = MissingRequiredDatumsValidator.validate(context, state, transaction)

        assert(result.isRight)
    }

    test("MissingRequiredDatumsValidator failure with not allowed supplemental datum") {
        val input = Arbitrary.arbitrary[TransactionInput].sample.get
        val datum = Arbitrary.arbitrary[Data].sample.get
        val utxo = Map(
          input -> TransactionOutput(
            Arbitrary.arbitrary[ShelleyAddress].sample.get,
            Value(Coin(1000000L))
          )
        )
        val transaction = Transaction(
          body = KeepRaw(
            TransactionBody(
              inputs = TaggedSortedSet.from(Set(input)),
              outputs = IndexedSeq.empty,
              fee = Coin.zero
            )
          ),
          witnessSet = TransactionWitnessSet(
            plutusData = KeepRaw(
              TaggedSortedMap.from(
                Set(KeepRaw(datum))
              )
            )
          )
        )
        val context = Context()
        val state = State(utxos = utxo)

        val result = MissingRequiredDatumsValidator.validate(context, state, transaction)
        assert(result.isLeft)
    }

    test("MissingRequiredDatumsValidator success with native script") {
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
          body = KeepRaw(
            TransactionBody(
              inputs = TaggedSortedSet.from(Set(input)),
              outputs = IndexedSeq.empty,
              fee = Coin.zero
            )
          ),
          witnessSet = TransactionWitnessSet(
            nativeScripts = TaggedSortedMap(Script.Native(nativeScript))
          )
        )
        val context = Context()
        val state = State(utxos = utxo)

        val result = MissingRequiredDatumsValidator.validate(context, state, transaction)

        assert(result.isRight)
    }
}
