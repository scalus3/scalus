package scalus.cardano.ledger.rules

import org.scalacheck.Arbitrary
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.{Address, Network, ShelleyAddress}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.TransactionWitnessSet.given
import scalus.cardano.node.TestEmulatorFactory
import scalus.uplc.builtin.platform

class NativeScriptsValidatorTest extends AnyFunSuite with ArbitraryInstances {

    test("NativeScriptsValidator success with no native scripts") {
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
            referenceInputs = TaggedSortedSet.empty,
            outputs = IndexedSeq.empty,
            fee = Coin.zero
          )
        )

        val emulator = TestEmulatorFactory.create(
          utxos = utxo,
          validators = Seq(NativeScriptsValidator),
          mutators = Seq.empty
        )
        val result = emulator.submitSync(transaction)
        assert(result.isRight)
    }

    test("NativeScriptsValidator success with valid Signature native script") {
        val (privateKey, publicKey) = TestEmulatorFactory.generateKeyPair()
        val hash = AddrKeyHash(platform.blake2b_224(publicKey))
        val nativeScript = Timelock.Signature(hash)

        val input = Arbitrary.arbitrary[TransactionInput].sample.get
        val utxo = Map(
          input -> Output(
            Address(Network.Testnet, Credential.ScriptHash(nativeScript.scriptHash)),
            Value(Coin(1000000L))
          )
        )

        val transaction = {
            val tx = randomTransactionWithIsValidField
            tx.copy(
              body = KeepRaw(
                tx.body.value.copy(
                  inputs = TaggedSortedSet.from(Set(input)),
                  referenceInputs = TaggedSortedSet.empty,
                  outputs = IndexedSeq.empty,
                  fee = Coin.zero
                )
              ),
              witnessSetRaw = KeepRaw(
                tx.witnessSet.copy(
                  vkeyWitnesses = TaggedSortedSet(
                    VKeyWitness(publicKey, platform.signEd25519(privateKey, tx.id))
                  ),
                  nativeScripts = TaggedSortedMap(Script.Native(nativeScript))
                )
              )
            )
        }

        val emulator = TestEmulatorFactory.create(
          utxos = utxo,
          validators = Seq(NativeScriptsValidator),
          mutators = Seq.empty
        )
        val result = emulator.submitSync(transaction)
        assert(result.isRight)
    }

    test("NativeScriptsValidator failure with invalid Signature native script") {
        val (_, publicKey) = TestEmulatorFactory.generateKeyPair()
        val hash = AddrKeyHash(platform.blake2b_224(publicKey))
        val nativeScript = Timelock.Signature(hash)

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
            referenceInputs = TaggedSortedSet.empty,
            outputs = IndexedSeq.empty,
            fee = Coin.zero
          ),
          TransactionWitnessSet(
            nativeScripts = TaggedSortedMap(Script.Native(nativeScript))
          )
        )

        val emulator = TestEmulatorFactory.create(
          utxos = utxo,
          validators = Seq(NativeScriptsValidator),
          mutators = Seq.empty
        )
        val result = emulator.submitSync(transaction)
        assert(result.isLeft)
    }

    test("NativeScriptsValidator success with AllOf native script") {
        val (privateKey1, publicKey1) = TestEmulatorFactory.generateKeyPair()
        val (privateKey2, publicKey2) = TestEmulatorFactory.generateKeyPair()
        val hash1 = AddrKeyHash(platform.blake2b_224(publicKey1))
        val hash2 = AddrKeyHash(platform.blake2b_224(publicKey2))

        val nativeScript = Timelock.AllOf(
          IndexedSeq(
            Timelock.Signature(hash1),
            Timelock.Signature(hash2)
          )
        )

        val input = Arbitrary.arbitrary[TransactionInput].sample.get
        val utxo = Map(
          input -> Output(
            Address(Network.Testnet, Credential.ScriptHash(nativeScript.scriptHash)),
            Value(Coin(1000000L))
          )
        )

        val transaction = {
            val tx = randomTransactionWithIsValidField
            tx.copy(
              body = KeepRaw(
                tx.body.value.copy(
                  inputs = TaggedSortedSet.from(Set(input)),
                  referenceInputs = TaggedSortedSet.empty,
                  outputs = IndexedSeq.empty,
                  fee = Coin.zero
                )
              ),
              witnessSetRaw = KeepRaw(
                tx.witnessSet.copy(
                  vkeyWitnesses = TaggedSortedSet(
                    VKeyWitness(publicKey1, platform.signEd25519(privateKey1, tx.id)),
                    VKeyWitness(publicKey2, platform.signEd25519(privateKey2, tx.id))
                  ),
                  nativeScripts = TaggedSortedMap(Script.Native(nativeScript))
                )
              )
            )
        }

        val emulator = TestEmulatorFactory.create(
          utxos = utxo,
          validators = Seq(NativeScriptsValidator),
          mutators = Seq.empty
        )
        val result = emulator.submitSync(transaction)
        assert(result.isRight)
    }

    test("NativeScriptsValidator failure with AllOf native script missing one signature") {
        val (privateKey1, publicKey1) = TestEmulatorFactory.generateKeyPair()
        val (_, publicKey2) = TestEmulatorFactory.generateKeyPair()
        val hash1 = AddrKeyHash(platform.blake2b_224(publicKey1))
        val hash2 = AddrKeyHash(platform.blake2b_224(publicKey2))

        val nativeScript = Timelock.AllOf(
          IndexedSeq(
            Timelock.Signature(hash1),
            Timelock.Signature(hash2)
          )
        )

        val input = Arbitrary.arbitrary[TransactionInput].sample.get
        val utxo = Map(
          input -> Output(
            Address(Network.Testnet, Credential.ScriptHash(nativeScript.scriptHash)),
            Value(Coin(1000000L))
          )
        )

        val transaction = {
            val tx = randomTransactionWithIsValidField
            tx.copy(
              body = KeepRaw(
                tx.body.value.copy(
                  inputs = TaggedSortedSet.from(Set(input)),
                  referenceInputs = TaggedSortedSet.empty,
                  outputs = IndexedSeq.empty,
                  fee = Coin.zero
                )
              ),
              witnessSetRaw = KeepRaw(
                tx.witnessSet.copy(
                  vkeyWitnesses = TaggedSortedSet(
                    VKeyWitness(publicKey1, platform.signEd25519(privateKey1, tx.id))
                  ),
                  nativeScripts = TaggedSortedMap(Script.Native(nativeScript))
                )
              )
            )
        }

        val emulator = TestEmulatorFactory.create(
          utxos = utxo,
          validators = Seq(NativeScriptsValidator),
          mutators = Seq.empty
        )
        val result = emulator.submitSync(transaction)
        assert(result.isLeft)
    }

    test("NativeScriptsValidator success with AnyOf native script") {
        val (privateKey1, publicKey1) = TestEmulatorFactory.generateKeyPair()
        val (_, publicKey2) = TestEmulatorFactory.generateKeyPair()
        val hash1 = AddrKeyHash(platform.blake2b_224(publicKey1))
        val hash2 = AddrKeyHash(platform.blake2b_224(publicKey2))

        val nativeScript = Timelock.AnyOf(
          IndexedSeq(
            Timelock.Signature(hash1),
            Timelock.Signature(hash2)
          )
        )

        val input = Arbitrary.arbitrary[TransactionInput].sample.get
        val utxo = Map(
          input -> Output(
            Address(Network.Testnet, Credential.ScriptHash(nativeScript.scriptHash)),
            Value(Coin(1000000L))
          )
        )

        val transaction = {
            val tx = randomTransactionWithIsValidField
            tx.copy(
              body = KeepRaw(
                tx.body.value.copy(
                  inputs = TaggedSortedSet.from(Set(input)),
                  referenceInputs = TaggedSortedSet.empty,
                  outputs = IndexedSeq.empty,
                  fee = Coin.zero
                )
              ),
              witnessSetRaw = KeepRaw(
                tx.witnessSet.copy(
                  vkeyWitnesses = TaggedSortedSet(
                    VKeyWitness(publicKey1, platform.signEd25519(privateKey1, tx.id))
                  ),
                  nativeScripts = TaggedSortedMap(Script.Native(nativeScript))
                )
              )
            )
        }

        val emulator = TestEmulatorFactory.create(
          utxos = utxo,
          validators = Seq(NativeScriptsValidator),
          mutators = Seq.empty
        )
        val result = emulator.submitSync(transaction)
        assert(result.isRight)
    }

    test("NativeScriptsValidator failure with AnyOf native script no matching signatures") {
        val (_, publicKey1) = TestEmulatorFactory.generateKeyPair()
        val (_, publicKey2) = TestEmulatorFactory.generateKeyPair()
        val hash1 = AddrKeyHash(platform.blake2b_224(publicKey1))
        val hash2 = AddrKeyHash(platform.blake2b_224(publicKey2))

        val nativeScript = Timelock.AnyOf(
          IndexedSeq(
            Timelock.Signature(hash1),
            Timelock.Signature(hash2)
          )
        )

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
            referenceInputs = TaggedSortedSet.empty,
            outputs = IndexedSeq.empty,
            fee = Coin.zero
          ),
          TransactionWitnessSet(
            nativeScripts = TaggedSortedMap(Script.Native(nativeScript))
          )
        )

        val emulator = TestEmulatorFactory.create(
          utxos = utxo,
          validators = Seq(NativeScriptsValidator),
          mutators = Seq.empty
        )
        val result = emulator.submitSync(transaction)
        assert(result.isLeft)
    }

    test("NativeScriptsValidator success with MOf native script") {
        val (privateKey1, publicKey1) = TestEmulatorFactory.generateKeyPair()
        val (privateKey2, publicKey2) = TestEmulatorFactory.generateKeyPair()
        val (_, publicKey3) = TestEmulatorFactory.generateKeyPair()
        val hash1 = AddrKeyHash(platform.blake2b_224(publicKey1))
        val hash2 = AddrKeyHash(platform.blake2b_224(publicKey2))
        val hash3 = AddrKeyHash(platform.blake2b_224(publicKey3))

        val nativeScript = Timelock.MOf(
          2,
          IndexedSeq(
            Timelock.Signature(hash1),
            Timelock.Signature(hash2),
            Timelock.Signature(hash3)
          )
        )

        val input = Arbitrary.arbitrary[TransactionInput].sample.get
        val utxo = Map(
          input -> Output(
            Address(Network.Testnet, Credential.ScriptHash(nativeScript.scriptHash)),
            Value(Coin(1000000L))
          )
        )

        val transaction = {
            val tx = randomTransactionWithIsValidField
            tx.copy(
              body = KeepRaw(
                tx.body.value.copy(
                  inputs = TaggedSortedSet.from(Set(input)),
                  referenceInputs = TaggedSortedSet.empty,
                  outputs = IndexedSeq.empty,
                  fee = Coin.zero
                )
              ),
              witnessSetRaw = KeepRaw(
                tx.witnessSet.copy(
                  vkeyWitnesses = TaggedSortedSet(
                    VKeyWitness(publicKey1, platform.signEd25519(privateKey1, tx.id)),
                    VKeyWitness(publicKey2, platform.signEd25519(privateKey2, tx.id))
                  ),
                  nativeScripts = TaggedSortedMap(Script.Native(nativeScript))
                )
              )
            )
        }

        val emulator = TestEmulatorFactory.create(
          utxos = utxo,
          validators = Seq(NativeScriptsValidator),
          mutators = Seq.empty
        )
        val result = emulator.submitSync(transaction)
        assert(result.isRight)
    }

    test("NativeScriptsValidator failure with MOf native script insufficient signatures") {
        val (privateKey1, publicKey1) = TestEmulatorFactory.generateKeyPair()
        val (_, publicKey2) = TestEmulatorFactory.generateKeyPair()
        val (_, publicKey3) = TestEmulatorFactory.generateKeyPair()
        val hash1 = AddrKeyHash(platform.blake2b_224(publicKey1))
        val hash2 = AddrKeyHash(platform.blake2b_224(publicKey2))
        val hash3 = AddrKeyHash(platform.blake2b_224(publicKey3))

        val nativeScript = Timelock.MOf(
          2,
          IndexedSeq(
            Timelock.Signature(hash1),
            Timelock.Signature(hash2),
            Timelock.Signature(hash3)
          )
        )

        val input = Arbitrary.arbitrary[TransactionInput].sample.get
        val utxo = Map(
          input -> Output(
            Address(Network.Testnet, Credential.ScriptHash(nativeScript.scriptHash)),
            Value(Coin(1000000L))
          )
        )

        val transaction = {
            val tx = randomTransactionWithIsValidField
            tx.copy(
              body = KeepRaw(
                tx.body.value.copy(
                  inputs = TaggedSortedSet.from(Set(input)),
                  referenceInputs = TaggedSortedSet.empty,
                  outputs = IndexedSeq.empty,
                  fee = Coin.zero
                )
              ),
              witnessSetRaw = KeepRaw(
                tx.witnessSet.copy(
                  vkeyWitnesses = TaggedSortedSet(
                    VKeyWitness(publicKey1, platform.signEd25519(privateKey1, tx.id))
                  ),
                  nativeScripts = TaggedSortedMap(Script.Native(nativeScript))
                )
              )
            )
        }

        val emulator = TestEmulatorFactory.create(
          utxos = utxo,
          validators = Seq(NativeScriptsValidator),
          mutators = Seq.empty
        )
        val result = emulator.submitSync(transaction)
        assert(result.isLeft)
    }

    test("NativeScriptsValidator success with TimeStart script within validity interval") {
        val nativeScript = Timelock.TimeStart(100L)

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
            referenceInputs = TaggedSortedSet.empty,
            outputs = IndexedSeq.empty,
            fee = Coin.zero,
            validityStartSlot = Some(150L)
          ),
          TransactionWitnessSet(
            nativeScripts = TaggedSortedMap(Script.Native(nativeScript))
          )
        )

        val emulator = TestEmulatorFactory.create(
          utxos = utxo,
          validators = Seq(NativeScriptsValidator),
          mutators = Seq.empty
        )
        val result = emulator.submitSync(transaction)
        assert(result.isRight)
    }

    test("NativeScriptsValidator failure with TimeStart script outside validity interval") {
        val nativeScript = Timelock.TimeStart(100L)

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
            fee = Coin.zero,
            validityStartSlot = Some(50L)
          ),
          TransactionWitnessSet(
            nativeScripts = TaggedSortedMap(Script.Native(nativeScript))
          )
        )

        val emulator = TestEmulatorFactory.create(
          utxos = utxo,
          validators = Seq(NativeScriptsValidator),
          mutators = Seq.empty
        )
        val result = emulator.submitSync(transaction)
        assert(result.isLeft)
    }

    test("NativeScriptsValidator success with TimeExpire script within validity interval") {
        val nativeScript = Timelock.TimeExpire(200L)

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
            fee = Coin.zero,
            ttl = Some(150L)
          ),
          TransactionWitnessSet(
            nativeScripts = TaggedSortedMap(Script.Native(nativeScript))
          )
        )

        val emulator = TestEmulatorFactory.create(
          utxos = utxo,
          validators = Seq(NativeScriptsValidator),
          mutators = Seq.empty
        )
        val result = emulator.submitSync(transaction)
        assert(result.isRight)
    }

    test("NativeScriptsValidator failure with TimeExpire script outside validity interval") {
        val nativeScript = Timelock.TimeExpire(100L)

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
            fee = Coin.zero,
            ttl = Some(150L)
          ),
          TransactionWitnessSet(
            nativeScripts = TaggedSortedMap(Script.Native(nativeScript))
          )
        )

        val emulator = TestEmulatorFactory.create(
          utxos = utxo,
          validators = Seq(NativeScriptsValidator),
          mutators = Seq.empty
        )
        val result = emulator.submitSync(transaction)
        assert(result.isLeft)
    }

    test("NativeScriptsValidator success with empty AllOf script") {
        val nativeScript = Timelock.AllOf(IndexedSeq.empty)

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
          validators = Seq(NativeScriptsValidator),
          mutators = Seq.empty
        )
        val result = emulator.submitSync(transaction)
        assert(result.isRight)
    }

    test("NativeScriptsValidator failure with empty AnyOf script") {
        val nativeScript = Timelock.AnyOf(IndexedSeq.empty)

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
          validators = Seq(NativeScriptsValidator),
          mutators = Seq.empty
        )
        val result = emulator.submitSync(transaction)
        assert(result.isLeft)
    }

    test("NativeScriptsValidator success with MOf script where m=0") {
        val (_, publicKey) = TestEmulatorFactory.generateKeyPair()
        val hash = AddrKeyHash(platform.blake2b_224(publicKey))
        val nativeScript = Timelock.MOf(0, IndexedSeq(Timelock.Signature(hash)))

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
          validators = Seq(NativeScriptsValidator),
          mutators = Seq.empty
        )
        val result = emulator.submitSync(transaction)
        assert(result.isRight)
    }
}
