package scalus.cardano.ledger.rules

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.*
import scalus.cardano.node.TestEmulatorFactory
import scalus.uplc.builtin.platform

class VerifiedSignaturesInWitnessesValidatorTest extends AnyFunSuite with ArbitraryInstances {

    test("VerifiedSignaturesInWitnessesValidator VkeyWitnesses rule success") {
        val (privateKey1, publicKey1) = TestEmulatorFactory.generateKeyPair()
        val (privateKey2, publicKey2) = TestEmulatorFactory.generateKeyPair()
        val (privateKey3, publicKey3) = TestEmulatorFactory.generateKeyPair()
        val tx = randomTransactionWithIsValidField
        val transaction = tx.withWitness(
          _.copy(
            vkeyWitnesses = TaggedSortedSet(
              VKeyWitness(publicKey1, platform.signEd25519(privateKey1, tx.id)),
              VKeyWitness(publicKey2, platform.signEd25519(privateKey2, tx.id)),
              VKeyWitness(publicKey3, platform.signEd25519(privateKey3, tx.id))
            ),
            bootstrapWitnesses = TaggedSortedSet.empty
          )
        )

        val emulator = TestEmulatorFactory.create(
          validators = Seq(VerifiedSignaturesInWitnessesValidator),
          mutators = Seq.empty
        )
        val result = emulator.submitSync(transaction)
        assert(result.isRight)
    }

    test("VerifiedSignaturesInWitnessesValidator VkeyWitnesses rule failure") {
        val (privateKey1, publicKey1) = TestEmulatorFactory.generateKeyPair()
        val (privateKey2, publicKey2) = TestEmulatorFactory.generateKeyPair()
        val (privateKey3, publicKey3) = TestEmulatorFactory.generateKeyPair()
        val tx = randomTransactionWithIsValidField
        val transaction = tx.withWitness(
          _.copy(
            vkeyWitnesses = TaggedSortedSet(
              VKeyWitness(publicKey1, platform.signEd25519(privateKey1, tx.id)),
              VKeyWitness(publicKey2, platform.signEd25519(privateKey2, tx.id)),
              VKeyWitness(
                publicKey3, {
                    val signature = platform.signEd25519(privateKey3, tx.id)
                    signature.bytes(0) =
                        (signature.bytes(0) + 1).toByte // Intentionally corrupt the signature
                    signature
                }
              )
            ),
            bootstrapWitnesses = TaggedSortedSet.empty
          )
        )

        val emulator = TestEmulatorFactory.create(
          validators = Seq(VerifiedSignaturesInWitnessesValidator),
          mutators = Seq.empty
        )
        val result = emulator.submitSync(transaction)
        assert(result.isLeft)
    }

    test("VerifiedSignaturesInWitnessesValidator BootstrapWitnesses rule success") {
        val (privateKey1, publicKey1) = TestEmulatorFactory.generateKeyPair()
        val (privateKey2, publicKey2) = TestEmulatorFactory.generateKeyPair()
        val (privateKey3, publicKey3) = TestEmulatorFactory.generateKeyPair()
        val tx = randomTransactionWithIsValidField
        val transaction = tx.withWitness(
          _.copy(
            vkeyWitnesses = TaggedSortedSet.empty,
            bootstrapWitnesses = TaggedSortedSet(
              BootstrapWitness(
                publicKey1,
                platform.signEd25519(privateKey1, tx.id),
                genByteStringOfN(32).sample.get,
                genByteStringOfN(32).sample.get
              ),
              BootstrapWitness(
                publicKey2,
                platform.signEd25519(privateKey2, tx.id),
                genByteStringOfN(32).sample.get,
                genByteStringOfN(32).sample.get
              ),
              BootstrapWitness(
                publicKey3,
                platform.signEd25519(privateKey3, tx.id),
                genByteStringOfN(32).sample.get,
                genByteStringOfN(32).sample.get
              )
            )
          )
        )

        val emulator = TestEmulatorFactory.create(
          validators = Seq(VerifiedSignaturesInWitnessesValidator),
          mutators = Seq.empty
        )
        val result = emulator.submitSync(transaction)
        assert(result.isRight)
    }

    test("VerifiedSignaturesInWitnessesValidator BootstrapWitnesses rule failure") {
        val (privateKey1, publicKey1) = TestEmulatorFactory.generateKeyPair()
        val (privateKey2, publicKey2) = TestEmulatorFactory.generateKeyPair()
        val (privateKey3, publicKey3) = TestEmulatorFactory.generateKeyPair()
        val tx = randomTransactionWithIsValidField
        val transaction = tx.withWitness(
          _.copy(
            vkeyWitnesses = TaggedSortedSet.empty,
            bootstrapWitnesses = TaggedSortedSet(
              BootstrapWitness(
                publicKey1,
                platform.signEd25519(privateKey1, tx.id),
                genByteStringOfN(32).sample.get,
                genByteStringOfN(32).sample.get
              ),
              BootstrapWitness(
                publicKey2,
                platform.signEd25519(privateKey2, tx.id),
                genByteStringOfN(32).sample.get,
                genByteStringOfN(32).sample.get
              ),
              BootstrapWitness(
                publicKey3, {
                    val signature = platform.signEd25519(privateKey3, tx.id)
                    signature.bytes(0) =
                        (signature.bytes(0) + 1).toByte // Intentionally corrupt the signature
                    signature
                },
                genByteStringOfN(32).sample.get,
                genByteStringOfN(32).sample.get
              )
            )
          )
        )

        val emulator = TestEmulatorFactory.create(
          validators = Seq(VerifiedSignaturesInWitnessesValidator),
          mutators = Seq.empty
        )
        val result = emulator.submitSync(transaction)
        assert(result.isLeft)
    }
}
