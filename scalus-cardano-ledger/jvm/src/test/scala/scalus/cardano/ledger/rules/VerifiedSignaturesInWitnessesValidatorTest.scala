package scalus.cardano.ledger
package rules

import scalus.builtin.platform
import org.scalatest.funsuite.AnyFunSuite

class VerifiedSignaturesInWitnessesValidatorTest extends AnyFunSuite, ValidatorRulesTestKit {
    test("VerifiedSignaturesInWitnessesValidator VkeyWitnesses rule success") {
        val context = Context()
        val transaction = {
            val (privateKey1, publicKey1) = generateKeyPair()
            val (privateKey2, publicKey2) = generateKeyPair()
            val (privateKey3, publicKey3) = generateKeyPair()
            val tx = randomValidTransaction
            tx.copy(
              witnessSet = tx.witnessSet.copy(
                vkeyWitnesses = TaggedSortedSet(
                  VKeyWitness(publicKey1, platform.signEd25519(privateKey1, tx.id)),
                  VKeyWitness(publicKey2, platform.signEd25519(privateKey2, tx.id)),
                  VKeyWitness(publicKey3, platform.signEd25519(privateKey3, tx.id))
                ),
                bootstrapWitnesses = TaggedSortedSet.empty
              )
            )
        }
        val state = State()

        val result = VerifiedSignaturesInWitnessesValidator.validate(context, state, transaction)
        assert(result.isRight)
    }

    test("VerifiedSignaturesInWitnessesValidator VkeyWitnesses rule failure") {
        val context = Context()
        val transaction = {
            val (privateKey1, publicKey1) = generateKeyPair()
            val (privateKey2, publicKey2) = generateKeyPair()
            val (privateKey3, publicKey3) = generateKeyPair()
            val tx = randomValidTransaction
            tx.copy(
              witnessSet = tx.witnessSet.copy(
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
        }
        val state = State()

        val result = VerifiedSignaturesInWitnessesValidator.validate(context, state, transaction)
        assert(result.isLeft)
    }

    test("VerifiedSignaturesInWitnessesValidator BootstrapWitnesses rule success") {
        val context = Context()
        val transaction = {
            val (privateKey1, publicKey1) = generateKeyPair()
            val (privateKey2, publicKey2) = generateKeyPair()
            val (privateKey3, publicKey3) = generateKeyPair()
            val tx = randomValidTransaction
            tx.copy(
              witnessSet = tx.witnessSet.copy(
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
        }
        val state = State()

        val result = VerifiedSignaturesInWitnessesValidator.validate(context, state, transaction)
        assert(result.isRight)
    }

    test("VerifiedSignaturesInWitnessesValidator BootstrapWitnesses rule failure") {
        val context = Context()
        val transaction = {
            val (privateKey1, publicKey1) = generateKeyPair()
            val (privateKey2, publicKey2) = generateKeyPair()
            val (privateKey3, publicKey3) = generateKeyPair()
            val tx = randomValidTransaction
            tx.copy(
              witnessSet = tx.witnessSet.copy(
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
        }
        val state = State()

        val result = VerifiedSignaturesInWitnessesValidator.validate(context, state, transaction)
        assert(result.isLeft)
    }
}
