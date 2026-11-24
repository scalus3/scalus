package scalus.cardano.ledger
package rules

import org.scalatest.funsuite.AnyFunSuite
import scalus.testing.kit.Party
import scalus.testing.kit.Party.{Alice, Bob, Charles}
import scalus.uplc.builtin.ByteString

class VerifiedSignaturesInWitnessesValidatorTest extends AnyFunSuite, ValidatorRulesTestKit {

    private val signers = Seq(Alice, Bob, Charles)

    // The rule reads only the witnesses, so any fixed body serves.
    private val unsigned = txSpending(input(0))(TransactionWitnessSet.empty)

    private def key(party: Party): ByteString = party.account.paymentKeyPair.verificationKey

    private def signature(party: Party): ByteString =
        party.account.paymentKeyPair.sign(unsigned.id)

    private def corrupt(signature: ByteString): ByteString = {
        val bytes = signature.bytes.clone()
        bytes(0) = (bytes(0) + 1).toByte
        ByteString.fromArray(bytes)
    }

    // A bootstrap witness checks only the Ed25519 signature; the chain code and the Byron
    // attributes do not reach this rule, so fixed bytes serve.
    private def bootstrap(party: Party, sig: ByteString): BootstrapWitness =
        BootstrapWitness(
          key(party),
          sig,
          ByteString.fromHex("cc" * 32),
          ByteString.fromHex("aa" * 32)
        )

    private def withVkeys(witnesses: VKeyWitness*): Transaction =
        unsigned.withWitness(
          _.copy(
            vkeyWitnesses = TaggedSortedSet(witnesses*),
            bootstrapWitnesses = TaggedSortedSet.empty
          )
        )

    private def withBootstraps(witnesses: BootstrapWitness*): Transaction =
        unsigned.withWitness(
          _.copy(
            vkeyWitnesses = TaggedSortedSet.empty,
            bootstrapWitnesses = TaggedSortedSet(witnesses*)
          )
        )

    test("VerifiedSignaturesInWitnessesValidator VkeyWitnesses rule success") {
        val transaction = withVkeys(signers.map(p => VKeyWitness(key(p), signature(p)))*)

        val result = validate(VerifiedSignaturesInWitnessesValidator, Map.empty, transaction)
        assert(result.isRight)
    }

    test("VerifiedSignaturesInWitnessesValidator VkeyWitnesses rule failure") {
        val transaction = withVkeys(
          VKeyWitness(key(Alice), signature(Alice)),
          VKeyWitness(key(Bob), signature(Bob)),
          VKeyWitness(key(Charles), corrupt(signature(Charles)))
        )

        val result = validate(VerifiedSignaturesInWitnessesValidator, Map.empty, transaction)
        assert(result.isLeft)
    }

    test("VerifiedSignaturesInWitnessesValidator BootstrapWitnesses rule success") {
        val transaction = withBootstraps(signers.map(p => bootstrap(p, signature(p)))*)

        val result = validate(VerifiedSignaturesInWitnessesValidator, Map.empty, transaction)
        assert(result.isRight)
    }

    test("VerifiedSignaturesInWitnessesValidator BootstrapWitnesses rule failure") {
        val transaction = withBootstraps(
          bootstrap(Alice, signature(Alice)),
          bootstrap(Bob, signature(Bob)),
          bootstrap(Charles, corrupt(signature(Charles)))
        )

        val result = validate(VerifiedSignaturesInWitnessesValidator, Map.empty, transaction)
        assert(result.isLeft)
    }
}
