package scalus.cardano.node

import org.bouncycastle.crypto.AsymmetricCipherKeyPair
import org.bouncycastle.crypto.generators.Ed25519KeyPairGenerator
import org.bouncycastle.crypto.params.{Ed25519KeyGenerationParameters, Ed25519PrivateKeyParameters, Ed25519PublicKeyParameters}
import scalus.cardano.ledger.Utxos
import scalus.cardano.ledger.rules.{Context, STS}
import scalus.uplc.builtin.ByteString

import java.security.SecureRandom

object TestEmulatorFactory {
    def create(
        utxos: Utxos = Map.empty,
        context: Context = Context(),
        validators: Iterable[STS.Validator] = Emulator.defaultValidators,
        mutators: Iterable[STS.Mutator] = Emulator.defaultMutators
    ): EmulatorBase = new Emulator(utxos, context, validators, mutators)

    def generateKeyPair(): (ByteString, ByteString) = {
        val asymmetricCipherKeyPair: AsymmetricCipherKeyPair = keyPairGenerator.generateKeyPair()
        val privateKeyParams: Ed25519PrivateKeyParameters =
            asymmetricCipherKeyPair.getPrivate.asInstanceOf[Ed25519PrivateKeyParameters]
        val publicKeyParams: Ed25519PublicKeyParameters =
            asymmetricCipherKeyPair.getPublic.asInstanceOf[Ed25519PublicKeyParameters]
        val privateKey: ByteString = ByteString.fromArray(privateKeyParams.getEncoded)
        val publicKey: ByteString = ByteString.fromArray(publicKeyParams.getEncoded)
        (privateKey, publicKey)
    }

    private val keyPairGenerator = {
        val keyPairGenerator = new Ed25519KeyPairGenerator()
        keyPairGenerator.init(new Ed25519KeyGenerationParameters(new SecureRandom()))
        keyPairGenerator
    }
}
