package scalus.cardano.ledger.rules

import org.bouncycastle.crypto.AsymmetricCipherKeyPair
import org.bouncycastle.crypto.generators.Ed25519KeyPairGenerator
import org.bouncycastle.crypto.params.{Ed25519KeyGenerationParameters, Ed25519PrivateKeyParameters, Ed25519PublicKeyParameters}
import org.scalacheck.Arbitrary
import scalus.builtin.ByteString
import scalus.cardano.address.{Network, ShelleyAddress, StakeAddress}
import scalus.cardano.ledger.*

import java.security.SecureRandom
import scala.collection.immutable

trait ValidatorRulesTestKit extends ArbitraryInstances {
    extension (tx: Transaction)
        def withNetwork(network: Network): Transaction = tx.copy(
          body = KeepRaw(
            tx.body.value.copy(
              networkId = Some(network.networkId),
              outputs = tx.body.value.outputs
                  .map(x =>
                      Sized(
                        Output(
                          Arbitrary.arbitrary[ShelleyAddress].sample.get.copy(network = network),
                          x.value.value
                        )
                      )
                  )
                  .appended(
                    Sized(
                      Output(
                        Arbitrary.arbitrary[ShelleyAddress].sample.get.copy(network = network),
                        Arbitrary.arbitrary[Value].sample.get
                      )
                    )
                  ),
              withdrawals = tx.body.value.withdrawals
                  .map(w =>
                      w.copy(withdrawals =
                          w.withdrawals.map((k, v) =>
                              (k.copy(address = k.address.copy(network = network)), v)
                          )
                      )
                  )
                  .orElse(
                    Some(
                      Withdrawals(
                        immutable.SortedMap(
                          (
                            RewardAccount(
                              Arbitrary.arbitrary[StakeAddress].sample.get.copy(network = network)
                            ),
                            Arbitrary.arbitrary[Coin].sample.get
                          )
                        )
                      )
                    )
                  )
            )
          )
        )

    end extension

    protected def randomTransactionWithIsValidField: Transaction =
        Arbitrary.arbitrary[Transaction].sample.get.copy(isValid = true)

    protected def generateKeyPair(): (ByteString, ByteString) = {
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
