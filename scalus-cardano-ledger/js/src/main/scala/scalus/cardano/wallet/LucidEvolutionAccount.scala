package scalus.cardano.wallet

import scalus.cardano.address.Network
import scalus.cardano.txbuilder.TransactionSigner

import scala.util.{Failure, Success, Try}

class LucidEvolutionKeyPair(bech32PrivateKey: String) extends KeyPair {
    type Underlying = CMLPrivateKey

    private lazy val cmlKey: CMLPrivateKey = {
        Try(CML.PrivateKey.from_bech32(bech32PrivateKey)) match {
            case Success(key) => key
            case Failure(ex) =>
                throw new IllegalArgumentException(
                  s"Failed to parse Bech32 private key: ${ex.getMessage}",
                  ex
                )
        }
    }

    override def underlying: CMLPrivateKey = cmlKey

    override def publicKeyBytes: Array[Byte] = {
        TypeConversions.uint8ArrayToBytes(cmlKey.to_public().to_raw_bytes())
    }

    override def privateKeyBytes: Array[Byte] = {
        TypeConversions.uint8ArrayToBytes(cmlKey.to_raw_bytes())
    }

    override def sign(message: Array[Byte]): Array[Byte] = {
        val messageUint8 = TypeConversions.bytesToUint8Array(message)
        val signature = cmlKey.sign(messageUint8)
        TypeConversions.uint8ArrayToBytes(signature.to_raw_bytes())
    }
}

class LucidEvolutionAccount(
    mnemonic: String,
    derivationPath: String,
    network: Network
) extends Account {

    private val accountIndex: Int = {
        val parts = derivationPath.split("/")
        if parts.length >= 4 then parts(3).stripSuffix("'").toInt
        else 0
    }

    private val lucidNetwork: String = network match {
        case Network.Mainnet => "Mainnet"
        case Network.Testnet => "Testnet"
        case Network.Other(_) =>
            throw new IllegalArgumentException(
              s"Unsupported network: $network. Only Mainnet and Testnet are supported."
            )
    }

    private def credentials: WalletCredentials = {
        val options = new WalletOptions(
          addressType = "Base",
          accountIndex = accountIndex,
          network = lucidNetwork
        )
        Try(walletFromSeed(mnemonic, options)) match {
            case Success(creds) => creds
            case Failure(ex) =>
                throw new IllegalArgumentException(
                  s"Failed to derive wallet from seed: ${ex.getMessage}",
                  ex
                )
        }
    }

    override def paymentKeyPair: KeyPair = new LucidEvolutionKeyPair(credentials.paymentKey)

    override def changeKeyPair: KeyPair = paymentKeyPair

    override def stakeKeyPair: KeyPair = {
        credentials.stakeKey.toOption match {
            case Some(stakeKey) => new LucidEvolutionKeyPair(stakeKey)
            case None =>
                throw new IllegalStateException(
                  "No stake key available. Was the address type set to 'Base'?"
                )
        }
    }

    override def drepKeyPair: KeyPair =
        throw new UnsupportedOperationException("DRep keys are not supported")

    override def signerForUtxos: TransactionSigner = new TransactionSigner(Set(paymentKeyPair))

    def baseAddress: String = credentials.address
}

object LucidEvolutionAccount {

    def apply(network: Network, mnemonic: String, derivationPath: String): LucidEvolutionAccount = {
        new LucidEvolutionAccount(mnemonic, derivationPath, network)
    }
}
