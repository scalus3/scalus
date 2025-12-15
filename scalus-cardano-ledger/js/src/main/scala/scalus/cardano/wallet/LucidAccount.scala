package scalus.cardano.wallet

import scalus.builtin.ByteString
import scalus.cardano.address.Network
import scalus.cardano.txbuilder.TransactionSigner
import scalus.builtin.NodeJsPlatformSpecific.toByteString
import scalus.builtin.NodeJsPlatformSpecific.toUint8Array
import scalus.crypto.ed25519.{JsEd25519Signer, Signature, VerificationKey}

import scala.util.{Failure, Success, Try}

/** KeyPair implementation backed by CML's PrivateKey. */
class LucidKeyPair(bech32PrivateKey: String) extends KeyPair {
    type Underlying = CMLPrivateKey

    private lazy val cmlKey: CMLPrivateKey =
        Try(CML.PrivateKey.from_bech32(bech32PrivateKey)) match
            case Success(key) => key
            case Failure(ex) =>
                throw new IllegalArgumentException(
                  s"Failed to parse Bech32 private key: ${ex.getMessage}",
                  ex
                )

    override def underlying: CMLPrivateKey = cmlKey

    override def verificationKey: VerificationKey =
        VerificationKey.unsafeFromByteString(cmlKey.to_public().to_raw_bytes().toByteString)

    // CML keys can be either 32 bytes (standard) or 64 bytes (extended)
    private def keyBytes: ByteString = cmlKey.to_raw_bytes().toByteString

    override def sign(message: ByteString): Signature = {
        // Use CML's native signing which handles both standard and extended keys
        val messageUint8 = message.toUint8Array
        val signature = cmlKey.sign(messageUint8)
        Signature.unsafeFromByteString(signature.to_raw_bytes().toByteString)
    }

    override def verify(message: ByteString, signature: Signature): Boolean =
        JsEd25519Signer.verify(verificationKey, message, signature)

    @deprecated("Use verificationKey.bytes instead", "0.9.0")
    override def publicKeyBytes: Array[Byte] = cmlKey.to_public().to_raw_bytes().toByteString.bytes

    @deprecated("Use the signing key directly", "0.9.0")
    override def privateKeyBytes: Array[Byte] = keyBytes.bytes

    @deprecated("Use sign(ByteString) instead", "0.9.0")
    override def sign(message: Array[Byte]): Array[Byte] = {
        val messageUint8 = ByteString(message*).toUint8Array
        val signature = cmlKey.sign(messageUint8)
        signature.to_raw_bytes().toByteString.bytes
    }
}

class LucidAccount(
    mnemonic: String,
    derivationPath: String,
    network: Network
) extends Account:

    private val accountIndex: Int =
        val parts = derivationPath.split("/")
        if parts.length >= 4 then parts(3).stripSuffix("'").toInt
        else 0

    private val lucidNetwork: String = network match
        case Network.Mainnet => "Mainnet"
        case Network.Testnet => "Testnet"
        case Network.Other(_) =>
            throw new IllegalArgumentException(
              s"Unsupported network: $network. Only Mainnet and Testnet are supported."
            )

    private def credentials: WalletCredentials =
        val options = new WalletOptions(
          addressType = "Base",
          accountIndex = accountIndex,
          network = lucidNetwork
        )
        Try(walletFromSeed(mnemonic, options)) match
            case Success(creds) => creds
            case Failure(ex) =>
                throw new IllegalArgumentException(
                  s"Failed to derive wallet from seed: ${ex.getMessage}",
                  ex
                )

    override def paymentKeyPair: KeyPair = new LucidKeyPair(credentials.paymentKey)

    override def changeKeyPair: KeyPair = paymentKeyPair

    override def stakeKeyPair: KeyPair =
        credentials.stakeKey.toOption match
            case Some(stakeKey) => new LucidKeyPair(stakeKey)
            case None =>
                throw new IllegalStateException(
                  "No stake key available. Was the address type set to 'Base'?"
                )

    override def drepKeyPair: KeyPair =
        throw new UnsupportedOperationException("DRep keys are not supported")

    override def signerForUtxos: TransactionSigner =
        new TransactionSigner(Set(paymentKeyPair))

    def baseAddress: String = credentials.address

object LucidAccount:

    def apply(network: Network, mnemonic: String, derivationPath: String): LucidAccount =
        new LucidAccount(mnemonic, derivationPath, network)
