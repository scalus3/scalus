package scalus.cardano.txbuilder

import scalus.builtin.ByteString

import com.bloxbean.cardano.client.crypto.Blake2bUtil
import com.bloxbean.cardano.client.crypto.bip32.key.{HdPrivateKey, HdPublicKey}
import com.bloxbean.cardano.client.crypto.config.CryptoConfiguration
import com.bloxbean.cardano.client.transaction.util.TransactionBytes
import scalus.cardano.ledger.{Transaction, VKeyWitness}

import scala.language.implicitConversions

// Wrapper types for cryptographic keys and signatures
case class VerificationKeyBytes(bytes: ByteString)
case class Ed25519Signature(bytes: IArray[Byte])

case class WalletId(name: String)

trait WalletModule:

    type VerificationKey
    type SigningKey

    def exportVerificationKeyBytes(publicKey: VerificationKey): VerificationKeyBytes

    def createTxKeyWitness(
        tx: Transaction,
        verificationKey: VerificationKey,
        signingKey: SigningKey
    ): VKeyWitness

    def createEd25519Signature(
        msg: IArray[Byte],
        signingKey: SigningKey
    ): Ed25519Signature

class Wallet(
    name: String,
    walletModule: WalletModule,
    verificationKey: walletModule.VerificationKey,
    signingKey: walletModule.SigningKey
):
    private lazy val verificationKeysBytes =
        walletModule.exportVerificationKeyBytes(verificationKey)

    def exportVerificationKeyBytes: VerificationKeyBytes = verificationKeysBytes

    def createTxKeyWitness(tx: Transaction): VKeyWitness =
        walletModule.createTxKeyWitness(tx, verificationKey, signingKey)

    def getWalletId: WalletId = WalletId(getName)

    def getName: String = name

    def createEd25519Signature(msg: IArray[Byte]): Ed25519Signature =
        walletModule.createEd25519Signature(msg, signingKey)

object WalletModuleBloxbean extends WalletModule:

    override type VerificationKey = HdPublicKey
    override type SigningKey = HdPrivateKey

    override def exportVerificationKeyBytes(
        verificationKey: VerificationKey
    ): VerificationKeyBytes =
        VerificationKeyBytes(ByteString.fromArray(verificationKey.getKeyData))

    override def createTxKeyWitness(
        tx: Transaction,
        verificationKey: VerificationKey,
        signingKey: SigningKey
    ): VKeyWitness =
        // See BloxBean's TransactionSigner.class
        val txBytes = TransactionBytes(tx.toCbor)
        val txnBodyHash = Blake2bUtil.blake2bHash256(txBytes.getTxBodyBytes)
        val signingProvider = CryptoConfiguration.INSTANCE.getSigningProvider
        val signature = signingProvider.signExtended(txnBodyHash, signingKey.getKeyData)
        VKeyWitness(
          signature = ByteString.fromArray(signature),
          vkey = ByteString.fromArray(verificationKey.getKeyData)
        )

    override def createEd25519Signature(
        msg: IArray[Byte],
        signingKey: SigningKey
    ): Ed25519Signature =
        val signingProvider = CryptoConfiguration.INSTANCE.getSigningProvider
        val signature = signingProvider.signExtended(
          IArray.genericWrapArray(msg).toArray,
          signingKey.getKeyData
        )
        Ed25519Signature(IArray.from(signature))
