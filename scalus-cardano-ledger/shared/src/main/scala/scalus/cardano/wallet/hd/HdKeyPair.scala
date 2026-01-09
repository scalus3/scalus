package scalus.cardano.wallet.hd

import scalus.builtin.ByteString
import scalus.cardano.wallet.ExtendedKeyPair
import scalus.crypto.ed25519.{Ed25519Signer, ExtendedSigningKey, Signature, SigningKey, VerificationKey}

/** HD wallet key pair backed by a SLIP-0010 extended key.
  *
  * This key pair uses the extended 64-byte format (32-byte private key + 32-byte chain code) from
  * SLIP-0010 derivation. The chain code enables hierarchical key derivation.
  *
  * @param extendedKey
  *   the SLIP-0010 extended key containing private key and chain code
  * @param verificationKey
  *   the Ed25519 public key derived from the private key
  */
class HdKeyPair(
    val extendedKey: Slip0010.ExtendedKey,
    override val verificationKey: VerificationKey
)(using signer: Ed25519Signer)
    extends ExtendedKeyPair {

    override type Underlying = Slip0010.ExtendedKey
    override def underlying: Underlying = extendedKey

    /** The extended signing key (private key bytes + chain code = 64 bytes). */
    override lazy val extendedSigningKey: ExtendedSigningKey = {
        val combined = extendedKey.privateKeyBytes ++ extendedKey.chainCode
        ExtendedSigningKey.unsafeFromArray(combined)
    }

    /** The standard 32-byte signing key (without chain code). */
    lazy val signingKey: SigningKey =
        SigningKey.unsafeFromArray(extendedKey.privateKeyBytes)

    /** The chain code for further derivation. */
    def chainCode: Array[Byte] = extendedKey.chainCode.clone()

    override def sign(message: ByteString): Signature =
        // For SLIP-0010 derived keys, we use standard Ed25519 signing with the 32-byte private key.
        // This is different from Cardano's BIP32-Ed25519 which uses a 64-byte extended format.
        signer.sign(signingKey, message)

    override def verify(message: ByteString, signature: Signature): Boolean =
        signer.verify(verificationKey, message, signature)

    /** Derive a hardened child key pair.
      *
      * @param index
      *   the child index (will be hardened)
      * @return
      *   the derived child key pair
      */
    def deriveHardened(index: Int): HdKeyPair = {
        val childKey = extendedKey.deriveHardened(index)
        HdKeyPair.fromExtendedKey(childKey)
    }

    /** Derive a child key pair at the given index.
      *
      * Note: For Ed25519, only hardened derivation is supported (index >= 0x80000000).
      *
      * @param index
      *   the child index
      * @return
      *   the derived child key pair
      */
    def deriveChild(index: Int): HdKeyPair = {
        val childKey = extendedKey.deriveChild(index)
        HdKeyPair.fromExtendedKey(childKey)
    }

    override def equals(obj: Any): Boolean = obj match
        case other: HdKeyPair => extendedKey == other.extendedKey
        case _                => false

    override def hashCode(): Int = extendedKey.hashCode()
}

object HdKeyPair {

    /** Create an HdKeyPair from a SLIP-0010 extended key.
      *
      * Derives the public key from the private key using the Ed25519 signer.
      *
      * @param extendedKey
      *   the SLIP-0010 extended key
      * @return
      *   the HD key pair
      */
    def fromExtendedKey(
        extendedKey: Slip0010.ExtendedKey
    )(using signer: Ed25519Signer): HdKeyPair = {
        val signingKey = SigningKey.unsafeFromArray(extendedKey.privateKeyBytes)
        val verificationKey = signer.derivePublicKey(signingKey)
        new HdKeyPair(extendedKey, verificationKey)
    }

    /** Create an HdKeyPair from a BIP-39 seed at a given derivation path.
      *
      * @param seed
      *   the BIP-39 seed (typically 64 bytes)
      * @param path
      *   the derivation path (e.g., "m/1852'/1815'/0'/0/0")
      * @return
      *   the HD key pair
      */
    def fromSeed(seed: Array[Byte], path: String)(using Ed25519Signer): HdKeyPair = {
        val extendedKey = Slip0010.deriveFromPath(seed, path)
        fromExtendedKey(extendedKey)
    }

    /** Create an HdKeyPair from a mnemonic at a given derivation path.
      *
      * @param mnemonic
      *   the BIP-39 mnemonic
      * @param passphrase
      *   optional BIP-39 passphrase
      * @param path
      *   the derivation path
      * @return
      *   the HD key pair
      */
    def fromMnemonic(mnemonic: String, passphrase: String, path: String)(using
        Ed25519Signer
    ): HdKeyPair = {
        val seed = Bip39.mnemonicToSeed(mnemonic, passphrase)
        fromSeed(seed, path)
    }

    /** Create an HdKeyPair from a mnemonic at a given derivation path (no passphrase).
      *
      * @param mnemonic
      *   the BIP-39 mnemonic
      * @param path
      *   the derivation path
      * @return
      *   the HD key pair
      */
    def fromMnemonic(mnemonic: String, path: String)(using Ed25519Signer): HdKeyPair =
        fromMnemonic(mnemonic, "", path)

    /** Create a master HdKeyPair from a BIP-39 seed.
      *
      * @param seed
      *   the BIP-39 seed
      * @return
      *   the master HD key pair at path "m"
      */
    def masterFromSeed(seed: Array[Byte])(using Ed25519Signer): HdKeyPair = {
        val masterKey = Slip0010.masterKeyFromSeed(seed)
        fromExtendedKey(masterKey)
    }
}
