package scalus.cardano.wallet.hd

import scalus.builtin.ByteString
import scalus.cardano.wallet.ExtendedKeyPair
import scalus.crypto.ed25519.{Ed25519Signer, ExtendedSigningKey, Signature, SigningKey, VerificationKey}

/** HD wallet key pair backed by BIP32-Ed25519 extended key.
  *
  * This key pair uses the Cardano-style extended format (96 bytes: 64-byte extended secret key +
  * 32-byte chain code) from BIP32-Ed25519 derivation. Supports both hardened and non-hardened child
  * key derivation for CIP-1852 compatibility.
  *
  * @param extendedKey
  *   the BIP32-Ed25519 extended key containing private key components and chain code
  * @param verificationKey
  *   the Ed25519 public key derived from the private key
  */
class HdKeyPair(
    val extendedKey: Bip32Ed25519.ExtendedKey,
    override val verificationKey: VerificationKey
)(using val signer: Ed25519Signer)
    extends ExtendedKeyPair {

    override type Underlying = Bip32Ed25519.ExtendedKey
    override def underlying: Underlying = extendedKey

    /** The extended signing key (kL || kR = 64 bytes).
      *
      * This is the Cardano-style extended key format used for signing.
      */
    override lazy val extendedSigningKey: ExtendedSigningKey =
        ExtendedSigningKey.unsafeFromArray(extendedKey.extendedSecretKey)

    /** The standard 32-byte signing key (kL only, without kR). */
    lazy val signingKey: SigningKey =
        SigningKey.unsafeFromArray(extendedKey.kL)

    /** The chain code for further derivation. */
    def chainCode: Array[Byte] = extendedKey.chainCode.clone()

    override def sign(message: ByteString): Signature =
        // For BIP32-Ed25519 derived keys, use extended signing with the full 64-byte key.
        // This is compatible with Cardano's signing requirements.
        signer.signExtended(extendedSigningKey, verificationKey, message)

    override def verify(message: ByteString, signature: Signature): Boolean =
        signer.verify(verificationKey, message, signature)

    /** Derive a hardened child key pair.
      *
      * Hardened derivation uses the private key and produces keys that cannot be derived from the
      * parent public key.
      *
      * @param index
      *   the child index (will be hardened: index + 0x80000000)
      * @return
      *   the derived child key pair
      */
    def deriveHardened(index: Int): HdKeyPair = {
        val childKey = extendedKey.deriveHardened(index)
        HdKeyPair.fromExtendedKey(childKey)
    }

    /** Derive a non-hardened (normal) child key pair.
      *
      * Non-hardened derivation uses the public key. This is required for CIP-1852 paths where role
      * and index are not hardened.
      *
      * @param index
      *   the child index (must be < 0x80000000)
      * @return
      *   the derived child key pair
      */
    def deriveNormal(index: Int): HdKeyPair = {
        val childKey = extendedKey.deriveNormal(index)
        HdKeyPair.fromExtendedKey(childKey)
    }

    /** Derive a child key pair at the given index.
      *
      * @param index
      *   the child index (hardened if >= 0x80000000, normal otherwise)
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

    /** Create an HdKeyPair from a BIP32-Ed25519 extended key.
      *
      * Derives the public key from the private key using Ed25519 base point multiplication.
      *
      * @param extendedKey
      *   the BIP32-Ed25519 extended key
      * @return
      *   the HD key pair
      */
    def fromExtendedKey(
        extendedKey: Bip32Ed25519.ExtendedKey
    )(using signer: Ed25519Signer): HdKeyPair = {
        val pubKeyBytes = scalus.crypto.ed25519.Ed25519Math.scalarMultiplyBase(extendedKey.kL)
        val verificationKey = VerificationKey.unsafeFromArray(pubKeyBytes)
        new HdKeyPair(extendedKey, verificationKey)
    }

    /** Create an HdKeyPair from a mnemonic at a given derivation path.
      *
      * Uses BIP32-Ed25519 (Icarus-style) derivation which is compatible with standard Cardano
      * wallets.
      *
      * @param mnemonic
      *   the BIP-39 mnemonic
      * @param passphrase
      *   optional passphrase
      * @param path
      *   the derivation path (e.g., "m/1852'/1815'/0'/0/0")
      * @return
      *   the HD key pair
      */
    def fromMnemonic(mnemonic: String, passphrase: String, path: String)(using
        Ed25519Signer
    ): HdKeyPair = {
        val extendedKey = Bip32Ed25519.deriveFromPath(mnemonic, passphrase, path)
        fromExtendedKey(extendedKey)
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

    /** Create a master HdKeyPair from a BIP-39 mnemonic.
      *
      * @param mnemonic
      *   the BIP-39 mnemonic
      * @param passphrase
      *   optional passphrase
      * @return
      *   the master HD key pair at path "m"
      */
    def masterFromMnemonic(mnemonic: String, passphrase: String = "")(using
        Ed25519Signer
    ): HdKeyPair = {
        val masterKey = Bip32Ed25519.masterKeyFromMnemonic(mnemonic, passphrase)
        fromExtendedKey(masterKey)
    }
}
