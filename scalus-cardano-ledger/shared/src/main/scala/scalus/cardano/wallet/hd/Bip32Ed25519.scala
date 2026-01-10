package scalus.cardano.wallet.hd

import scalus.crypto.{Hmac, Pbkdf2}

/** BIP32-Ed25519 key derivation for Cardano (Icarus-style).
  *
  * Implements the BIP32-Ed25519 specification as used by Cardano wallets, supporting both hardened
  * and non-hardened child key derivation.
  *
  * Key differences from SLIP-0010:
  *   - Extended keys are 96 bytes (64-byte secret + 32-byte chain code)
  *   - Non-hardened derivation is supported using the public key
  *   - Key clamping is applied to make Ed25519 keys valid BIP32 keys
  *
  * @see
  *   https://github.com/cardano-foundation/CIPs/tree/master/CIP-1852
  * @see
  *   https://input-output-hk.github.io/adrestia/cardano-wallet/concepts/address-derivation
  */
object Bip32Ed25519 {

    /** Ed25519 group order L = 2^252 + 27742317777372353535851937790883648493 */
    private val L: BigInt = BigInt(
      "7237005577332262213973186563042994240857116359379907606001950938285454250989"
    )

    /** Extended key containing private key components and chain code.
      *
      * @param kL
      *   Left half of extended secret key (32 bytes, clamped scalar)
      * @param kR
      *   Right half of extended secret key (32 bytes)
      * @param chainCode
      *   Chain code for derivation (32 bytes)
      */
    case class ExtendedKey(
        kL: Array[Byte],
        kR: Array[Byte],
        chainCode: Array[Byte]
    ) {
        require(kL.length == 32, s"kL must be 32 bytes, got ${kL.length}")
        require(kR.length == 32, s"kR must be 32 bytes, got ${kR.length}")
        require(chainCode.length == 32, s"chainCode must be 32 bytes, got ${chainCode.length}")

        /** Get the 64-byte extended secret key (kL || kR). */
        def extendedSecretKey: Array[Byte] = kL ++ kR

        /** Derive a hardened child key.
          *
          * @param index
          *   Child index (will be hardened: index + 0x80000000)
          * @return
          *   Derived child key
          */
        def deriveHardened(index: Int): ExtendedKey = {
            require(index >= 0, s"Index must be non-negative, got $index")
            deriveChild(index | 0x80000000)
        }

        /** Derive a non-hardened (normal) child key.
          *
          * @param index
          *   Child index (must be in range [0, 2^31))
          * @return
          *   Derived child key
          */
        def deriveNormal(index: Int): ExtendedKey = {
            // Explicit non-negative check for clarity (also rejects negative values)
            require(index >= 0, s"Index must be non-negative, got $index")
            // Check that high bit is not set (i.e., index is in [0, 2^31))
            require((index & 0x80000000) == 0, s"Normal index must be < 2^31, got $index")
            deriveChild(index)
        }

        /** Derive a child key at the given index.
          *
          * @param index
          *   Child index (hardened if >= 0x80000000)
          * @return
          *   Derived child key
          */
        def deriveChild(index: Int): ExtendedKey = {
            val indexBytes = intToLittleEndian(index)
            val isHardened = (index & 0x80000000) != 0

            val (zBytes, ccBytes) = if isHardened then {
                // Hardened derivation: use private key
                val data = Array[Byte](0x00) ++ kL ++ kR ++ indexBytes
                val ccData = Array[Byte](0x01) ++ kL ++ kR ++ indexBytes
                val z = Hmac.hmacSha512(chainCode, data)
                val cc = Hmac.hmacSha512(chainCode, ccData)
                (z, cc.drop(32))
            } else {
                // Non-hardened derivation: use public key
                val pubKey = derivePublicKey(kL)
                val data = Array[Byte](0x02) ++ pubKey ++ indexBytes
                val ccData = Array[Byte](0x03) ++ pubKey ++ indexBytes
                val z = Hmac.hmacSha512(chainCode, data)
                val cc = Hmac.hmacSha512(chainCode, ccData)
                (z, cc.drop(32))
            }

            // Derive child kL: kL' = (8 * zL) + kL
            //
            // Critical: zL is truncated to 28 bytes (224 bits) for two reasons:
            // 1. Prevents arithmetic overflow: 8 * (28-byte value) + (32-byte kL) stays within
            //    256 bits, avoiding the need for modular reduction which would break key validity.
            // 2. Preserves Ed25519 clamping: The parent kL has bit 254 set (from clamping).
            //    With 8 * zL being at most 227 bits, the addition cannot overflow into bit 254,
            //    ensuring the child kL retains this required property.
            //
            // Reference: "BIP32-Ed25519 Hierarchical Deterministic Keys over a Non-linear Keyspace"
            // by Khovratovich and Law. Also see Cardano wallet design documentation:
            // https://input-output-hk.github.io/adrestia/cardano-wallet/concepts/address-derivation
            val zL = zBytes.take(28)
            val zLScaled = scalarMultiply8(zL)
            val kLChild = add256BitLE(zLScaled, kL)

            // Derive child kR: kR' = zR + kR (mod 2^256)
            val zR = zBytes.drop(32)
            val kRChild = addMod256(zR, kR)

            ExtendedKey(kLChild, kRChild, ccBytes)
        }

        override def equals(obj: Any): Boolean = obj match {
            case other: ExtendedKey =>
                java.util.Arrays.equals(kL, other.kL) &&
                java.util.Arrays.equals(kR, other.kR) &&
                java.util.Arrays.equals(chainCode, other.chainCode)
            case _ => false
        }

        override def hashCode(): Int =
            java.util.Arrays.hashCode(kL) ^
                java.util.Arrays.hashCode(kR) ^
                java.util.Arrays.hashCode(chainCode)
    }

    /** Generate master key from BIP-39 entropy using Icarus-style derivation.
      *
      * Uses PBKDF2-HMAC-SHA512 with:
      *   - Password: passphrase (empty string if none)
      *   - Salt: raw entropy bytes from mnemonic
      *   - Iterations: 4096
      *   - Output: 96 bytes
      *
      * Note: This differs from standard BIP-39 seed derivation which uses the mnemonic words as
      * password and "mnemonic" + passphrase as salt.
      *
      * @param entropy
      *   BIP-39 entropy (typically 16-32 bytes)
      * @param passphrase
      *   Optional passphrase (empty string if none)
      * @return
      *   Master extended key
      */
    def masterKeyFromEntropy(entropy: Array[Byte], passphrase: String = ""): ExtendedKey = {
        // Icarus-style: PBKDF2 with passphrase as password and entropy as salt
        val derived = Pbkdf2.deriveKey(
          password = passphrase.getBytes("UTF-8"),
          salt = entropy,
          iterations = 4096,
          keyLength = 96
        )

        val kL = derived.take(32)
        val kR = derived.slice(32, 64)
        val chainCode = derived.drop(64)

        // Clamp kL to make it a valid Ed25519 scalar
        clampKey(kL)

        ExtendedKey(kL, kR, chainCode)
    }

    /** Generate master key from BIP-39 mnemonic using Icarus-style derivation.
      *
      * @param mnemonic
      *   BIP-39 mnemonic sentence
      * @param passphrase
      *   Optional passphrase
      * @return
      *   Master extended key
      */
    def masterKeyFromMnemonic(mnemonic: String, passphrase: String = ""): ExtendedKey = {
        val entropy = Bip39.mnemonicToEntropy(mnemonic)
        masterKeyFromEntropy(entropy, passphrase)
    }

    /** Derive a key from a path string.
      *
      * Path format: "m/purpose'/coin_type'/account'/role/index"
      *
      * @param mnemonic
      *   BIP-39 mnemonic
      * @param passphrase
      *   Optional passphrase
      * @param path
      *   Derivation path
      * @return
      *   Derived extended key
      */
    def deriveFromPath(mnemonic: String, passphrase: String, path: String): ExtendedKey = {
        val master = masterKeyFromMnemonic(mnemonic, passphrase)
        val indices = parsePath(path)
        indices.foldLeft(master) { (key, index) =>
            key.deriveChild(index)
        }
    }

    /** Parse a derivation path string into indices.
      *
      * @param path
      *   Path like "m/1852'/1815'/0'/0/0"
      * @return
      *   Sequence of indices (hardened indices have high bit set)
      * @throws IllegalArgumentException
      *   if path is malformed
      */
    def parsePath(path: String): Seq[Int] = {
        require(path.nonEmpty, "Path cannot be empty")
        require(
          path.startsWith("m/") || path.startsWith("M/"),
          s"Path must start with 'm/' or 'M/', got: $path"
        )

        val segments = path.drop(2).split("/").toSeq // Skip "m/"
        require(segments.nonEmpty, s"Path must have at least one segment after 'm/', got: $path")

        segments.zipWithIndex.map { case (segment, idx) =>
            require(segment.nonEmpty, s"Empty segment at position $idx in path: $path")

            val isHardened = segment.endsWith("'") || segment.endsWith("H") || segment.endsWith("h")
            val indexStr = segment.stripSuffix("'").stripSuffix("H").stripSuffix("h")

            val index =
                try indexStr.toInt
                catch
                    case _: NumberFormatException =>
                        throw new IllegalArgumentException(
                          s"Invalid index '$indexStr' at position $idx in path: $path"
                        )

            require(
              index >= 0,
              s"Index must be non-negative, got $index at position $idx in path: $path"
            )

            if isHardened then index | 0x80000000 else index
        }
    }

    /** Clamp a 32-byte key to make it a valid Ed25519 scalar for BIP32.
      *
      * Sets:
      *   - Lowest 3 bits of byte 0 to 0
      *   - Highest bit of byte 31 to 0
      *   - Second highest bit of byte 31 to 1
      */
    private def clampKey(key: Array[Byte]): Unit = {
        key(0) = (key(0) & 0xf8).toByte
        key(31) = (key(31) & 0x1f).toByte
        key(31) = (key(31) | 0x40).toByte
    }

    /** Convert an Int to 4 bytes in little-endian order. */
    private def intToLittleEndian(value: Int): Array[Byte] = {
        Array(
          (value & 0xff).toByte,
          ((value >> 8) & 0xff).toByte,
          ((value >> 16) & 0xff).toByte,
          ((value >> 24) & 0xff).toByte
        )
    }

    /** Multiply a scalar by 8 (left shift by 3). */
    private def scalarMultiply8(bytes: Array[Byte]): Array[Byte] = {
        // Convert to BigInt, multiply by 8, convert back
        val value = bytesToBigIntLE(bytes)
        val result = value * 8
        bigIntToBytesLE(result, 32)
    }

    /** Add two 256-bit little-endian integers for child key derivation.
      *
      * Both inputs are interpreted as little-endian integers. Per BIP32-Ed25519 spec, the addition
      * is performed as 256-bit integer addition (NOT mod L). The 28-byte truncation of zL and the
      * multiply-by-8 ensure the result stays within bounds and preserves clamping properties.
      */
    private def add256BitLE(a: Array[Byte], b: Array[Byte]): Array[Byte] = {
        val aInt = bytesToBigIntLE(a)
        val bInt = bytesToBigIntLE(b)
        val sum = aInt + bInt
        // 256-bit addition, no mod L - result is guaranteed to be valid due to zL truncation
        bigIntToBytesLE(sum, 32)
    }

    /** Add two 32-byte values modulo 2^256.
      *
      * Both inputs are interpreted as little-endian integers.
      */
    private def addMod256(a: Array[Byte], b: Array[Byte]): Array[Byte] = {
        val aInt = bytesToBigIntLE(a)
        val bInt = bytesToBigIntLE(b)
        val modulus = BigInt(1) << 256
        val sum = (aInt + bInt) % modulus
        bigIntToBytesLE(if sum < 0 then sum + modulus else sum, 32)
    }

    /** Convert little-endian bytes to BigInt. */
    private def bytesToBigIntLE(bytes: Array[Byte]): BigInt = {
        BigInt(1, bytes.reverse)
    }

    /** Convert BigInt to little-endian bytes with specified length.
      *
      * For BIP32-Ed25519 child key derivation, this is safe because:
      *   - The 28-byte truncation of zL before multiplication by 8 ensures the result of
      *     add256BitLE stays within 256 bits
      *   - addMod256 explicitly reduces modulo 2^256
      *
      * @throws IllegalArgumentException
      *   if value does not fit in the specified length
      */
    private def bigIntToBytesLE(value: BigInt, length: Int): Array[Byte] = {
        require(value >= 0, s"Value must be non-negative, got $value")
        val bytes = value.toByteArray
        // BigInt.toByteArray is big-endian and may have leading zeros or sign byte
        val unsigned = if bytes.length > 1 && bytes(0) == 0 then bytes.drop(1) else bytes
        require(
          unsigned.length <= length,
          s"Value does not fit in $length bytes (requires ${unsigned.length} bytes)"
        )
        val padded =
            if unsigned.length < length then new Array[Byte](length - unsigned.length) ++ unsigned
            else unsigned
        padded.reverse // Convert to little-endian
    }

    /** Derive Ed25519 public key from private key scalar.
      *
      * This performs scalar multiplication: A = kL * G where G is the Ed25519 base point.
      */
    private def derivePublicKey(kL: Array[Byte]): Array[Byte] = {
        // Use platform-specific Ed25519 implementation
        scalus.crypto.ed25519.Ed25519Math.scalarMultiplyBase(kL)
    }
}
