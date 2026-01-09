package scalus.cardano.wallet.hd

import scalus.crypto.Hmac

/** SLIP-0010 key derivation for Ed25519.
  *
  * SLIP-0010 specifies hierarchical deterministic key derivation for Ed25519 curves. Unlike BIP-32
  * (which uses secp256k1), SLIP-0010 only supports hardened derivation for Ed25519.
  *
  * @see
  *   https://github.com/satoshilabs/slips/blob/master/slip-0010.md
  */
object Slip0010 {

    /** The HMAC key for Ed25519 master key derivation. */
    private val Ed25519SeedKey = "ed25519 seed".getBytes("UTF-8")

    /** Hardened index offset (0x80000000). */
    val HardenedOffset: Long = 0x80000000L

    /** Extended key consisting of private key bytes and chain code.
      *
      * @param privateKeyBytes
      *   32-byte Ed25519 private key
      * @param chainCode
      *   32-byte chain code for derivation
      */
    case class ExtendedKey(
        privateKeyBytes: Array[Byte],
        chainCode: Array[Byte]
    ) {
        require(
          privateKeyBytes.length == 32,
          s"Private key must be 32 bytes, got ${privateKeyBytes.length}"
        )
        require(chainCode.length == 32, s"Chain code must be 32 bytes, got ${chainCode.length}")

        /** Derive a hardened child key.
          *
          * @param index
          *   the child index (will be hardened, i.e., index + 0x80000000)
          * @return
          *   the derived child extended key
          */
        def deriveHardened(index: Int): ExtendedKey = {
            require(index >= 0, s"Index must be non-negative, got $index")
            val hardenedIndex = index.toLong + HardenedOffset
            deriveChild(hardenedIndex.toInt)
        }

        /** Derive a child key at the given index.
          *
          * For Ed25519, only hardened derivation is supported (index >= 0x80000000).
          *
          * @param index
          *   the child index (must be >= 0x80000000 for Ed25519)
          * @return
          *   the derived child extended key
          */
        def deriveChild(index: Int): ExtendedKey = {
            // For Ed25519, we only support hardened derivation
            // Data = 0x00 || private_key || index (4 bytes big-endian)
            val data = new Array[Byte](1 + 32 + 4)
            data(0) = 0x00.toByte
            System.arraycopy(privateKeyBytes, 0, data, 1, 32)
            data(33) = (index >>> 24).toByte
            data(34) = (index >>> 16).toByte
            data(35) = (index >>> 8).toByte
            data(36) = index.toByte

            val i = Hmac.hmacSha512(chainCode, data)

            val childPrivateKey = i.take(32)
            val childChainCode = i.drop(32)

            ExtendedKey(childPrivateKey, childChainCode)
        }

        /** Derive a child key following a derivation path.
          *
          * @param path
          *   sequence of indices (each will be treated as hardened)
          * @return
          *   the derived extended key
          */
        def derivePath(path: Seq[Int]): ExtendedKey =
            path.foldLeft(this)((key, index) => key.deriveHardened(index))

        override def equals(obj: Any): Boolean = obj match
            case other: ExtendedKey =>
                java.util.Arrays.equals(privateKeyBytes, other.privateKeyBytes) &&
                java.util.Arrays.equals(chainCode, other.chainCode)
            case _ => false

        override def hashCode(): Int =
            java.util.Arrays.hashCode(privateKeyBytes) * 31 + java.util.Arrays.hashCode(chainCode)
    }

    /** Derive a master extended key from a BIP-39 seed.
      *
      * Uses HMAC-SHA512 with key "ed25519 seed".
      *
      * @param seed
      *   the BIP-39 seed (typically 64 bytes)
      * @return
      *   the master extended key
      */
    def masterKeyFromSeed(seed: Array[Byte]): ExtendedKey = {
        require(seed.length >= 16, s"Seed must be at least 16 bytes, got ${seed.length}")

        val i = Hmac.hmacSha512(Ed25519SeedKey, seed)

        val privateKey = i.take(32)
        val chainCode = i.drop(32)

        ExtendedKey(privateKey, chainCode)
    }

    /** Parse a derivation path string.
      *
      * Supports paths like "m/44'/1815'/0'/0/0" where ' denotes hardened indices.
      *
      * @param path
      *   the derivation path string
      * @return
      *   sequence of indices (with hardened offset applied where indicated)
      */
    def parsePath(path: String): Seq[Int] = {
        val trimmed = path.trim
        val normalized =
            if trimmed == "m" || trimmed == "m/" then ""
            else if trimmed.startsWith("m/") then trimmed.drop(2)
            else trimmed
        if normalized.isEmpty then Seq.empty
        else
            normalized
                .split("/")
                .filter(_.nonEmpty)
                .map { segment =>
                    val isHardened = segment.endsWith("'") || segment.endsWith("H")
                    val indexStr = if isHardened then segment.dropRight(1) else segment
                    val index = indexStr.toInt
                    if isHardened then (index.toLong + HardenedOffset).toInt else index
                }
                .toSeq
    }

    /** Derive an extended key from seed following a path string.
      *
      * @param seed
      *   the BIP-39 seed
      * @param path
      *   the derivation path (e.g., "m/44'/1815'/0'/0/0")
      * @return
      *   the derived extended key
      */
    def deriveFromPath(seed: Array[Byte], path: String): ExtendedKey = {
        val master = masterKeyFromSeed(seed)
        val indices = parsePath(path)

        indices.foldLeft(master) { (key, index) =>
            // Index already has hardened offset applied if needed
            key.deriveChild(index)
        }
    }
}
