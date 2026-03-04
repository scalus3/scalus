package scalus.examples.anonymousdata

import scalus.uplc.builtin.Builtins.{blake2b_256, xorByteString}
import scalus.uplc.builtin.ByteString

/** Off-chain cryptographic utilities for anonymous data operations.
  *
  * Provides key derivation and XOR encryption for the anonymous data contract. Anonymity is
  * achieved through:
  *   - '''Map key''': `blake2b_256(pubkeyhash || nonce)` — unlinkable without knowing the nonce
  *   - '''Encryption key''': `blake2b_256(nonce || "enc")` — separate from map key, can be shared
  *     for on-chain verification without revealing identity
  *   - '''XOR encryption''': data XOR with key stream derived from encryption key
  */
object AnonymousDataCrypto {

    /** Derive the map key: blake2b_256(pubkeyhash || nonce).
      *
      * This is the key used in the on-chain AssocMap. Without knowing the nonce, an observer cannot
      * link the key to the pubkeyhash.
      */
    def deriveKey(pubkeyhash: ByteString, nonce: ByteString): ByteString =
        blake2b_256(pubkeyhash ++ nonce)

    /** Derive the encryption key: blake2b_256(nonce || "enc").
      *
      * Separate from the map key so it can be shared with verifiers without revealing the nonce or
      * the identity link.
      */
    def deriveEncKey(nonce: ByteString): ByteString =
        blake2b_256(nonce ++ ByteString.fromString("enc"))

    /** XOR encrypt/decrypt data using key expansion.
      *
      * For data longer than 32 bytes, the key is expanded by hashing successive blocks: key0 =
      * encKey, key1 = blake2b_256(encKey || 0x01), key2 = blake2b_256(encKey || 0x02), etc.
      *
      * XOR is its own inverse, so encrypt and decrypt are the same operation.
      */
    def encrypt(data: ByteString, encKey: ByteString): ByteString = {
        val len = data.length.toInt
        if len == 0 then ByteString.empty
        else
            val keyStream = expandKey(encKey, len)
            // XOR with padding=false requires same length, so take exactly len from keyStream
            xorByteString(false, data, keyStream.take(len))
    }

    /** Decrypt is the same as encrypt (XOR is its own inverse). */
    def decrypt(encData: ByteString, encKey: ByteString): ByteString =
        encrypt(encData, encKey)

    /** Expand the encryption key to at least `length` bytes using blake2b_256 chaining. */
    private def expandKey(encKey: ByteString, length: Int): ByteString = {
        var result = encKey // 32 bytes from blake2b_256
        var counter: Byte = 1
        while result.length < length do
            val nextBlock = blake2b_256(encKey ++ ByteString.fromArray(Array(counter)))
            result = result ++ nextBlock
            counter = (counter + 1).toByte
        result
    }
}
