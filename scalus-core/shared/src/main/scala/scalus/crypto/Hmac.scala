package scalus.crypto

/** Pure Scala implementation of HMAC (Hash-based Message Authentication Code).
  *
  * Implementation follows RFC 2104. Provides HMAC-SHA512 for use in BIP-39 and SLIP-0010.
  *
  * @see
  *   https://datatracker.ietf.org/doc/html/rfc2104
  */
object Hmac {

    // Block size for SHA-512 in bytes
    private val Sha512BlockSize = 128

    /** Compute HMAC-SHA512.
      *
      * @param key
      *   the secret key (any length)
      * @param data
      *   the data to authenticate
      * @return
      *   64-byte MAC
      */
    def hmacSha512(key: Array[Byte], data: Array[Byte]): Array[Byte] = {
        // Step 1: If key is longer than block size, hash it
        val keyBytes =
            if key.length > Sha512BlockSize then Sha512.hash(key)
            else key

        // Step 2: Pad key to block size with zeros
        val paddedKey = new Array[Byte](Sha512BlockSize)
        System.arraycopy(keyBytes, 0, paddedKey, 0, keyBytes.length)

        // Step 3: Create inner and outer padding
        val ipad = new Array[Byte](Sha512BlockSize)
        val opad = new Array[Byte](Sha512BlockSize)

        var i = 0
        while i < Sha512BlockSize do
            ipad(i) = (paddedKey(i) ^ 0x36).toByte
            opad(i) = (paddedKey(i) ^ 0x5c).toByte
            i += 1

        // Step 4: HMAC = H(K XOR opad || H(K XOR ipad || data))
        val innerInput = new Array[Byte](Sha512BlockSize + data.length)
        System.arraycopy(ipad, 0, innerInput, 0, Sha512BlockSize)
        System.arraycopy(data, 0, innerInput, Sha512BlockSize, data.length)
        val innerHash = Sha512.hash(innerInput)

        val outerInput = new Array[Byte](Sha512BlockSize + 64)
        System.arraycopy(opad, 0, outerInput, 0, Sha512BlockSize)
        System.arraycopy(innerHash, 0, outerInput, Sha512BlockSize, 64)
        Sha512.hash(outerInput)
    }
}
