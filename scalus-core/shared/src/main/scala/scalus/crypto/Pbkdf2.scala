package scalus.crypto

/** Pure Scala implementation of PBKDF2 (Password-Based Key Derivation Function 2).
  *
  * Implementation follows RFC 8018. Uses HMAC-SHA512 as the pseudorandom function (PRF).
  *
  * @see
  *   https://datatracker.ietf.org/doc/html/rfc8018
  */
object Pbkdf2 {

    /** Maximum allowed iterations to prevent resource exhaustion. */
    private val MaxIterations: Int = 1_000_000

    /** Maximum allowed key length in bytes to prevent excessive memory allocation. */
    private val MaxKeyLength: Int = 1024

    /** Derive a key using PBKDF2 with HMAC-SHA512.
      *
      * @param password
      *   the password bytes
      * @param salt
      *   the salt bytes
      * @param iterations
      *   number of iterations (2048 for BIP-39)
      * @param keyLength
      *   desired output key length in bytes (64 for BIP-39)
      * @return
      *   derived key
      */
    def deriveKey(
        password: Array[Byte],
        salt: Array[Byte],
        iterations: Int,
        keyLength: Int
    ): Array[Byte] = {
        require(iterations > 0, "iterations must be positive")
        require(iterations <= MaxIterations, s"iterations must be <= $MaxIterations")
        require(keyLength > 0, "keyLength must be positive")
        require(keyLength <= MaxKeyLength, s"keyLength must be <= $MaxKeyLength")

        // HMAC-SHA512 output is 64 bytes
        val hLen = 64

        // Calculate number of blocks needed
        val numBlocks = (keyLength + hLen - 1) / hLen

        val result = new Array[Byte](numBlocks * hLen)

        var blockIndex = 1
        while blockIndex <= numBlocks do
            val block = f(password, salt, iterations, blockIndex)
            System.arraycopy(block, 0, result, (blockIndex - 1) * hLen, hLen)
            blockIndex += 1

        // Truncate to requested length
        if result.length == keyLength then result
        else {
            val truncated = new Array[Byte](keyLength)
            System.arraycopy(result, 0, truncated, 0, keyLength)
            truncated
        }
    }

    /** F function: F(Password, Salt, c, i) = U_1 XOR U_2 XOR ... XOR U_c
      *
      * where: U_1 = PRF(Password, Salt || INT(i)) U_j = PRF(Password, U_{j-1})
      */
    private def f(
        password: Array[Byte],
        salt: Array[Byte],
        iterations: Int,
        blockIndex: Int
    ): Array[Byte] = {
        // U_1 = PRF(Password, Salt || INT(i))
        // INT(i) is a 4-byte big-endian encoding of i
        val saltWithIndex = new Array[Byte](salt.length + 4)
        System.arraycopy(salt, 0, saltWithIndex, 0, salt.length)
        saltWithIndex(salt.length) = (blockIndex >>> 24).toByte
        saltWithIndex(salt.length + 1) = (blockIndex >>> 16).toByte
        saltWithIndex(salt.length + 2) = (blockIndex >>> 8).toByte
        saltWithIndex(salt.length + 3) = blockIndex.toByte

        var u = Hmac.hmacSha512(password, saltWithIndex)
        val result = u.clone()

        // U_2 to U_c
        var i = 2
        while i <= iterations do
            u = Hmac.hmacSha512(password, u)
            // XOR into result
            var j = 0
            while j < 64 do
                result(j) = (result(j) ^ u(j)).toByte
                j += 1
            i += 1

        result
    }
}
