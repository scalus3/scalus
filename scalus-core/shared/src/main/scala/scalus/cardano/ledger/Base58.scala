package scalus.cardano.ledger

/** Base58 encoding/decoding implementation using Bitcoin alphabet.
  *
  * Base58 is a binary-to-text encoding scheme that uses 58 alphanumeric characters, excluding
  * characters that can be visually confused: 0, O, I, l.
  *
  * This implementation is used for Byron-era Cardano addresses.
  */
object Base58 {

    /** Bitcoin Base58 alphabet (excludes 0, O, I, l to avoid visual confusion) */
    private final val ALPHABET = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"

    private final val ALPHABET_MAP: Map[Char, Int] = {
        var m = Map.empty[Char, Int]
        var i = 0
        while i < ALPHABET.length do {
            m += ALPHABET.charAt(i) -> i
            i += 1
        }
        m
    }

    private final val BASE = BigInt(58)

    /** Encode a byte array to a Base58 string.
      *
      * @param input
      *   The bytes to encode
      * @return
      *   Base58 encoded string
      */
    final def encode(input: Array[Byte]): String = {
        if input.isEmpty then return ""

        // Count leading zeros - they map to '1' characters
        val leadingZeros = input.iterator.takeWhile(_ == 0).length

        // If all zeros, return '1' for each zero byte
        if leadingZeros == input.length then return ALPHABET(0).toString * leadingZeros

        // Convert bytes to BigInt (unsigned interpretation)
        val dataBytes = input.drop(leadingZeros)
        var num = BigInt(1, dataBytes)

        // Convert to base58
        val builder = new StringBuilder
        while num > 0 do {
            val (quotient, remainder) = num /% BASE
            builder.append(ALPHABET(remainder.toInt))
            num = quotient
        }

        // Prepend '1's for leading zero bytes and reverse
        (ALPHABET(0).toString * leadingZeros) + builder.reverse.toString
    }

    /** Decode a Base58 string to a byte array.
      *
      * @param input
      *   The Base58 string to decode
      * @return
      *   Decoded bytes
      * @throws IllegalArgumentException
      *   if the string contains invalid characters
      */
    final def decode(input: String): Array[Byte] = {
        if input.isEmpty then return Array.empty

        // Count leading '1's - they represent zero bytes
        val leadingOnes = input.iterator.takeWhile(_ == ALPHABET(0)).length

        // If all '1's, return that many zero bytes
        if leadingOnes == input.length then return Array.fill(leadingOnes)(0.toByte)

        // Convert from base58 to BigInt
        var num = BigInt(0)
        val dataChars = input.drop(leadingOnes)
        var i = 0
        while i < dataChars.length do {
            val c = dataChars.charAt(i)
            val digit = ALPHABET_MAP.getOrElse(
              c,
              throw new IllegalArgumentException(s"Invalid Base58 character: '$c'")
            )
            num = num * BASE + digit
            i += 1
        }

        // Convert BigInt to bytes
        val decoded =
            if num == BigInt(0) then Array.empty[Byte]
            else {
                val bytes = num.toByteArray
                // BigInt may add a leading zero byte for sign; remove it
                if bytes.length > 1 && bytes(0) == 0 then bytes.tail else bytes
            }

        // Prepend zero bytes for leading '1's
        Array.fill(leadingOnes)(0.toByte) ++ decoded
    }
}
