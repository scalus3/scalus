package scalus.uplc.builtin

import io.bullet.borer.{Decoder, Encoder, Reader, Writer}
import scalus.utils.Hex.hexToBytes

private trait ByteStringOffchainApi {
    self: ByteString.type =>

    /** Creates an empty ByteString
      *
      * Onchain and offchain operation.
      *
      * @note
      *   This field is specially treated by the Scalus compiler plugin, thus it's not required to
      *   be in the @Compile module.
      */
    val empty = new ByteString(Array.emptyByteArray)

    /** Creates a ByteString from an [[Array[Byte]]
      *
      * Offchain operation, not available onchain.
      *
      * @note
      *   This method is specially treated by the Scalus compiler plugin, thus it's not required to
      *   be in the @Compile module.
      */
    def fromArray(bytes: Array[Byte]): ByteString = new ByteString(bytes.clone)

    /** Creates a ByteString from a variable number of bytes
      *
      * Offchain operation, not available onchain.
      */
    def apply(bytes: Byte*): ByteString = new ByteString(bytes.toArray)

    /** Creates a ByteString of a given size filled with a specific byte
      *
      * Offchain operation, not available onchain.
      *
      * @param size
      *   the size of the ByteString
      * @param byte
      *   the byte to fill the ByteString with
      */
    def fill(size: Int, byte: Byte): ByteString =
        val result = new Array[Byte](size)
        if byte != 0 then java.util.Arrays.fill(result, byte)
        new ByteString(result)

    /** Creates a ByteString from an array of bytes without copying the array */
    def unsafeFromArray(bytes: Array[Byte]): ByteString = new ByteString(bytes)

    /** Creates a ByteString from a hexadecimal string
      *
      * Onchain and offchain operation.
      *
      * @param bytes
      *   the hexadecimal string to convert to a ByteString
      * @note
      *   This method is specially treated by the Scalus compiler plugin, thus it's not required to
      *   be in the @Compile module.
      */
    def fromHex(bytes: String): ByteString = new ByteString(bytes.hexToBytes)

    /** Creates a ByteString from a UTF-8 encoded string
      *
      * Onchain and offchain operation.
      *
      * @param s
      *   the string to convert to a ByteString
      * @note
      *   This method is specially treated by the Scalus compiler plugin, thus it's not required to
      *   be in the @Compile module.
      */
    def fromString(s: String): ByteString = {
        if s.isEmpty then return empty // return cached empty ByteString, don't allocate
        new ByteString(s.getBytes("UTF-8"))
    }

    extension (sc: StringContext)
        /** Hex string interpolator
          *
          * Works on and offchain. Converts a hexadecimal string to a ByteString.
          *
          * @example
          *   {{{
          * val hexString = hex"deadbeef"
          * val withSpaces = hex"de ad be ef"
          * val upperCase = hex"DEADBEEF"
          *   }}}
          * @note
          *   This method is specially treated by the Scalus compiler plugin, thus it's not required
          *   to be in the @Compile module.
          *
          * It supports string interpolation only offchain, so you can do `hex"0102"` onchain and
          * offchain, but `val x = "01"; hex"$x"` only offchain.
          */
        def hex(args: Any*): ByteString =
            fromHex(sc.s(args*))

        /** Creates a ByteString from a UTF-8 encoded string using string interpolation
          *
          * @note
          *   Works only offchain.
          */
        def utf8(args: Any*): ByteString =
            fromString(sc.s(args*))

    given Encoder[ByteString] with
        def write(w: Writer, value: ByteString): Writer =
            w.writeBytes(value.bytes)

    given Decoder[ByteString] with
        def read(r: Reader): ByteString =
            ByteString.unsafeFromArray(r.readBytes())

    /** Platform-agnostic unsigned byte array comparison. Compares two byte arrays
      * lexicographically, treating bytes as unsigned values.
      *
      * @param a
      *   first byte array
      * @param b
      *   second byte array
      * @return
      *   negative if a < b, zero if a == b, positive if a > b
      */
    private def compareUnsigned(a: Array[Byte], b: Array[Byte]): Int = {
        val minLength = math.min(a.length, b.length)

        // Compare byte by byte, treating each as unsigned (0-255)
        var i = 0
        while i < minLength do
            // Convert signed byte to unsigned int for comparison
            val aUnsigned = a(i) & 0xff
            val bUnsigned = b(i) & 0xff

            if aUnsigned != bUnsigned then return aUnsigned - bUnsigned
            i += 1

        // If all compared bytes are equal, compare lengths
        a.length - b.length
    }

    given Ordering[ByteString] with
        def compare(x: ByteString, y: ByteString): Int = {
            // Use the unsigned comparison function defined above
            // because java.util.Arrays.compareUnsigned is not available on Scala.js
            compareUnsigned(x.bytes, y.bytes)
        }

}
