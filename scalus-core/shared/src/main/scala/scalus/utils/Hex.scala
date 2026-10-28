package scalus.utils

import scala.util.control.NonFatal

object Hex {
    private val HEX_ARRAY = "0123456789abcdef".toCharArray.nn

    extension (bytes: Array[Byte])
        /** Converts an array of bytes to a hex string.
          *
          * Example:
          * {{{
          *   val bytes = Array[Byte](0x0f, 0xa0.toByte, 0x5c)
          *   val hexString = bytes.toHex // "0fa05c"
          * }}}
          *
          * @param bytes
          *   the array of bytes to convert
          * @return
          *   the hex string representation of the byte array
          */
        def toHex: String = bytesToHex(bytes)

    extension (hex: String)
        /** Converts a hex string to an array of bytes.
          *
          * The hex string may contain whitespace characters, which will be ignored. Case
          * insensitive.
          *
          * Example:
          * {{{
          *   val bytes = "0fa05c".hexToBytes // Array[Byte](0x0f, 0xa0.toByte, 0x5c)
          * }}}
          *
          * @param hex
          *   the hex string to convert
          * @return
          *   the array of bytes represented by the hex string
          * @throws IllegalArgumentException
          *   if the string is not a valid hex string (not even length or contains non-hex
          *   characters)
          */
        def hexToBytes: Array[Byte] = {
            val hexString = hex.replaceAll("\\s+", "").nn
            if hexString.isEmpty then return Array.emptyByteArray
            try
                if (hexString.length & 1) != 0 then sys.error("string length is not even")
                hexString.grouped(2).map(Integer.parseInt(_, 16).toByte).toArray
            catch
                case NonFatal(e) =>
                    throw new IllegalArgumentException(s"`$hexString` is not a valid hex string", e)
        }

    def bytesToHex(bytes: Array[Byte]): String = {
        if bytes.isEmpty then return "" // return cached empty string, don't allocate
        val hexChars = new Array[Char](bytes.length * 2)
        var j = 0
        while j < bytes.length do
            val v = bytes(j) & 0xff
            hexChars(j * 2) = HEX_ARRAY(v >>> 4)
            hexChars(j * 2 + 1) = HEX_ARRAY(v & 0x0f)
            j += 1
        new String(hexChars)
    }
}
