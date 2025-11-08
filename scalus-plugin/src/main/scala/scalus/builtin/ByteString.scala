package scalus.builtin

import scalus.utils.Hex
import scalus.utils.Hex.hexToBytes

class ByteString private (val bytes: Array[Byte]) {
    def toHex: String = Hex.bytesToHex(bytes)
}

object ByteString extends ByteStringFlatInstance {

    /** Creates an empty ByteString
      *
      * Onchain and offchain operation.
      */
    val empty = new ByteString(Array.empty)

    def unsafeFromArray(bytes: Array[Byte]): ByteString = new ByteString(bytes)

    def fromHex(bytes: String): ByteString =
        if bytes.isEmpty then ByteString.empty else new ByteString(bytes.hexToBytes)

    def fromString(s: String): ByteString = new ByteString(s.getBytes("UTF-8"))

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
          */
        def hex(args: Any*): ByteString =
            val hexString = sc.s(args*).replace(" ", "")
            fromHex(hexString)
}
