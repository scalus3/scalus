package scalus.uplc.builtin.bls12_381

import scalus.uplc.builtin.ByteString

/** BLS12-381 G2 group element for the compiler plugin.
  *
  * Stores the compressed representation (96 bytes). This mirrors the Native platform implementation
  * since the plugin doesn't have access to the blst library.
  */
class G2Element private[builtin] (private[builtin] val compressed: Array[Byte]):
    require(compressed.length == 96, s"G2 compressed must be 96 bytes, got ${compressed.length}")

    def toCompressedByteString: ByteString = ByteString.unsafeFromArray(compressed)

object G2Element:
    def fromCompressedByteString(bs: ByteString): G2Element =
        new G2Element(bs.bytes.clone())

    extension (sc: StringContext) def g2(args: Any*): G2Element = ???
