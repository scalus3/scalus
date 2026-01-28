package scalus.uplc.builtin.bls12_381

import scalus.uplc.builtin.ByteString

/** BLS12-381 G1 group element for the compiler plugin.
  *
  * Stores the compressed representation (48 bytes). This mirrors the Native platform implementation
  * since the plugin doesn't have access to the blst library.
  */
class G1Element private[builtin] (private[builtin] val compressed: Array[Byte]):
    require(compressed.length == 48, s"G1 compressed must be 48 bytes, got ${compressed.length}")

    def toCompressedByteString: ByteString = ByteString.unsafeFromArray(compressed)

object G1Element:
    def fromCompressedByteString(bs: ByteString): G1Element =
        new G1Element(bs.bytes.clone())

    extension (sc: StringContext) def g1(args: Any*): G1Element = ???
