package scalus.uplc.builtin

/** BLS12-381 G1 group element for the compiler plugin.
  *
  * Stores the compressed representation (48 bytes). This mirrors the Native platform implementation
  * since the plugin doesn't have access to the blst library.
  */
class BLS12_381_G1_Element private[builtin] (private[builtin] val compressed: Array[Byte]):
    require(compressed.length == 48, s"G1 compressed must be 48 bytes, got ${compressed.length}")

    def toCompressedByteString: ByteString = ByteString.unsafeFromArray(compressed)

object BLS12_381_G1_Element:
    def fromCompressedByteString(bs: ByteString): BLS12_381_G1_Element =
        new BLS12_381_G1_Element(bs.bytes.clone())

    extension (sc: StringContext) def g1(args: Any*): BLS12_381_G1_Element = ???

/** BLS12-381 G2 group element for the compiler plugin.
  *
  * Stores the compressed representation (96 bytes). This mirrors the Native platform implementation
  * since the plugin doesn't have access to the blst library.
  */
class BLS12_381_G2_Element private[builtin] (private[builtin] val compressed: Array[Byte]):
    require(compressed.length == 96, s"G2 compressed must be 96 bytes, got ${compressed.length}")

    def toCompressedByteString: ByteString = ByteString.unsafeFromArray(compressed)

object BLS12_381_G2_Element:
    def fromCompressedByteString(bs: ByteString): BLS12_381_G2_Element =
        new BLS12_381_G2_Element(bs.bytes.clone())

    extension (sc: StringContext) def g2(args: Any*): BLS12_381_G2_Element = ???

class BLS12_381_MlResult
