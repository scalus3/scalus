package scalus.serialization.cbor

import io.bullet.borer.{Cbor as Cborer, Decoder, Encoder, Input, Output, Receiver}
import scalus.uplc.builtin.ByteString

/** Utility object for CBOR (Concise Binary Object Representation) encoding and decoding operations.
  *
  * Provides convenient methods for serializing Scala objects to CBOR format and deserializing CBOR
  * data back to Scala objects using the borer library.
  */
object Cbor {

    /** Encodes a value to CBOR format.
      *
      * This implementation allows [[scalus.cardano.ledger.KeepRaw]] to store original CBOR bytes
      * when encoding.
      * @note
      *   this method uses a workaround to enable storing original CBOR bytes described
      *   [here](https://github.com/sirthias/borer/issues/761#issuecomment-2939269119)
      *
      * @param value
      *   the value to encode
      * @tparam A
      *   the type of the value, must have an implicit Encoder available
      * @return
      *   the CBOR-encoded data as a byte array
      */
    def encode[A: Encoder](value: A): Array[Byte] = {
        val output = Output.ToByteArray(bufferSize = 1024, allowBufferCaching = true)
        val writer = Cborer.writer(output, Cborer.EncodingConfig(), Receiver.nopTransformer)
        writer.write(value)
        output.result()
    }

    def encodeToByteString[A: Encoder](value: A): ByteString = {
        ByteString.unsafeFromArray(encode(value))
    }

    /** Decodes CBOR data back to a Scala object of the specified type.
      *
      * @param data
      *   the CBOR-encoded byte array to decode
      * @tparam A
      *   the target type to decode to, must have an implicit Decoder available
      * @return
      *   the decoded object of type A
      * @throws io.bullet.borer.Borer.Error
      *   if the data cannot be decoded to the specified type
      */
    def decode[A: Decoder](data: Array[Byte]): A = {
        Cborer.decode(data).to[A].value
    }

    /** Unwraps the CBOR byte string that `data` starts with: returns its content, without the
      * header, and the bytes after that byte string.
      *
      * Reads the byte string as cborg's `decodeBytes` does, which is how the ledger reads a Plutus
      * script: the length must be definite, and may be encoded in more bytes than it needs.
      *
      * @throws IllegalArgumentException
      *   if `data` does not start with a definite-length byte string
      */
    private[scalus] def unwrapCborByteString(data: Array[Byte]): (Array[Byte], Array[Byte]) = {
        require(data.nonEmpty, "Expected a CBOR byte string, got end of input")
        // the length follows the header byte as a big-endian unsigned integer of `size` bytes
        def length(size: Int): Long = {
            require(data.length > size, "CBOR byte string header is cut short")
            (1 to size).foldLeft(0L)((n, i) => (n << 8) | (data(i) & 0xff))
        }
        val (headerSize, contentSize) = data(0) & 0xff match
            case h if h >= 0x40 && h <= 0x57 => (1, (h - 0x40).toLong)
            case 0x58                        => (2, length(1))
            case 0x59                        => (3, length(2))
            case 0x5a                        => (5, length(4))
            case 0x5b                        => (9, length(8))
            case h =>
                throw new IllegalArgumentException(
                  f"Expected a definite-length CBOR byte string, got header byte 0x$h%02x"
                )
        // a 64-bit length above Long.MaxValue reads as negative
        require(
          contentSize >= 0 && contentSize <= data.length - headerSize,
          s"CBOR byte string of $contentSize bytes, but only ${data.length - headerSize} bytes follow its header"
        )
        val end = headerSize + contentSize.toInt
        (data.slice(headerSize, end), data.drop(end))
    }
}
