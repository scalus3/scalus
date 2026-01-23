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
}
