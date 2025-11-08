package scalus.builtin

import scalus.serialization.flat
import scalus.serialization.flat.{DecoderState, EncoderState, Flat, given}

private trait ByteStringFlatInstance {
    given Flat[ByteString] with
        val flatArray = summon[Flat[Array[Byte]]]

        def bitSize(a: ByteString): Int =
            flatArray.bitSize(a.bytes)

        def encode(a: ByteString, encode: EncoderState): Unit =
            flatArray.encode(a.bytes, encode)

        def decode(decode: DecoderState): ByteString =
            ByteString.unsafeFromArray(flatArray.decode(decode))
}
