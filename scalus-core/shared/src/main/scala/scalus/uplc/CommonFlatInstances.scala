package scalus.uplc

import scalus.builtin
import scalus.serialization.flat
import scalus.serialization.flat.{DecoderState, EncoderState, Flat, given}

object CommonFlatInstances:
    given Flat[builtin.ByteString] with
        val flatArray = summon[Flat[Array[Byte]]]

        def bitSize(a: builtin.ByteString): Int =
            flatArray.bitSize(a.bytes)

        def encode(a: builtin.ByteString, encode: EncoderState): Unit =
            flatArray.encode(a.bytes, encode)

        def decode(decode: DecoderState): builtin.ByteString =
            builtin.ByteString.unsafeFromArray(flatArray.decode(decode))
