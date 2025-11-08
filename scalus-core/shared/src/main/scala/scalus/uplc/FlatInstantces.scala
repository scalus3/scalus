package scalus.uplc

import io.bullet.borer.Cbor
import scalus.builtin.Data
import scalus.serialization.flat
import scalus.serialization.flat.{DecoderState, EncoderState, Flat, given}

object FlatInstantces:
    given Flat[Data] with

        def bitSize(a: Data): Int =
            summon[Flat[Array[Byte]]].bitSize(Cbor.encode(a).toByteArray)

        def encode(a: Data, encode: EncoderState): Unit =
            flat.encode(Cbor.encode(a).toByteArray, encode)

        def decode(decode: DecoderState): Data =
            val bytes = summon[Flat[Array[Byte]]].decode(decode)
            Cbor.decode(bytes).to[Data].value
