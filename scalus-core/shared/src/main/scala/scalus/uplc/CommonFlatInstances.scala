package scalus.uplc

import scalus.builtin
import scalus.serialization.flat
import scalus.serialization.flat.{DecoderState, EncoderState, Flat, given}

object CommonFlatInstances:
    val constantWidth = 4

    given Flat[builtin.ByteString] with
        val flatArray = summon[Flat[Array[Byte]]]

        def bitSize(a: builtin.ByteString): Int =
            flatArray.bitSize(a.bytes)

        def encode(a: builtin.ByteString, encode: EncoderState): Unit =
            flatArray.encode(a.bytes, encode)

        def decode(decode: DecoderState): builtin.ByteString =
            builtin.ByteString.unsafeFromArray(flatArray.decode(decode))

    def flatForUni(uni: DefaultUni)(using Flat[builtin.Data]): Flat[Any] =
        import DefaultUni.*
        uni match
            case Integer             => summon[Flat[BigInt]].asInstanceOf[Flat[Any]]
            case ByteString          => summon[Flat[builtin.ByteString]].asInstanceOf[Flat[Any]]
            case String              => summon[Flat[String]].asInstanceOf[Flat[Any]]
            case Unit                => summon[Flat[Unit]].asInstanceOf[Flat[Any]]
            case Bool                => summon[Flat[Boolean]].asInstanceOf[Flat[Any]]
            case Data                => summon[Flat[builtin.Data]].asInstanceOf[Flat[Any]]
            case Apply(ProtoList, a) => listFlat(using flatForUni(a)).asInstanceOf[Flat[Any]]
            case Apply(Apply(ProtoPair, a), b) =>
                pairFlat(using flatForUni(a), flatForUni(b)).asInstanceOf[Flat[Any]]
            case _ => throw new Exception(s"Unsupported uni: $uni")

    def encodeUni(uni: DefaultUni): List[Int] =
        uni match
            case DefaultUni.Integer              => List(0)
            case DefaultUni.ByteString           => List(1)
            case DefaultUni.String               => List(2)
            case DefaultUni.Unit                 => List(3)
            case DefaultUni.Bool                 => List(4)
            case DefaultUni.ProtoList            => List(5)
            case DefaultUni.ProtoPair            => List(6)
            case DefaultUni.Apply(uniF, uniA)    => 7 :: encodeUni(uniF) ++ encodeUni(uniA)
            case DefaultUni.Data                 => List(8)
            case DefaultUni.BLS12_381_G1_Element => List(9)
            case DefaultUni.BLS12_381_G2_Element => List(10)
            case DefaultUni.BLS12_381_MlResult   => List(11)

    def decodeUni(state: List[Int]): (DefaultUni, List[Int]) =
        state match
            case 0 :: tail => (DefaultUni.Integer, tail)
            case 1 :: tail => (DefaultUni.ByteString, tail)
            case 2 :: tail => (DefaultUni.String, tail)
            case 3 :: tail => (DefaultUni.Unit, tail)
            case 4 :: tail => (DefaultUni.Bool, tail)
            case 5 :: tail => (DefaultUni.ProtoList, tail)
            case 6 :: tail => (DefaultUni.ProtoPair, tail)
            case 7 :: tail =>
                val (uniF, tail1) = decodeUni(tail)
                val (uniA, tail2) = decodeUni(tail1)
                (DefaultUni.Apply(uniF, uniA), tail2)
            case 8 :: tail  => (DefaultUni.Data, tail)
            case 9 :: tail  => (DefaultUni.BLS12_381_G1_Element, tail)
            case 10 :: tail => (DefaultUni.BLS12_381_G2_Element, tail)
            case 11 :: tail => (DefaultUni.BLS12_381_MlResult, tail)
            case _          => throw new Exception(s"Invalid uni: $state")

    def flatConstant(using Flat[builtin.Data]): Flat[Constant] = new Flat[Constant]:

        val constantTypeTagFlat = new Flat[Int]:
            def bitSize(a: Int): Int = constantWidth

            def encode(a: Int, encode: EncoderState): Unit = encode.bits(constantWidth, a.toByte)

            def decode(decode: DecoderState): Int = decode.bits8(constantWidth)

        def bitSize(a: Constant): Int =
            val uniSize = encodeUni(
              a.tpe
            ).length * (1 + constantWidth) + 1 // List Cons (1 bit) + constant + List Nil (1 bit)
            val valueSize = flatForUni(a.tpe).bitSize(Constant.toValue(a))
            val retval = uniSize + valueSize
            retval

        def encode(a: Constant, encoder: EncoderState): Unit =
            val tags = encodeUni(a.tpe)
            listFlat[Int](using constantTypeTagFlat).encode(tags, encoder)
            flatForUni(a.tpe).encode(Constant.toValue(a), encoder)

        def decode(decoder: DecoderState): Constant =
            val tags = listFlat[Int](using constantTypeTagFlat).decode(decoder)
            val (tpe, _) = decodeUni(tags)
            val uniDecoder = flatForUni(tpe)
            val decoded = uniDecoder.decode(decoder)
            val result = Constant.fromValue(tpe, decoded)
            result
