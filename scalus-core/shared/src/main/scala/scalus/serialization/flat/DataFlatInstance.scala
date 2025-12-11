package scalus.serialization.flat

import scalus.builtin
import scalus.builtin.Data
import scalus.prelude.List as PList

/** Flat serialization instance for Data type.
  *
  * This is kept separate from FlatInstances.scala because Data uses scalus.prelude.List, which is
  * not available in the compiler plugin. The compiler plugin doesn't need to serialize Data values
  * directly - it only needs the SIR/SIRType serialization from FlatInstances.
  */
object DataFlatInstance:

    given Flat[Data] with
        private val width = 3

        def bitSize(a: Data): Int = a match
            case Data.Constr(constr, args) =>
                require(
                  constr.isValidLong,
                  s"Data.Constr tag must fit in Long, got $constr"
                )
                width + summon[Flat[Long]].bitSize(constr.toLong) + summon[Flat[List[Data]]]
                    .bitSize(
                      args.toScalaList
                    )
            case Data.Map(values) =>
                width + summon[Flat[List[(Data, Data)]]].bitSize(values.toScalaList)
            case Data.List(values) => width + summon[Flat[List[Data]]].bitSize(values.toScalaList)
            case Data.I(value)     => width + summon[Flat[BigInt]].bitSize(value)
            case Data.B(value)     => width + summon[Flat[builtin.ByteString]].bitSize(value)

        def encode(a: Data, enc: EncoderState): Unit =
            a match
                case Data.Constr(constr, args) =>
                    require(
                      constr.isValidLong,
                      s"Data.Constr tag must fit in Long, got $constr"
                    )
                    enc.bits(width, 0)
                    summon[Flat[Long]].encode(constr.toLong, enc)
                    summon[Flat[List[Data]]].encode(args.toScalaList, enc)
                case Data.Map(values) =>
                    enc.bits(width, 1)
                    summon[Flat[List[(Data, Data)]]].encode(values.toScalaList, enc)
                case Data.List(values) =>
                    enc.bits(width, 2)
                    summon[Flat[List[Data]]].encode(values.toScalaList, enc)
                case Data.I(value) =>
                    enc.bits(width, 3)
                    summon[Flat[BigInt]].encode(value, enc)
                case Data.B(value) =>
                    enc.bits(width, 4)
                    summon[Flat[builtin.ByteString]].encode(value, enc)

        def decode(decode: DecoderState): Data =
            decode.bits8(width) match
                case 0 =>
                    val constr = BigInt(summon[Flat[Long]].decode(decode))
                    val args = summon[Flat[List[Data]]].decode(decode)
                    Data.Constr(constr, PList.from(args))
                case 1 =>
                    val values = summon[Flat[List[(Data, Data)]]].decode(decode)
                    Data.Map(PList.from(values))
                case 2 =>
                    val values = summon[Flat[List[Data]]].decode(decode)
                    Data.List(PList.from(values))
                case 3 =>
                    val value = summon[Flat[BigInt]].decode(decode)
                    Data.I(value)
                case 4 =>
                    val value = summon[Flat[builtin.ByteString]].decode(decode)
                    Data.B(value)
                case c => throw new Exception(s"Invalid data code: $c")
