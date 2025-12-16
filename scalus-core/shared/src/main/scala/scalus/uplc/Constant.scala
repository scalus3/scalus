package scalus.uplc

import scalus.builtin
import scalus.serialization.flat.{DecoderState, EncoderState, Flat, given}
import scalus.uplc.DefaultUni.{decodeUni, encodeUni, flatForUni}

import scala.collection.immutable

sealed trait Constant {
    def tpe: DefaultUni
}

object Constant {

    trait LiftValue[-A]:
        def lift(a: A): Constant

    given LiftValue[BigInt] with {
        def lift(a: BigInt): Constant = Integer(a)
    }

    given LiftValue[Int] with {
        def lift(a: Int): Constant = Integer(a)
    }

    given LiftValue[Long] with {
        def lift(a: Long): Constant = Integer(a)
    }

    given LiftValue[builtin.ByteString] with {
        def lift(a: builtin.ByteString): Constant = ByteString(a)
    }

    given LiftValue[java.lang.String] with {
        def lift(a: java.lang.String): Constant = String(a)
    }

    given LiftValue[Boolean] with {
        def lift(a: Boolean): Constant = Bool(a)
    }

    given LiftValue[Unit] with {
        def lift(a: Unit): Constant = Unit
    }

    given LiftValueData[A <: scalus.builtin.Data]: LiftValue[A] = new LiftValue[A] {
        def lift(a: A): Constant = Data(a)
    }

    given seqLiftValue[A: LiftValue: DefaultUni.Lift]: LiftValue[Seq[A]] with {
        def lift(a: Seq[A]): Constant =
            List(
              summon[DefaultUni.Lift[A]].defaultUni,
              a.view.map(summon[LiftValue[A]].lift).toList
            )
    }

    given tupleLiftValue[A: LiftValue: DefaultUni.Lift, B: LiftValue: DefaultUni.Lift]
        : LiftValue[(A, B)] = new LiftValue[(A, B)] {
        def lift(a: (A, B)): Constant = Pair(
          summon[LiftValue[A]].lift(a._1),
          summon[LiftValue[B]].lift(a._2)
        )
    }

    case class Integer(value: BigInt) extends Constant:
        def tpe = DefaultUni.Integer

    case class ByteString(value: builtin.ByteString) extends Constant:
        def tpe = DefaultUni.ByteString

    case class String(value: java.lang.String) extends Constant:
        def tpe = DefaultUni.String

    case object Unit extends Constant:
        def tpe = DefaultUni.Unit

    case class Bool(value: Boolean) extends Constant:
        def tpe = DefaultUni.Bool

    case class Data(value: scalus.builtin.Data) extends Constant:
        def tpe = DefaultUni.Data

    case class List(elemType: DefaultUni, value: immutable.List[Constant]) extends Constant:
        def tpe = DefaultUni.Apply(DefaultUni.ProtoList, elemType)

    case class Pair(a: Constant, b: Constant) extends Constant:
        def tpe = DefaultUni.Apply(DefaultUni.Apply(DefaultUni.ProtoPair, a.tpe), b.tpe)

    case class Array(elemType: DefaultUni, value: IndexedSeq[Constant]) extends Constant:
        def tpe = DefaultUni.Apply(DefaultUni.ProtoArray, elemType)

    case class BLS12_381_G1_Element(value: builtin.BLS12_381_G1_Element) extends Constant:
        def tpe = DefaultUni.BLS12_381_G1_Element

    case class BLS12_381_G2_Element(value: builtin.BLS12_381_G2_Element) extends Constant:
        def tpe = DefaultUni.BLS12_381_G2_Element

    case class BLS12_381_MlResult(value: builtin.BLS12_381_MlResult) extends Constant:
        def tpe = DefaultUni.BLS12_381_MlResult

    extension [A: LiftValue](c: A) {

        /** Extension method to convert a Scala value to a UPLC [[Constant]].
          *
          * Provides a convenient syntax for lifting Scala values into the Plutus constant
          * representation. The conversion is type-safe and requires an implicit [[LiftValue]]
          * instance for the value type.
          *
          * Supported types include:
          *   - Numeric types: [[BigInt]], [[Int]], [[Long]]
          *   - [[builtin.ByteString]]
          *   - [[java.lang.String]]
          *   - [[Boolean]]
          *   - [[Unit]]
          *   - [[scalus.builtin.Data]] and its subtypes
          *   - Collections: [[Seq]] (requires element type to have LiftValue)
          *   - Tuples: pairs of values with LiftValue instances
          *
          * @example
          *   {{{
          *   import scalus.uplc.Constant.*
          *
          *   val intConst = 42.asConstant
          *   // Integer(42)
          *
          *   val strConst = "hello".asConstant
          *   // String("hello")
          *
          *   val listConst = Seq(1, 2, 3).asConstant
          *   // List(Integer, List(Integer(1), Integer(2), Integer(3)))
          *   }}}
          *
          * @tparam A
          *   the type of the value to convert, must have an implicit [[LiftValue]] instance
          * @return
          *   a [[Constant]] representing the lifted value
          * @see
          *   [[LiftValue]] for defining custom value conversions
          */

        def asConstant: Constant = summon[LiftValue[A]].lift(c)
    }

    def fromValue(tpe: DefaultUni, a: Any): Constant = tpe match {
        case DefaultUni.Integer    => Integer(a.asInstanceOf[BigInt])
        case DefaultUni.ByteString => ByteString(a.asInstanceOf[builtin.ByteString])
        case DefaultUni.String     => String(a.asInstanceOf[java.lang.String])
        case DefaultUni.Unit       => Unit
        case DefaultUni.Bool       => Bool(a.asInstanceOf[Boolean])
        case DefaultUni.Data =>
            Data(a.asInstanceOf[scalus.builtin.Data])
        case DefaultUni.Apply(DefaultUni.ProtoList, elemType) =>
            List(elemType, a.asInstanceOf[Seq[Any]].view.map(fromValue(elemType, _)).toList)
        case DefaultUni.Apply(DefaultUni.Apply(DefaultUni.ProtoPair, aType), bType) =>
            Pair(
              fromValue(aType, a.asInstanceOf[(Any, Any)]._1),
              fromValue(bType, a.asInstanceOf[(Any, Any)]._2)
            )
        case DefaultUni.Apply(DefaultUni.ProtoArray, elemType) =>
            Array(
              elemType,
              a.asInstanceOf[IndexedSeq[Any]].view.map(fromValue(elemType, _)).toIndexedSeq
            )
        case DefaultUni.BLS12_381_G1_Element =>
            BLS12_381_G1_Element(a.asInstanceOf[builtin.BLS12_381_G1_Element])
        case DefaultUni.BLS12_381_G2_Element =>
            BLS12_381_G2_Element(a.asInstanceOf[builtin.BLS12_381_G2_Element])
        case DefaultUni.BLS12_381_MlResult =>
            throw new IllegalArgumentException("Cannot convert to BLS12_381_MlResult")
        case _ => throw new IllegalArgumentException(s"Cannot convert $a to $tpe")
    }

    def toValue(c: Constant): Any = c match
        case Integer(value)              => value
        case ByteString(value)           => value
        case String(value)               => value
        case Unit                        => ()
        case Bool(value)                 => value
        case Data(value)                 => value
        case List(_, value)              => value.map(toValue)
        case Pair(a, b)                  => (toValue(a), toValue(b))
        case Array(_, value)             => value.map(toValue)
        case BLS12_381_G1_Element(value) => value
        case BLS12_381_G2_Element(value) => value
        case BLS12_381_MlResult(value) =>
            throw new IllegalArgumentException("Cannot convert BLS12_381_MlResult")

    given flatConstant(using Flat[builtin.Data]): Flat[Constant] = new Flat[Constant]:
        val constantWidth = 4
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

}
