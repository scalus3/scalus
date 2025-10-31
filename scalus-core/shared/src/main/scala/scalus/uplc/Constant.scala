package scalus.uplc

import scalus.builtin

import scala.collection.immutable

sealed trait Constant:
    def tpe: DefaultUni

object Constant:

    trait LiftValue[-A]:
        def lift(a: A): Constant

    given LiftValue[BigInt] with { def lift(a: BigInt): Constant = Integer(a) }
    given LiftValue[Int] with { def lift(a: Int): Constant = Integer(a) }
    given LiftValue[Long] with { def lift(a: Long): Constant = Integer(a) }
    given LiftValue[builtin.ByteString] with {
        def lift(a: builtin.ByteString): Constant = ByteString(a)
    }
    given LiftValue[java.lang.String] with { def lift(a: java.lang.String): Constant = String(a) }
    given LiftValue[Boolean] with { def lift(a: Boolean): Constant = Bool(a) }
    given LiftValue[Unit] with { def lift(a: Unit): Constant = Unit }
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

    case class BLS12_381_G1_Element(value: builtin.BLS12_381_G1_Element) extends Constant:
        def tpe = DefaultUni.BLS12_381_G1_Element

    case class BLS12_381_G2_Element(value: builtin.BLS12_381_G2_Element) extends Constant:
        def tpe = DefaultUni.BLS12_381_G2_Element

    case class BLS12_381_MlResult(value: builtin.BLS12_381_MlResult) extends Constant:
        def tpe = DefaultUni.BLS12_381_MlResult

    def fromValue(tpe: DefaultUni, a: Any): Constant = tpe match {
        case DefaultUni.Integer    => Integer(a.asInstanceOf[BigInt])
        case DefaultUni.ByteString => ByteString(a.asInstanceOf[builtin.ByteString])
        case DefaultUni.String     => String(a.asInstanceOf[java.lang.String])
        case DefaultUni.Unit       => Unit
        case DefaultUni.Bool       => Bool(a.asInstanceOf[Boolean])
        case DefaultUni.Data       =>
            Data(a.asInstanceOf[scalus.builtin.Data])
        case DefaultUni.Apply(DefaultUni.ProtoList, elemType) =>
            List(elemType, a.asInstanceOf[Seq[Any]].view.map(fromValue(elemType, _)).toList)
        case DefaultUni.Apply(DefaultUni.Apply(DefaultUni.ProtoPair, aType), bType) =>
            Pair(
              fromValue(aType, a.asInstanceOf[(Any, Any)]._1),
              fromValue(bType, a.asInstanceOf[(Any, Any)]._2)
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
        case BLS12_381_G1_Element(value) => value
        case BLS12_381_G2_Element(value) => value
        case BLS12_381_MlResult(value)   =>
            throw new IllegalArgumentException("Cannot convert BLS12_381_MlResult")
