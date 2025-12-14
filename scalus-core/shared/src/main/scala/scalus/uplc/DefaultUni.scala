package scalus.uplc

import scalus.builtin
import scalus.builtin.{ByteString, Data}
import scalus.serialization.flat.{listFlat, Flat, given}

import scala.collection.immutable.List

sealed abstract class DefaultUni:
    type Unlifted

object DefaultUni:

    trait Lift[A]:
        def defaultUni: DefaultUni

    sealed abstract class LiftedUni[A] extends DefaultUni with Lift[A]:
        @deprecated("Not used and will be removed", "0.13.0")
        type Unlifted = A
        def defaultUni: DefaultUni = this

    case object Integer extends LiftedUni[BigInt]
    case object ByteString extends LiftedUni[ByteString]
    case object String extends LiftedUni[String]
    case object Unit extends LiftedUni[Unit]
    case object Bool extends LiftedUni[Boolean]
    case object BLS12_381_G1_Element extends DefaultUni
    case object BLS12_381_G2_Element extends DefaultUni
    case object BLS12_381_MlResult extends DefaultUni
    case object Data extends DefaultUni:
        type Unlifted = Data

    case object ProtoList extends DefaultUni:
        type Unlifted = Nothing // [A] =>> immutable.List[A]

    case object ProtoPair extends DefaultUni:
        type Unlifted = Nothing // [A, B] =>> (A, B)

    case object ProtoArray extends DefaultUni:
        type Unlifted = Nothing // [A] =>> Vector[A]

    case class Apply(f: DefaultUni, arg: DefaultUni) extends DefaultUni:
        type Unlifted = f.Unlifted => arg.Unlifted

    def defaultUniFromValue[A: Lift](value: A): DefaultUni = summon[Lift[A]].defaultUni

    @deprecated("Use Constant.asConstant instead", "0.13.0")
    def asConstant[A: Constant.LiftValue](value: A): Constant =
        summon[Constant.LiftValue[A]].lift(value)

    given Lift[Int] with
        def defaultUni: DefaultUni = DefaultUni.Integer

    given Lift[Long] with
        def defaultUni: DefaultUni = DefaultUni.Integer

    given Lift[BigInt] = Integer
    given Lift[ByteString] = ByteString
    given Lift[String] = String
    given Lift[Unit] = Unit
    given Lift[Boolean] = Bool
    given Lift[scalus.builtin.Data] with
        def defaultUni: DefaultUni = DefaultUni.Data

    def Pair(a: DefaultUni, b: DefaultUni): DefaultUni = Apply(Apply(ProtoPair, a), b)
    def List(a: DefaultUni): DefaultUni = Apply(ProtoList, a)
    def Array(a: DefaultUni): DefaultUni = Apply(ProtoArray, a)

    def flatForUni(uni: DefaultUni)(using Flat[builtin.Data]): Flat[Any] =
        uni match
            case Integer              => summon[Flat[BigInt]].asInstanceOf[Flat[Any]]
            case ByteString           => summon[Flat[builtin.ByteString]].asInstanceOf[Flat[Any]]
            case String               => summon[Flat[String]].asInstanceOf[Flat[Any]]
            case Unit                 => summon[Flat[Unit]].asInstanceOf[Flat[Any]]
            case Bool                 => summon[Flat[Boolean]].asInstanceOf[Flat[Any]]
            case Data                 => summon[Flat[builtin.Data]].asInstanceOf[Flat[Any]]
            case Apply(ProtoList, a)  => listFlat(using flatForUni(a)).asInstanceOf[Flat[Any]]
            case Apply(ProtoArray, a) =>
                // Arrays use the same flat encoding as lists, converted to/from Vector
                vectorFlat(using flatForUni(a)).asInstanceOf[Flat[Any]]
            case Apply(Apply(ProtoPair, a), b) =>
                pairFlat(using flatForUni(a), flatForUni(b)).asInstanceOf[Flat[Any]]
            case _ => throw new Exception(s"Unsupported uni: $uni")

    def encodeUni(uni: DefaultUni): List[Int] =
        uni match
            case DefaultUni.Integer              => scala.List(0)
            case DefaultUni.ByteString           => scala.List(1)
            case DefaultUni.String               => scala.List(2)
            case DefaultUni.Unit                 => scala.List(3)
            case DefaultUni.Bool                 => scala.List(4)
            case DefaultUni.ProtoList            => scala.List(5)
            case DefaultUni.ProtoPair            => scala.List(6)
            case DefaultUni.Apply(uniF, uniA)    => 7 :: encodeUni(uniF) ++ encodeUni(uniA)
            case DefaultUni.Data                 => scala.List(8)
            case DefaultUni.BLS12_381_G1_Element => scala.List(9)
            case DefaultUni.BLS12_381_G2_Element => scala.List(10)
            case DefaultUni.BLS12_381_MlResult   => scala.List(11)
            case DefaultUni.ProtoArray           => scala.List(12)

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
            case 12 :: tail => (DefaultUni.ProtoArray, tail)
            case _          => throw new Exception(s"Invalid uni: $state")
