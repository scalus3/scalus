package scalus.serialization.flat

import scalus.serialization.flat.HashConsed.ForwardRefAcceptor

class HashConsedEncoderState(
    val encode: EncoderState,
    val hashConsed: HashConsed.State,
    val debug: Boolean = false
) {

    inline def lookupValue(ihc: Int, tag: HashConsed.Tag): Option[HashConsedRef[?]] = {
        hashConsed.lookupValue(ihc, tag)
    }

    inline def setRef[A <: AnyRef](ihc: Int, tag: HashConsed.Tag, a: HashConsedRef[A]): Unit = {
        hashConsed.setRef(ihc, tag, a)
    }

    inline def putForwardRefAcceptor(fw: HashConsed.ForwardRefAcceptor): Unit = {
        hashConsed.putForwardRef(fw)
    }

    def withDebug(debug: Boolean): HashConsedEncoderState =
        new HashConsedEncoderState(encode, hashConsed, debug)

}

object HashConsedEncoderState {

    def withSize(size: Int): HashConsedEncoderState =
        HashConsedEncoderState(EncoderState(size), HashConsed.State.empty)

}

class HashConsedDecoderState(
    val decode: DecoderState,
    val hashConsed: HashConsed.State,
    val debug: Boolean = false
) {

    def runFinCallbacks(): Unit =
        hashConsed.finishCallbacks(debug)

}

trait HashConsedFlat[A] extends Flat[A] {

    final override def bitSize(a: A): Int =
        bitSizeHC(a, HashConsed.State.empty)

    final override def encode(a: A, encoderState: EncoderState): Unit =
        encodeHC(a, HashConsedEncoderState(encoderState, HashConsed.State.empty))

    def decode(decode: DecoderState): A =
        decodeHC(HashConsedDecoderState(decode, HashConsed.State.empty))

    def bitSizeHC(a: A, encoderState: HashConsed.State): Int

    def encodeHC(a: A, encoderState: HashConsedEncoderState): Unit

    def decodeHC(decoderState: HashConsedDecoderState): A

}

trait HashConsedReprFlat[A <: AnyRef, SA <: HashConsedRef[A]] {

    type Repr = SA

    def toRepr(a: A): SA

    def bitSizeHC(a: A, hashConsed: HashConsed.State): Int

    def encodeHC(a: A, encode: HashConsedEncoderState): Unit

    def decodeHC(decode: HashConsedDecoderState): SA

}

type HashConsedReprRefFlat[A <: AnyRef] = HashConsedReprFlat[A, HashConsedRef[A]]

object HashConsedReprFlat {

    case class ListRepl[A <: AnyRef, SA <: HashConsedRef[A]](elems: List[SA])
        extends HashConsedRef[List[A]] {

        var result: List[A] | Null = null

        override def finValue(
            hashConsed: HashConsed.State,
            level: Int,
            parents: HSRIdentityHashMap
        ): List[A] =
            if !(result eq null) then result
            else
                // if parents.get(this) != null then
                //    throw new Exception(s"Cycle detected, this=${this}")
                // parents.put(this, this)
                val nextLevel = level + 1
                val retval = elems.map(_.finValue(hashConsed, nextLevel, parents))
                // parents.remove(this)
                result = retval
                retval
    }

    implicit def listHashConsedRepr[A <: AnyRef, SA <: HashConsedRef[A]](using
        flatA: HashConsedReprFlat[A, SA]
    ): HashConsedReprFlat[List[A], ListRepl[A, SA]] =
        new HashConsedReprFlat[List[A], ListRepl[A, SA]] {

            def toRepr(a: List[A]): ListRepl[A, SA] = ListRepl[A, SA](a.map(flatA.toRepr))

            def bitSizeHC(a: List[A], hashConsed: HashConsed.State): Int = {
                a.foldLeft(1)((acc, elem) => acc + 1 + flatA.bitSizeHC(elem, hashConsed))
            }

            def encodeHC(a: List[A], encode: HashConsedEncoderState): Unit = {
                a.foreach { elem =>
                    encode.encode.bits(1, 1)
                    flatA.encodeHC(elem, encode)
                }
                encode.encode.bits(1, 0)
            }

            def decodeHC(decode: HashConsedDecoderState): ListRepl[A, SA] = {
                val builder = List.newBuilder[SA]
                while decode.decode.bits8(1) == 1.toByte
                do
                    val elem = flatA.decodeHC(decode)
                    builder += elem
                ListRepl(builder.result())
            }

        }

    def listRepr[A <: AnyRef, SA <: HashConsedRef[A]](
        flatRepr: HashConsedReprFlat[A, SA]
    ): HashConsedReprFlat[List[A], ListRepl[A, SA]] =
        listHashConsedRepr[A, SA](using flatRepr)

    case class StringMapRep[A <: AnyRef, SA <: HashConsedRef[A]](elems: Map[String, SA])
        extends HashConsedRef[Map[String, A]] {

        var result: Map[String, A] | Null = null

        override def finValue(
            hashConsed: HashConsed.State,
            level: Int,
            parents: HSRIdentityHashMap
        ): Map[String, A] =
            if !(result eq null) then result
            else
                // if parents.get(this) != null then
                //    throw new Exception(s"Cycle detected, this=${this}")
                // parents.put(this, this)
                val nextLevel = level + 1
                val retval = elems.view.mapValues(_.finValue(hashConsed, nextLevel, parents)).toMap
                // parents.remove(this)
                result = retval
                retval
    }

    implicit def stringMapHashConsedRepr[A <: AnyRef, SA <: HashConsedRef[A]](using
        flatA: HashConsedReprFlat[A, SA]
    ): HashConsedReprFlat[Map[String, A], StringMapRep[A, SA]] =
        new HashConsedReprFlat[Map[String, A], StringMapRep[A, SA]] {

            import scalus.serialization.flat.given

            def toRepr(a: Map[String, A]): StringMapRep[A, SA] =
                StringMapRep[A, SA](a.view.mapValues(flatA.toRepr).toMap)

            def bitSizeHC(a: Map[String, A], hashConsed: HashConsed.State): Int = {
                a.foldLeft(1)((acc, elem) =>
                    acc + 1 + summon[Flat[String]].bitSize(elem._1) + flatA.bitSizeHC(
                      elem._2,
                      hashConsed
                    )
                )
            }

            def encodeHC(a: Map[String, A], encode: HashConsedEncoderState): Unit = {
                a.foreach { elem =>
                    encode.encode.bits(1, 1)
                    summon[Flat[String]].encode(elem._1, encode.encode)
                    flatA.encodeHC(elem._2, encode)
                }
                encode.encode.bits(1, 0)
            }

            def decodeHC(decode: HashConsedDecoderState): StringMapRep[A, SA] = {
                val builder = Map.newBuilder[String, SA]
                while decode.decode.bits8(1) == 1.toByte
                do
                    val key = summon[Flat[String]].decode(decode.decode)
                    val elem = flatA.decodeHC(decode)
                    builder += (key -> elem)
                StringMapRep(builder.result())
            }

        }

}

/*
trait HashConsedTagged[A] {

    def tag: HashConsed.Tag

}
 */

object HashConsedFlat {

    given listHashConsedFlat[A](using flatA: HashConsedFlat[A]): HashConsedFlat[List[A]] with

        def bitSizeHC(a: List[A], hashConsed: HashConsed.State): Int = {
            val elemsSize =
                a.foldLeft(0)((acc, elem) => acc + 1 + flatA.bitSizeHC(elem, hashConsed))
            val retval = elemsSize + 1
            retval
        }
        def encodeHC(a: List[A], encode: HashConsedEncoderState): Unit = {
            a.foreach { elem =>
                encode.encode.bits(1, 1)
                flatA.encodeHC(elem, encode)
            }
            encode.encode.bits(1, 0)
        }
        def decodeHC(decode: HashConsedDecoderState): List[A] = {
            val builder = List.newBuilder[A]
            while decode.decode.bits8(1) == 1.toByte do builder += flatA.decodeHC(decode)
            builder.result()
        }

}

object PlainIntFlat extends HashConsedFlat[Int] {

    def bitSizeHC(a: Int, hashConsed: HashConsed.State): Int = 4 * 8

    def encodeHC(a: Int, encoderState: HashConsedEncoderState): Unit =
        encoderState.encode.bits(8, (a >>> 24).toByte)
        encoderState.encode.bits(8, (a >>> 16).toByte)
        encoderState.encode.bits(8, (a >>> 8).toByte)
        encoderState.encode.bits(8, a.toByte)

    def decodeHC(decoderState: HashConsedDecoderState): Int =
        var retval = 0
        retval |= ((decoderState.decode.bits8(8) << 24) & 0xff000000)
        retval |= ((decoderState.decode.bits8(8) << 16) & 0x00ff0000)
        retval |= ((decoderState.decode.bits8(8) << 8) & 0x0000ff00)
        retval |= (decoderState.decode.bits8(8) & 0x000000ff)
        retval

}

trait HashConsedMutRefReprFlat[A <: AnyRef, SA <: HashConsedRef[A]]
    extends HashConsedReprFlat[A, SA] {

    def tag: HashConsed.Tag

    def bitSizeHCNew(a: A, encode: HashConsed.State): Int

    def encodeHCNew(a: A, encode: HashConsedEncoderState): Unit

    def decodeHCNew(decoderState: HashConsedDecoderState): SA

    def saFromForward(state: HashConsed.State, fw: ForwardRefAcceptor): SA

    def saFromRef(ref: HashConsedRef[A]): SA

    override def bitSizeHC(a: A, encoderState: HashConsed.State): Int = {
        val ihc = encoderState.allocKey(a, tag)
        var retval = PlainIntFlat.bitSize(ihc)
        encoderState.lookup(ihc, tag) match
            case None =>
                encoderState.putForwardRef(HashConsed.ForwardRefAcceptor(ihc, tag, Nil))
                retval += bitSizeHCNew(a, encoderState)
                encoderState.setRef(ihc, tag, toRepr(a))
            case Some(_) =>
        retval
    }

    override def encodeHC(a: A, encoderState: HashConsedEncoderState): Unit = {
        val ihc = encoderState.hashConsed.allocKey(a, tag)
        PlainIntFlat.encode(ihc, encoderState.encode)
        encoderState.hashConsed.lookup(ihc, tag) match
            case None =>
                encoderState.putForwardRefAcceptor(HashConsed.ForwardRefAcceptor(ihc, tag, Nil))
                encodeHCNew(a, encoderState)
                encoderState.setRef(ihc, tag, toRepr(a))
            case Some(ref) =>
    }

    override def decodeHC(decoderState: HashConsedDecoderState): SA = {
        val ihc = PlainIntFlat.decode(decoderState.decode)
        decoderState.hashConsed.lookup(ihc, tag) match
            case None =>
                decoderState.hashConsed.putForwardRef(HashConsed.ForwardRefAcceptor(ihc, tag, Nil))
                val sa = decodeHCNew(decoderState)
                if sa.isForward then
                    throw new IllegalStateException("decodeHCNew returned a forward reference")
                decoderState.hashConsed.setRef(ihc, tag, sa)
                sa
            case Some(Left(fw)) =>
                saFromForward(decoderState.hashConsed, fw)
            case Some(Right(saa)) =>
                saFromRef(saa.asInstanceOf[HashConsedRef[A]])

    }

}
