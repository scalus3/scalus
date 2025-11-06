package scalus.cardano.ledger

import io.bullet.borer.{Decoder, Encoder}

import scala.collection.immutable.ListSet

/** Represents a tagged ordered set, which is an indexed sequence of unique elements with a tag.
  *
  * It's a new requirement for the Cardano ledger to have a tagged set. It's a stupid idea and God
  * knows why they came up with it, but now we have to implement it.
  *
  * Under the hood it's an `IndexedSeq` because we need to preserve the order of elements and need a
  * fast access by index. And technically, it can contain duplicates in CBOR.
  *
  * Unfortunately, we cannot make it as
  *
  * `opaque type TaggedOrderedSet[+A] <: IndexedSeq[A] = IndexedSeq[A]`
  *
  * because then `Encoder[TaggedOrderedSet[A]]` conflicts with [[Encoder.forIndexedSeq]]
  *
  * Important: This implementation does not allow duplicates in input (i.e. throws exception) and
  * keeps order of data (does not sort).
  */
opaque type TaggedOrderedStrictSet[+A] = IndexedSeq[A]
object TaggedOrderedStrictSet extends TaggedSeq:
    inline def apply[A](elems: A*): TaggedOrderedStrictSet[A] = from(elems)
    inline def empty[A]: TaggedOrderedStrictSet[A] = IndexedSeq.empty[A]
    inline def from[A](s: IterableOnce[A])(using
        pv: ProtocolVersion = ProtocolVersion.conwayPV
    ): TaggedOrderedStrictSet[A] = ListSet
        .from(
          if pv >= ProtocolVersion.conwayPV
          then checkDuplicates(checkNonEmpty(s))
          else s
        )
        .toIndexedSeq

    extension [A](s: TaggedOrderedStrictSet[A])
        inline def toSeq: IndexedSeq[A] = s
        inline def toSet: Set[A] = ListSet.from(s)

    given [A: Encoder]: Encoder[TaggedOrderedStrictSet[A]] = writeTagged(_, _)
    given [A: Decoder](using
        pv: ProtocolVersion = ProtocolVersion.conwayPV
    ): Decoder[TaggedOrderedStrictSet[A]] = r => from(readTagged(r))
