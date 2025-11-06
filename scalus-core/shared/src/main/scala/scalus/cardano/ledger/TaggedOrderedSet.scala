package scalus.cardano.ledger

import io.bullet.borer.{Decoder, Encoder}

import scala.collection.immutable.ListSet

/** Represents a tagged set, which is an indexed sequence of unique elements with a tag.
  *
  * It's a new requirement for the Cardano ledger to have a tagged set. It's a stupid idea and God
  * knows why they came up with it, but now we have to implement it.
  *
  * Under the hood it's an `IndexedSeq` because we need to preserve the order of elements and need a
  * fast access by index. And technically, it can contain duplicates in CBOR.
  *
  * Unfortunately, we cannot make it as
  *
  * `opaque type TaggedSet[+A] <: IndexedSeq[A] = IndexedSeq[A]`
  *
  * because then `Encoder[TaggedSet[A]]` conflicts with [[Encoder.forIndexedSeq]]
  *
  * Important: This implementation allows duplicates in input (i.e. does not throw exception) and
  * keeps order of data (does not sort) but eliminates duplicates.
  */
opaque type TaggedOrderedSet[A] = IndexedSeq[A]
object TaggedOrderedSet extends TaggedSeq {
    inline def apply[A](elems: A*): TaggedOrderedSet[A] = from(elems)
    inline def empty[A]: TaggedOrderedSet[A] = IndexedSeq.empty
    inline def from[A](a: IterableOnce[A]): TaggedOrderedSet[A] = ListSet.from(a).toIndexedSeq

    extension [A](s: TaggedOrderedSet[A])
        inline def toSeq: IndexedSeq[A] = s
        inline def toSet: Set[A] = ListSet.from(s)

    given [A: Encoder]: Encoder[TaggedOrderedSet[A]] = writeTagged(_, _)
    given [A: Decoder]: Decoder[TaggedOrderedSet[A]] = r => from(readTagged(r))
}
