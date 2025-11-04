package scalus.cardano.ledger

import io.bullet.borer.{Decoder, Encoder}

import scala.collection.immutable.SortedSet

/** Represents a tagged sorted set, which is a sorted set of elements with a tag.
  *
  * Under the hood it's a `SortedSet` because we need to enforce the order of elements. Also, it
  * allows make right CBOR serialization with tag 258.
  *
  * Important: This implementation allows duplicates in input (i.e. does not throw exception),
  * silently removes duplicates and does not keep order in data (sorts elements by given ordering).
  */
opaque type TaggedSortedSet[A] = SortedSet[A]
object TaggedSortedSet extends TaggedSeq:
    inline def apply[A](s: SortedSet[A]): TaggedSortedSet[A] = s
    inline def apply[A: Ordering](elems: A*): TaggedSortedSet[A] = from(elems)
    inline def empty[A: Ordering]: TaggedSortedSet[A] = SortedSet.empty
    inline def from[A: Ordering](s: IterableOnce[A]): TaggedSortedSet[A] = SortedSet.from(s)

    extension [A](s: TaggedSortedSet[A])
        inline def toSet: SortedSet[A] = s
        inline def toSeq: IndexedSeq[A] = s.toIndexedSeq

    given [A: Encoder]: Encoder[TaggedSortedSet[A]] = writeTagged(_, _)
    given [A: Decoder: Ordering]: Decoder[TaggedSortedSet[A]] = r => from(readTagged(r))
