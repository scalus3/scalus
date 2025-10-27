package scalus.cardano.ledger

import scala.collection.immutable.SortedSet
import io.bullet.borer.{DataItem, Decoder, Encoder, Reader, Tag, Writer}

/** Represents a tagged sorted set, which is a sorted set of elements with a tag.
  *
  * Under the hood it's a `SortedSet` because we need to enforce the order of elements. Also, it
  * allows make right CBOR serialization with tag 258.
  *
  * Important: This implementation does not allow duplicates in input (i.e. throws exception) and
  * does not keep order in data (sorts).
  */
opaque type TaggedSortedSet[A] = SortedSet[A]
object TaggedSortedSet {
    inline def empty[A: Ordering]: TaggedSortedSet[A] = SortedSet.empty[A]

    extension [A](s: TaggedSortedSet[A]) {

        /** Converts a `TaggedOrderedSet` to a `SortedSet` */
        inline def toSortedSet: SortedSet[A] = s

        /** Converts a `TaggedOrderedSet` to a `Seq` */
        inline def toSeq: Seq[A] = s.toSeq
    }

    inline def apply[A](s: SortedSet[A]): TaggedSortedSet[A] = s

    /** Creates a `TaggedOrderedSet` with the specified elements.
      * @tparam A
      *   the type of the `TaggedOrderedSet`'s elements
      * @param elems
      *   the elements of the created `TaggedOrderedSet`
      * @return
      *   a new `TaggedOrderedSet` with elements `elems`
      */
    def apply[A: Ordering](elems: A*): TaggedSortedSet[A] = from(elems)

    def from[A: Ordering](it: IterableOnce[A]): TaggedSortedSet[A] = {
        val seq = IndexedSeq.from(it)
        val set = SortedSet.from(seq)
        require(
          seq.size == set.size,
          s"Final number of elements: ${seq.size}" +
              s" does not match the total count that was decoded: ${set.size}"
        )
        set
    }

    given [A: Encoder]: Encoder[TaggedSortedSet[A]] with
        def write(w: Writer, value: TaggedSortedSet[A]): Writer = {
            w.writeTag(Tag.Other(258))
            w.writeArrayHeader(value.size)
            value.foreach(w.write(_))
            w
        }

    given [A: Decoder: Ordering]: Decoder[TaggedSortedSet[A]] with
        def read(r: Reader): TaggedSortedSet[A] = {
            // Check for indefinite array tag (258)
            if r.dataItem() == DataItem.Tag then
                val tag = r.readTag()
                if tag.code != 258 then
                    r.validationFailure(s"Expected tag 258 for definite Set, got $tag")
            Decoder.fromFactory[A, SortedSet].read(r)
        }
}
