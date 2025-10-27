package scalus.cardano.ledger

import io.bullet.borer.{DataItem, Decoder, Encoder, Reader, Tag, Writer}

/** Represents a tagged set, which is an indexed sequence of elements with a tag.
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
  * Important: This implementation does not allow duplicates in input (i.e. throws exception) and
  * keeps order of data (does not sort).
  */
opaque type TaggedOrderedSet[+A] = IndexedSeq[A]
object TaggedOrderedSet {
    inline def empty[A]: TaggedOrderedSet[A] = IndexedSeq.empty[A]

    extension [A](s: TaggedOrderedSet[A]) {

        /** Converts a `TaggedSet` to an `IndexedSeq` */
        inline def toIndexedSeq: IndexedSeq[A] = s
    }

    inline def apply[A](s: IndexedSeq[A]): TaggedOrderedSet[A] = from(s)

    /** Creates a `TaggedSet` with the specified elements.
      * @tparam A
      *   the type of the `TaggedSet`'s elements
      * @param elems
      *   the elements of the created `TaggedSet`
      * @return
      *   a new `TaggedSet` with elements `elems`
      */
    def apply[A](elems: A*): TaggedOrderedSet[A] = from(elems)

    def from[A](it: IterableOnce[A]): TaggedOrderedSet[A] = {
        val seq = IndexedSeq.from(it)
        val set = Set.from(seq)
        require(
          seq.size == set.size,
          s"Final number of elements: ${seq.size}" +
              s" does not match the total count that was decoded: ${set.size}"
        )
        seq
    }

    given [A: Encoder]: Encoder[TaggedOrderedSet[A]] with
        def write(w: Writer, value: TaggedOrderedSet[A]): Writer = {
            w.writeTag(Tag.Other(258))
            w.writeArrayHeader(value.size)
            value.foreach(w.write(_))
            w
        }

    given [A: Decoder]: Decoder[TaggedOrderedSet[A]] with
        def read(r: Reader): TaggedOrderedSet[A] = {
            // Check for indefinite array tag (258)
            if r.dataItem() == DataItem.Tag then
                val tag = r.readTag()
                if tag.code != 258 then
                    r.validationFailure(s"Expected tag 258 for definite Set, got $tag")
            from(Decoder.fromFactory[A, IndexedSeq].read(r))
        }
}
