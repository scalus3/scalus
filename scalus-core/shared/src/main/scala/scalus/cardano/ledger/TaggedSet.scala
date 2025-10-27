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
  * Important: This implementation allows duplicates in input (i.e. does not throw exception) and
  * keeps order of data (does not sort) .
  */
opaque type TaggedSet[+A] = IndexedSeq[A]
object TaggedSet {
    inline def empty[A]: TaggedSet[A] = IndexedSeq.empty[A]

    extension [A](s: TaggedSet[A]) {

        /** Converts a `TaggedSet` to an `IndexedSeq` */
        inline def toIndexedSeq: IndexedSeq[A] = s
    }

    inline def apply[A](s: IndexedSeq[A]): TaggedSet[A] = from(s)

    /** Creates a `TaggedSet` with the specified elements.
      * @tparam A
      *   the type of the `TaggedSet`'s elements
      * @param elems
      *   the elements of the created `TaggedSet`
      * @return
      *   a new `TaggedSet` with elements `elems`
      */
    def apply[A](elems: A*): TaggedSet[A] = from(elems)

    def from[A](it: IterableOnce[A]): TaggedSet[A] = Set.from(it).toIndexedSeq

    given [A: Encoder]: Encoder[TaggedSet[A]] with
        def write(w: Writer, value: TaggedSet[A]): Writer = {
            w.writeTag(Tag.Other(258))
            w.writeArrayHeader(value.size)
            value.foreach(w.write(_))
            w
        }

    given [A: Decoder]: Decoder[TaggedSet[A]] with
        def read(r: Reader): TaggedSet[A] = {
            // Check for indefinite array tag (258)
            if r.dataItem() == DataItem.Tag then
                val tag = r.readTag()
                if tag.code != 258 then r.validationFailure(s"Expected tag 258, got $tag")
            from(Decoder.fromFactory[A, IndexedSeq].read(r))
        }
}
