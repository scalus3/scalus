package scalus.cardano.ledger

import scala.collection.immutable.SortedMap
import io.bullet.borer.{DataItem, Decoder, Encoder, Reader, Tag, Writer}

/** Represents a tagged sorted map, which is a sorted set of keys and elements with a tag.
  *
  * Under the hood it's a `SortedMap` because we need to enforce the order of elements. Also, it
  * allows make right CBOR serialization with tag 258.
  *
  * Important: This implementation allows duplicate keys in input (i.e. does not throw exception)
  * and does not keep order in data (sorts by keys).
  */
opaque type TaggedSortedMap[K, A] = SortedMap[K, A]
object TaggedSortedMap {
    inline def empty[K: Ordering, A]: TaggedSortedMap[K, A] = SortedMap.empty[K, A]

    extension [K, A](s: TaggedSortedMap[K, A]) {
        inline def toMap: Map[K, A] = s
    }

    inline def apply[K: Ordering, A](s: SortedMap[K, A]): TaggedSortedMap[K, A] = s

    /** Creates a `TaggedSortedMap` with the specified elements.
      * @tparam A
      *   the type of the `TaggedSortedMap`'s elements
      * @param elems
      *   the elements of the created `TaggedSortedMap`
      * @return
      *   a new `TaggedSortedMap` with elements `elems`
      */
    inline def apply[K: Ordering, A](elems: (K, A)*): TaggedSortedMap[K, A] = from(elems)

    inline def apply[K: Ordering, A](elems: A*)(using K KeyOf A): TaggedSortedMap[K, A] =
        from(elems)

    inline def apply[K: Ordering, A](elems: IterableOnce[A])(using
        K KeyOf A
    ): TaggedSortedMap[K, A] =
        from(elems)

    def from[K: Ordering, A](it: IterableOnce[A])(using keyOf: K KeyOf A): TaggedSortedMap[K, A] =
        from(it.iterator.map(a => keyOf(a) -> a))

    def from[K: Ordering, A](it: IterableOnce[(K, A)]): TaggedSortedMap[K, A] =
        SortedMap.from(it)

    given [K, A: Encoder]: Encoder[TaggedSortedMap[K, A]] with
        def write(w: Writer, value: TaggedSortedMap[K, A]): Writer = {
            w.writeTag(Tag.Other(258))
            w.writeArrayHeader(value.size)
            val seq = value.toSeq.map(_._2)
            seq.foreach(w.write(_))
            w
        }

    infix trait KeyOf[K, A] extends (A => K) {}

    given [K: Ordering, A: Decoder](using K KeyOf A): Decoder[
      TaggedSortedMap[K, A]
    ] with
        def read(r: Reader): TaggedSortedMap[K, A] = {
            // Check for indefinite array tag (258)
            if r.dataItem() == DataItem.Tag then
                val tag = r.readTag()
                if tag.code != 258 then
                    r.validationFailure(s"Expected tag 258 for definite Set, got $tag")
            val seq = Decoder.fromFactory[A, Seq].read(r)
            TaggedSortedMap.from(seq)
        }
}
