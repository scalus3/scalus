package scalus.cardano.ledger

import io.bullet.borer.{Decoder, Encoder}

import scala.collection.immutable.{ListSet, SortedMap}

/** Represents a tagged sorted map, which is a sorted set of keys and elements with a tag.
  *
  * Under the hood it's a `SortedMap` because we need to enforce the order of elements. Also, it
  * allows make right CBOR serialization with tag 258.
  *
  * Important: This implementation allows duplicate keys in input (i.e. does not throw exception)
  * silently removes duplicates and does not keep order in data (sorts by keys).
  */
opaque type TaggedSortedMap[K, A] = SortedMap[K, A]
object TaggedSortedMap extends TaggedSeq:
    inline def apply[K, A](s: SortedMap[K, A]): TaggedSortedMap[K, A] = s
    inline def empty[K: Ordering, A]: TaggedSortedMap[K, A] = SortedMap.empty[K, A]
    inline def apply[K: Ordering, A](s: A*)(using K KeyOf A): TaggedSortedMap[K, A] = from(s)

    infix trait KeyOf[K, A] extends (A => K) {}
    def from[K: Ordering, A](s: IterableOnce[A])(using keyOf: K KeyOf A): TaggedSortedMap[K, A] =
        SortedMap.from(s.iterator.map(a => keyOf(a) -> a))

    extension [K, A](s: TaggedSortedMap[K, A])
        inline def toMap: Map[K, A] = s
        inline def toSet: Set[A] = ListSet.from(s.values)

    given [K, A: Encoder]: Encoder[TaggedSortedMap[K, A]] = (w, a) => writeTagged(w, a.values)
    given [K: Ordering, A: Decoder](using K KeyOf A): Decoder[TaggedSortedMap[K, A]] = r =>
        from(checkNonEmpty(readTagged(r)))
