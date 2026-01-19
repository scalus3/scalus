package scalus.cardano.ledger

import io.bullet.borer.{Decoder, Encoder}
import org.typelevel.paiges.Doc
import org.typelevel.paiges.Doc.*
import scalus.utils.{Pretty, Style}

import scala.collection.immutable.*

/** Represents a tagged sorted map with strict duplicate checking.
  *
  * This type enforces that no duplicate keys exist in the input during decoding. If duplicates are
  * found, an exception is thrown. This matches the behavior of Haskell's
  * `decodeMapLikeEnforceNoDuplicates` used for Plutus scripts.
  *
  * Under the hood it's a `SortedMap` because we need to enforce the order of elements. Also, it
  * allows make right CBOR serialization with tag 258.
  *
  * Important: This implementation throws an exception if duplicate keys are found in input during
  * Conway protocol version (PV9+). Pre-Conway, duplicates are silently handled (last value wins).
  */
opaque type TaggedSortedStrictMap[K, A] = SortedMap[K, A]
object TaggedSortedStrictMap extends TaggedSeq:
    inline def apply[K, A](s: SortedMap[K, A]): TaggedSortedStrictMap[K, A] = s
    inline def empty[K: Ordering, A]: TaggedSortedStrictMap[K, A] = SortedMap.empty[K, A]
    inline def apply[K: Ordering, A](s: A*)(using
        pv: ProtocolVersion = ProtocolVersion.conwayPV
    )(using K KeyOf A): TaggedSortedStrictMap[K, A] = from(s)

    extension [K, A](s: TaggedSortedStrictMap[K, A])
        inline def toMap: Map[K, A] = s
        inline def toSortedMap: SortedMap[K, A] = s
        inline def toSet: Set[A] = ListSet.from(s.values)

    infix trait KeyOf[K, A] extends (A => K) {}

    def from[K: Ordering, A](s: IterableOnce[A])(using
        pv: ProtocolVersion = ProtocolVersion.conwayPV
    )(using
        keyOf: K KeyOf A
    ): TaggedSortedStrictMap[K, A] = SortedMap.from(
      (if pv >= ProtocolVersion.conwayPV
       then checkDuplicates(s)
       else s).iterator.map(a => keyOf(a) -> a)
    )

    given [K, A: Encoder]: Encoder[TaggedSortedStrictMap[K, A]] = (w, a) => writeTagged(w, a.values)

    given [K: Ordering, A: Decoder](using
        pv: ProtocolVersion = ProtocolVersion.conwayPV
    )(using K KeyOf A): Decoder[TaggedSortedStrictMap[K, A]] = r => from(readTagged(r))

    given [K, A](using p: Pretty[A]): Pretty[TaggedSortedStrictMap[K, A]] with
        def pretty(a: TaggedSortedStrictMap[K, A], style: Style): Doc =
            val items = a.toMap.values.map(p.pretty(_, style)).toList
            fill(comma + line, items).tightBracketBy(char('['), char(']')).grouped
