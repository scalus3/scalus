package scalus.cardano.ledger

import io.bullet.borer.*
import io.bullet.borer.derivation.ArrayBasedCodecs.*
import scalus.builtin.Data

import scala.annotation.threadUnsafe
import scala.collection.immutable
import scala.collection.immutable.TreeMap
import scala.math.Ordered.orderingToOrdered
import scala.math.Ordering.ordered

/** Represents the purpose of a redeemer in Cardano
  *
  * 0: spend 1: mint 2: cert 3: reward 4: voting 5: proposing
  */
enum RedeemerTag:
    case Spend, Mint, Cert, Reward, Voting, Proposing

object RedeemerTag:
    given Ordering[RedeemerTag] with
        def compare(x: RedeemerTag, y: RedeemerTag): Int =
            x.ordinal.compareTo(y.ordinal)

    /** CBOR encoder for RedeemerTag */
    given Encoder[RedeemerTag] with
        def write(w: Writer, value: RedeemerTag): Writer =
            w.writeInt(value.ordinal)

    /** CBOR decoder for RedeemerTag */
    given Decoder[RedeemerTag] with
        def read(r: Reader): RedeemerTag =
            val ordinal = r.readInt()
            if ordinal < 0 || ordinal > 5 then
                r.validationFailure(s"Invalid RedeemerTag value: $ordinal")
            RedeemerTag.fromOrdinal(ordinal)

/** Represents a single redeemer in Cardano transaction A redeemer contains the execution context
  * for a script
  */
case class Redeemer(
    /** The purpose of the redeemer */
    tag: RedeemerTag,

    /** The index within the purpose group */
    index: Int,

    /** The data passed to the script */
    data: Data,

    /** The execution units allocated to the script */
    exUnits: ExUnits
) derives Codec:
    require(index >= 0, s"Redeemer index must be non-negative, got $index")

object Redeemer {
    given Ordering[Redeemer] with
        def compare(x: Redeemer, y: Redeemer): Int =
            x.tag.compareTo(y.tag) match
                case 0 => x.index.compareTo(y.index)
                case c => c
}

/** Represents a collection of redeemers in the transaction witness set */
sealed trait Redeemers:
    /** Convert to list of Redeemer objects */
    def toSeq: Seq[Redeemer] = toIndexedSeq

    /** Convert to list of Redeemer objects */
    def toIndexedSeq: IndexedSeq[Redeemer] = this match
        case Redeemers.Array(list) => list
        case Redeemers.Map(map) =>
            map.view.map { case ((tag, index), (data, exUnits)) =>
                Redeemer(tag, index, data, exUnits)
            }.toIndexedSeq

    def toMap: immutable.Map[(RedeemerTag, Int), (Data, ExUnits)] = this match
        case Redeemers.Array(redeemers) =>
            immutable.TreeMap.from(
              redeemers.iterator.map(r => ((r.tag, r.index), (r.data, r.exUnits)))
            )
        case Redeemers.Map(redeemers) => redeemers

    def isEmpty: Boolean = this match
        case Redeemers.Array(redeemers) => redeemers.isEmpty
        case Redeemers.Map(redeemers)   => redeemers.isEmpty

    /** Calculate total execution units from all redeemers */
    @threadUnsafe
    lazy val totalExUnits: ExUnits = this match
        case Redeemers.Array(redeemers) =>
            redeemers.foldLeft(ExUnits.zero)(_ + _.exUnits)
        case Redeemers.Map(redeemers) =>
            redeemers.values.foldLeft(ExUnits.zero) { case (acc, (_, exUnits)) => acc + exUnits }

object Redeemers:
    /** Array-based representation (legacy format) */
    final case class Array(redeemers: IndexedSeq[Redeemer]) extends Redeemers:
        require(redeemers.nonEmpty, "Must have at least one redeemer")

    /** Map-based representation (new format) Maps (tag, index) pairs to (data, exUnits) pairs
      */
    final case class Map(redeemers: immutable.Map[(RedeemerTag, Int), (Data, ExUnits)])
        extends Redeemers:
        require(redeemers.nonEmpty, "Must have at least one redeemer")

    def apply(redeemers: Redeemer*): Redeemers = from(redeemers)

    def from(redeemers: IterableOnce[Redeemer]): Redeemers =
        // Convert to map format, preserve order
        Map(
          immutable.TreeMap.from(
            redeemers.iterator.map(r => ((r.tag, r.index), (r.data, r.exUnits)))
          )
        )

    /** CBOR encoder for Redeemers */
    given Encoder[Redeemers] with
        def write(w: Writer, value: Redeemers): Writer = value match
            case Redeemers.Array(redeemers) =>
                // Write as array of redeemers
                w.writeIndexedSeq(redeemers)

            case Redeemers.Map(redeemers) =>
                // Write as map from keys to data+exunits
                w.writeMapHeader(redeemers.size)
                // Canonically, the map is sorted by keys, so we can use TreeMap
                TreeMap.from(redeemers).foreach { case ((tag, index), (data, exUnits)) =>
                    // Write key as [tag, index]
                    w.writeArrayHeader(2)
                    w.write(tag)
                    w.writeInt(index)

                    // Write value as [data, exUnits]
                    w.writeArrayHeader(2)
                    w.write(data)
                    w.write(exUnits)
                }
                w

    /** CBOR decoder for Redeemers */
    given Decoder[Redeemers] with
        def read(r: Reader): Redeemers =
            r.dataItem() match
                case DataItem.ArrayHeader | DataItem.ArrayStart =>
                    Redeemers.Array(r.read[IndexedSeq[Redeemer]]())
                case DataItem.MapHeader | DataItem.MapStart =>
                    // Map format
                    given decoder: Decoder[
                      immutable.VectorMap[(RedeemerTag, Int), (Data, ExUnits)]
                    ] = Decoder
                        .constructForMap[(RedeemerTag, Int), (Data, ExUnits), immutable.VectorMap[
                          (RedeemerTag, Int),
                          (Data, ExUnits)
                        ]](immutable.VectorMap.empty)
                    val redeemers =
                        r.read[immutable.VectorMap[(RedeemerTag, Int), (Data, ExUnits)]]()
                    Redeemers.Map(redeemers)
                case _ => r.validationFailure("Expected Array or Map for Redeemers")
