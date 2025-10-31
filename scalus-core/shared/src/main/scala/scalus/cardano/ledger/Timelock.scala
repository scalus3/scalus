package scalus.cardano.ledger

import io.bullet.borer.*
import scalus.builtin.{platform, ByteString, given}

import scala.annotation.tailrec

// SlotNo wrapped value
type SlotNo = Long

/** ValidityInterval is a half-open interval: closed on bottom, open on top.
  *
  * [[None]] on bottom is negative infinity, [[None]] on top is positive infinity
  */
case class ValidityInterval(
    invalidBefore: Option[SlotNo],
    invalidHereafter: Option[SlotNo]
)

/** Cardano Timelock script
  *
  * Timelock scripts are used to enforce time-based constraints on transactions. Implemented
  * according to "A Formal Specification of the Cardano Ledger with a Native Multi-Asset
  * Implementation" specification.
  *
  * @see
  *   [evalTimelock](https://github.com/IntersectMBO/cardano-ledger/blob/d428f5bfcf60c9e0d9503f097175e61c968fefb9/eras/allegra/impl/src/Cardano/Ledger/Allegra/Scripts.hs#L346)
  */
enum Timelock:
    case Signature(keyHash: AddrKeyHash)
    case AllOf(scripts: IndexedSeq[Timelock])
    case AnyOf(scripts: IndexedSeq[Timelock])
    case MOf(m: Int, scripts: IndexedSeq[Timelock])
    case TimeStart(lockStart: SlotNo)
    case TimeExpire(lockExpire: SlotNo)

    @transient lazy val scriptHash: ScriptHash = Hash(
      platform.blake2b_224(
        ByteString.unsafeFromArray(0 +: Cbor.encode(this).toByteArray)
      )
    )

    // String representation of Timelock
    lazy val show: String = this match
        case Timelock.TimeStart(i)  => s"(Start >= $i)"
        case Timelock.TimeExpire(i) => s"(Expire < $i)"
        case Timelock.AllOf(xs)     =>
            val inner = xs.map(_.show).mkString(" ")
            s"(AllOf $inner)"
        case Timelock.AnyOf(xs) =>
            val inner = xs.map(_.show).mkString(" ")
            s"(AnyOf $inner)"
        case Timelock.MOf(m, xs) =>
            val inner = xs.map(_.show).mkString(" ")
            s"(MOf $m $inner)"
        case Timelock.Signature(hash) => s"(Signature $hash)"

    /** Evaluates a Timelock script against a set of validator keys and a validity interval
      *
      * @param validatorKeys
      *   Set of validator key hashes
      * @param interval
      *   The transaction validity interval
      * @return
      *   true if the script evaluates to true, false otherwise
      */
    def evaluate(
        validatorKeys: Set[AddrKeyHash],
        interval: ValidityInterval
    ): Boolean = {
        this match
            case Timelock.Signature(keyHash) =>
                validatorKeys.contains(keyHash)

            case Timelock.AllOf(scripts) =>
                scripts.forall(s => s.evaluate(validatorKeys, interval))

            case Timelock.AnyOf(scripts) =>
                scripts.exists(s => s.evaluate(validatorKeys, interval))

            case Timelock.MOf(m, scripts) =>
                // Using a recursive approach to validate M of N scripts
                @tailrec
                def isValidMOf(n: Int, remaining: Seq[Timelock]): Boolean =
                    if n <= 0 then true
                    else
                        remaining match
                            case Seq()              => false
                            case Seq(script, tail*) =>
                                if script.evaluate(validatorKeys, interval) then
                                    isValidMOf(n - 1, tail)
                                else isValidMOf(n, tail)

                isValidMOf(m, scripts)

            case Timelock.TimeStart(lockStart) =>
                Timelock.lteNegInfty(lockStart, interval.invalidBefore)

            case Timelock.TimeExpire(lockExpire) =>
                Timelock.ltePosInfty(interval.invalidHereafter, lockExpire)
    }

    def toCbor: Array[Byte] = Cbor.encode(this).toByteArray

// Companion object with factory methods
object Timelock:
    // Timelock encoder - based on the Haskell code which uses sum types with tags
    given Encoder[Timelock] = Encoder: (w, value) =>
        value match
            case Timelock.Signature(hash) =>
                w.writeArrayHeader(2)
                    .write(0) // Tag for Signature
                    .write(hash)

            case Timelock.AllOf(scripts) =>
                w.writeArrayHeader(2)
                    .write(1) // Tag for AllOf
                    .write(scripts)

            case Timelock.AnyOf(scripts) =>
                w.writeArrayHeader(2)
                    .write(2) // Tag for AnyOf
                    .write(scripts)

            case Timelock.MOf(m, scripts) =>
                w.writeArrayHeader(3)
                    .write(3) // Tag for MOfN
                    .write(m)
                    .write(scripts)

            case Timelock.TimeStart(slot) =>
                w.writeArrayHeader(2)
                    .write(4) // Tag for TimeStart
                    .write(slot)

            case Timelock.TimeExpire(slot) =>
                w.writeArrayHeader(2)
                    .write(5) // Tag for TimeExpire
                    .write(slot)

    // Timelock decoder
    given Decoder[Timelock] = Decoder: r =>
        r.readArrayHeader()
        r.readInt() match
            case 0 => // Signature
                Timelock.Signature(r.read[AddrKeyHash]())

            case 1 => // AllOf
                Timelock.AllOf(r.read[IndexedSeq[Timelock]]())

            case 2 => // AnyOf
                Timelock.AnyOf(r.read[IndexedSeq[Timelock]]())

            case 3 => // MOfN
                val m = r.read[Int]()
                val scripts = r.read[IndexedSeq[Timelock]]()
                Timelock.MOf(m, scripts)

            case 4 => // TimeStart
                Timelock.TimeStart(r.read[SlotNo]())

            case 5 => // TimeExpire
                Timelock.TimeExpire(r.read[SlotNo]())

            case tag =>
                r.validationFailure(s"Invalid Timelock tag: $tag")

    /** Reads a Timelock script from a CBOR encoded byte array
      *
      * @param cbor
      *   The CBOR encoded byte array
      * @return
      *   The Timelock script
      */
    def fromCbor(cbor: Array[Byte]): Timelock =
        Cbor.decode(cbor).to[Timelock].value

    // Helper method to check if a slot is in a validity interval
    def inInterval(slot: SlotNo, interval: ValidityInterval): Boolean =
        (interval.invalidBefore, interval.invalidHereafter) match
            case (None, None)              => true
            case (None, Some(top))         => slot < top
            case (Some(bottom), None)      => bottom <= slot
            case (Some(bottom), Some(top)) => bottom <= slot && slot < top

    /** Checks if less-than-equal comparison holds where Nothing is negative infinity
      * @return
      *   true if i <= j, false if i > j or j is SNothing
      */
    def lteNegInfty(i: SlotNo, j: Option[SlotNo]): Boolean = j match
        case None    => false // i > -∞
        case Some(j) => i <= j

    /** Checks if less-than-equal comparison holds where Nothing is positive infinity
      * @return
      *   true if i <= j, false if i > j or i is SNothing
      */
    def ltePosInfty(i: Option[SlotNo], j: SlotNo): Boolean = i match
        case None    => false // ∞ > j
        case Some(i) => i <= j
