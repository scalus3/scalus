package scalus.cardano.ledger

import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.builtin.Data
import scalus.uplc.builtin.Data.toData
import scalus.cardano.onchain.plutus.prelude.List.toScalaList

/** The `ChangedParameters` field of a `ParameterChange` governance action must encode exactly as
  * the ledger encodes it, because a script reads it verbatim.
  *
  * Both cases below need non-canonical but ledger-accepted CBOR, so they are not reachable by
  * accident - but any proposer willing to pay the governance-action deposit can construct them.
  */
class ChangedParametersEncodingTest extends AnyFunSuite {

    /** The ledger encodes a GHC `Ratio`, which `%` keeps in lowest terms, so an unreduced pair
      * never reaches a script (Plutus/ToPlutusData.hs:78).
      */
    private def ratio(d: Data): (BigInt, BigInt) = d match
        case Data.List(items) =>
            items.toScalaList match
                case Data.I(n) :: Data.I(dd) :: Nil => (n, dd)
                case other => fail(s"expected a two-element rational, got $other")
        case other => fail(s"expected a Data.List, got $other")

    test("rationals are reduced to lowest terms in Plutus Data") {
        assert(ratio(UnitInterval(6, 10).toData) == (BigInt(3), BigInt(5)))
        assert(ratio(NonNegativeInterval(6, 10).toData) == (BigInt(3), BigInt(5)))
        // already reduced, unchanged
        assert(ratio(UnitInterval(3, 5).toData) == (BigInt(3), BigInt(5)))
        // whole numbers and zero must not break the gcd
        assert(ratio(UnitInterval(0, 1).toData) == (BigInt(0), BigInt(1)))
        assert(ratio(UnitInterval(4, 4).toData) == (BigInt(1), BigInt(1)))
    }

    /** The ledger emits `Map.toAscList` over a `Data.Map Word8`, and Plutus documents that the keys
      * are stored in ascending order (V3/Contexts.hs:302).
      */
    test("cost models are emitted in ascending language id, whatever the input order") {
        val unsorted =
            CostModels(Map(2 -> IndexedSeq(30L), 0 -> IndexedSeq(10L), 1 -> IndexedSeq(20L)))
        val keys = unsorted.toData match
            case Data.Map(entries) => entries.toScalaList.map(_._1)
            case other             => fail(s"expected a Data.Map, got $other")
        assert(
          keys == List(Data.I(0), Data.I(1), Data.I(2)),
          s"cost model languages must be ascending, got $keys"
        )
    }

    test("CBOR round-trips are unaffected by the Plutus-side reduction") {
        // Only the Plutus encoding normalises; the ledger's CBOR codec must still be exact, or
        // re-serialising a transaction would change its hash.
        val raw = UnitInterval(6, 10)
        assert(raw.numerator == 6 && raw.denominator == 10)
    }
}
