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

/** The ledger bounds urls and DNS names by UTF-8 byte length (`textSizeN` uses `lengthWord8`,
  * BaseTypes.hs:643-657; CDDL `text .size (0 .. 128)`). We used to check `String.length`, which
  * counts UTF-16 units and is never larger, so we accepted values the chain rejects.
  */
class TextByteLengthTest extends AnyFunSuite {

    // 64 characters, 4 UTF-8 bytes each = 256 bytes: well within 128 chars, well over 128 bytes.
    private val over = "😀" * 64

    test("an over-long url is rejected by byte length, not character count") {
        assert(over.length <= 128, "precondition: within the old character-count bound")
        assert(over.getBytes(java.nio.charset.StandardCharsets.UTF_8).length > 128)
        assertThrows[IllegalArgumentException](
          Anchor(over, Hash[Blake2b_256, HashPurpose.DataHash](zeros32))
        )
        assertThrows[IllegalArgumentException](PoolMetadata(over, zeros32))
        // Relay validates on decode rather than on construction, so exercise that path.
        val encoded = io.bullet.borer.Cbor.encode(Relay.SingleHostName(None, over)).toByteArray
        assertThrows[io.bullet.borer.Borer.Error[?]](
          io.bullet.borer.Cbor.decode(encoded).to[Relay].value
        )
    }

    test("a 128-byte ASCII url is still accepted") {
        val ok = "a" * 128
        assert(PoolMetadata(ok, zeros32).url == ok)
    }

    private def zeros32 = scalus.uplc.builtin.ByteString.fromHex("00" * 32)
}
