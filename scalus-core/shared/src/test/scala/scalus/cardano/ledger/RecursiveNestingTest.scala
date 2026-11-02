package scalus.cardano.ledger

import io.bullet.borer.Borer
import org.scalatest.funsuite.AnyFunSuite
import scalus.serialization.cbor.Cbor
import scalus.uplc.builtin.ByteString
import scalus.utils.Hex.hexToBytes
import scalus.utils.Utils

/** Transactions are read without borer's limit of 1000 nesting levels, which a datum may exceed
  * (#423). The decoders of metadata and of native scripts keep that limit themselves: a deeper
  * value fails to decode with borer's `Overflow`. How deep they read and write does not depend on
  * the stack (NestingStackTest, on the JVM).
  */
class RecursiveNestingTest extends AnyFunSuite {
    import RecursiveNestingTest.*

    private val limit = Cbor.maxMetadataAndScriptNestingLevels

    test("metadata nested as deep as the limit decodes") {
        val hex = nestedLists(limit)
        assert(reencoded(Cbor.decode[Metadatum](hex.hexToBytes)) == hex)
        val maps = nestedMaps(limit)
        assert(reencoded(Cbor.decode[Metadatum](maps.hexToBytes)) == maps)
    }

    test("metadata nested deeper than the limit fails to decode") {
        val lists = intercept[Borer.Error.Overflow[?]] {
            Cbor.decode[Metadatum](nestedLists(limit + 1).hexToBytes)
        }
        assert(lists.getMessage.contains(s"Exceeded $limit nesting levels of metadata"))
        val maps = intercept[Borer.Error.Overflow[?]] {
            Cbor.decode[Metadatum](nestedMaps(limit + 1).hexToBytes)
        }
        assert(maps.getMessage.contains(s"Exceeded $limit nesting levels of metadata"))
    }

    test("metadata in indefinite-length arrays and maps decodes as in definite ones") {
        // [_ [_ 0]] and {_ 0: {_ 1: 2}}
        assert(
          Cbor.decode[Metadatum]("9f9f00ffff".hexToBytes) ==
              Metadatum.List(Vector(Metadatum.List(Vector(Metadatum.Int(0)))))
        )
        assert(
          Cbor.decode[Metadatum]("bf00bf0102ffff".hexToBytes) ==
              Metadatum.Map(
                Map(Metadatum.Int(0) -> Metadatum.Map(Map(Metadatum.Int(1) -> Metadatum.Int(2))))
              )
        )
        // as deep as the limit, and one level deeper fails
        val hex = nestedIndefiniteLists(limit)
        assert(reencoded(Cbor.decode[Metadatum](hex.hexToBytes)) == nestedLists(limit))
        val deeper = intercept[Borer.Error.Overflow[?]] {
            Cbor.decode[Metadatum](nestedIndefiniteLists(limit + 1).hexToBytes)
        }
        assert(deeper.getMessage.contains(s"Exceeded $limit nesting levels of metadata"))
    }

    test("a break between a key and its value does not end an indefinite-length map") {
        // {_ 0: and the break
        intercept[Borer.Error[?]](Cbor.decode[Metadatum]("bf00ff".hexToBytes))
        // {_ 0: 1} is complete before its break
        assert(
          Cbor.decode[Metadatum]("bf0001ff".hexToBytes) ==
              Metadatum.Map(Map(Metadatum.Int(0) -> Metadatum.Int(1)))
        )
    }

    test("a transaction with metadata nested 20000 levels deep fails to decode") {
        val input =
            TransactionInput(TransactionHash.fromByteString(ByteString.fromHex("1" * 64)), 0)
        val body = TransactionBody(TaggedSortedSet(input), IndexedSeq.empty, Coin.zero)
        val metadata = AuxiliaryData.Metadata(Map(Word64(0) -> Metadatum.Int(0)))
        val cbor = Utils.bytesToHex(Transaction(body, TransactionWitnessSet.empty, metadata).toCbor)
        // the auxiliary data, `{0: 0}`, comes last: put the deep metadatum under its key
        assert(cbor.endsWith("a10000"))
        val deep = cbor.dropRight(2) + nestedLists(20000)
        intercept[Borer.Error.Overflow[?]](Transaction.fromCbor(deep.hexToBytes))
    }

    test("a native script nested as deep as the limit decodes") {
        // 2 * 499 + 1 = 999 levels; one more AllOf makes 1001
        val levels = (limit - 1) / 2
        val hex = nestedAllOf(levels)
        assert(reencoded(Cbor.decode[Timelock](hex.hexToBytes)) == hex)
    }

    test("a native script nested deeper than the limit fails to decode") {
        val levels = (limit - 1) / 2 + 1
        val error = intercept[Borer.Error.Overflow[?]] {
            Cbor.decode[Timelock](nestedAllOf(levels).hexToBytes)
        }
        assert(error.getMessage.contains(s"Exceeded $limit nesting levels of a native script"))
        intercept[Borer.Error.Overflow[?]](Cbor.decode[Timelock](nestedAllOf(20000).hexToBytes))
    }
}

object RecursiveNestingTest {

    /** `value` encoded, in hexadecimal. */
    def reencoded[A: io.bullet.borer.Encoder](value: A): String =
        Utils.bytesToHex(Cbor.encode(value))

    /** `levels` arrays of one item around the integer 0. */
    def nestedLists(levels: Int): String = "81" * levels + "00"

    /** `levels` indefinite-length arrays of one item around the integer 0. */
    def nestedIndefiniteLists(levels: Int): String = "9f" * levels + "00" + "ff" * levels

    /** `levels` maps of one entry, its key 0, around the integer 0. */
    def nestedMaps(levels: Int): String = "a100" * levels + "00"

    /** `levels` AllOf around a signature: two arrays per AllOf, and one for the signature. */
    def nestedAllOf(levels: Int): String = "820181" * levels + "8200581c" + "00" * 28
}
