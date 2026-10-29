package scalus.cardano.ledger

import io.bullet.borer.Borer
import org.scalatest.funsuite.AnyFunSuite
import scalus.serialization.cbor.Cbor
import scalus.uplc.builtin.ByteString
import scalus.utils.Hex.hexToBytes
import scalus.utils.Utils

/** Transactions are read without borer's limit of 1000 nesting levels, which a datum may exceed
  * (#423). The decoders of metadata and of native scripts recurse over the nesting, so they keep
  * that limit themselves: a deeper value fails to decode with borer's `Overflow` instead of
  * overflowing the stack.
  */
class RecursiveNestingTest extends AnyFunSuite {

    private val limit = Cbor.maxRecursiveNestingLevels

    // `levels` arrays of one item around the integer 0
    private def nestedLists(levels: Int): String = "81" * levels + "00"

    // `levels` AllOf around a signature: two arrays per AllOf, and one for the signature
    private def nestedAllOf(levels: Int): String = "820181" * levels + "8200581c" + "00" * 28

    private def reencoded[A: io.bullet.borer.Encoder](value: A): String =
        Utils.bytesToHex(Cbor.encode(value))

    test("metadata nested as deep as the limit decodes") {
        val hex = nestedLists(limit)
        assert(reencoded(Cbor.decode[Metadatum](hex.hexToBytes)) == hex)
        val maps = "a100" * limit + "00"
        assert(reencoded(Cbor.decode[Metadatum](maps.hexToBytes)) == maps)
    }

    test("metadata nested deeper than the limit fails to decode") {
        val lists = intercept[Borer.Error.Overflow[?]] {
            Cbor.decode[Metadatum](nestedLists(limit + 1).hexToBytes)
        }
        assert(lists.getMessage.contains(s"Exceeded $limit nesting levels of metadata"))
        intercept[Borer.Error.Overflow[?]] {
            Cbor.decode[Metadatum](("a100" * (limit + 1) + "00").hexToBytes)
        }
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
