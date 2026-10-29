package scalus.uplc.builtin

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.{Coin, TaggedSortedSet, Transaction, TransactionBody, TransactionHash, TransactionInput, TransactionWitnessSet}
import scalus.cardano.onchain.plutus.prelude.List as PList
import scalus.serialization.flat.{Flat, FlatCodec}
import scalus.uplc.builtin.Data.{B, Constr, I}
import scalus.uplc.eval.{CostingInteger, MemoryUsage}
import scalus.uplc.test.DeepData.nestedLists
import scalus.utils.Hex.hexToBytes
import scalus.utils.{Pretty, Style, Utils}

/** `encodeData` and `decodeData` of the reference have no limit on the nesting depth (#398, #423).
  * A transaction can reach about 12000 levels: the CPU budget of `serialiseData` alone allows that
  * much. Neither borer's validation, which stops at 1000 levels, nor the stack, may limit the
  * encoding, the decoding or the memory usage that costs it.
  *
  * Deep data is compared by its CBOR here: the equality of [[Data]] recurses.
  */
class DataCborDepthTest extends AnyFunSuite {

    private val depth = 20000

    private def nestedConstrs(depth: Int): Data = {
        val inner = B(ByteString.fromArray(Array.fill(70)(7.toByte)))
        (1 to depth).foldLeft(inner: Data)((d, _) => Constr(0, PList.singleton(d)))
    }

    private def nestedMaps(depth: Int): Data =
        (1 to depth).foldLeft(I(1): Data)((d, _) => Data.Map(PList.singleton((I(0), d))))

    // an indefinite array per level around the innermost item
    private val listsHex = "9f" * depth + "01" + "ff" * depth

    test("serialiseData of deeply nested lists") {
        assert(Utils.bytesToHex(Builtins.serialiseData(nestedLists(depth)).bytes) == listsHex)
    }

    test("Data.toCbor of deeply nested lists") {
        assert(Utils.bytesToHex(nestedLists(depth).toCbor) == listsHex)
    }

    test("serialiseData of deeply nested constrs") {
        // tag 121 and an indefinite array per level; 70 bytes are chunked as 64 + 6
        val inner = "5f" + "5840" + "07" * 64 + "46" + "07" * 6 + "ff"
        val expected = "d8799f" * depth + inner + "ff" * depth
        assert(Utils.bytesToHex(Builtins.serialiseData(nestedConstrs(depth)).bytes) == expected)
    }

    test("serialiseData of deeply nested maps") {
        // a definite map of one entry per level, so no closing bytes
        val expected = "a100" * depth + "01"
        assert(Utils.bytesToHex(Builtins.serialiseData(nestedMaps(depth)).bytes) == expected)
    }

    test("Flat encoding of a deeply nested data constant") {
        // a data constant is flat-encoded as the byte string of its CBOR
        val cbor = Builtins.serialiseData(nestedLists(depth)).bytes
        val expected = Utils.bytesToHex(FlatCodec.encodeBytes(cbor))
        val flat = FlatCodec.encode(nestedLists(depth), summon[Flat[Data]])
        assert(Utils.bytesToHex(flat) == expected)
    }

    private def reencoded(hex: String): String =
        Utils.bytesToHex(Data.fromCbor(hex.hexToBytes).toCbor)

    test("Data.fromCbor of deeply nested lists") {
        assert(reencoded(listsHex) == listsHex)
        // definite arrays of one item are written back as indefinite ones
        assert(reencoded("81" * depth + "01") == listsHex)
    }

    test("Data.fromCbor of deeply nested constrs") {
        // tags 121, 1280 and 102 hold the fields of constrs 0, 7 and 128
        for tag <- Seq("d879", "d90500", "d866821880") do
            val hex = (tag + "9f") * depth + "01" + "ff" * depth
            assert(reencoded(hex) == hex)
        // the indefinite array of tag 102 is written back as a definite one
        val indefinite = "d8669f18809f" * depth + "01" + "ffff" * depth
        assert(reencoded(indefinite) == "d8668218809f" * depth + "01" + "ff" * depth)
    }

    test("Data.fromCbor of deeply nested maps") {
        val definite = "a100" * depth + "01"
        assert(reencoded(definite) == definite)
        // nested in the values of indefinite maps, and in the keys of definite ones
        assert(reencoded("bf00" * depth + "01" + "ff" * depth) == definite)
        assert(reencoded("a1" * depth + "01" + "00" * depth) == "a1" * depth + "01" + "00" * depth)
    }

    test("Flat decoding of a deeply nested data constant") {
        val flat = FlatCodec.encode(nestedLists(depth), summon[Flat[Data]])
        val decoded = FlatCodec.decode(flat, summon[Flat[Data]])
        assert(Utils.bytesToHex(decoded.toCbor) == listsHex)
    }

    test("Transaction.fromCbor of a deeply nested datum") {
        // the ledger reads a datum with `decodeData`, so the transaction is valid
        val witnessSet = TransactionWitnessSet(Seq.empty, None, Set.empty, Seq(nestedLists(depth)))
        // a transaction spends at least one input
        val input =
            TransactionInput(TransactionHash.fromByteString(ByteString.fromHex("1" * 64)), 0)
        val body = TransactionBody(TaggedSortedSet(input), IndexedSeq.empty, Coin.zero)
        val decoded = Transaction.fromCbor(Transaction(body, witnessSet).toCbor)
        val datums = decoded.witnessSet.plutusData.value.toSet.toSeq
        assert(datums.map(datum => Utils.bytesToHex(datum.value.toCbor)) == Seq(listsHex))
        assert(datums.map(datum => Utils.bytesToHex(datum.raw)) == Seq(listsHex))
    }

    test("toString of deeply nested data") {
        val text = nestedLists(depth).toString
        assert(text == "[" * depth + "1" + "]" * depth)
    }

    test("Data.toJson of deeply nested data") {
        val json = Data.toJson(nestedLists(depth))
        assert(json == """{"list":[""" * depth + """{"int":1}""" + "]}" * depth)
    }

    test("Pretty of deeply nested data elides what is nested more than 100 levels deep") {
        def pretty(data: Data) = Pretty[Data].pretty(data, Style.Normal).render(Int.MaxValue)
        assert(pretty(nestedLists(100)) == "[" * 100 + "1" + "]" * 100)
        assert(pretty(nestedLists(depth)) == "[" * 100 + "..." + "]" * 100)
    }

    test("a long list") {
        // reversing the items read, and printing them, must not recurse over the list either
        val length = 100000
        val hex = "9f" + "00" * length + "ff"
        val decoded = Data.fromCbor(hex.hexToBytes)
        assert(Utils.bytesToHex(decoded.toCbor) == hex)
        assert(decoded.toString == Seq.fill(length)("0").mkString("[", ", ", "]"))
        assert(
          Data.toJson(decoded) == Seq
              .fill(length)("""{"int":0}""")
              .mkString("""{"list":[""", ",", "]}")
        )
    }

    test("memory usage of deeply nested data") {
        // 4 per list node, and 4 + 1 for the innermost integer
        assert(MemoryUsage.memoryUsageData(nestedLists(depth)) == CostingInteger(4L * depth + 5))
        // a map node and its key per level, and the innermost integer
        assert(
          MemoryUsage.memoryUsageDataNodeCount(nestedMaps(depth)) ==
              CostingInteger(2L * depth + 1)
        )
    }
}
