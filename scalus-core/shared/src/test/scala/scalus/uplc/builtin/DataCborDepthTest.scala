package scalus.uplc.builtin

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.onchain.plutus.prelude.List as PList
import scalus.serialization.flat.{Flat, FlatCodec}
import scalus.uplc.builtin.Data.{B, Constr, I}
import scalus.uplc.eval.{CostingInteger, MemoryUsage}
import scalus.uplc.test.DeepData.nestedLists
import scalus.utils.Utils

/** `encodeData` of the reference has no limit on the nesting depth (#398). A transaction can reach
  * about 12000 levels: the CPU budget of `serialiseData` alone allows that much. Neither borer's
  * validation, which stops at 1000 levels, nor the stack, may limit the encoding or the memory
  * usage that costs it.
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
