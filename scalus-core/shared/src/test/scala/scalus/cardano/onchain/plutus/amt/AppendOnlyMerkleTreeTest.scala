package scalus.cardano.onchain.plutus.amt

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.offchain.amt.AppendOnlyMerkleTree as OffChainAmt
import scalus.cardano.onchain.RequirementError
import scalus.uplc.builtin.Builtins.*
import scalus.uplc.builtin.ByteString

class AppendOnlyMerkleTreeTest extends AnyFunSuite {
    import AppendOnlyMerkleTree.*

    private val depth = 4 // small tree for unit tests (16 slots)

    test("empty tree root is deterministic") {
        val tree1 = OffChainAmt.empty(depth)
        val tree2 = OffChainAmt.empty(depth)
        assert(tree1.rootHash == tree2.rootHash)
        assert(tree1.rootHash.length == 32)
    }

    test("append and verify single member") {
        val key = ByteString.fromString("hello")
        val tree0 = OffChainAmt.empty(depth)
        val tree1 = tree0.append(key)

        // verify membership on-chain
        val proof = tree1.proveMembership(key)
        val slot = byteStringToInteger(true, sliceByteString(0, 3, proof))
        val siblings = sliceByteString(3, depth * 32, proof)
        verifyMember(tree1.rootHash, key, slot, BigInt(depth), siblings)
    }

    test("append and verify multiple members") {
        var tree = OffChainAmt.empty(depth)
        val keys = (0 until 10).map(i => ByteString.fromString(s"key-$i"))

        for key <- keys do tree = tree.append(key)

        for key <- keys do
            val proof = tree.proveMembership(key)
            val slot = byteStringToInteger(true, sliceByteString(0, 3, proof))
            val siblings = sliceByteString(3, depth * 32, proof)
            verifyMember(tree.rootHash, key, slot, BigInt(depth), siblings)
    }

    test("append proof verifies on-chain") {
        val key = ByteString.fromString("first")
        val tree0 = OffChainAmt.empty(depth)

        val appendProof = tree0.proveAppend()
        val newRoot = append(tree0.rootHash, BigInt(tree0.size), BigInt(depth), key, appendProof)

        val tree1 = tree0.append(key)
        assert(newRoot == tree1.rootHash)
    }

    test("multiple appends with on-chain verification") {
        var tree = OffChainAmt.empty(depth)
        val keys = (0 until 5).map(i => ByteString.fromString(s"elem-$i"))

        for key <- keys do
            val appendProof = tree.proveAppend()
            val newRoot =
                append(tree.rootHash, BigInt(tree.size), BigInt(depth), key, appendProof)
            tree = tree.append(key)
            assert(newRoot == tree.rootHash, s"Root mismatch after appending $key")
    }

    test("wrong key fails membership verification") {
        val tree = OffChainAmt.empty(depth).append(ByteString.fromString("real-key"))

        val proof = tree.proveMembership(ByteString.fromString("real-key"))
        val slot = byteStringToInteger(true, sliceByteString(0, 3, proof))
        val siblings = sliceByteString(3, depth * 32, proof)

        assertThrows[RequirementError] {
            verifyMember(tree.rootHash, ByteString.fromString("fake-key"), slot, BigInt(depth), siblings)
        }
    }

    test("wrong slot fails membership verification") {
        val tree = OffChainAmt.empty(depth)
            .append(ByteString.fromString("key-0"))
            .append(ByteString.fromString("key-1"))

        val proof = tree.proveMembership(ByteString.fromString("key-0"))
        val siblings = sliceByteString(3, depth * 32, proof)

        // use wrong slot
        assertThrows[RequirementError] {
            verifyMember(
              tree.rootHash,
              ByteString.fromString("key-0"),
              BigInt(1),
              BigInt(depth),
              siblings
            )
        }
    }

    test("append to non-empty slot fails") {
        val tree = OffChainAmt.empty(depth).append(ByteString.fromString("first"))

        // get siblings for slot 0 (already occupied)
        val proof = tree.proveMembership(ByteString.fromString("first"))
        val siblings = sliceByteString(3, depth * 32, proof)

        assertThrows[RequirementError] {
            append(tree.rootHash, BigInt(0), BigInt(depth), ByteString.fromString("second"), siblings)
        }
    }

    test("fill tree to capacity") {
        val smallDepth = 3 // 8 slots
        var tree = OffChainAmt.empty(smallDepth)
        val keys = (0 until 8).map(i => ByteString.fromString(s"k$i"))

        for key <- keys do tree = tree.append(key)

        // verify all
        for key <- keys do
            val proof = tree.proveMembership(key)
            val slot = byteStringToInteger(true, sliceByteString(0, 3, proof))
            val siblings = sliceByteString(3, smallDepth * 32, proof)
            verifyMember(tree.rootHash, key, slot, BigInt(smallDepth), siblings)
    }

    test("larger depth D=10") {
        val d = 10
        var tree = OffChainAmt.empty(d)
        val keys = (0 until 50).map(i => ByteString.fromString(s"element-$i"))

        for key <- keys do tree = tree.append(key)

        // spot-check a few
        for i <- Seq(0, 10, 25, 49) do
            val key = keys(i)
            val proof = tree.proveMembership(key)
            val slot = byteStringToInteger(true, sliceByteString(0, 3, proof))
            val siblings = sliceByteString(3, d * 32, proof)
            verifyMember(tree.rootHash, key, slot, BigInt(d), siblings)
    }

    test("proof sizes are correct") {
        val d = 15
        val tree = OffChainAmt.empty(d).append(ByteString.fromString("test"))

        val memberProof = tree.proveMembership(ByteString.fromString("test"))
        assert(memberProof.length == 3 + d * 32, s"membership proof: expected ${3 + d * 32}, got ${memberProof.length}")

        val appendProof = tree.proveAppend()
        assert(appendProof.length == d * 32, s"append proof: expected ${d * 32}, got ${appendProof.length}")
    }
}
