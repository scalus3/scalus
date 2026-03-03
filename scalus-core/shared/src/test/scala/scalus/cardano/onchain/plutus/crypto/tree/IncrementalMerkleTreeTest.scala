package scalus.cardano.onchain.plutus.crypto.tree

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.offchain.crypto.tree.IncrementalMerkleTree as OffChainImt
import scalus.cardano.onchain.RequirementError
import scalus.uplc.builtin.ByteString

class IncrementalMerkleTreeTest extends AnyFunSuite {
    import IncrementalMerkleTree.*

    private val depth = 4 // small tree for unit tests (16 slots)

    test("empty tree root is deterministic") {
        val tree1 = OffChainImt.empty(depth)
        val tree2 = OffChainImt.empty(depth)
        assert(tree1.rootHash == tree2.rootHash)
        assert(tree1.rootHash.length == 32)
    }

    test("append and verify single member") {
        val key = ByteString.fromString("hello")
        val tree0 = OffChainImt.empty(depth)
        val tree1 = tree0.append(key)

        // verify membership on-chain
        val proof = tree1.proveMembership(key)
        verifyMember(tree1.rootHash, key, BigInt(depth), proof)
    }

    test("append and verify multiple members") {
        var tree = OffChainImt.empty(depth)
        val keys = (0 until 10).map(i => ByteString.fromString(s"key-$i"))

        for key <- keys do tree = tree.append(key)

        for key <- keys do
            val proof = tree.proveMembership(key)
            verifyMember(tree.rootHash, key, BigInt(depth), proof)
    }

    test("append proof verifies on-chain") {
        val key = ByteString.fromString("first")
        val tree0 = OffChainImt.empty(depth)

        val appendProof = tree0.proveAppend()
        val newRoot = append(tree0.rootHash, BigInt(tree0.size), BigInt(depth), key, appendProof)

        val tree1 = tree0.append(key)
        assert(newRoot == tree1.rootHash)
    }

    test("multiple appends with on-chain verification") {
        var tree = OffChainImt.empty(depth)
        val keys = (0 until 5).map(i => ByteString.fromString(s"elem-$i"))

        for key <- keys do
            val appendProof = tree.proveAppend()
            val newRoot =
                append(tree.rootHash, BigInt(tree.size), BigInt(depth), key, appendProof)
            tree = tree.append(key)
            assert(newRoot == tree.rootHash, s"Root mismatch after appending $key")
    }

    test("wrong key fails membership verification") {
        val tree = OffChainImt.empty(depth).append(ByteString.fromString("real-key"))

        val proof = tree.proveMembership(ByteString.fromString("real-key"))

        assertThrows[RequirementError] {
            verifyMember(tree.rootHash, ByteString.fromString("fake-key"), BigInt(depth), proof)
        }
    }

    test("wrong path fails membership verification") {
        val tree = OffChainImt
            .empty(depth)
            .append(ByteString.fromString("key-0"))
            .append(ByteString.fromString("key-1"))

        // get proof for key-1 but try to verify key-0 with it
        val proof1 = tree.proveMembership(ByteString.fromString("key-1"))

        assertThrows[RequirementError] {
            verifyMember(
              tree.rootHash,
              ByteString.fromString("key-0"),
              BigInt(depth),
              proof1
            )
        }
    }

    test("append to non-empty slot fails") {
        val tree = OffChainImt.empty(depth).append(ByteString.fromString("first"))

        // get siblings for slot 0 (already occupied) via proveAppend on empty tree
        val emptyTree = OffChainImt.empty(depth)
        val siblings = emptyTree.proveAppend()

        assertThrows[RequirementError] {
            append(
              tree.rootHash,
              BigInt(0),
              BigInt(depth),
              ByteString.fromString("second"),
              siblings
            )
        }
    }

    test("fill tree to capacity") {
        val smallDepth = 3 // 8 slots
        var tree = OffChainImt.empty(smallDepth)
        val keys = (0 until 8).map(i => ByteString.fromString(s"k$i"))

        for key <- keys do tree = tree.append(key)

        // verify all
        for key <- keys do
            val proof = tree.proveMembership(key)
            verifyMember(tree.rootHash, key, BigInt(smallDepth), proof)
    }

    test("larger depth D=10") {
        val d = 10
        var tree = OffChainImt.empty(d)
        val keys = (0 until 50).map(i => ByteString.fromString(s"element-$i"))

        for key <- keys do tree = tree.append(key)

        // spot-check a few
        for i <- Seq(0, 10, 25, 49) do
            val key = keys(i)
            val proof = tree.proveMembership(key)
            verifyMember(tree.rootHash, key, BigInt(d), proof)
    }

    test("proof sizes are correct") {
        val d = 15
        val tree = OffChainImt.empty(d).append(ByteString.fromString("test"))

        val memberProof = tree.proveMembership(ByteString.fromString("test"))
        assert(
          memberProof.length == d * 33,
          s"membership proof: expected ${d * 33}, got ${memberProof.length}"
        )

        val appendProof = tree.proveAppend()
        assert(
          appendProof.length == d * 32,
          s"append proof: expected ${d * 32}, got ${appendProof.length}"
        )
    }
}
