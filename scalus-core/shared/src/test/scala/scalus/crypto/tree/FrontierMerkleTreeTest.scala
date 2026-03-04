package scalus.crypto.tree

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.onchain.plutus.crypto.tree.IncrementalMerkleTree as OnChainImt
import scalus.uplc.builtin.ByteString

class FrontierMerkleTreeTest extends AnyFunSuite {

    test("empty tree root matches IMT for depths 1..20") {
        for d <- 1 to 20 do
            val imtRoot = IncrementalMerkleTree.empty(d).rootHash
            val frontierRoot = FrontierMerkleTree.empty(d).rootHash
            assert(
              frontierRoot == imtRoot,
              s"Root mismatch at depth $d"
            )
    }

    test("append produces same root as IMT") {
        val depth = 10
        var imt = IncrementalMerkleTree.empty(depth)
        var frontier = FrontierMerkleTree.empty(depth)
        val keys = (0 until 20).map(i => ByteString.fromString(s"key-$i"))

        for key <- keys do
            imt = imt.append(key)
            val (newFrontier, _) = frontier.append(key)
            frontier = newFrontier
            assert(
              frontier.rootHash == imt.rootHash,
              s"Root mismatch after appending $key"
            )
    }

    test("append proof works on-chain") {
        val depth = 8
        var frontier = FrontierMerkleTree.empty(depth)
        val keys = (0 until 10).map(i => ByteString.fromString(s"elem-$i"))

        for key <- keys do
            val root = frontier.rootHash
            val size = frontier.size
            val (newFrontier, proof) = frontier.append(key)
            val onChainRoot =
                OnChainImt.append(root, size, BigInt(depth), key, proof)
            assert(
              onChainRoot == newFrontier.rootHash,
              s"On-chain root mismatch after appending $key"
            )
            frontier = newFrontier
    }

    test("proveAppend matches append proof") {
        val depth = 6
        var frontier = FrontierMerkleTree.empty(depth)
        val keys = (0 until 5).map(i => ByteString.fromString(s"pa-$i"))

        for key <- keys do
            val proofBefore = frontier.proveAppend()
            val (newFrontier, appendProof) = frontier.append(key)
            assert(
              proofBefore == appendProof,
              s"proveAppend mismatch before appending $key"
            )
            frontier = newFrontier
    }

    test("large depth D=64") {
        var frontier = FrontierMerkleTree.empty(64)
        val keys = (0 until 5).map(i => ByteString.fromString(s"big-$i"))

        for key <- keys do
            val root = frontier.rootHash
            val size = frontier.size
            val (newFrontier, proof) = frontier.append(key)
            val onChainRoot =
                OnChainImt.append(root, size, BigInt(64), key, proof)
            assert(
              onChainRoot == newFrontier.rootHash,
              s"On-chain root mismatch at D=64 after appending $key"
            )
            frontier = newFrontier
    }

    test("large depth D=256") {
        var frontier = FrontierMerkleTree.empty(256)
        val keys = (0 until 3).map(i => ByteString.fromString(s"huge-$i"))

        for key <- keys do
            val root = frontier.rootHash
            val size = frontier.size
            val (newFrontier, proof) = frontier.append(key)
            assert(proof.length == 256 * 32, s"Proof size should be ${256 * 32}")
            val onChainRoot =
                OnChainImt.append(root, size, BigInt(256), key, proof)
            assert(
              onChainRoot == newFrontier.rootHash,
              s"On-chain root mismatch at D=256 after appending $key"
            )
            frontier = newFrontier
    }

    test("full small tree (depth=3, 8 elements)") {
        val depth = 3
        var frontier = FrontierMerkleTree.empty(depth)
        val keys = (0 until 8).map(i => ByteString.fromString(s"full-$i"))

        for key <- keys do
            val root = frontier.rootHash
            val size = frontier.size
            val (newFrontier, proof) = frontier.append(key)
            val onChainRoot =
                OnChainImt.append(root, size, BigInt(depth), key, proof)
            assert(
              onChainRoot == newFrontier.rootHash,
              s"On-chain root mismatch after appending $key"
            )
            frontier = newFrontier

        // also verify against IMT
        val imt = IncrementalMerkleTree.fromKeys(keys, depth)
        assert(frontier.rootHash == imt.rootHash, "Final root should match IMT")
    }

    test("single element") {
        val depth = 5
        val key = ByteString.fromString("only-one")
        val frontier0 = FrontierMerkleTree.empty(depth)
        val (frontier1, proof) = frontier0.append(key)

        assert(frontier1.size == BigInt(1))
        assert(proof.length == depth * 32)

        // verify on-chain
        val onChainRoot =
            OnChainImt.append(frontier0.rootHash, BigInt(0), BigInt(depth), key, proof)
        assert(onChainRoot == frontier1.rootHash)

        // verify against IMT
        val imt = IncrementalMerkleTree.empty(depth).append(key)
        assert(frontier1.rootHash == imt.rootHash)
    }

    test("fromKeys produces same tree as sequential appends") {
        val depth = 8
        val keys = (0 until 15).map(i => ByteString.fromString(s"fk-$i"))

        val tree1 = FrontierMerkleTree.fromKeys(keys, depth)

        var tree2 = FrontierMerkleTree.empty(depth)
        for key <- keys do tree2 = tree2.append(key)._1

        assert(tree1.rootHash == tree2.rootHash)
        assert(tree1.size == tree2.size)
    }

    test("capacity is 2^depth") {
        assert(FrontierMerkleTree.empty(1).capacity == BigInt(2))
        assert(FrontierMerkleTree.empty(10).capacity == BigInt(1024))
        assert(FrontierMerkleTree.empty(20).capacity == BigInt(1048576))
    }

    test("append to full tree throws") {
        val depth = 1
        val frontier = FrontierMerkleTree.empty(depth)
        val (f1, _) = frontier.append(ByteString.fromString("a"))
        val (f2, _) = f1.append(ByteString.fromString("b"))
        assertThrows[IllegalArgumentException] {
            f2.append(ByteString.fromString("c"))
        }
    }

    // --- proveMembership tests ---

    test("proveMembership matches IMT membership proof") {
        val depth = 8
        val keys = (0 until 20).map(i => ByteString.fromString(s"mem-$i"))
        var frontier = FrontierMerkleTree.empty(depth)
        for key <- keys do frontier = frontier.append(key)._1

        val imt = IncrementalMerkleTree.fromKeys(keys, depth)

        for (key, slot) <- keys.zipWithIndex do
            val imtProof = imt.proveMembership(key)
            val frontierProof = frontier.proveMembership(BigInt(slot), i => keys(i.toInt))
            assert(
              frontierProof == imtProof,
              s"Membership proof mismatch at slot $slot"
            )
    }

    test("proveMembership verifies on-chain") {
        val depth = 6
        val keys = (0 until 10).map(i => ByteString.fromString(s"oc-$i"))
        var frontier = FrontierMerkleTree.empty(depth)
        for key <- keys do frontier = frontier.append(key)._1

        for (key, slot) <- keys.zipWithIndex do
            val proof = frontier.proveMembership(BigInt(slot), i => keys(i.toInt))
            OnChainImt.verifyMembership(frontier.rootHash, key, BigInt(depth), proof)
    }

    test("proveMembership with cache reuses subtree hashes") {
        val depth = 10
        val keys = (0 until 50).map(i => ByteString.fromString(s"cache-$i"))
        var frontier = FrontierMerkleTree.empty(depth)
        for key <- keys do frontier = frontier.append(key)._1

        val imt = IncrementalMerkleTree.fromKeys(keys, depth)
        val cache = SubtreeHashCache.inMemory()

        // all proofs should match IMT and verify on-chain
        for (key, slot) <- keys.zipWithIndex do
            val proof = frontier.proveMembership(BigInt(slot), i => keys(i.toInt), cache)
            assert(proof == imt.proveMembership(key), s"Cached proof mismatch at slot $slot")
            OnChainImt.verifyMembership(frontier.rootHash, key, BigInt(depth), proof)
    }

    test("proveMembership for full small tree (depth=3)") {
        val depth = 3
        val keys = (0 until 8).map(i => ByteString.fromString(s"ft-$i"))
        var frontier = FrontierMerkleTree.empty(depth)
        for key <- keys do frontier = frontier.append(key)._1

        val imt = IncrementalMerkleTree.fromKeys(keys, depth)

        for (key, slot) <- keys.zipWithIndex do
            val proof = frontier.proveMembership(BigInt(slot), i => keys(i.toInt))
            assert(proof == imt.proveMembership(key))
            assert(proof.length == depth * 33)
            OnChainImt.verifyMembership(frontier.rootHash, key, BigInt(depth), proof)
    }

    test("proveMembership single element") {
        val depth = 5
        val key = ByteString.fromString("solo")
        var frontier = FrontierMerkleTree.empty(depth)
        frontier = frontier.append(key)._1

        val imt = IncrementalMerkleTree.empty(depth).append(key)
        val proof = frontier.proveMembership(BigInt(0), _ => key)
        assert(proof == imt.proveMembership(key))
        OnChainImt.verifyMembership(frontier.rootHash, key, BigInt(depth), proof)
    }

    test("proveMembership slot out of range throws") {
        val depth = 4
        val key = ByteString.fromString("x")
        var frontier = FrontierMerkleTree.empty(depth)
        frontier = frontier.append(key)._1

        assertThrows[IllegalArgumentException] {
            frontier.proveMembership(BigInt(1), _ => key)
        }
        assertThrows[IllegalArgumentException] {
            frontier.proveMembership(BigInt(-1), _ => key)
        }
    }
}
