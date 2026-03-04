package scalus.crypto.tree

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.onchain.RequirementError
import scalus.cardano.onchain.plutus.crypto.tree.MerkleTree as OnChainMerkleTree
import scalus.uplc.builtin.ByteString

class MerkleTreeTest extends AnyFunSuite {

    test("single element: build, prove, verify on-chain") {
        val element = ByteString.fromString("hello")
        val tree = MerkleTree.fromElements(IndexedSeq(element))

        assert(tree.depth == 1)
        assert(tree.contains(element))
        assert(tree.rootHash.length == 32)

        val proof = tree.proveMembership(element)
        assert(proof.length == 1 * 33)

        OnChainMerkleTree.verifyMembership(tree.rootHash, element, proof)
    }

    test("multiple elements (10)") {
        val elements = (0 until 10).map(i => ByteString.fromString(s"elem-$i"))
        val tree = MerkleTree.fromElements(elements)

        assert(tree.depth == 4) // ceil(log2(10)) = 4

        for elem <- elements do
            assert(tree.contains(elem))
            val proof = tree.proveMembership(elem)
            OnChainMerkleTree.verifyMembership(tree.rootHash, elem, proof)
    }

    test("multiple elements (100)") {
        val elements = (0 until 100).map(i => ByteString.fromString(s"element-$i"))
        val tree = MerkleTree.fromElements(elements)

        assert(tree.depth == 7) // ceil(log2(100)) = 7

        for elem <- elements do
            val proof = tree.proveMembership(elem)
            OnChainMerkleTree.verifyMembership(tree.rootHash, elem, proof)
    }

    test("power-of-2 size (8 elements)") {
        val elements = (0 until 8).map(i => ByteString.fromString(s"p2-$i"))
        val tree = MerkleTree.fromElements(elements)

        assert(tree.depth == 3)
        assert(tree.capacity == 8)

        for elem <- elements do
            val proof = tree.proveMembership(elem)
            OnChainMerkleTree.verifyMembership(tree.rootHash, elem, proof)
    }

    test("non-power-of-2 size (5 elements)") {
        val elements = (0 until 5).map(i => ByteString.fromString(s"np2-$i"))
        val tree = MerkleTree.fromElements(elements)

        assert(tree.depth == 3) // ceil(log2(5)) = 3
        assert(tree.capacity == 8)

        for elem <- elements do
            val proof = tree.proveMembership(elem)
            OnChainMerkleTree.verifyMembership(tree.rootHash, elem, proof)
    }

    test("wrong element fails verification") {
        val elements = (0 until 4).map(i => ByteString.fromString(s"real-$i"))
        val tree = MerkleTree.fromElements(elements)

        val proof = tree.proveMembership(elements.head)

        assertThrows[RequirementError] {
            OnChainMerkleTree.verifyMembership(
              tree.rootHash,
              ByteString.fromString("fake"),
              proof
            )
        }
    }

    test("wrong proof fails verification") {
        val elements = (0 until 4).map(i => ByteString.fromString(s"key-$i"))
        val tree = MerkleTree.fromElements(elements)

        // get proof for key-0 but try to verify key-1 with it
        val proof0 = tree.proveMembership(elements(0))

        assertThrows[RequirementError] {
            OnChainMerkleTree.verifyMembership(tree.rootHash, elements(1), proof0)
        }
    }

    test("proof size = depth * 33 bytes") {
        for n <- Seq(1, 2, 3, 8, 15, 16, 17, 100) do
            val elements = (0 until n).map(i => ByteString.fromString(s"sz-$i"))
            val tree = MerkleTree.fromElements(elements)
            val proof = tree.proveMembership(elements.head)
            assert(
              proof.length == tree.depth * 33,
              s"n=$n: expected ${tree.depth * 33}, got ${proof.length}"
            )
    }

    test("root is deterministic") {
        val elements = (0 until 10).map(i => ByteString.fromString(s"det-$i"))
        val tree1 = MerkleTree.fromElements(elements)
        val tree2 = MerkleTree.fromElements(elements)
        assert(tree1.rootHash == tree2.rootHash)
    }

    test("element not in tree throws on proveMembership") {
        val tree = MerkleTree.fromElements(IndexedSeq(ByteString.fromString("only")))
        assertThrows[NoSuchElementException] {
            tree.proveMembership(ByteString.fromString("missing"))
        }
    }

    test("contains returns false for non-member") {
        val tree = MerkleTree.fromElements(IndexedSeq(ByteString.fromString("a")))
        assert(!tree.contains(ByteString.fromString("b")))
    }
}
