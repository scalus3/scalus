package scalus.cardano.onchain.plutus.amt4

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.offchain.amt4.AppendOnlyMerkleTree4 as OffChainAmt4
import scalus.cardano.onchain.RequirementError
import scalus.uplc.builtin.Builtins.*
import scalus.uplc.builtin.ByteString

class AppendOnlyMerkleTree4Test extends AnyFunSuite {
    import AppendOnlyMerkleTree4.*

    private val depth = 2 // 4^2 = 16 slots

    test("empty tree root is deterministic") {
        val tree1 = OffChainAmt4.empty(depth)
        val tree2 = OffChainAmt4.empty(depth)
        assert(tree1.rootHash == tree2.rootHash)
        assert(tree1.rootHash.length == 32)
    }

    test("append and verify single member") {
        val key = ByteString.fromString("hello")
        val tree0 = OffChainAmt4.empty(depth)
        val tree1 = tree0.append(key)

        val proof = tree1.proveMembership(key)
        val slot = byteStringToInteger(true, sliceByteString(0, 3, proof))
        val siblings = sliceByteString(3, depth * 96, proof)
        verifyMember(tree1.rootHash, key, slot, BigInt(depth), siblings)
    }

    test("append and verify multiple members") {
        var tree = OffChainAmt4.empty(depth)
        val keys = (0 until 10).map(i => ByteString.fromString(s"key-$i"))

        for key <- keys do tree = tree.append(key)

        for key <- keys do
            val proof = tree.proveMembership(key)
            val slot = byteStringToInteger(true, sliceByteString(0, 3, proof))
            val siblings = sliceByteString(3, depth * 96, proof)
            verifyMember(tree.rootHash, key, slot, BigInt(depth), siblings)
    }

    test("append proof verifies on-chain") {
        val key = ByteString.fromString("first")
        val tree0 = OffChainAmt4.empty(depth)

        val appendProof = tree0.proveAppend()
        val newRoot = append(tree0.rootHash, BigInt(tree0.size), BigInt(depth), key, appendProof)

        val tree1 = tree0.append(key)
        assert(newRoot == tree1.rootHash)
    }

    test("multiple appends with on-chain verification") {
        var tree = OffChainAmt4.empty(depth)
        val keys = (0 until 5).map(i => ByteString.fromString(s"elem-$i"))

        for key <- keys do
            val appendProof = tree.proveAppend()
            val newRoot =
                append(tree.rootHash, BigInt(tree.size), BigInt(depth), key, appendProof)
            tree = tree.append(key)
            assert(newRoot == tree.rootHash, s"Root mismatch after appending $key")
    }

    test("wrong key fails membership verification") {
        val tree = OffChainAmt4.empty(depth).append(ByteString.fromString("real-key"))

        val proof = tree.proveMembership(ByteString.fromString("real-key"))
        val slot = byteStringToInteger(true, sliceByteString(0, 3, proof))
        val siblings = sliceByteString(3, depth * 96, proof)

        assertThrows[RequirementError] {
            verifyMember(
              tree.rootHash,
              ByteString.fromString("fake-key"),
              slot,
              BigInt(depth),
              siblings
            )
        }
    }

    test("wrong slot fails membership verification") {
        val tree = OffChainAmt4.empty(depth)
            .append(ByteString.fromString("key-0"))
            .append(ByteString.fromString("key-1"))

        val proof = tree.proveMembership(ByteString.fromString("key-0"))
        val siblings = sliceByteString(3, depth * 96, proof)

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
        val tree = OffChainAmt4.empty(depth).append(ByteString.fromString("first"))

        val proof = tree.proveMembership(ByteString.fromString("first"))
        val siblings = sliceByteString(3, depth * 96, proof)

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
        val smallDepth = 2 // 16 slots
        var tree = OffChainAmt4.empty(smallDepth)
        val keys = (0 until 16).map(i => ByteString.fromString(s"k$i"))

        for key <- keys do tree = tree.append(key)

        for key <- keys do
            val proof = tree.proveMembership(key)
            val slot = byteStringToInteger(true, sliceByteString(0, 3, proof))
            val siblings = sliceByteString(3, smallDepth * 96, proof)
            verifyMember(tree.rootHash, key, slot, BigInt(smallDepth), siblings)
    }

    test("larger depth D=5") {
        val d = 5 // 1024 slots
        var tree = OffChainAmt4.empty(d)
        val keys = (0 until 50).map(i => ByteString.fromString(s"element-$i"))

        for key <- keys do tree = tree.append(key)

        for i <- Seq(0, 10, 25, 49) do
            val key = keys(i)
            val proof = tree.proveMembership(key)
            val slot = byteStringToInteger(true, sliceByteString(0, 3, proof))
            val siblings = sliceByteString(3, d * 96, proof)
            verifyMember(tree.rootHash, key, slot, BigInt(d), siblings)
    }

    test("proof sizes are correct") {
        val d = 8 // suitable for N=32K
        val tree = OffChainAmt4.empty(d).append(ByteString.fromString("test"))

        val memberProof = tree.proveMembership(ByteString.fromString("test"))
        assert(
          memberProof.length == 3 + d * 96,
          s"membership proof: expected ${3 + d * 96}, got ${memberProof.length}"
        )

        val appendProof = tree.proveAppend()
        assert(
          appendProof.length == d * 96,
          s"append proof: expected ${d * 96}, got ${appendProof.length}"
        )
    }
}
