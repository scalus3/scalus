package scalus.cardano.onchain.plutus.crypto.tree

import scalus.compiler.Compile
import scalus.cardano.onchain.plutus.prelude.require
import scalus.uplc.builtin.Builtins.*
import scalus.uplc.builtin.ByteString

/** Plain Merkle tree for static membership verification.
  *
  * Built once from a known set of elements. Supports only membership proofs — no
  * insert/delete/append. This is the cheapest on-chain option when the set is static.
  *
  * Membership proofs use the same interleaved format as IMT: D * (direction[1] + sibling[32]) = D *
  * 33 bytes. The depth is derived from the proof length, so no depth parameter is needed.
  *
  * Cost: D + 1 blake2b_256 calls for depth D.
  */
@Compile
object MerkleTree {

    /** Verify that `element` is a member of the tree with the given `root`.
      *
      * @param root
      *   expected root hash (32 bytes)
      * @param element
      *   the element to verify membership for
      * @param proof
      *   membership proof: D * 33 bytes (D repetitions of direction[1] + sibling[32])
      */
    def verifyMembership(root: ByteString, element: ByteString, proof: ByteString): Unit = {
        val leafHash = blake2b_256(element)
        val proofLen = lengthOfByteString(proof)
        val computedRoot = merkleUp(proof, leafHash, BigInt(0), proofLen)
        require(computedRoot == root, "MerkleTree: not a member")
    }

    /** Walk from a leaf up to the root using an interleaved proof.
      *
      * Same format as IMT's merkleUp: direction[1] + sibling[32] per level.
      */
    private def merkleUp(
        proof: ByteString,
        hash: ByteString,
        offset: BigInt,
        endOffset: BigInt
    ): ByteString =
        if offset == endOffset then hash
        else
            val direction = indexByteString(proof, offset)
            val sibling = sliceByteString(offset + 1, 32, proof)
            val parentHash =
                if direction == BigInt(0) then blake2b_256(appendByteString(hash, sibling))
                else blake2b_256(appendByteString(sibling, hash))
            merkleUp(proof, parentHash, offset + 33, endOffset)
}
