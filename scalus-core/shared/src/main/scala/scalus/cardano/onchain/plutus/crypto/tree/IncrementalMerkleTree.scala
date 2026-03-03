package scalus.cardano.onchain.plutus.crypto.tree

import scalus.compiler.Compile
import scalus.cardano.onchain.plutus.prelude.require
import scalus.uplc.builtin.Builtins.*
import scalus.uplc.builtin.ByteString
import scalus.uplc.builtin.ByteString.hex

/** Incremental Merkle tree for oracle-managed sets.
  *
  * Fixed-depth D binary Merkle tree where leaves are appended sequentially. The tree is 1-indexed:
  * tree(1) = root, tree(2^D + i) = leaf i.
  *
  * Leaf i stores blake2b_256(key_i) or EmptyLeafHash if unused. EmptyLeafHash =
  * blake2b_256(0x00..00) (hash of 32 zero bytes).
  *
  * State: (root, size) where size = number of appended elements (next free slot).
  *
  * Operations:
  *   - append: oracle adds a new member (2*D + 2 blake2b, single pass)
  *   - verifyMember: user proves membership (D + 1 blake2b)
  *
  * Membership proofs use interleaved format: D * (direction[1] + sibling[32]) = D*33 bytes. Append
  * proofs are flat ByteStrings: D consecutive 32-byte sibling hashes.
  */
@Compile
object IncrementalMerkleTree {
    val NullHash: ByteString =
        hex"0000000000000000000000000000000000000000000000000000000000000000"

    /** Hash of 32 zero bytes — the hash stored in empty leaf slots. */
    val EmptyLeafHash: ByteString =
        hex"89eb0d6a8a691dae2cd15ed0369931ce0a949ecafa5c3f93f8121833646e15c3"

    /** Combine two 32-byte hashes into a parent hash. */
    def combine(left: ByteString, right: ByteString): ByteString =
        blake2b_256(appendByteString(left, right))

    /** Walk from a leaf up to the root using an interleaved proof.
      *
      * proof is a flat ByteString of D * 33 bytes: D repetitions of (direction[1] + sibling[32]).
      * direction is 0 (left child) or 1 (right child).
      *
      * Uses path-byte encoding (indexByteString) instead of BigInt modInteger/quotientInteger to
      * determine left/right at each level, and offset tracking (offset increments by 33).
      */
    def merkleUp(
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
                if direction == BigInt(0) then combine(hash, sibling)
                else combine(sibling, hash)
            merkleUp(proof, parentHash, offset + 33, endOffset)

    /** Verify that `key` is a member of the tree.
      *
      * proof is D * 33 bytes: D repetitions of (direction[1] + sibling[32]). Costs D + 1 blake2b
      * calls.
      */
    def verifyMember(
        root: ByteString,
        key: ByteString,
        depth: BigInt,
        proof: ByteString
    ): Unit =
        val leafHash = blake2b_256(key)
        val computedRoot = merkleUp(proof, leafHash, BigInt(0), depth * 33)
        require(computedRoot == root, "IMT: not a member")

    /** Append a new key at position `size`, returning the new root hash.
      *
      * Uses a combined single-pass algorithm: verifies the slot is empty AND computes the new root
      * in one recursive walk. This saves D recursive calls, D sliceByteString, D modInteger, and D
      * quotientInteger operations compared to two separate merkleUp calls.
      *
      * Cost: 2*D + 2 blake2b calls.
      */
    def append(
        root: ByteString,
        size: BigInt,
        depth: BigInt,
        key: ByteString,
        siblings: ByteString
    ): ByteString =
        val leafHash = blake2b_256(key)
        appendUp(siblings, size, EmptyLeafHash, leafHash, BigInt(0), depth * 32, root)

    /** Combined single-pass: verify empty slot and compute new root simultaneously.
      *
      * Carries both `oldHash` (expected empty path) and `newHash` (new key path) up the tree in a
      * single recursive walk. Uses offset tracking (offset increments by 32) instead of level * 32
      * multiplication.
      */
    private def appendUp(
        siblings: ByteString,
        slot: BigInt,
        oldHash: ByteString,
        newHash: ByteString,
        offset: BigInt,
        endOffset: BigInt,
        expectedRoot: ByteString
    ): ByteString =
        if offset == endOffset then
            require(oldHash == expectedRoot, "IMT: slot not empty")
            newHash
        else
            val sibling = sliceByteString(offset, 32, siblings)
            val parentSlot = quotientInteger(slot, 2)
            if modInteger(slot, 2) == BigInt(0) then
                appendUp(
                  siblings,
                  parentSlot,
                  combine(oldHash, sibling),
                  combine(newHash, sibling),
                  offset + 32,
                  endOffset,
                  expectedRoot
                )
            else
                appendUp(
                  siblings,
                  parentSlot,
                  combine(sibling, oldHash),
                  combine(sibling, newHash),
                  offset + 32,
                  endOffset,
                  expectedRoot
                )
}
