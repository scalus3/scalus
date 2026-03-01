package scalus.cardano.onchain.plutus.amt

import scalus.compiler.Compile
import scalus.cardano.onchain.plutus.prelude.require
import scalus.uplc.builtin.Builtins.*
import scalus.uplc.builtin.ByteString
import scalus.uplc.builtin.ByteString.hex

/** Append-only Merkle tree for oracle-managed sets.
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
  * Proofs are flat ByteStrings: siblings are D consecutive 32-byte hashes.
  */
@Compile
object AppendOnlyMerkleTree {
    val NullHash: ByteString =
        hex"0000000000000000000000000000000000000000000000000000000000000000"

    /** Hash of 32 zero bytes — the hash stored in empty leaf slots. */
    val EmptyLeafHash: ByteString =
        hex"89eb0d6a8a691dae2cd15ed0369931ce0a949ecafa5c3f93f8121833646e15c3"

    /** Combine two 32-byte hashes into a parent hash. */
    def combine(left: ByteString, right: ByteString): ByteString =
        blake2b_256(appendByteString(left, right))

    /** Walk from a leaf at `slot` up to the root, combining `hash` with sibling hashes from
      * `siblings` at each level.
      *
      * siblings is a flat ByteString of D * 32 bytes, ordered from leaf level (level 0) to root
      * level (level D-1).
      *
      * At each level, if the current node is a left child (slot is even), the sibling is on the
      * right; otherwise it's on the left.
      */
    def merkleUp(
        siblings: ByteString,
        slot: BigInt,
        hash: ByteString,
        level: BigInt,
        depth: BigInt
    ): ByteString =
        if level == depth then hash
        else
            val sibling = sliceByteString(level * 32, 32, siblings)
            val parentSlot = quotientInteger(slot, 2)
            val parentHash =
                if modInteger(slot, 2) == BigInt(0) then combine(hash, sibling)
                else combine(sibling, hash)
            merkleUp(siblings, parentSlot, parentHash, level + 1, depth)

    /** Verify that `key` is a member of the tree at the given `slot`.
      *
      * Costs D + 1 blake2b calls.
      */
    def verifyMember(
        root: ByteString,
        key: ByteString,
        slot: BigInt,
        depth: BigInt,
        siblings: ByteString
    ): Unit =
        val leafHash = blake2b_256(key)
        val computedRoot = merkleUp(siblings, slot, leafHash, BigInt(0), depth)
        require(computedRoot == root, "AMT: not a member")

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
        appendUp(siblings, size, EmptyLeafHash, leafHash, BigInt(0), depth, root)

    /** Combined single-pass: verify empty slot and compute new root simultaneously.
      *
      * Carries both `oldHash` (expected empty path) and `newHash` (new key path) up the tree in a
      * single recursive walk.
      */
    private def appendUp(
        siblings: ByteString,
        slot: BigInt,
        oldHash: ByteString,
        newHash: ByteString,
        level: BigInt,
        depth: BigInt,
        expectedRoot: ByteString
    ): ByteString =
        if level == depth then
            require(oldHash == expectedRoot, "AMT: slot not empty")
            newHash
        else
            val sibling = sliceByteString(level * 32, 32, siblings)
            val parentSlot = quotientInteger(slot, 2)
            if modInteger(slot, 2) == BigInt(0) then
                appendUp(
                  siblings,
                  parentSlot,
                  combine(oldHash, sibling),
                  combine(newHash, sibling),
                  level + 1,
                  depth,
                  expectedRoot
                )
            else
                appendUp(
                  siblings,
                  parentSlot,
                  combine(sibling, oldHash),
                  combine(sibling, newHash),
                  level + 1,
                  depth,
                  expectedRoot
                )
}
