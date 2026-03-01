package scalus.cardano.onchain.plutus.imt4

import scalus.compiler.Compile
import scalus.cardano.onchain.plutus.prelude.require
import scalus.uplc.builtin.Builtins.*
import scalus.uplc.builtin.ByteString
import scalus.uplc.builtin.ByteString.hex

/** 4-ary append-only Merkle tree.
  *
  * Like `amt.AppendOnlyMerkleTree` but each internal node has 4 children instead of 2. This halves
  * the tree depth (D_4 ≈ D_2/2), reducing the number of blake2b calls and recursive steps at the
  * cost of hashing 128 bytes per level instead of 64.
  *
  * Proof format: siblings are D groups of 3×32 = 96 bytes (leaf level first).
  *   - Verify membership: D + 1 blake2b calls (each hashing 128 bytes)
  *   - Append: 2×D + 2 blake2b calls
  */
@Compile
object IncrementalMerkleTree4 {
    val NullHash: ByteString =
        hex"0000000000000000000000000000000000000000000000000000000000000000"

    val EmptyLeafHash: ByteString = blake2b_256(NullHash)

    /** Combine 4 child hashes into a parent hash (blake2b of 128 bytes). */
    def combine4(
        c0: ByteString,
        c1: ByteString,
        c2: ByteString,
        c3: ByteString
    ): ByteString =
        blake2b_256(
          appendByteString(appendByteString(appendByteString(c0, c1), c2), c3)
        )

    /** Walk from a leaf at `slot` up to the root in a 4-ary tree.
      *
      * At each level, `siblings` contains 3×32 = 96 bytes: the other 3 children in child-index
      * order (skipping the current child's position).
      *
      * @param siblings
      *   flat ByteString of D × 96 bytes
      * @param slot
      *   leaf index (0-based)
      * @param hash
      *   current hash being propagated up
      * @param level
      *   current level (starts at 0)
      * @param depth
      *   tree depth D
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
            val off = level * 96
            val s0 = sliceByteString(off, 32, siblings)
            val s1 = sliceByteString(off + 32, 32, siblings)
            val s2 = sliceByteString(off + 64, 32, siblings)
            val branch = modInteger(slot, 4)
            val parentSlot = quotientInteger(slot, 4)
            val parentHash =
                if branch == BigInt(0) then combine4(hash, s0, s1, s2)
                else if branch == BigInt(1) then combine4(s0, hash, s1, s2)
                else if branch == BigInt(2) then combine4(s0, s1, hash, s2)
                else combine4(s0, s1, s2, hash)
            merkleUp(siblings, parentSlot, parentHash, level + 1, depth)

    def verifyMember(
        root: ByteString,
        key: ByteString,
        slot: BigInt,
        depth: BigInt,
        siblings: ByteString
    ): Unit =
        val leafHash = blake2b_256(key)
        val computedRoot = merkleUp(siblings, slot, leafHash, BigInt(0), depth)
        require(computedRoot == root, "AMT4: not a member")

    def append(
        root: ByteString,
        size: BigInt,
        depth: BigInt,
        key: ByteString,
        siblings: ByteString
    ): ByteString =
        val emptyRoot = merkleUp(siblings, size, EmptyLeafHash, BigInt(0), depth)
        require(emptyRoot == root, "AMT4: slot not empty")
        val leafHash = blake2b_256(key)
        merkleUp(siblings, size, leafHash, BigInt(0), depth)
}
