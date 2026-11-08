package scalus.crypto.tree

import scalus.uplc.builtin.Builtins.{appendByteString, blake2b_256}
import scalus.uplc.builtin.ByteString

/** Fixed-depth binary Merkle tree addressed by position, with immutable emptying.
  *
  * Leaves are `blake2b_256(element)`, unused or emptied leaves are 32 zero bytes, and parents are
  * `nodeHash(left ++ right)`. With the default node hash this is SSZ's fixed-size chunk tree with
  * BLAKE2b-256 in place of SHA-256, without length mixing or domain tags. Equal elements at
  * different positions are independent leaves. The depth must be authenticated separately from the
  * root.
  *
  * The materialized tree uses O(2^depth) memory and supports depths 0..29. For a root alone, use
  * [[PositionalMerkleTree.rootFromElements]], which streams in O(depth) memory up to depth 256.
  */
final class PositionalMerkleTree private (
    val depth: Int,
    private val hashes: Vector[ByteString],
    private val nodeHash: ByteString => ByteString
) {
    import PositionalMerkleTree.*

    /** Number of leaf slots, including unused and emptied slots. */
    def capacity: Int = 1 << depth

    /** Root hash, without mixing in a length. */
    def rootHash: ByteString = hashes(1)

    /** Siblings of `position`, bottom-up, concatenated into exactly `depth * 32` bytes.
      *
      * Positions are zero-based and must be in range. Paths for empty slots can also be generated;
      * the on-chain verifier refuses membership or emptying of those slots.
      */
    def proveMembership(position: Int): ByteString = {
        checkPosition(position)
        val out = new java.io.ByteArrayOutputStream(depth * 32)
        var index = capacity + position
        while index > 1 do
            out.write(hashes(index ^ 1).bytes)
            index >>= 1
        ByteString.unsafeFromArray(out.toByteArray)
    }

    /** Minimal SSZ multiproof for strictly ascending, distinct, in-range positions.
      *
      * The query must be nonempty. Helpers are concatenated 32-byte hashes in descending
      * generalized-index order (`get_helper_indices` in ethereum/ssz-specs v0.1.0). There are no
      * direction bytes, flags or indices in the proof itself.
      */
    def proveMulti(positions: IndexedSeq[Int]): ByteString = {
        checkPositions(positions)
        val paths = collection.mutable.Set.empty[Int]
        val siblings = collection.mutable.Set.empty[Int]
        for position <- positions do
            var index = capacity + position
            while index > 1 do
                paths += index
                siblings += (index ^ 1)
                index >>= 1
        val out = new java.io.ByteArrayOutputStream()
        for index <- siblings.iterator.filterNot(paths.contains).toSeq.sorted.reverse do
            out.write(hashes(index).bytes)
        ByteString.unsafeFromArray(out.toByteArray)
    }

    /** Empty occupied positions and return a new tree, preserving every other slot.
      *
      * Positions must be nonempty, strictly ascending, distinct and in range. An unused or already
      * emptied slot is rejected. Proofs for a later call must be generated from the returned tree.
      */
    def emptied(positions: IndexedSeq[Int]): PositionalMerkleTree = {
        checkPositions(positions)
        var updated = hashes
        for position <- positions do
            var index = capacity + position
            require(updated(index) != EmptyLeafHash, "PositionalMerkleTree: empty slot")
            updated = updated.updated(index, EmptyLeafHash)
            index >>= 1
            while index > 0 do
                updated = updated.updated(
                  index,
                  nodeHash(appendByteString(updated(index * 2), updated(index * 2 + 1)))
                )
                index >>= 1
        new PositionalMerkleTree(depth, updated, nodeHash)
    }

    private def checkPosition(position: Int): Unit =
        require(position >= 0 && position < capacity, "PositionalMerkleTree: position out of range")

    private def checkPositions(positions: IndexedSeq[Int]): Unit = {
        require(positions.nonEmpty, "PositionalMerkleTree: empty query")
        var previous = -1
        for position <- positions do
            checkPosition(position)
            require(
              position > previous,
              "PositionalMerkleTree: positions must be strictly ascending"
            )
            previous = position
    }
}

object PositionalMerkleTree {

    /** Reserved empty chunk, with no known preimage under BLAKE2b-256. */
    val EmptyLeafHash: ByteString = ByteString.fromArray(new Array[Byte](32))

    /** Build a tree filled from position zero, allowing duplicate elements. */
    def fromElements(elements: IterableOnce[ByteString], depth: Int): PositionalMerkleTree =
        fromElements(elements, depth, blake2b_256)

    /** Build with a custom 32-byte node digest, for interoperability tests or off-chain use.
      *
      * Leaf hashing remains BLAKE2b-256. The digest is applied to the concatenation of two child
      * hashes. Only the default BLAKE2b-256 digest is compatible with the on-chain API.
      */
    def fromElements(
        elements: IterableOnce[ByteString],
        depth: Int,
        nodeHash: ByteString => ByteString
    ): PositionalMerkleTree = {
        require(depth >= 0 && depth <= 29, "PositionalMerkleTree: materialized depth must be 0..29")
        val capacity = 1 << depth
        val hashes = Array.fill(capacity * 2)(EmptyLeafHash)
        val input = elements.iterator
        var count = 0
        while input.hasNext do
            require(count < capacity, "PositionalMerkleTree: too many elements")
            hashes(capacity + count) = leafHash(input.next())
            count += 1
        var index = capacity - 1
        while index > 0 do
            hashes(index) = nodeHash(appendByteString(hashes(index * 2), hashes(index * 2 + 1)))
            index -= 1
        new PositionalMerkleTree(depth, hashes.toVector, nodeHash)
    }

    /** Root of at most 2^depth elements in one pass with O(depth) auxiliary memory (depth 0..256).
      *
      * The input is consumed once. Unfilled slots are zero chunks; equal elements are allowed.
      */
    def rootFromElements(elements: IterableOnce[ByteString], depth: Int): ByteString =
        rootFromElements(elements, depth, blake2b_256)

    /** Streaming root with a custom node digest; leaf hashing remains BLAKE2b-256.
      *
      * See [[fromElements]] for the node digest contract. Only BLAKE2b-256 is accepted on chain.
      */
    def rootFromElements(
        elements: IterableOnce[ByteString],
        depth: Int,
        nodeHash: ByteString => ByteString
    ): ByteString = {
        require(depth >= 0 && depth <= 256, "PositionalMerkleTree: depth must be 0..256")
        val capacity = BigInt(1) << depth
        val frontier = new Array[ByteString](depth + 1)
        var count = BigInt(0)
        val input = elements.iterator
        while input.hasNext do
            require(count < capacity, "PositionalMerkleTree: too many elements")
            var hash = leafHash(input.next())
            var level = 0
            while count.testBit(level) do
                hash = nodeHash(appendByteString(frontier(level), hash))
                level += 1
            frontier(level) = hash
            count += 1

        if count == capacity then frontier(depth)
        else
            var hash = EmptyLeafHash
            var zero = EmptyLeafHash
            var level = 0
            while level < depth do
                hash =
                    if count.testBit(level) then nodeHash(appendByteString(frontier(level), hash))
                    else nodeHash(appendByteString(hash, zero))
                zero = nodeHash(appendByteString(zero, zero))
                level += 1
            hash
    }

    private def leafHash(element: ByteString): ByteString = {
        val hash = blake2b_256(element)
        require(hash != EmptyLeafHash, "PositionalMerkleTree: reserved empty leaf")
        hash
    }
}
