package scalus.crypto.tree

import scalus.uplc.builtin.Builtins.{appendByteString, blake2b_256}
import scalus.uplc.builtin.ByteString

/** Fixed-depth binary Merkle tree addressed by position, with immutable deletion.
  *
  * Leaves are `blake2b_256(element)`, unused or deleted leaves are 32 zero bytes, and parents are
  * `nodeHash(left ++ right)`. With the default node hash this is SSZ's fixed-size chunk tree with
  * BLAKE2b-256 in place of SHA-256, without length mixing or domain tags. Equal elements at
  * different positions are independent leaves. The depth must be authenticated separately from the
  * root.
  *
  * The materialized tree stores only the nodes over its occupied slots, about 2n hashes for n
  * elements, plus one empty-subtree hash per level, so its memory does not grow with 2^depth.
  * Positions are `Int`, and it supports depths 0..29. For a root alone, use
  * [[PositionalMerkleTree.rootFromElements]], which streams in O(depth) memory up to depth 256.
  */
final class PositionalMerkleTree private (
    val depth: Int,
    // levels(h) holds the nodes of height h (0 = leaves) over the slots the tree was built with,
    // positions 0 until levels(0).length. Every node past them is zeros(h), the root of an empty
    // subtree of height h.
    private val levels: Vector[Vector[ByteString]],
    private val zeros: Vector[ByteString],
    private val nodeHash: ByteString => ByteString
) {
    import PositionalMerkleTree.*

    /** Number of leaf slots, including unused and deleted slots. */
    def capacity: Int = 1 << depth

    /** Root hash, without mixing in a length. */
    def rootHash: ByteString = hashAt(1)

    /** Siblings of `position`, bottom-up, concatenated into exactly `depth * 32` bytes.
      *
      * Positions are zero-based and must be in range. Paths for empty slots can also be generated;
      * the on-chain verifier refuses membership or deletion of those slots.
      */
    def proveMembership(position: Int): ByteString = {
        val out = new java.io.ByteArrayOutputStream(depth * 32)
        var index = leafIndex(depth, position)
        while index > 1 do
            out.write(hashAt(index ^ 1).bytes)
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
            out.write(hashAt(index).bytes)
        ByteString.unsafeFromArray(out.toByteArray)
    }

    /** Delete an occupied position and return a new tree, preserving every other slot.
      *
      * The slot is set to [[PositionalMerkleTree.NullHash]]; positions and capacity do not change.
      * An unused or already deleted slot is rejected. A deletion changes a hash on every other
      * slot's path, so later proofs must be generated from the returned tree.
      */
    def delete(position: Int): PositionalMerkleTree = deleteMany(Vector(position))

    /** Delete several occupied positions and return a new tree, as [[delete]] does for one.
      *
      * Positions must be nonempty, strictly ascending, distinct and in range.
      */
    def deleteMany(positions: IndexedSeq[Int]): PositionalMerkleTree = {
        checkPositions(positions)
        var below = levels(0)
        for position <- positions do
            require(
              position < below.length && below(position) != NullHash,
              "PositionalMerkleTree: empty slot"
            )
            below = below.updated(position, NullHash)
        val updated = Vector.newBuilder[Vector[ByteString]]
        updated += below
        // Level by level, so a parent shared by several positions is hashed once. The indices stay
        // ascending, so equal parents are adjacent.
        var changed = positions
        var height = 1
        while height <= depth do
            changed = changed.map(_ >> 1).distinct
            var level = levels(height)
            for index <- changed do
                val left = node(below, height - 1, index * 2)
                val right = node(below, height - 1, index * 2 + 1)
                level = level.updated(index, nodeHash(appendByteString(left, right)))
            updated += level
            below = level
            height += 1
        new PositionalMerkleTree(depth, updated.result(), zeros, nodeHash)
    }

    // The node at generalized index `index`: the root is 1, and node g has children 2g and 2g + 1.
    private def hashAt(index: Int): ByteString = {
        val row = 31 - Integer.numberOfLeadingZeros(index)
        val height = depth - row
        node(levels(height), height, index - (1 << row))
    }

    private def node(level: Vector[ByteString], height: Int, index: Int): ByteString =
        if index < level.length then level(index) else zeros(height)

    private def checkPosition(position: Int): Unit = {
        leafIndex(depth, position)
        ()
    }

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

    /** 32 zero bytes: the reserved chunk of unused and deleted slots, with no known preimage under
      * BLAKE2b-256.
      */
    val NullHash: ByteString = ByteString.fromArray(new Array[Byte](32))

    /** Build a tree filled from position zero, allowing duplicate elements. */
    def fromElements(elements: IterableOnce[ByteString], depth: Int): PositionalMerkleTree =
        fromElements(elements, depth, blake2b_256)

    /** Build with a custom 32-byte node digest, for interoperability tests or off-chain use.
      *
      * Leaf hashing remains BLAKE2b-256. The digest is applied to the concatenation of two child
      * hashes. Each result must be exactly 32 bytes; malformed results are rejected and digest
      * exceptions propagate. Only the default BLAKE2b-256 digest is compatible with the on-chain
      * API.
      */
    def fromElements(
        elements: IterableOnce[ByteString],
        depth: Int,
        nodeHash: ByteString => ByteString
    ): PositionalMerkleTree = {
        val capacity = capacityAtDepth(depth)
        val digest = checkedNodeHash(nodeHash)
        val leaves = Vector.newBuilder[ByteString]
        val input = elements.iterator
        var count = 0
        while input.hasNext do
            require(count < capacity, "PositionalMerkleTree: too many elements")
            leaves += leafHash(input.next())
            count += 1
        val zeros =
            Vector.iterate(NullHash, depth + 1)(zero => digest(appendByteString(zero, zero)))
        val levels = Vector.newBuilder[Vector[ByteString]]
        var level = leaves.result()
        levels += level
        var height = 1
        while height <= depth do
            val below = level
            val zero = zeros(height - 1)
            level = Vector.tabulate((below.length + 1) / 2) { index =>
                val right = if index * 2 + 1 < below.length then below(index * 2 + 1) else zero
                digest(appendByteString(below(index * 2), right))
            }
            levels += level
            height += 1
        new PositionalMerkleTree(depth, levels.result(), zeros, digest)
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
        val digest = checkedNodeHash(nodeHash)
        val capacity = BigInt(1) << depth
        val frontier = new Array[ByteString](depth + 1)
        var count = BigInt(0)
        val input = elements.iterator
        while input.hasNext do
            require(count < capacity, "PositionalMerkleTree: too many elements")
            var hash = leafHash(input.next())
            var level = 0
            while count.testBit(level) do
                hash = digest(appendByteString(frontier(level), hash))
                level += 1
            frontier(level) = hash
            count += 1

        if count == capacity then frontier(depth)
        else
            var hash = NullHash
            var zero = NullHash
            var level = 0
            while level < depth do
                hash =
                    if count.testBit(level) then digest(appendByteString(frontier(level), hash))
                    else digest(appendByteString(hash, zero))
                zero = digest(appendByteString(zero, zero))
                level += 1
            hash
    }

    private[tree] def capacityAtDepth(depth: Int): Int = {
        require(depth >= 0 && depth <= 29, "PositionalMerkleTree: materialized depth must be 0..29")
        1 << depth
    }

    private[tree] def leafIndex(depth: Int, position: Int): Int = {
        val capacity = capacityAtDepth(depth)
        require(position >= 0 && position < capacity, "PositionalMerkleTree: position out of range")
        capacity + position
    }

    private def checkedNodeHash(nodeHash: ByteString => ByteString): ByteString => ByteString =
        input =>
            val hash = nodeHash(input)
            require(hash.size == 32, "PositionalMerkleTree: node digest must be 32 bytes")
            hash

    private def leafHash(element: ByteString): ByteString = {
        val hash = blake2b_256(element)
        require(hash != NullHash, "PositionalMerkleTree: reserved empty leaf")
        hash
    }
}
