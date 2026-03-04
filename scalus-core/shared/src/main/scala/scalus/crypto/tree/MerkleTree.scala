package scalus.crypto.tree

import scalus.uplc.builtin.Builtins.{appendByteString, blake2b_256}
import scalus.uplc.builtin.ByteString

/** Off-chain static Merkle tree.
  *
  * Built once from a known set of elements. Supports membership proofs but no insert/delete/append.
  * Uses a heap-indexed array of hashes: index 1 = root, 2i/2i+1 = children of node i.
  *
  * Leaf hashing: `blake2b_256(element)`. Empty leaf: `blake2b_256(ByteString.empty)`.
  */
class MerkleTree private (
    private val hashes: IndexedSeq[ByteString],
    private val elementToSlot: Map[ByteString, Int],
    val depth: Int
) {

    /** Number of leaf slots (2^depth). */
    def capacity: Int = 1 << depth

    /** Root hash of the tree. */
    def rootHash: ByteString = hashes(1)

    /** Check if an element is in the tree. */
    def contains(element: ByteString): Boolean =
        elementToSlot.contains(blake2b_256(element))

    /** Generate a membership proof for an element.
      *
      * Returns D * 33 bytes: D repetitions of (direction[1] + sibling[32]), bottom-up (leaf level
      * first).
      */
    def proveMembership(element: ByteString): ByteString = {
        val hashedElement = blake2b_256(element)
        val slot = elementToSlot.getOrElse(
          hashedElement,
          throw new NoSuchElementException("Element not in tree")
        )
        val leafIdx = capacity + slot // heap index of this leaf
        val buf = new java.io.ByteArrayOutputStream(depth * 33)
        var idx = leafIdx
        while idx > 1 do
            val siblingIdx = idx ^ 1
            val direction: Byte = (idx & 1).toByte // 0 = left child, 1 = right child
            buf.write(direction)
            buf.write(hashes(siblingIdx).bytes)
            idx = idx >> 1
        ByteString.unsafeFromArray(buf.toByteArray)
    }
}

object MerkleTree {
    private val EmptyLeafHash: ByteString = blake2b_256(ByteString.empty)

    private def combine(left: ByteString, right: ByteString): ByteString =
        blake2b_256(appendByteString(left, right))

    /** Build a static Merkle tree from a sequence of elements.
      *
      * Elements are hashed into leaf slots, padded to the next power-of-2 with empty hashes, and
      * internal nodes are computed bottom-up.
      */
    def fromElements(elements: IndexedSeq[ByteString]): MerkleTree = {
        require(elements.nonEmpty, "Cannot build MerkleTree from empty elements")
        val depth = depthForSize(elements.size)
        val capacity = 1 << depth
        val totalNodes = 2 * capacity // heap: indices [1, 2*capacity)

        val h = new Array[ByteString](totalNodes)

        // hash elements into leaf slots [capacity, capacity + elements.size)
        val elemToSlot = Map.newBuilder[ByteString, Int]
        var i = 0
        while i < elements.size do
            val hashed = blake2b_256(elements(i))
            h(capacity + i) = hashed
            elemToSlot += (hashed -> i)
            i += 1

        // pad remaining leaves with empty hash
        i = elements.size
        while i < capacity do
            h(capacity + i) = EmptyLeafHash
            i += 1

        // fill internal nodes bottom-up
        i = capacity - 1
        while i >= 1 do
            h(i) = combine(h(2 * i), h(2 * i + 1))
            i -= 1

        val map = elemToSlot.result()
        require(
          map.size == elements.size,
          s"Duplicate elements: ${elements.size} elements but ${map.size} unique hashes"
        )

        new MerkleTree(h.toIndexedSeq, map, depth)
    }

    /** Compute the minimum depth needed to hold n elements. */
    def depthForSize(n: Int): Int = {
        require(n >= 1, s"Need at least 1 element, got $n")
        var d = 1
        while (1 << d) < n do d += 1
        d
    }
}
