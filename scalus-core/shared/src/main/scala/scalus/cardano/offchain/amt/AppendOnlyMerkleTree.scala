package scalus.cardano.offchain.amt

import scalus.uplc.builtin.Builtins.{blake2b_256, appendByteString}
import scalus.uplc.builtin.ByteString

/** Off-chain append-only Merkle tree.
  *
  * Fixed-depth D binary Merkle tree stored as a 1-indexed array: tree(1) = root, tree(2^D + i) =
  * leaf i.
  *
  * Leaves are appended sequentially. Each leaf stores blake2b_256(key) or EmptyLeafHash if unused.
  */
class AppendOnlyMerkleTree private (
    val depth: Int,
    private val tree: Array[ByteString],
    val size: Int,
    private val keyToSlot: Map[ByteString, Int]
) {

    /** Current root hash. */
    def rootHash: ByteString = tree(1)

    /** Number of leaf slots (2^depth). */
    def capacity: Int = 1 << depth

    /** Append a new key to the next available slot. */
    def append(key: ByteString): AppendOnlyMerkleTree = {
        require(size < capacity, s"Tree is full: size=$size, capacity=$capacity")
        val hashedKey = blake2b_256(key)
        require(!keyToSlot.contains(hashedKey), s"Key already exists in tree")

        val newTree = tree.clone()
        val leafIndex = capacity + size
        newTree(leafIndex) = blake2b_256(key)

        // propagate up to root
        var idx = leafIndex
        while idx > 1 do
            idx = idx / 2
            newTree(idx) = combine(newTree(2 * idx), newTree(2 * idx + 1))

        new AppendOnlyMerkleTree(depth, newTree, size + 1, keyToSlot + (hashedKey -> size))
    }

    /** Generate a membership proof for a key.
      *
      * Returns D * 33 bytes: D repetitions of (direction[1] + sibling[32]). direction is 0 (left
      * child) or 1 (right child).
      */
    def proveMembership(key: ByteString): ByteString = {
        val hashedKey = blake2b_256(key)
        val slot = keyToSlot.getOrElse(hashedKey, throw new NoSuchElementException(s"Key not found"))
        var result = ByteString.empty
        var idx = capacity + slot
        var level = 0
        while level < depth do
            val direction = idx & 1 // 0 = left child, 1 = right child
            val siblingIdx = idx ^ 1
            result = appendByteString(
              result,
              appendByteString(
                ByteString.fromArray(Array(direction.toByte)),
                tree(siblingIdx)
              )
            )
            idx = idx / 2
            level += 1
        result
    }

    /** Generate an append proof for the next empty slot.
      *
      * Returns siblings (D * 32 bytes) for slot `size`.
      */
    def proveAppend(): ByteString = {
        require(size < capacity, "Tree is full")
        getSiblings(size)
    }

    /** Check if a key is in the tree. */
    def contains(key: ByteString): Boolean =
        keyToSlot.contains(blake2b_256(key))

    /** Get the siblings for a given slot (D * 32 bytes, leaf level first). */
    private def getSiblings(slot: Int): ByteString = {
        var result = ByteString.empty
        var idx = capacity + slot
        var level = 0
        while level < depth do
            val siblingIdx = idx ^ 1 // flip last bit to get sibling
            result = appendByteString(result, tree(siblingIdx))
            idx = idx / 2
            level += 1
        result
    }

    private def combine(left: ByteString, right: ByteString): ByteString =
        blake2b_256(appendByteString(left, right))
}

object AppendOnlyMerkleTree {

    /** Create an empty tree with the given depth. */
    def empty(depth: Int): AppendOnlyMerkleTree = {
        require(depth >= 1 && depth <= 20, s"Depth must be 1..20, got $depth")
        val capacity = 1 << depth
        val treeSize = 2 * capacity // 1-indexed, so indices 1..2*capacity-1
        val tree = new Array[ByteString](treeSize)

        val emptyLeafHash = blake2b_256(
          ByteString.fromArray(new Array[Byte](32))
        )

        // initialize leaves
        for i <- 0 until capacity do tree(capacity + i) = emptyLeafHash

        // build internal nodes bottom-up
        for i <- (capacity - 1) to 1 by -1 do
            tree(i) = blake2b_256(appendByteString(tree(2 * i), tree(2 * i + 1)))

        new AppendOnlyMerkleTree(depth, tree, 0, Map.empty)
    }

    /** Compute the minimum depth needed to hold n elements. */
    def depthForSize(n: Int): Int = {
        var d = 1
        while (1 << d) < n do d += 1
        d
    }
}
