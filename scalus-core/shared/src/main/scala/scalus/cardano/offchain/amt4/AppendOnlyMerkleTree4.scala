package scalus.cardano.offchain.amt4

import scalus.uplc.builtin.Builtins.{blake2b_256, appendByteString, integerToByteString}
import scalus.uplc.builtin.ByteString

/** Off-chain 4-ary append-only Merkle tree.
  *
  * Stored as a 0-indexed array using k-ary heap layout: children of node i are at 4*i+1 .. 4*i+4,
  * parent of node j is (j-1)/4.
  *
  * Leaves start at index `firstLeaf = (4^D - 1) / 3`.
  */
class AppendOnlyMerkleTree4 private (
    val depth: Int,
    private val tree: Array[ByteString],
    val size: Int,
    private val firstLeaf: Int,
    private val keyToSlot: Map[ByteString, Int]
) {

    def rootHash: ByteString = tree(0)

    def capacity: Int = math.pow(4, depth).toInt

    def append(key: ByteString): AppendOnlyMerkleTree4 = {
        require(size < capacity, s"Tree is full: size=$size, capacity=$capacity")
        val hashedKey = blake2b_256(key)
        require(!keyToSlot.contains(hashedKey), s"Key already exists in tree")

        val newTree = tree.clone()
        val leafIndex = firstLeaf + size
        newTree(leafIndex) = blake2b_256(key)

        // propagate up to root
        var idx = leafIndex
        while idx > 0 do
            idx = (idx - 1) / 4
            newTree(idx) = combine4(
              newTree(4 * idx + 1),
              newTree(4 * idx + 2),
              newTree(4 * idx + 3),
              newTree(4 * idx + 4)
            )

        new AppendOnlyMerkleTree4(depth, newTree, size + 1, firstLeaf, keyToSlot + (hashedKey -> size))
    }

    /** Membership proof: slot (3 bytes BE) ++ siblings (D × 96 bytes). */
    def proveMembership(key: ByteString): ByteString = {
        val hashedKey = blake2b_256(key)
        val slot = keyToSlot.getOrElse(hashedKey, throw new NoSuchElementException(s"Key not found"))
        val siblings = getSiblings(slot)
        val slotBytes = integerToByteString(true, 3, BigInt(slot))
        appendByteString(slotBytes, siblings)
    }

    /** Append proof: siblings (D × 96 bytes) for slot `size`. */
    def proveAppend(): ByteString = {
        require(size < capacity, "Tree is full")
        getSiblings(size)
    }

    def contains(key: ByteString): Boolean =
        keyToSlot.contains(blake2b_256(key))

    /** Get siblings for a given slot: D groups of 3×32 bytes.
      *
      * At each level, siblings are the 3 other children (in child-index order, skipping the current
      * child's position).
      */
    private def getSiblings(slot: Int): ByteString = {
        var result = ByteString.empty
        var idx = firstLeaf + slot
        while idx > 0 do
            val parentIdx = (idx - 1) / 4
            val childBase = 4 * parentIdx + 1
            val childPos = idx - childBase // 0, 1, 2, or 3
            for i <- 0 until 4 do
                if i != childPos then
                    result = appendByteString(result, tree(childBase + i))
            idx = parentIdx
        result
    }

    private def combine4(
        c0: ByteString,
        c1: ByteString,
        c2: ByteString,
        c3: ByteString
    ): ByteString =
        blake2b_256(appendByteString(appendByteString(appendByteString(c0, c1), c2), c3))
}

object AppendOnlyMerkleTree4 {

    def empty(depth: Int): AppendOnlyMerkleTree4 = {
        require(depth >= 1 && depth <= 10, s"Depth must be 1..10, got $depth")
        val cap = math.pow(4, depth).toInt
        val firstLeaf = (cap - 1) / 3
        val treeSize = firstLeaf + cap
        val tree = new Array[ByteString](treeSize)

        val emptyLeafHash = blake2b_256(ByteString.fromArray(new Array[Byte](32)))

        // initialize leaves
        for i <- 0 until cap do tree(firstLeaf + i) = emptyLeafHash

        // build internal nodes bottom-up
        for i <- (firstLeaf - 1) to 0 by -1 do
            tree(i) = blake2b_256(
              appendByteString(
                appendByteString(
                  appendByteString(tree(4 * i + 1), tree(4 * i + 2)),
                  tree(4 * i + 3)
                ),
                tree(4 * i + 4)
              )
            )

        new AppendOnlyMerkleTree4(depth, tree, 0, firstLeaf, Map.empty)
    }

    /** Minimum depth for a 4-ary tree to hold n elements. */
    def depthForSize(n: Int): Int = {
        var d = 1
        var cap = 4
        while cap < n do
            d += 1
            cap *= 4
        d
    }
}
