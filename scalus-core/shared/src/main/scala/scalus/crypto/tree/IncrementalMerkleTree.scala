package scalus.crypto.tree

import scalus.uplc.builtin.Builtins.{appendByteString, blake2b_256}
import scalus.uplc.builtin.ByteString

/** Internal node of the persistent binary Merkle tree. */
private[tree] sealed trait MerkleNode {
    def hash: ByteString
}

private[tree] object MerkleNode {
    final class Leaf(val hash: ByteString) extends MerkleNode
    final class Branch(val hash: ByteString, val left: MerkleNode, val right: MerkleNode)
        extends MerkleNode
}

/** Off-chain incremental Merkle tree.
  *
  * Fixed-depth binary Merkle tree backed by a persistent (structurally-shared) tree.
  *
  *   - `append` replaces one leaf and rebuilds O(depth) nodes on the path — everything else is
  *     shared with the previous version.
  *   - `rootHash` is O(1) — just the root node's hash.
  *   - `proveMembership` / `proveAppend` walk root→leaf collecting siblings — O(depth).
  *   - Empty subtrees are shared: an empty tree of depth 20 uses only 21 node objects.
  */
class IncrementalMerkleTree private (
    val depth: Int,
    val size: Int,
    private val root: MerkleNode,
    private val keyToSlot: Map[ByteString, Int]
) {

    /** Number of leaf slots (2^depth). */
    def capacity: Int = 1 << depth

    /** Current root hash. */
    def rootHash: ByteString = root.hash

    /** Append a new key to the next available slot. */
    def append(key: ByteString): IncrementalMerkleTree = {
        require(size < capacity, s"Tree is full: size=$size, capacity=$capacity")
        val hashedKey = blake2b_256(key)
        require(!keyToSlot.contains(hashedKey), s"Key already exists in tree")
        val newRoot = updateLeaf(root, depth, size, hashedKey)
        new IncrementalMerkleTree(depth, size + 1, newRoot, keyToSlot + (hashedKey -> size))
    }

    /** Check if a key is in the tree. */
    def contains(key: ByteString): Boolean =
        keyToSlot.contains(blake2b_256(key))

    /** Generate a membership proof for a key.
      *
      * Returns D * 33 bytes: D repetitions of (direction[1] + sibling[32]). direction is 0 (left
      * child) or 1 (right child).
      */
    def proveMembership(key: ByteString): ByteString = {
        val hashedKey = blake2b_256(key)
        val slot =
            keyToSlot.getOrElse(hashedKey, throw new NoSuchElementException(s"Key not found"))
        val buf = new java.io.ByteArrayOutputStream(depth * 33)
        collectProof(root, depth, slot, buf, withDirection = true)
        ByteString.unsafeFromArray(buf.toByteArray)
    }

    /** Generate an append proof for the next empty slot.
      *
      * Returns siblings (D * 32 bytes) for slot `size`.
      */
    def proveAppend(): ByteString = {
        require(size < capacity, "Tree is full")
        val buf = new java.io.ByteArrayOutputStream(depth * 32)
        collectProof(root, depth, size, buf, withDirection = false)
        ByteString.unsafeFromArray(buf.toByteArray)
    }

    /** Walk from root to leaf at `slot`, writing sibling hashes (and optionally direction bytes) in
      * bottom-up order (leaf level first) as expected by the on-chain verifier.
      */
    private def collectProof(
        node: MerkleNode,
        level: Int,
        slot: Int,
        buf: java.io.ByteArrayOutputStream,
        withDirection: Boolean
    ): Unit = {
        if level > 0 then
            val branch = node.asInstanceOf[MerkleNode.Branch]
            val goRight = ((slot >> (level - 1)) & 1) == 1
            if goRight then
                collectProof(branch.right, level - 1, slot, buf, withDirection)
                if withDirection then buf.write(1)
                buf.write(branch.left.hash.bytes)
            else
                collectProof(branch.left, level - 1, slot, buf, withDirection)
                if withDirection then buf.write(0)
                buf.write(branch.right.hash.bytes)
    }

    /** Replace leaf at `slot` and rebuild branch hashes along the path. */
    private def updateLeaf(
        node: MerkleNode,
        level: Int,
        slot: Int,
        leafHash: ByteString
    ): MerkleNode = {
        if level == 0 then MerkleNode.Leaf(leafHash)
        else
            val branch = node.asInstanceOf[MerkleNode.Branch]
            val goRight = ((slot >> (level - 1)) & 1) == 1
            if goRight then
                val newRight = updateLeaf(branch.right, level - 1, slot, leafHash)
                MerkleNode.Branch(combine(branch.left.hash, newRight.hash), branch.left, newRight)
            else
                val newLeft = updateLeaf(branch.left, level - 1, slot, leafHash)
                MerkleNode.Branch(combine(newLeft.hash, branch.right.hash), newLeft, branch.right)
    }

    private def combine(left: ByteString, right: ByteString): ByteString =
        blake2b_256(appendByteString(left, right))
}

object IncrementalMerkleTree {

    private val EmptyLeafHash: ByteString = blake2b_256(
      ByteString.fromArray(new Array[Byte](32))
    )

    /** Pre-computed empty subtrees: emptySubtree(d) is a complete empty tree of depth d. Structural
      * sharing means an empty tree of depth 20 uses only 21 node objects.
      */
    private val MaxDepth = 20

    private val emptySubtrees: Array[MerkleNode] = {
        val arr = new Array[MerkleNode](MaxDepth + 1)
        arr(0) = MerkleNode.Leaf(EmptyLeafHash)
        for d <- 1 to MaxDepth do
            val child = arr(d - 1)
            val hash = blake2b_256(appendByteString(child.hash, child.hash))
            arr(d) = MerkleNode.Branch(hash, child, child)
        arr
    }

    /** Create an empty tree with the given depth. */
    def empty(depth: Int): IncrementalMerkleTree = {
        require(depth >= 1 && depth <= MaxDepth, s"Depth must be 1..$MaxDepth, got $depth")
        new IncrementalMerkleTree(depth, 0, emptySubtrees(depth), Map.empty)
    }

    /** Build a tree from a sequence of keys. Builds efficiently by filling leaves and computing
      * branch hashes bottom-up.
      */
    def fromKeys(keys: IndexedSeq[ByteString]): IncrementalMerkleTree =
        fromKeys(keys, depthForSize(keys.size))

    def fromKeys(keys: IndexedSeq[ByteString], depth: Int): IncrementalMerkleTree = {
        require(depth >= 1 && depth <= MaxDepth, s"Depth must be 1..$MaxDepth, got $depth")
        val capacity = 1 << depth
        require(keys.size <= capacity, s"Too many keys (${keys.size}) for depth $depth")

        val keyToSlot = Map.newBuilder[ByteString, Int]

        // hash all keys and build keyToSlot
        val hashedKeys = new Array[ByteString](keys.size)
        var i = 0
        while i < keys.size do
            val hk = blake2b_256(keys(i))
            hashedKeys(i) = hk
            keyToSlot += (hk -> i)
            i += 1

        // build tree bottom-up
        val root = buildSubtree(hashedKeys, depth, 0, capacity)
        val map = keyToSlot.result()
        require(map.size == keys.size, s"Duplicate keys: ${keys.size} keys but ${map.size} unique")
        new IncrementalMerkleTree(depth, keys.size, root, map)
    }

    /** Recursively build a subtree for the range [slotStart, slotStart + count). */
    private def buildSubtree(
        hashedKeys: Array[ByteString],
        level: Int,
        slotStart: Int,
        count: Int
    ): MerkleNode = {
        if level == 0 then
            if slotStart < hashedKeys.length then MerkleNode.Leaf(hashedKeys(slotStart))
            else MerkleNode.Leaf(EmptyLeafHash)
        else if slotStart >= hashedKeys.length then
            // entire subtree is empty — reuse shared structure
            emptySubtrees(level)
        else
            val half = count / 2
            val left = buildSubtree(hashedKeys, level - 1, slotStart, half)
            val right = buildSubtree(hashedKeys, level - 1, slotStart + half, half)
            val hash = blake2b_256(appendByteString(left.hash, right.hash))
            MerkleNode.Branch(hash, left, right)
    }

    /** Compute the minimum depth needed to hold n elements. */
    def depthForSize(n: Int): Int = {
        var d = 1
        while (1 << d) < n do d += 1
        d
    }
}
