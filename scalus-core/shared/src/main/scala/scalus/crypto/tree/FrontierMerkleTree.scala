package scalus.crypto.tree

import scalus.uplc.builtin.Builtins.{appendByteString, blake2b_256}
import scalus.uplc.builtin.ByteString

/** Frontier-based append-only Merkle tree.
  *
  * Stores only D sibling hashes (the "frontier") instead of the full tree, giving O(D) memory
  * regardless of how many elements have been inserted. Supports depths up to 256, enabling trees
  * with up to 2^256 leaf slots.
  *
  * Produces identical root hashes and append proofs as [[IncrementalMerkleTree]] for the same key
  * sequence. On-chain `IncrementalMerkleTree.append()` and `verifyMembership()` work unchanged with
  * proofs from this tree.
  *
  * Membership proofs require an external element source — see [[proveMembership]].
  */
class FrontierMerkleTree private (
    val depth: Int,
    val size: BigInt,
    private val frontier: Vector[ByteString],
    val rootHash: ByteString
) {
    import FrontierMerkleTree.*

    /** Total number of leaf slots: 2^depth. */
    def capacity: BigInt = BigInt(1) << depth

    /** Append a new key to the next available slot.
      *
      * Returns `(newTree, appendProof)` where `appendProof` is D * 32 bytes of sibling hashes
      * (bottom-up), compatible with on-chain `IncrementalMerkleTree.append()`.
      */
    def append(key: ByteString): (FrontierMerkleTree, ByteString) = {
        require(size < capacity, s"Tree is full: size=$size, capacity=$capacity")
        val leafHash = blake2b_256(key)
        val slot = size
        val newFrontier = frontier.toArray.clone()
        val proofBuf = new java.io.ByteArrayOutputStream(depth * 32)
        var currentHash = leafHash

        var level = 0
        while level < depth do
            val bit = (slot >> level) & 1
            if bit == BigInt(0) then
                // left child: sibling is the empty subtree hash at this level
                val sibling = emptySubtreeHashes(level)
                proofBuf.write(sibling.bytes)
                newFrontier(level) = currentHash
                currentHash = combine(currentHash, sibling)
            else
                // right child: sibling is the frontier value
                val sibling = frontier(level)
                proofBuf.write(sibling.bytes)
                currentHash = combine(sibling, currentHash)
            level += 1

        val proof = ByteString.unsafeFromArray(proofBuf.toByteArray)
        val tree = new FrontierMerkleTree(depth, size + 1, newFrontier.toVector, currentHash)
        (tree, proof)
    }

    /** Generate an append proof for the next empty slot (D * 32 bytes).
      *
      * This is equivalent to the proof returned by the next `append()` call, without actually
      * performing the append.
      */
    def proveAppend(): ByteString = {
        require(size < capacity, "Tree is full")
        val slot = size
        val proofBuf = new java.io.ByteArrayOutputStream(depth * 32)

        var level = 0
        while level < depth do
            val bit = (slot >> level) & 1
            if bit == BigInt(0) then proofBuf.write(emptySubtreeHashes(level).bytes)
            else proofBuf.write(frontier(level).bytes)
            level += 1

        ByteString.unsafeFromArray(proofBuf.toByteArray)
    }

    /** Generate a membership proof for the element at the given slot.
      *
      * Since the frontier tree does not store the full tree, an external element source is
      * required. The `getElement` function must return the original key at each slot index (0 until
      * size).
      *
      * Returns D * 33 bytes: D repetitions of (direction[1] + sibling[32]), compatible with
      * on-chain `IncrementalMerkleTree.verifyMembership()`.
      *
      * @param slot
      *   the leaf slot to prove (0 until size)
      * @param getElement
      *   function returning the original key at a given slot index
      * @param cache
      *   optional cache for subtree hashes; reuse across multiple proofs for O(D) per proof after
      *   the first O(N) warmup
      */
    def proveMembership(
        slot: BigInt,
        getElement: BigInt => ByteString,
        cache: SubtreeHashCache = SubtreeHashCache.NoCache
    ): ByteString = {
        require(slot >= 0 && slot < size, s"Slot $slot out of range [0, $size)")
        val buf = new java.io.ByteArrayOutputStream(depth * 33)

        var level = 0
        while level < depth do
            val childIndex = slot >> level
            val siblingIndex =
                if childIndex % 2 == 0 then childIndex + 1 else childIndex - 1
            val siblingHash = computeSubtreeHash(level, siblingIndex, getElement, cache)
            val direction = ((slot >> level) % 2).toInt
            buf.write(direction)
            buf.write(siblingHash.bytes)
            level += 1

        ByteString.unsafeFromArray(buf.toByteArray)
    }

    /** Recursively compute the hash of the subtree rooted at (level, index). Uses the cache to
      * avoid redundant computation — with a warm cache, each call is O(1).
      */
    private def computeSubtreeHash(
        level: Int,
        index: BigInt,
        getElement: BigInt => ByteString,
        cache: SubtreeHashCache
    ): ByteString = {
        cache.get(level, index) match
            case Some(h) => h
            case None =>
                val firstLeaf = index << level
                val hash =
                    if firstLeaf >= size then emptySubtreeHashes(level)
                    else if level == 0 then blake2b_256(getElement(index))
                    else
                        val left =
                            computeSubtreeHash(level - 1, index * 2, getElement, cache)
                        val right =
                            computeSubtreeHash(level - 1, index * 2 + 1, getElement, cache)
                        combine(left, right)
                cache.put(level, index, hash)
                hash
    }
}

/** Cache for intermediate subtree hashes, used by [[FrontierMerkleTree.proveMembership]].
  *
  * Reuse a single cache across multiple `proveMembership` calls: the first call is O(N) (computes
  * all subtree hashes), subsequent calls are O(D) (cache hits for shared subtrees).
  */
trait SubtreeHashCache {
    def get(level: Int, index: BigInt): Option[ByteString]
    def put(level: Int, index: BigInt, hash: ByteString): Unit
}

object SubtreeHashCache {

    /** No caching — every subtree hash is recomputed. O(N) per proof. */
    val NoCache: SubtreeHashCache = new SubtreeHashCache {
        def get(level: Int, index: BigInt): Option[ByteString] = None
        def put(level: Int, index: BigInt, hash: ByteString): Unit = ()
    }

    /** In-memory cache backed by a mutable HashMap. */
    def inMemory(): SubtreeHashCache = new SubtreeHashCache {
        private val store = collection.mutable.HashMap[(Int, BigInt), ByteString]()
        def get(level: Int, index: BigInt): Option[ByteString] = store.get((level, index))
        def put(level: Int, index: BigInt, hash: ByteString): Unit = {
            store((level, index)) = hash
        }
    }
}

object FrontierMerkleTree {

    private val MaxDepth = 256

    private val EmptyLeafHash: ByteString = blake2b_256(
      ByteString.fromArray(new Array[Byte](32))
    )

    /** Pre-computed empty subtree hashes for levels 0..256.
      *
      * emptySubtreeHashes(0) = blake2b_256(0x00..00) (same as IMT's EmptyLeafHash)
      * emptySubtreeHashes(L) = combine(emptySubtreeHashes(L-1), emptySubtreeHashes(L-1))
      */
    private[tree] val emptySubtreeHashes: Array[ByteString] = {
        val arr = new Array[ByteString](MaxDepth + 1)
        arr(0) = EmptyLeafHash
        var i = 1
        while i <= MaxDepth do
            arr(i) = combine(arr(i - 1), arr(i - 1))
            i += 1
        arr
    }

    /** Create an empty tree with the given depth (1..256). */
    def empty(depth: Int): FrontierMerkleTree = {
        require(depth >= 1 && depth <= MaxDepth, s"Depth must be 1..$MaxDepth, got $depth")
        val frontier = Vector.tabulate(depth)(level => emptySubtreeHashes(level))
        // root of an empty tree of depth D = emptySubtreeHashes(D)
        new FrontierMerkleTree(depth, BigInt(0), frontier, emptySubtreeHashes(depth))
    }

    /** Build a frontier tree from a sequence of keys. */
    def fromKeys(keys: IndexedSeq[ByteString], depth: Int): FrontierMerkleTree = {
        require(depth >= 1 && depth <= MaxDepth, s"Depth must be 1..$MaxDepth, got $depth")
        require(
          BigInt(keys.size) <= (BigInt(1) << depth),
          s"Too many keys (${keys.size}) for depth $depth"
        )
        var tree = empty(depth)
        for key <- keys do tree = tree.append(key)._1
        tree
    }

    private def combine(left: ByteString, right: ByteString): ByteString =
        blake2b_256(appendByteString(left, right))
}
