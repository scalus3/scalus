package scalus.cardano.offchain.mpf

import scalus.cardano.onchain.plutus.mpf.MerklePatriciaForestry as OnChainForestry
import scalus.cardano.onchain.plutus.mpf.MerklePatriciaForestry.{Neighbor, Proof, ProofStep}
import scalus.cardano.onchain.plutus.mpf.Merkling.*
import scalus.cardano.onchain.plutus.prelude.List as PList
import scalus.uplc.builtin.Builtins.*
import scalus.uplc.builtin.ByteString

private[mpf] sealed trait Node {
    def hash: ByteString
    def size: Int
}

private[mpf] object Node {

    case object Empty extends Node {
        val hash: ByteString = NullHash
        val size: Int = 0
    }

    /** A leaf node that stores the value and the full key. */
    case class Leaf(
        hash: ByteString,
        skip: Vector[Int],
        fullPath: ByteString,
        key: ByteString,
        value: ByteString
    ) extends Node {
        val size: Int = 1
    }

    /** A branch node with up to 16 children. Stores the common prefix in [[skip]]. */
    case class Branch(
        hash: ByteString,
        skip: Vector[Int],
        // up to 16 children
        children: Vector[Node],
        size: Int
    ) extends Node
}

/** Off-chain Merkle Patricia Forestry implementation.
  *
  * Allows lookup, insertion, deletion, and generation of succinct and on-chain MPF compatible
  * inclusion/exclusion proofs.
  */
case class MerklePatriciaForestry(root: Node) {
    import MerklePatriciaForestry.*

    /** The hash of this MPF */
    def rootHash: ByteString = root.hash

    /** The amount of elements in the tree. Note that the */
    def size: Int = root.size

    /** `true` if this trie has no elements, `false` otherwise */
    def isEmpty: Boolean = root == Node.Empty

    /** Inserts a new element into this trie */
    def insert(key: ByteString, value: ByteString): MerklePatriciaForestry = {
        val path = blake2b_256(key)
        MerklePatriciaForestry(doInsert(root, path, 0, key, value))
    }

    /** Deletes an element by the specified key from the trie. If this key is missing from the trie,
      * throws an exception.
      */
    def delete(key: ByteString): MerklePatriciaForestry = {
        val path = blake2b_256(key)
        MerklePatriciaForestry(doDelete(root, path, 0))
    }

    /** Returns the value stored by the specified key, or `None` */
    def get(key: ByteString): Option[ByteString] = {
        val path = blake2b_256(key)
        doGet(root, path, 0)
    }

    /** Creates a succinct, on-chain compatible proof of inclusion of an element by this [[key]].
      *
      * Proofs are compact because branch nodes with 2+ siblings use a sparse merkle tree encoding
      * (4 hashes / 128 bytes) rather than storing all 15 sibling hashes. Branches with a single
      * sibling are encoded as a [[ProofStep.Fork]] or [[ProofStep.Leaf]], requiring even less
      * space.
      *
      * If there's no element by this key, throws an exception.
      */
    def proveExists(key: ByteString): Proof = {
        val path = blake2b_256(key)
        val (found, steps) = doProve(root, path, 0)
        if !found then throw new NoSuchElementException(s"Key not in trie: ${key.toHex}")
        toProof(steps)
    }

    def proveMissing(key: ByteString): Proof = {
        if get(key).isDefined then
            throw new IllegalArgumentException(s"Key already in trie: ${key.toHex}")
        // Insert with a dummy value, then prove in the expanded trie.
        // The proof's `excluding` mode will reconstruct the original root.
        val expanded = insert(key, ByteString.empty)
        expanded.proveExists(key)
    }

    /** Wrap the root hash as an on-chain [[OnChainForestry]] value. */
    def toOnChain: OnChainForestry = OnChainForestry(rootHash)
}

object MerklePatriciaForestry {

    /** Number of nibbles in a blake2b-256 path (32 bytes = 64 nibbles). */
    private val PathNibbles = 64

    def empty: MerklePatriciaForestry = MerklePatriciaForestry(Node.Empty)

    def fromList(entries: Iterable[(ByteString, ByteString)]): MerklePatriciaForestry =
        entries.foldLeft(empty) { case (trie, (k, v)) => trie.insert(k, v) }

    /** Leaf hash: `combine(suffix(path, cursor), blake2b_256(value))` */
    private def leafHash(fullPath: ByteString, cursor: Int, value: ByteString): ByteString =
        combine(suffix(fullPath, cursor), blake2b_256(value))

    /** Branch hash: `combine(skipBytes, merkleRoot16(children))` */
    private def branchHash(skip: Vector[Int], children: Vector[Node]): ByteString =
        combine(nibblesAsBytes(skip), merkleRoot16(children))

    /** Encode a nibble vector as a ByteString (one byte per nibble, 0x00..0x0F). Matches on-chain
      * `Merkling.nibbles` output format.
      */
    private def nibblesAsBytes(nibbles: Vector[Int]): ByteString =
        ByteString.unsafeFromArray(nibbles.map(_.toByte).toArray)

    /** Compute the merkle root of 16 child hashes using the same binary tree structure as on-chain
      * `Merkling.merkle16`.
      */
    private def merkleRoot16(children: Vector[Node]): ByteString = {
        var hashes = children.map(_.hash)
        while hashes.length > 1 do
            hashes = hashes.grouped(2).map { pair => combine(pair(0), pair(1)) }.toVector
        hashes.head
    }

    /** Extract the 4 sibling hashes needed for a [[ProofStep.Branch]] step. These are the
      * `neighbor8, neighbor4, neighbor2, neighbor1` values expected by on-chain `merkle16`.
      */
    private def merkleProof4(children: Vector[Node], branch: Int): ByteString = {
        val hashes = children.map(_.hash)
        // Binary tree levels: 16 -> 8 -> 4 -> 2 -> 1
        val level1 = hashes.grouped(2).map { p => combine(p(0), p(1)) }.toVector
        val level2 = level1.grouped(2).map { p => combine(p(0), p(1)) }.toVector
        val level3 = level2.grouped(2).map { p => combine(p(0), p(1)) }.toVector

        val sibling8 = if branch < 8 then level3(1) else level3(0)
        val sibling4 =
            if branch % 8 < 4 then level2(branch / 8 * 2 + 1) else level2(branch / 8 * 2)
        val sibling2 =
            if branch % 4 < 2 then level1(branch / 4 * 2 + 1) else level1(branch / 4 * 2)
        val sibling1 =
            if branch % 2 == 0 then hashes(branch + 1) else hashes(branch - 1)

        // 128 bytes total, 32 per sibling
        sibling8.concat(sibling4).concat(sibling2).concat(sibling1)
    }

    private def nibbleAt(path: ByteString, index: Int): Int = {
        val byte = path.bytes(index / 2) & 0xff
        if index % 2 == 0 then byte >> 4 else byte & 0x0f
    }

    private def extractNibbles(path: ByteString, start: Int, end: Int): Vector[Int] =
        (start until end).map(i => nibbleAt(path, i)).toVector

    private def mkLeaf(
        skip: Vector[Int],
        fullPath: ByteString,
        cursor: Int,
        key: ByteString,
        value: ByteString
    ): Node.Leaf =
        Node.Leaf(leafHash(fullPath, cursor, value), skip, fullPath, key, value)

    private def mkBranch(skip: Vector[Int], children: Vector[Node]): Node.Branch =
        Node.Branch(branchHash(skip, children), skip, children, children.map(_.size).sum)

    private val emptyChildren: Vector[Node] = Vector.fill(16)(Node.Empty)

    private def commonPrefixLen(a: Vector[Int], b: Vector[Int]): Int = {
        val len = math.min(a.length, b.length)
        var i = 0
        while i < len && a(i) == b(i) do i += 1
        i
    }

    private def doInsert(
        node: Node,
        path: ByteString,
        cursor: Int,
        key: ByteString,
        value: ByteString
    ): Node = node match
        case Node.Empty =>
            mkLeaf(extractNibbles(path, cursor, PathNibbles), path, cursor, key, value)

        case leaf: Node.Leaf =>
            val remaining = extractNibbles(path, cursor, PathNibbles)
            val commonPrefix = commonPrefixLen(remaining, leaf.skip)

            if commonPrefix == remaining.length && commonPrefix == leaf.skip.length then
                throw new IllegalArgumentException(s"Key already in trie: ${key.toHex}")

            // Split at the divergence point
            val newNibble = remaining(commonPrefix)
            val oldNibble = leaf.skip(commonPrefix)
            val splitCursor = cursor + commonPrefix + 1

            val newLeaf = mkLeaf(remaining.drop(commonPrefix + 1), path, splitCursor, key, value)
            val oldLeaf = mkLeaf(
              leaf.skip.drop(commonPrefix + 1),
              leaf.fullPath,
              splitCursor,
              leaf.key,
              leaf.value
            )

            val children = emptyChildren
                .updated(newNibble, newLeaf)
                .updated(oldNibble, oldLeaf)
            mkBranch(remaining.take(commonPrefix), children)

        case branch: Node.Branch =>
            val remaining = extractNibbles(path, cursor, cursor + branch.skip.length + 1)
            val prefixPart = remaining.dropRight(1)
            val cp = commonPrefixLen(prefixPart, branch.skip)

            if cp < branch.skip.length then
                // Path diverges within the branch's prefix -- split the branch
                val splitCursor = cursor + cp + 1
                val newNibble = prefixPart(cp)
                val oldNibble = branch.skip(cp)

                val newLeaf =
                    mkLeaf(
                      extractNibbles(path, splitCursor, PathNibbles),
                      path,
                      splitCursor,
                      key,
                      value
                    )
                val oldBranch = mkBranch(branch.skip.drop(cp + 1), branch.children)

                val children = emptyChildren
                    .updated(newNibble, newLeaf)
                    .updated(oldNibble, oldBranch)
                mkBranch(branch.skip.take(cp), children)
            else
                // Prefix matches -- descend into the appropriate child
                val childNibble = remaining.last
                val childCursor = cursor + branch.skip.length + 1
                val newChild = doInsert(branch.children(childNibble), path, childCursor, key, value)
                mkBranch(branch.skip, branch.children.updated(childNibble, newChild))

    private def doGet(node: Node, path: ByteString, cursor: Int): Option[ByteString] =
        node match
            case Node.Empty => None
            case leaf: Node.Leaf =>
                if leaf.fullPath == path then Some(leaf.value) else None
            case branch: Node.Branch =>
                val remaining = extractNibbles(path, cursor, cursor + branch.skip.length)
                if remaining != branch.skip then None
                else
                    val childNibble = nibbleAt(path, cursor + branch.skip.length)
                    doGet(branch.children(childNibble), path, cursor + branch.skip.length + 1)

    private def doDelete(node: Node, path: ByteString, cursor: Int): Node = node match
        case Node.Empty =>
            throw new NoSuchElementException("Key not in trie")

        case leaf: Node.Leaf =>
            if leaf.fullPath == path then Node.Empty
            else throw new NoSuchElementException("Key not in trie")

        case branch: Node.Branch =>
            val remaining = extractNibbles(path, cursor, cursor + branch.skip.length)
            if remaining != branch.skip then throw new NoSuchElementException("Key not in trie")

            val childNibble = nibbleAt(path, cursor + branch.skip.length)
            val childCursor = cursor + branch.skip.length + 1
            val newChild = doDelete(branch.children(childNibble), path, childCursor)
            val newChildren = branch.children.updated(childNibble, newChild)

            // If only one child remains, collapse the branch
            val nonEmpty = newChildren.zipWithIndex.filter(_._1 != Node.Empty)
            nonEmpty.size match
                case 0 => Node.Empty
                case 1 =>
                    val (child, childIdx) = nonEmpty.head
                    child match
                        case leaf: Node.Leaf =>
                            mkLeaf(
                              branch.skip :+ childIdx :++ leaf.skip,
                              leaf.fullPath,
                              cursor,
                              leaf.key,
                              leaf.value
                            )
                        case inner: Node.Branch =>
                            mkBranch(branch.skip :+ childIdx :++ inner.skip, inner.children)
                        case Node.Empty => Node.Empty
                case _ =>
                    mkBranch(branch.skip, newChildren)

    /** Walk the trie root-to-leaf, collecting one [[ProofStep]] at each branch along the path.
      *
      * At each branch we record just enough information about the *siblings* for the on-chain
      * verifier to reconstruct the branch hash.
      */
    private def doProve(
        node: Node,
        path: ByteString,
        cursor: Int
    ): (Boolean, scala.List[ProofStep]) = node match
        case Node.Empty =>
            (false, Nil)

        case leaf: Node.Leaf =>
            // Reached a leaf -- the key is present if and only if the full path matches.
            // No proof step here, leaves are terminal.
            (leaf.fullPath == path, Nil)

        case branch: Node.Branch =>
            val skip = branch.skip.length
            // The nibble to branch on is after the common prefix (skip).
            val childNibble = nibbleAt(path, cursor + skip)
            val child = branch.children(childNibble)

            // Recurse into the child we're following
            val (found, childSteps) = child match
                case Node.Empty => (false, Nil)
                case _          => doProve(child, path, cursor + skip + 1)

            // Emit a proof step that captures the siblings at this branch
            val step = makeProofStep(branch, childNibble, skip)
            (found, step :: childSteps)

    /** Emit the most compact [[ProofStep]] for a branch, based on sibling count. */
    private def makeProofStep(branch: Node.Branch, targetIndex: Int, skip: Int): ProofStep = {
        // Siblings = non-empty children other than the one we're descending into
        val siblings = branch.children.zipWithIndex.filter { case (child, idx) =>
            child != Node.Empty && idx != targetIndex
        }

        if siblings.length >= 2 then
            // Dense case: 2+ siblings. Encode via the binary merkle tree over all 16 slots and
            // extract 4 neighbor hashes (128 bytes). The on-chain merkle16 reconstructs the full
            // 16-slot root from these 4 hashes plus the target child's hash.
            ProofStep.Branch(skip, merkleProof4(branch.children, targetIndex))
        else if siblings.length == 1 then
            // Sparse case: exactly one sibling. We can avoid the 128-byte merkle proof entirely.
            val (neighbor, neighborIdx) = siblings.head
            neighbor match
                case leaf: Node.Leaf =>
                    // Sibling is a leaf: record its full path and hashed value.
                    // The on-chain verifier uses sparseMerkle16 to place both entries.
                    ProofStep.Leaf(skip, leaf.fullPath, blake2b_256(leaf.value))
                case inner: Node.Branch =>
                    // Sibling is a branch: record its index, skip prefix, and merkle root.
                    ProofStep.Fork(
                      skip,
                      Neighbor(
                        nibble = neighborIdx,
                        prefix = nibblesAsBytes(inner.skip),
                        root = merkleRoot16(inner.children)
                      )
                    )
                case Node.Empty =>
                    throw new IllegalStateException("Unexpected empty neighbor")
        else throw new IllegalStateException("Branch with fewer than 2 children")
    }

    private def toProof(steps: scala.List[ProofStep]): Proof =
        steps.foldRight(PList.Nil: Proof) { (step, acc) => PList.Cons(step, acc) }
}
