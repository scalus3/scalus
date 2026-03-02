package scalus.cardano.offchain.mpfb

import scalus.cardano.onchain.plutus.mpfb.Merkling.*
import scalus.cardano.onchain.plutus.mpfb.{MerklePatriciaForestry as OnChainBinary}
import scalus.uplc.builtin.Builtins.*
import scalus.uplc.builtin.ByteString

private[mpfb] sealed trait Node {
    def hash: ByteString
    def size: Int
}

private[mpfb] object Node {

    case object Empty extends Node {
        val hash: ByteString = NullHash
        val size: Int = 0
    }

    /** A leaf node that stores the value and the full key.
      *
      * @param skipStart
      *   cursor position at creation; nibbles are derived via `nibbleAt(fullPath, skipStart + i)`
      */
    case class Leaf(
        hash: ByteString,
        skipStart: Int,
        fullPath: ByteString,
        key: ByteString,
        value: ByteString
    ) extends Node {
        val size: Int = 1
    }

    /** A branch node with up to 16 children.
      *
      * @param skipStart
      *   cursor position at creation
      * @param skipLen
      *   number of common prefix nibbles
      * @param repPath
      *   any descendant leaf's fullPath (shared reference, used to read nibble values)
      */
    case class Branch(
        hash: ByteString,
        skipStart: Int,
        skipLen: Int,
        repPath: ByteString,
        children: Vector[Node],
        size: Int
    ) extends Node
}

/** Internal proof step representation used only for proof generation before binary serialization.
  * Not the on-chain types — just intermediate representation.
  */
private[mpfb] sealed trait InternalProofStep

private[mpfb] object InternalProofStep {
    case class Branch(skip: Int, neighbors: ByteString) extends InternalProofStep
    case class Fork(skip: Int, nibble: Int, prefixLen: Int, halfLeft: ByteString, halfRight: ByteString)
        extends InternalProofStep
    case class Leaf(skip: Int, key: ByteString, value: ByteString) extends InternalProofStep
}

/** Off-chain Merkle Patricia Forestry implementation producing binary proofs for the `mpfb`
  * on-chain verifier.
  */
case class MerklePatriciaForestry(root: Node) {
    import MerklePatriciaForestry.*

    /** The hash of this MPF */
    def rootHash: ByteString = root.hash

    /** The amount of elements in the tree. */
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

    /** Creates a binary-encoded proof of inclusion, suitable for the `mpfb` on-chain verifier. */
    def proveExistsBinary(key: ByteString): ByteString = {
        val path = blake2b_256(key)
        val (found, steps) = MerklePatriciaForestry.doProve(root, path, 0)
        if !found then throw new NoSuchElementException(s"Key not in trie: ${key.toHex}")
        MerklePatriciaForestry.proofToBinary(steps)
    }

    /** Creates a binary-encoded proof of exclusion, suitable for the `mpfb` on-chain verifier. */
    def proveMissingBinary(key: ByteString): ByteString = {
        if get(key).isDefined then
            throw new IllegalArgumentException(s"Key already in trie: ${key.toHex}")
        val expanded = insert(key, ByteString.empty)
        expanded.proveExistsBinary(key)
    }

    /** Wrap the root hash as an on-chain `mpfb` value. */
    def toOnChain: OnChainBinary = OnChainBinary(rootHash)
}

object MerklePatriciaForestry {

    /** Number of nibbles in a blake2b-256 path (32 bytes = 64 nibbles). */
    private val PathNibbles = 64

    def empty: MerklePatriciaForestry = MerklePatriciaForestry(Node.Empty)

    def fromList(entries: Iterable[(ByteString, ByteString)]): MerklePatriciaForestry =
        entries.foldLeft(empty) { case (trie, (k, v)) => trie.insert(k, v) }

    private def combine3(a: ByteString, b: ByteString, c: ByteString): ByteString =
        blake2b_256(appendByteString(a, appendByteString(b, c)))

    /** Leaf hash: `combine(suffix(path, cursor), blake2b_256(value))` */
    private def leafHash(fullPath: ByteString, cursor: Int, value: ByteString): ByteString =
        combine(suffix(fullPath, cursor), blake2b_256(value))

    /** Branch hash: `combine3(prefix, halfLeft, halfRight)` — matches on-chain doBranch. */
    private def branchHash(skipLen: Int, children: Vector[Node]): ByteString = {
        val prefix = consByteString(skipLen, ByteString.empty)
        val (hL, hR) = merkleHalves(children.map(_.hash))
        combine3(prefix, hL, hR)
    }

    /** Reduce hashes to two halves (left and right subtree roots). */
    private def merkleHalves(hashes: Vector[ByteString]): (ByteString, ByteString) = {
        var current = hashes
        while current.length > 2 do
            current = current.grouped(2).map { pair => combine(pair(0), pair(1)) }.toVector
        (current(0), current(1))
    }

    /** Extract the 4 sibling hashes needed for a Branch step. These are the `neighbor8, neighbor4,
      * neighbor2, neighbor1` values expected by on-chain `merkle16`.
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

    private def mkLeaf(
        skipStart: Int,
        fullPath: ByteString,
        key: ByteString,
        value: ByteString
    ): Node.Leaf =
        Node.Leaf(leafHash(fullPath, skipStart, value), skipStart, fullPath, key, value)

    private def mkBranch(
        skipStart: Int,
        skipLen: Int,
        repPath: ByteString,
        children: Vector[Node]
    ): Node.Branch =
        Node.Branch(
          branchHash(skipLen, children),
          skipStart,
          skipLen,
          repPath,
          children,
          children.map(_.size).sum
        )

    private val emptyChildren: Vector[Node] = Vector.fill(16)(Node.Empty)

    /** Compare nibbles of two paths starting at `start`, up to `end` (exclusive). Returns the
      * number of matching nibbles.
      */
    private def commonPrefixLen(
        pathA: ByteString,
        pathB: ByteString,
        start: Int,
        end: Int
    ): Int = {
        var i = start
        while i < end && nibbleAt(pathA, i) == nibbleAt(pathB, i) do i += 1
        i - start
    }

    /** Check whether the branch's skip prefix matches the given path at cursor. */
    private def branchSkipMatchesPath(
        branch: Node.Branch,
        path: ByteString,
        cursor: Int
    ): Boolean = {
        var i = 0
        while i < branch.skipLen do
            if nibbleAt(branch.repPath, cursor + i) != nibbleAt(path, cursor + i) then return false
            i += 1
        true
    }

    private def doInsert(
        node: Node,
        path: ByteString,
        cursor: Int,
        key: ByteString,
        value: ByteString
    ): Node = node match
        case Node.Empty =>
            mkLeaf(cursor, path, key, value)

        case leaf: Node.Leaf =>
            val remainingLen = PathNibbles - cursor
            val cp = commonPrefixLen(path, leaf.fullPath, cursor, PathNibbles)

            if cp == remainingLen then
                throw new IllegalArgumentException(s"Key already in trie: ${key.toHex}")

            // Split at the divergence point
            val newNibble = nibbleAt(path, cursor + cp)
            val oldNibble = nibbleAt(leaf.fullPath, cursor + cp)
            val splitCursor = cursor + cp + 1

            val newLeaf = mkLeaf(splitCursor, path, key, value)
            val oldLeaf = mkLeaf(splitCursor, leaf.fullPath, leaf.key, leaf.value)

            val children = emptyChildren
                .updated(newNibble, newLeaf)
                .updated(oldNibble, oldLeaf)
            mkBranch(cursor, cp, path, children)

        case branch: Node.Branch =>
            val cp = commonPrefixLen(path, branch.repPath, cursor, cursor + branch.skipLen)

            if cp < branch.skipLen then
                // Path diverges within the branch's prefix -- split the branch
                val splitCursor = cursor + cp + 1
                val newNibble = nibbleAt(path, cursor + cp)
                val oldNibble = nibbleAt(branch.repPath, cursor + cp)

                val newLeaf = mkLeaf(splitCursor, path, key, value)
                val oldBranch = mkBranch(
                  branch.skipStart + cp + 1,
                  branch.skipLen - cp - 1,
                  branch.repPath,
                  branch.children
                )

                val children = emptyChildren
                    .updated(newNibble, newLeaf)
                    .updated(oldNibble, oldBranch)
                mkBranch(cursor, cp, path, children)
            else
                // Prefix matches -- descend into the appropriate child
                val childNibble = nibbleAt(path, cursor + branch.skipLen)
                val childCursor = cursor + branch.skipLen + 1
                val newChild = doInsert(branch.children(childNibble), path, childCursor, key, value)
                mkBranch(
                  branch.skipStart,
                  branch.skipLen,
                  branch.repPath,
                  branch.children.updated(childNibble, newChild)
                )

    private def doGet(node: Node, path: ByteString, cursor: Int): Option[ByteString] =
        node match
            case Node.Empty => None
            case leaf: Node.Leaf =>
                if leaf.fullPath == path then Some(leaf.value) else None
            case branch: Node.Branch =>
                if !branchSkipMatchesPath(branch, path, cursor) then None
                else
                    val childNibble = nibbleAt(path, cursor + branch.skipLen)
                    doGet(branch.children(childNibble), path, cursor + branch.skipLen + 1)

    private def doDelete(node: Node, path: ByteString, cursor: Int): Node = node match
        case Node.Empty =>
            throw new NoSuchElementException("Key not in trie")

        case leaf: Node.Leaf =>
            if leaf.fullPath == path then Node.Empty
            else throw new NoSuchElementException("Key not in trie")

        case branch: Node.Branch =>
            if !branchSkipMatchesPath(branch, path, cursor) then
                throw new NoSuchElementException("Key not in trie")

            val childNibble = nibbleAt(path, cursor + branch.skipLen)
            val childCursor = cursor + branch.skipLen + 1
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
                            mkLeaf(cursor, leaf.fullPath, leaf.key, leaf.value)
                        case inner: Node.Branch =>
                            mkBranch(
                              cursor,
                              branch.skipLen + 1 + inner.skipLen,
                              inner.repPath,
                              inner.children
                            )
                        case Node.Empty => Node.Empty
                case _ =>
                    mkBranch(branch.skipStart, branch.skipLen, branch.repPath, newChildren)

    /** Walk the trie root-to-leaf, collecting one [[InternalProofStep]] at each branch along the
      * path.
      */
    private def doProve(
        node: Node,
        path: ByteString,
        cursor: Int
    ): (Boolean, scala.List[InternalProofStep]) = node match
        case Node.Empty =>
            (false, Nil)

        case leaf: Node.Leaf =>
            (leaf.fullPath == path, Nil)

        case branch: Node.Branch =>
            val childNibble = nibbleAt(path, cursor + branch.skipLen)
            val child = branch.children(childNibble)

            val (found, childSteps) = child match
                case Node.Empty => (false, Nil)
                case _          => doProve(child, path, cursor + branch.skipLen + 1)

            val step = makeProofStep(branch, childNibble, branch.skipLen)
            (found, step :: childSteps)

    /** Emit the most compact [[InternalProofStep]] for a branch, based on sibling count. */
    private def makeProofStep(
        branch: Node.Branch,
        targetIndex: Int,
        skip: Int
    ): InternalProofStep = {
        val siblings = branch.children.zipWithIndex.filter { case (child, idx) =>
            child != Node.Empty && idx != targetIndex
        }

        if siblings.length >= 2 then
            InternalProofStep.Branch(skip, merkleProof4(branch.children, targetIndex))
        else if siblings.length == 1 then
            val (neighbor, neighborIdx) = siblings.head
            neighbor match
                case leaf: Node.Leaf =>
                    InternalProofStep.Leaf(skip, leaf.fullPath, blake2b_256(leaf.value))
                case inner: Node.Branch =>
                    val (hL, hR) = merkleHalves(inner.children.map(_.hash))
                    InternalProofStep.Fork(skip, neighborIdx, inner.skipLen, hL, hR)
                case Node.Empty =>
                    throw new IllegalStateException("Unexpected empty neighbor")
        else throw new IllegalStateException("Branch with fewer than 2 children")
    }

    /** Serialize proof steps into the compact binary format used by `mpfb`. */
    private def proofToBinary(steps: scala.List[InternalProofStep]): ByteString = {
        val buf = new java.io.ByteArrayOutputStream()
        for step <- steps do
            step match
                case InternalProofStep.Branch(skip, neighbors) =>
                    buf.write(0)
                    buf.write(skip)
                    buf.write(neighbors.bytes)
                case InternalProofStep.Fork(skip, nibble, prefixLen, halfLeft, halfRight) =>
                    buf.write(1)
                    buf.write(skip)
                    buf.write(nibble)
                    buf.write(prefixLen)
                    buf.write(halfLeft.bytes)
                    buf.write(halfRight.bytes)
                case InternalProofStep.Leaf(skip, key, value) =>
                    buf.write(2)
                    buf.write(skip)
                    buf.write(key.bytes)
                    buf.write(value.bytes)
        ByteString.unsafeFromArray(buf.toByteArray)
    }
}
