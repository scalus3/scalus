package scalus.cardano.offchain.mpf

import scalus.cardano.onchain.plutus.mpf.Merkling.*
import scalus.uplc.builtin.Builtins.*
import scalus.uplc.builtin.ByteString

sealed trait Node {
    def hash: ByteString
    def size: Int
}

object Node {

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

/** Shared base for off-chain Merkle Patricia Forestry trie implementations.
  *
  * Subclasses override `branchHash` to produce the correct hash for their on-chain verifier format.
  */
private[offchain] abstract class MerklePatriciaTrieBase {

    /** Compute the hash of a branch node. Each variant encodes the skip prefix differently. */
    private[offchain] def branchHash(
        skipStart: Int,
        skipLen: Int,
        repPath: ByteString,
        children: Vector[Node]
    ): ByteString

    private[offchain] final val PathNibbles = 64

    private[offchain] final val emptyChildren: Vector[Node] = Vector.fill(16)(Node.Empty)

    private[offchain] final def leafHash(fullPath: ByteString, cursor: Int, value: ByteString): ByteString =
        combine(suffix(fullPath, cursor), blake2b_256(value))

    private[offchain] final def nibbleAt(path: ByteString, index: Int): Int = {
        val byte = path.bytes(index / 2) & 0xff
        if index % 2 == 0 then byte >> 4 else byte & 0x0f
    }

    private[offchain] final def commonPrefixLen(
        pathA: ByteString,
        pathB: ByteString,
        start: Int,
        end: Int
    ): Int = {
        var i = start
        while i < end && nibbleAt(pathA, i) == nibbleAt(pathB, i) do i += 1
        i - start
    }

    private[offchain] final def branchSkipMatchesPath(
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

    /** Extract the 4 sibling hashes needed for a Branch proof step. These are the `neighbor8,
      * neighbor4, neighbor2, neighbor1` values expected by on-chain `merkle16`.
      */
    private[offchain] final def merkleProof4(children: Vector[Node], branch: Int): ByteString = {
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

    private[offchain] final def mkLeaf(
        skipStart: Int,
        fullPath: ByteString,
        key: ByteString,
        value: ByteString
    ): Node.Leaf =
        Node.Leaf(leafHash(fullPath, skipStart, value), skipStart, fullPath, key, value)

    private[offchain] final def mkBranch(
        skipStart: Int,
        skipLen: Int,
        repPath: ByteString,
        children: Vector[Node]
    ): Node.Branch =
        Node.Branch(
          branchHash(skipStart, skipLen, repPath, children),
          skipStart,
          skipLen,
          repPath,
          children,
          children.map(_.size).sum
        )

    private[offchain] final def doInsert(
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

    private[offchain] final def doGet(node: Node, path: ByteString, cursor: Int): Option[ByteString] =
        node match
            case Node.Empty => None
            case leaf: Node.Leaf =>
                if leaf.fullPath == path then Some(leaf.value) else None
            case branch: Node.Branch =>
                if !branchSkipMatchesPath(branch, path, cursor) then None
                else
                    val childNibble = nibbleAt(path, cursor + branch.skipLen)
                    doGet(branch.children(childNibble), path, cursor + branch.skipLen + 1)

    private[offchain] final def doDelete(node: Node, path: ByteString, cursor: Int): Node = node match
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

    /** Walk the trie root-to-leaf, collecting one proof step at each branch along the path.
      * Parameterized on proof step type — each variant provides its own `mkStep` function.
      */
    private[offchain] final def doProve[S](
        node: Node,
        path: ByteString,
        cursor: Int,
        mkStep: (Node.Branch, Int, Int) => S
    ): (Boolean, List[S]) = node match
        case Node.Empty =>
            (false, Nil)

        case leaf: Node.Leaf =>
            (leaf.fullPath == path, Nil)

        case branch: Node.Branch =>
            val childNibble = nibbleAt(path, cursor + branch.skipLen)
            val child = branch.children(childNibble)

            val (found, childSteps) = child match
                case Node.Empty => (false, Nil)
                case _          => doProve(child, path, cursor + branch.skipLen + 1, mkStep)

            val step = mkStep(branch, childNibble, branch.skipLen)
            (found, step :: childSteps)
}
