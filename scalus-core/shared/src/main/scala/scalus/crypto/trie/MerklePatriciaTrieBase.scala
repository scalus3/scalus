package scalus.crypto.trie

import scalus.cardano.onchain.plutus.crypto.trie.Merkling.*
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
      * Hash is computed lazily — only when first accessed (e.g. for rootHash or proof generation).
      * This avoids redundant blake2b_256 calls during bulk trie construction.
      *
      * @param skipStart
      *   cursor position at creation; nibbles are derived via `nibbleAt(fullPath, skipStart + i)`
      */
    final class Leaf(
        val skipStart: Int,
        val fullPath: ByteString,
        val key: ByteString,
        val value: ByteString
    ) extends Node {
        lazy val hash: ByteString = combine(suffix(fullPath, skipStart), blake2b_256(value))
        val size: Int = 1
    }

    /** A branch node with up to 16 children.
      *
      * Hash is computed lazily via the provided thunk, which calls the variant-specific branchHash.
      * This avoids redundant blake2b_256 calls during bulk trie construction — intermediate branch
      * hashes that get replaced on the next insert are never computed.
      *
      * @param skipStart
      *   cursor position at creation
      * @param skipLen
      *   number of common prefix nibbles
      * @param repPath
      *   any descendant leaf's fullPath (shared reference, used to read nibble values)
      */
    final class Branch(
        val skipStart: Int,
        val skipLen: Int,
        val repPath: ByteString,
        val children: Vector[Node],
        val size: Int,
        computeHash: () => ByteString
    ) extends Node {
        lazy val hash: ByteString = computeHash()
    }
}

/** Shared base for off-chain Merkle Patricia Forestry trie implementations.
  *
  * Subclasses override `branchHash` to produce the correct hash for their on-chain verifier format.
  */
private[trie] abstract class MerklePatriciaTrieBase {

    /** Compute the hash of a branch node. Each variant encodes the skip prefix differently. */
    private[trie] def branchHash(
        skipStart: Int,
        skipLen: Int,
        repPath: ByteString,
        children: Vector[Node]
    ): ByteString

    private[trie] final val PathNibbles = 64

    private[trie] final val emptyChildren: Vector[Node] = Vector.fill(16)(Node.Empty)

    private[trie] final def leafHash(
        fullPath: ByteString,
        cursor: Int,
        value: ByteString
    ): ByteString =
        combine(suffix(fullPath, cursor), blake2b_256(value))

    private[trie] final def nibbleAt(path: ByteString, index: Int): Int = {
        val byte = path.bytes(index / 2) & 0xff
        if index % 2 == 0 then byte >> 4 else byte & 0x0f
    }

    private[trie] final def commonPrefixLen(
        pathA: ByteString,
        pathB: ByteString,
        start: Int,
        end: Int
    ): Int = {
        var i = start
        while i < end && nibbleAt(pathA, i) == nibbleAt(pathB, i) do i += 1
        i - start
    }

    private[trie] final def branchSkipMatchesPath(
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
      *
      * Computes only the 11 combines needed for the 4 siblings (not all 14 in the full tree). Uses
      * bit-flip indexing to find sibling subtree roots without intermediate allocations.
      */
    private[trie] final def merkleProof4(children: Vector[Node], branch: Int): ByteString = {
        // sibling1: direct pair partner (flip bit 0)
        val s1 = children(branch ^ 1).hash

        // sibling2: sibling pair in group of 4 (flip bit 1)
        val p = ((branch >> 1) ^ 1) << 1
        val s2 = combine(children(p).hash, children(p + 1).hash)

        // sibling4: sibling quad in group of 8 (flip bit 2)
        val q = ((branch >> 2) ^ 1) << 2
        val s4 = combine(
          combine(children(q).hash, children(q + 1).hash),
          combine(children(q + 2).hash, children(q + 3).hash)
        )

        // sibling8: sibling octet (flip bit 3)
        val o = ((branch >> 3) ^ 1) << 3
        val s8 = combine(
          combine(
            combine(children(o).hash, children(o + 1).hash),
            combine(children(o + 2).hash, children(o + 3).hash)
          ),
          combine(
            combine(children(o + 4).hash, children(o + 5).hash),
            combine(children(o + 6).hash, children(o + 7).hash)
          )
        )

        // 128 bytes: neighbor8 ++ neighbor4 ++ neighbor2 ++ neighbor1
        s8.concat(s4).concat(s2).concat(s1)
    }

    private[trie] final def mkLeaf(
        skipStart: Int,
        fullPath: ByteString,
        key: ByteString,
        value: ByteString
    ): Node.Leaf =
        Node.Leaf(skipStart, fullPath, key, value)

    private[trie] final def mkBranch(
        skipStart: Int,
        skipLen: Int,
        repPath: ByteString,
        children: Vector[Node],
        size: Int
    ): Node.Branch =
        Node.Branch(
          skipStart,
          skipLen,
          repPath,
          children,
          size,
          () => branchHash(skipStart, skipLen, repPath, children)
        )

    private[trie] final def doInsert(
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
            mkBranch(cursor, cp, path, children, 2)

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
                  branch.children,
                  branch.size
                )

                val children = emptyChildren
                    .updated(newNibble, newLeaf)
                    .updated(oldNibble, oldBranch)
                mkBranch(cursor, cp, path, children, branch.size + 1)
            else
                // Prefix matches -- descend into the appropriate child
                val childNibble = nibbleAt(path, cursor + branch.skipLen)
                val childCursor = cursor + branch.skipLen + 1
                val newChild = doInsert(branch.children(childNibble), path, childCursor, key, value)
                mkBranch(
                  branch.skipStart,
                  branch.skipLen,
                  branch.repPath,
                  branch.children.updated(childNibble, newChild),
                  branch.size + 1
                )

    private[trie] final def doGet(
        node: Node,
        path: ByteString,
        cursor: Int
    ): Option[ByteString] =
        node match
            case Node.Empty => None
            case leaf: Node.Leaf =>
                if leaf.fullPath == path then Some(leaf.value) else None
            case branch: Node.Branch =>
                if !branchSkipMatchesPath(branch, path, cursor) then None
                else
                    val childNibble = nibbleAt(path, cursor + branch.skipLen)
                    doGet(branch.children(childNibble), path, cursor + branch.skipLen + 1)

    private[trie] final def doDelete(node: Node, path: ByteString, cursor: Int): Node =
        node match
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

                // Count non-empty children to decide whether to collapse
                var nonEmptyCount = 0
                var lastNonEmptyIdx = 0
                var i = 0
                while i < 16 do
                    if newChildren(i) != Node.Empty then
                        nonEmptyCount += 1
                        lastNonEmptyIdx = i
                    i += 1

                if nonEmptyCount == 1 then
                    newChildren(lastNonEmptyIdx) match
                        case leaf: Node.Leaf =>
                            mkLeaf(cursor, leaf.fullPath, leaf.key, leaf.value)
                        case inner: Node.Branch =>
                            mkBranch(
                              cursor,
                              branch.skipLen + 1 + inner.skipLen,
                              inner.repPath,
                              inner.children,
                              inner.size
                            )
                        case Node.Empty => Node.Empty
                else
                    mkBranch(
                      branch.skipStart,
                      branch.skipLen,
                      branch.repPath,
                      newChildren,
                      branch.size - 1
                    )

    /** Walk the trie root-to-leaf, collecting one proof step at each branch along the path.
      * Parameterized on proof step type — each variant provides its own `mkStep` function.
      */
    private[trie] final def doProve[S](
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
