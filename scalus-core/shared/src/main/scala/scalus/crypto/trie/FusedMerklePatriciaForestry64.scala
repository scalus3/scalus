package scalus.crypto.trie

import scalus.cardano.onchain.plutus.crypto.trie.Merkling.{NullHash, combine, combine3}
import scalus.cardano.onchain.plutus.crypto.trie.Merkling64.*
import scalus.cardano.onchain.plutus.crypto.trie.FusedMerklePatriciaForestry64 as OnChain64
import scalus.uplc.builtin.Builtins.*
import scalus.uplc.builtin.ByteString

/** Off-chain radix-64 Merkle Patricia Forestry with binary (fused) proofs.
  *
  * Uses 6-bit path units (42 levels for 32-byte keys). Branch nodes have 64 children. Proofs use
  * the same binary format as [[FusedMerklePatriciaForestry]] but with 192-byte neighbors per branch
  * step (6 sibling hashes x 32 bytes) instead of 128 bytes (4 x 32).
  */
case class FusedMerklePatriciaForestry64(root: Node64) {
    import FusedMerklePatriciaForestry64.*

    def rootHash: ByteString = root.hash
    def size: Int = root.size
    def isEmpty: Boolean = root == Node64.Empty

    def insert(key: ByteString, value: ByteString): FusedMerklePatriciaForestry64 = {
        val path = blake2b_256(key)
        FusedMerklePatriciaForestry64(doInsert(root, path, 0, key, value))
    }

    def delete(key: ByteString): FusedMerklePatriciaForestry64 = {
        val path = blake2b_256(key)
        FusedMerklePatriciaForestry64(doDelete(root, path, 0))
    }

    def get(key: ByteString): Option[ByteString] = {
        val path = blake2b_256(key)
        doGet(root, path, 0)
    }

    def proveMembership(key: ByteString): ByteString = {
        val path = blake2b_256(key)
        val (found, steps) = doProve(root, path, 0)
        if !found then throw new NoSuchElementException(s"Key not in trie: ${key.toHex}")
        proofToBinary(steps)
    }

    def proveNonMembership(key: ByteString): ByteString = {
        if get(key).isDefined then
            throw new IllegalArgumentException(s"Key already in trie: ${key.toHex}")
        val expanded = insert(key, ByteString.empty)
        expanded.proveMembership(key)
    }

    def toOnChain: OnChain64 = OnChain64(rootHash)
}

/** Node types for radix-64 trie. Separate from [[Node]] (which is radix-16). */
sealed trait Node64 {
    def hash: ByteString
    def size: Int
}

object Node64 {
    case object Empty extends Node64 {
        val hash: ByteString = NullHash
        val size: Int = 0
    }

    final class Leaf(
        val skipStart: Int,
        val fullPath: ByteString,
        val key: ByteString,
        val value: ByteString
    ) extends Node64 {
        lazy val hash: ByteString = combine(suffix(fullPath, skipStart), blake2b_256(value))
        val size: Int = 1
    }

    final class Branch(
        val skipStart: Int,
        val skipLen: Int,
        val repPath: ByteString,
        val children: Vector[Node64],
        val size: Int,
        computeHash: () => ByteString
    ) extends Node64 {
        lazy val hash: ByteString = computeHash()
    }
}

object FusedMerklePatriciaForestry64 {

    /** 32 bytes = 256 bits -> 42 six-bit values (252 bits) + 4 leftover bits in suffix. */
    private val PathUnits = 42

    def empty: FusedMerklePatriciaForestry64 = FusedMerklePatriciaForestry64(Node64.Empty)

    def fromList(entries: Iterable[(ByteString, ByteString)]): FusedMerklePatriciaForestry64 =
        entries.foldLeft(empty) { case (trie, (k, v)) => trie.insert(k, v) }

    private val emptyChildren: Vector[Node64] = Vector.fill(64)(Node64.Empty)

    private def sixitAt(path: ByteString, index: Int): Int = {
        val base = (index / 4) * 3
        val pos = index % 4
        pos match
            case 0 => (path.bytes(base) & 0xff) >> 2
            case 1 => ((path.bytes(base) & 0x03) << 4) | ((path.bytes(base + 1) & 0xff) >> 4)
            case 2 => ((path.bytes(base + 1) & 0x0f) << 2) | ((path.bytes(base + 2) & 0xff) >> 6)
            case 3 => path.bytes(base + 2) & 0x3f
    }

    private def commonPrefixLen(
        pathA: ByteString,
        pathB: ByteString,
        start: Int,
        end: Int
    ): Int = {
        var i = start
        while i < end && sixitAt(pathA, i) == sixitAt(pathB, i) do i += 1
        i - start
    }

    private def branchSkipMatchesPath(
        branch: Node64.Branch,
        path: ByteString,
        cursor: Int
    ): Boolean = {
        var i = 0
        while i < branch.skipLen do
            if sixitAt(branch.repPath, cursor + i) != sixitAt(path, cursor + i) then return false
            i += 1
        true
    }

    private def branchHash(
        skipLen: Int,
        children: Vector[Node64]
    ): ByteString = {
        val prefix = consByteString(skipLen, ByteString.empty)
        val (hL, hR) = merkleHalves(children.map(_.hash))
        combine3(prefix, hL, hR)
    }

    private def merkleHalves(hashes: Vector[ByteString]): (ByteString, ByteString) = {
        var current = hashes
        while current.length > 2 do
            current = current.grouped(2).map { pair => combine(pair(0), pair(1)) }.toVector
        (current(0), current(1))
    }

    private def merkleRootFromHashes(hashes: Vector[ByteString]): ByteString = {
        var current = hashes
        while current.length > 1 do
            current = current.grouped(2).map { pair => combine(pair(0), pair(1)) }.toVector
        current.head
    }

    /** Extract 6 sibling hashes (192 bytes) for a Branch proof step. */
    private def extractSiblings(hashes: Vector[ByteString], idx: Int): ByteString = {
        if hashes.length <= 1 then ByteString.empty
        else
            val half = hashes.length / 2
            val (left, right) = hashes.splitAt(half)
            if idx < half then merkleRootFromHashes(right).concat(extractSiblings(left, idx))
            else merkleRootFromHashes(left).concat(extractSiblings(right, idx - half))
    }

    private def merkleProof6(children: Vector[Node64], branch: Int): ByteString = {
        val hashes = children.map(_.hash)
        extractSiblings(hashes, branch)
    }

    private def mkLeaf(
        skipStart: Int,
        fullPath: ByteString,
        key: ByteString,
        value: ByteString
    ): Node64.Leaf =
        Node64.Leaf(skipStart, fullPath, key, value)

    private def mkBranch(
        skipStart: Int,
        skipLen: Int,
        repPath: ByteString,
        children: Vector[Node64],
        size: Int
    ): Node64.Branch =
        Node64.Branch(
          skipStart,
          skipLen,
          repPath,
          children,
          size,
          () => branchHash(skipLen, children)
        )

    private def doInsert(
        node: Node64,
        path: ByteString,
        cursor: Int,
        key: ByteString,
        value: ByteString
    ): Node64 = node match
        case Node64.Empty =>
            mkLeaf(cursor, path, key, value)

        case leaf: Node64.Leaf =>
            val remainingLen = PathUnits - cursor
            val cp = commonPrefixLen(path, leaf.fullPath, cursor, PathUnits)

            if cp == remainingLen then
                throw new IllegalArgumentException(s"Key already in trie: ${key.toHex}")

            val newSixit = sixitAt(path, cursor + cp)
            val oldSixit = sixitAt(leaf.fullPath, cursor + cp)
            val splitCursor = cursor + cp + 1

            val newLeaf = mkLeaf(splitCursor, path, key, value)
            val oldLeaf = mkLeaf(splitCursor, leaf.fullPath, leaf.key, leaf.value)

            val children = emptyChildren
                .updated(newSixit, newLeaf)
                .updated(oldSixit, oldLeaf)
            mkBranch(cursor, cp, path, children, 2)

        case branch: Node64.Branch =>
            val cp = commonPrefixLen(path, branch.repPath, cursor, cursor + branch.skipLen)

            if cp < branch.skipLen then
                val splitCursor = cursor + cp + 1
                val newSixit = sixitAt(path, cursor + cp)
                val oldSixit = sixitAt(branch.repPath, cursor + cp)

                val newLeaf = mkLeaf(splitCursor, path, key, value)
                val oldBranch = mkBranch(
                  branch.skipStart + cp + 1,
                  branch.skipLen - cp - 1,
                  branch.repPath,
                  branch.children,
                  branch.size
                )

                val children = emptyChildren
                    .updated(newSixit, newLeaf)
                    .updated(oldSixit, oldBranch)
                mkBranch(cursor, cp, path, children, branch.size + 1)
            else
                val childSixit = sixitAt(path, cursor + branch.skipLen)
                val childCursor = cursor + branch.skipLen + 1
                val newChild =
                    doInsert(branch.children(childSixit), path, childCursor, key, value)
                mkBranch(
                  branch.skipStart,
                  branch.skipLen,
                  branch.repPath,
                  branch.children.updated(childSixit, newChild),
                  branch.size + 1
                )

    private def doGet(node: Node64, path: ByteString, cursor: Int): Option[ByteString] =
        node match
            case Node64.Empty => None
            case leaf: Node64.Leaf =>
                if leaf.fullPath == path then Some(leaf.value) else None
            case branch: Node64.Branch =>
                if !branchSkipMatchesPath(branch, path, cursor) then None
                else
                    val childSixit = sixitAt(path, cursor + branch.skipLen)
                    doGet(branch.children(childSixit), path, cursor + branch.skipLen + 1)

    private def doDelete(node: Node64, path: ByteString, cursor: Int): Node64 = node match
        case Node64.Empty =>
            throw new NoSuchElementException("Key not in trie")

        case leaf: Node64.Leaf =>
            if leaf.fullPath == path then Node64.Empty
            else throw new NoSuchElementException("Key not in trie")

        case branch: Node64.Branch =>
            if !branchSkipMatchesPath(branch, path, cursor) then
                throw new NoSuchElementException("Key not in trie")

            val childSixit = sixitAt(path, cursor + branch.skipLen)
            val childCursor = cursor + branch.skipLen + 1
            val newChild = doDelete(branch.children(childSixit), path, childCursor)
            val newChildren = branch.children.updated(childSixit, newChild)

            var nonEmptyCount = 0
            var lastNonEmptyIdx = 0
            var i = 0
            while i < 64 do
                if newChildren(i) != Node64.Empty then
                    nonEmptyCount += 1
                    lastNonEmptyIdx = i
                i += 1

            if nonEmptyCount == 1 then
                newChildren(lastNonEmptyIdx) match
                    case leaf: Node64.Leaf =>
                        mkLeaf(cursor, leaf.fullPath, leaf.key, leaf.value)
                    case inner: Node64.Branch =>
                        mkBranch(
                          cursor,
                          branch.skipLen + 1 + inner.skipLen,
                          inner.repPath,
                          inner.children,
                          inner.size
                        )
                    case Node64.Empty => Node64.Empty
            else
                mkBranch(
                  branch.skipStart,
                  branch.skipLen,
                  branch.repPath,
                  newChildren,
                  branch.size - 1
                )

    private def doProve(
        node: Node64,
        path: ByteString,
        cursor: Int
    ): (Boolean, List[InternalProofStep64]) = node match
        case Node64.Empty =>
            (false, Nil)

        case leaf: Node64.Leaf =>
            (leaf.fullPath == path, Nil)

        case branch: Node64.Branch =>
            val childSixit = sixitAt(path, cursor + branch.skipLen)
            val child = branch.children(childSixit)

            val (found, childSteps) = child match
                case Node64.Empty => (false, Nil)
                case _            => doProve(child, path, cursor + branch.skipLen + 1)

            val step = makeProofStep(branch, childSixit, branch.skipLen)
            (found, step :: childSteps)

    private def makeProofStep(
        branch: Node64.Branch,
        targetIndex: Int,
        skip: Int
    ): InternalProofStep64 = {
        val siblings = branch.children.zipWithIndex.filter { case (child, idx) =>
            child != Node64.Empty && idx != targetIndex
        }

        if siblings.length >= 2 then
            InternalProofStep64.Branch(skip, merkleProof6(branch.children, targetIndex))
        else if siblings.length == 1 then
            val (neighbor, neighborIdx) = siblings.head
            neighbor match
                case leaf: Node64.Leaf =>
                    InternalProofStep64.Leaf(skip, leaf.fullPath, blake2b_256(leaf.value))
                case inner: Node64.Branch =>
                    val (hL, hR) = merkleHalves(inner.children.map(_.hash))
                    InternalProofStep64.Fork(skip, neighborIdx, inner.skipLen, hL, hR)
                case Node64.Empty =>
                    throw new IllegalStateException("Unexpected empty neighbor")
        else throw new IllegalStateException("Branch with fewer than 2 children")
    }

    private def proofToBinary(steps: List[InternalProofStep64]): ByteString = {
        val buf = new java.io.ByteArrayOutputStream()
        for step <- steps do
            step match
                case InternalProofStep64.Branch(skip, neighbors) =>
                    buf.write(0)
                    buf.write(skip)
                    buf.write(neighbors.bytes)
                case InternalProofStep64.Fork(skip, index, prefixLen, halfLeft, halfRight) =>
                    buf.write(1)
                    buf.write(skip)
                    buf.write(index)
                    buf.write(prefixLen)
                    buf.write(halfLeft.bytes)
                    buf.write(halfRight.bytes)
                case InternalProofStep64.Leaf(skip, key, value) =>
                    buf.write(2)
                    buf.write(skip)
                    buf.write(key.bytes)
                    buf.write(value.bytes)
        ByteString.unsafeFromArray(buf.toByteArray)
    }
}

private[trie] sealed trait InternalProofStep64

private[trie] object InternalProofStep64 {
    case class Branch(skip: Int, neighbors: ByteString) extends InternalProofStep64
    case class Fork(
        skip: Int,
        index: Int,
        prefixLen: Int,
        halfLeft: ByteString,
        halfRight: ByteString
    ) extends InternalProofStep64
    case class Leaf(skip: Int, key: ByteString, value: ByteString) extends InternalProofStep64
}
