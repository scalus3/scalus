package scalus.crypto.trie

import scalus.cardano.onchain.plutus.crypto.trie.Merkling.{NullHash, combine, combine3}
import scalus.cardano.onchain.plutus.crypto.trie.FusedMerklePatriciaForestry2 as OnChain2
import scalus.uplc.builtin.Builtins.*
import scalus.uplc.builtin.ByteString

/** Off-chain radix-2 Fused Merkle Patricia Forestry with binary proofs.
  *
  * Each branch step consumes 1 bit. Branch nodes have 2 children. Branch hash =
  * `combine3(skipLen_byte, child0_hash, child1_hash)`.
  *
  * Proofs use the same binary format as other fused variants but with 32-byte neighbors per branch
  * step (1 sibling hash) instead of 128 (radix-16) or 192 (radix-64).
  */
case class FusedMerklePatriciaForestry2(root: Node2) {
    import FusedMerklePatriciaForestry2.*

    def rootHash: ByteString = root.hash
    def size: Int = root.size
    def isEmpty: Boolean = root == Node2.Empty

    def insert(key: ByteString, value: ByteString): FusedMerklePatriciaForestry2 = {
        val path = blake2b_256(key)
        FusedMerklePatriciaForestry2(doInsert(root, path, 0, key, value))
    }

    def delete(key: ByteString): FusedMerklePatriciaForestry2 = {
        val path = blake2b_256(key)
        FusedMerklePatriciaForestry2(doDelete(root, path, 0))
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

    def toOnChain: OnChain2 = OnChain2(rootHash)
}

sealed trait Node2 {
    def hash: ByteString
    def size: Int
}

object Node2 {
    case object Empty extends Node2 {
        val hash: ByteString = NullHash
        val size: Int = 0
    }

    final class Leaf(
        val skipStart: Int,
        val fullPath: ByteString,
        val key: ByteString,
        val value: ByteString
    ) extends Node2 {
        lazy val hash: ByteString = {
            val s = FusedMerklePatriciaForestry2.suffix(fullPath, skipStart)
            combine(s, blake2b_256(value))
        }
        val size: Int = 1
    }

    final class Branch(
        val skipStart: Int,
        val skipLen: Int,
        val repPath: ByteString,
        val children: Vector[Node2], // always size 2
        val size: Int,
        computeHash: () => ByteString
    ) extends Node2 {
        lazy val hash: ByteString = computeHash()
    }
}

object FusedMerklePatriciaForestry2 {

    private val PathBits = 256

    def empty: FusedMerklePatriciaForestry2 = FusedMerklePatriciaForestry2(Node2.Empty)

    def fromList(entries: Iterable[(ByteString, ByteString)]): FusedMerklePatriciaForestry2 =
        entries.foldLeft(empty) { case (trie, (k, v)) => trie.insert(k, v) }

    private val emptyChildren: Vector[Node2] = Vector(Node2.Empty, Node2.Empty)

    private def bitAt(path: ByteString, index: Int): Int = {
        val byte = path.bytes(index / 8) & 0xff
        (byte >> (7 - index % 8)) & 1
    }

    /** Suffix encoding matching the on-chain `FusedMerklePatriciaForestry2.suffix`. */
    private[trie] def suffix(fullPath: ByteString, cursor: Int): ByteString = {
        val byteOffset = cursor / 8
        val bitOffset = cursor % 8
        val remaining = ByteString.unsafeFromArray(
          fullPath.bytes.drop(byteOffset)
        )
        if bitOffset == 0 then ByteString.unsafeFromArray(Array(0xff.toByte) ++ remaining.bytes)
        else ByteString.unsafeFromArray(Array(bitOffset.toByte) ++ remaining.bytes)
    }

    private def commonPrefixLen(
        pathA: ByteString,
        pathB: ByteString,
        start: Int,
        end: Int
    ): Int = {
        var i = start
        while i < end && bitAt(pathA, i) == bitAt(pathB, i) do i += 1
        i - start
    }

    private def branchSkipMatchesPath(
        branch: Node2.Branch,
        path: ByteString,
        cursor: Int
    ): Boolean = {
        var i = 0
        while i < branch.skipLen do
            if bitAt(branch.repPath, cursor + i) != bitAt(path, cursor + i) then return false
            i += 1
        true
    }

    private def branchHash(skipLen: Int, children: Vector[Node2]): ByteString = {
        val prefix = ByteString.unsafeFromArray(Array(skipLen.toByte))
        combine3(prefix, children(0).hash, children(1).hash)
    }

    private def mkLeaf(
        skipStart: Int,
        fullPath: ByteString,
        key: ByteString,
        value: ByteString
    ): Node2.Leaf =
        Node2.Leaf(skipStart, fullPath, key, value)

    private def mkBranch(
        skipStart: Int,
        skipLen: Int,
        repPath: ByteString,
        children: Vector[Node2],
        size: Int
    ): Node2.Branch =
        Node2.Branch(
          skipStart,
          skipLen,
          repPath,
          children,
          size,
          () => branchHash(skipLen, children)
        )

    private def doInsert(
        node: Node2,
        path: ByteString,
        cursor: Int,
        key: ByteString,
        value: ByteString
    ): Node2 = node match
        case Node2.Empty =>
            mkLeaf(cursor, path, key, value)

        case leaf: Node2.Leaf =>
            val remainingLen = PathBits - cursor
            val cp = commonPrefixLen(path, leaf.fullPath, cursor, PathBits)

            if cp == remainingLen then
                throw new IllegalArgumentException(s"Key already in trie: ${key.toHex}")

            val newBit = bitAt(path, cursor + cp)
            val oldBit = bitAt(leaf.fullPath, cursor + cp)
            val splitCursor = cursor + cp + 1

            val newLeaf = mkLeaf(splitCursor, path, key, value)
            val oldLeaf = mkLeaf(splitCursor, leaf.fullPath, leaf.key, leaf.value)

            val children = emptyChildren
                .updated(newBit, newLeaf)
                .updated(oldBit, oldLeaf)
            mkBranch(cursor, cp, path, children, 2)

        case branch: Node2.Branch =>
            val cp = commonPrefixLen(path, branch.repPath, cursor, cursor + branch.skipLen)

            if cp < branch.skipLen then
                val splitCursor = cursor + cp + 1
                val newBit = bitAt(path, cursor + cp)
                val oldBit = bitAt(branch.repPath, cursor + cp)

                val newLeaf = mkLeaf(splitCursor, path, key, value)
                val oldBranch = mkBranch(
                  branch.skipStart + cp + 1,
                  branch.skipLen - cp - 1,
                  branch.repPath,
                  branch.children,
                  branch.size
                )

                val children = emptyChildren
                    .updated(newBit, newLeaf)
                    .updated(oldBit, oldBranch)
                mkBranch(cursor, cp, path, children, branch.size + 1)
            else
                val childBit = bitAt(path, cursor + branch.skipLen)
                val childCursor = cursor + branch.skipLen + 1
                val newChild =
                    doInsert(branch.children(childBit), path, childCursor, key, value)
                mkBranch(
                  branch.skipStart,
                  branch.skipLen,
                  branch.repPath,
                  branch.children.updated(childBit, newChild),
                  branch.size + 1
                )

    private def doGet(node: Node2, path: ByteString, cursor: Int): Option[ByteString] =
        node match
            case Node2.Empty => None
            case leaf: Node2.Leaf =>
                if leaf.fullPath == path then Some(leaf.value) else None
            case branch: Node2.Branch =>
                if !branchSkipMatchesPath(branch, path, cursor) then None
                else
                    val childBit = bitAt(path, cursor + branch.skipLen)
                    doGet(branch.children(childBit), path, cursor + branch.skipLen + 1)

    private def doDelete(node: Node2, path: ByteString, cursor: Int): Node2 = node match
        case Node2.Empty =>
            throw new NoSuchElementException("Key not in trie")

        case leaf: Node2.Leaf =>
            if leaf.fullPath == path then Node2.Empty
            else throw new NoSuchElementException("Key not in trie")

        case branch: Node2.Branch =>
            if !branchSkipMatchesPath(branch, path, cursor) then
                throw new NoSuchElementException("Key not in trie")

            val childBit = bitAt(path, cursor + branch.skipLen)
            val childCursor = cursor + branch.skipLen + 1
            val newChild = doDelete(branch.children(childBit), path, childCursor)
            val newChildren = branch.children.updated(childBit, newChild)

            // In radix-2, exactly one sibling (index 1-childBit)
            val siblingIdx = 1 - childBit
            if newChild == Node2.Empty then
                // Collapse: promote sibling
                newChildren(siblingIdx) match
                    case leaf: Node2.Leaf =>
                        mkLeaf(cursor, leaf.fullPath, leaf.key, leaf.value)
                    case inner: Node2.Branch =>
                        mkBranch(
                          cursor,
                          branch.skipLen + 1 + inner.skipLen,
                          inner.repPath,
                          inner.children,
                          inner.size
                        )
                    case Node2.Empty => Node2.Empty
            else
                mkBranch(
                  branch.skipStart,
                  branch.skipLen,
                  branch.repPath,
                  newChildren,
                  branch.size - 1
                )

    // --- Proof generation ---

    /** Internal proof step: stores the sibling node for later serialization. */
    private case class ProofStep2(skip: Int, siblingIdx: Int, sibling: Node2)

    private def doProve(
        node: Node2,
        path: ByteString,
        cursor: Int
    ): (Boolean, List[ProofStep2]) = node match
        case Node2.Empty =>
            (false, Nil)

        case leaf: Node2.Leaf =>
            (leaf.fullPath == path, Nil)

        case branch: Node2.Branch =>
            val childBit = bitAt(path, cursor + branch.skipLen)
            val child = branch.children(childBit)
            val siblingIdx = 1 - childBit

            val (found, childSteps) = child match
                case Node2.Empty => (false, Nil)
                case _           => doProve(child, path, cursor + branch.skipLen + 1)

            val step = ProofStep2(branch.skipLen, siblingIdx, branch.children(siblingIdx))
            (found, step :: childSteps)

    /** Serialize proof: Branch (34B) for non-last steps, Fork/Leaf (67/66B) for last step. */
    private def proofToBinary(steps: List[ProofStep2]): ByteString = {
        val buf = new java.io.ByteArrayOutputStream()
        val len = steps.length
        var idx = 0
        for step <- steps do
            val isLast = idx == len - 1
            if isLast then
                step.sibling match
                    case leaf: Node2.Leaf =>
                        // Leaf: 0x02 | skip | key | value_hash
                        buf.write(2)
                        buf.write(step.skip)
                        buf.write(leaf.fullPath.bytes)
                        buf.write(blake2b_256(leaf.value).bytes)
                    case branch: Node2.Branch =>
                        // Fork: 0x01 | skip | prefixLen | child0 | child1
                        buf.write(1)
                        buf.write(step.skip)
                        buf.write(branch.skipLen)
                        buf.write(branch.children(0).hash.bytes)
                        buf.write(branch.children(1).hash.bytes)
                    case Node2.Empty =>
                        // Should not happen in a valid trie
                        throw new IllegalStateException("Empty sibling in proof")
            else
                // Branch: 0x00 | skip | sibling_hash
                buf.write(0)
                buf.write(step.skip)
                buf.write(step.sibling.hash.bytes)
            idx += 1
        ByteString.unsafeFromArray(buf.toByteArray)
    }
}
