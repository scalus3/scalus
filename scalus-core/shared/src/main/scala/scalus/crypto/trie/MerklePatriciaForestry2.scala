package scalus.crypto.trie

import scalus.cardano.onchain.plutus.crypto.trie.Merkling.{NullHash, combine}
import scalus.cardano.onchain.plutus.crypto.trie.MerklePatriciaForestry2 as OnChain2
import scalus.cardano.onchain.plutus.crypto.trie.MerklePatriciaForestry2.{
    Neighbor,
    Proof,
    ProofStep
}
import scalus.cardano.onchain.plutus.prelude.List as PList
import scalus.uplc.builtin.Builtins.*
import scalus.uplc.builtin.ByteString

/** Off-chain radix-2 Merkle Patricia Forestry with Data-encoded proofs (unfused).
  *
  * Uses 1-bit path units (256 levels for 32-byte keys). Branch nodes have 2 children. Branch hash
  * uses two blake2b calls: `combine(bits_prefix, combine(child0, child1))`.
  *
  * Proofs are `List[ProofStep]` encoded as Plutus Data, compatible with the on-chain
  * [[scalus.cardano.onchain.plutus.crypto.trie.MerklePatriciaForestry2]] verifier.
  */
case class MerklePatriciaForestry2(root: UnfusedNode2) {
    import MerklePatriciaForestry2.*

    def rootHash: ByteString = root.hash
    def size: Int = root.size
    def isEmpty: Boolean = root == UnfusedNode2.Empty

    def insert(key: ByteString, value: ByteString): MerklePatriciaForestry2 = {
        val path = blake2b_256(key)
        MerklePatriciaForestry2(doInsert(root, path, 0, key, value))
    }

    def delete(key: ByteString): MerklePatriciaForestry2 = {
        val path = blake2b_256(key)
        MerklePatriciaForestry2(doDelete(root, path, 0))
    }

    def get(key: ByteString): Option[ByteString] = {
        val path = blake2b_256(key)
        doGet(root, path, 0)
    }

    def proveMembership(key: ByteString): Proof = {
        val path = blake2b_256(key)
        val (found, steps) = doProve(root, path, 0)
        if !found then throw new NoSuchElementException(s"Key not in trie: ${key.toHex}")
        toProof(steps)
    }

    def proveNonMembership(key: ByteString): Proof = {
        if get(key).isDefined then
            throw new IllegalArgumentException(s"Key already in trie: ${key.toHex}")
        val expanded = insert(key, ByteString.empty)
        expanded.proveMembership(key)
    }

    def toOnChain: OnChain2 = OnChain2(rootHash)
}

sealed trait UnfusedNode2 {
    def hash: ByteString
    def size: Int
}

object UnfusedNode2 {
    case object Empty extends UnfusedNode2 {
        val hash: ByteString = NullHash
        val size: Int = 0
    }

    final class Leaf(
        val skipStart: Int,
        val fullPath: ByteString,
        val key: ByteString,
        val value: ByteString
    ) extends UnfusedNode2 {
        lazy val hash: ByteString = {
            val s = MerklePatriciaForestry2.suffix2(fullPath, skipStart)
            combine(s, blake2b_256(value))
        }
        val size: Int = 1
    }

    final class Branch(
        val skipStart: Int,
        val skipLen: Int,
        val repPath: ByteString,
        val children: Vector[UnfusedNode2], // always size 2
        val size: Int,
        computeHash: () => ByteString
    ) extends UnfusedNode2 {
        lazy val hash: ByteString = computeHash()
    }
}

object MerklePatriciaForestry2 {

    private val PathBits = 256

    def empty: MerklePatriciaForestry2 = MerklePatriciaForestry2(UnfusedNode2.Empty)

    def fromList(entries: Iterable[(ByteString, ByteString)]): MerklePatriciaForestry2 =
        entries.foldLeft(empty) { case (trie, (k, v)) => trie.insert(k, v) }

    private val emptyChildren: Vector[UnfusedNode2] =
        Vector(UnfusedNode2.Empty, UnfusedNode2.Empty)

    private def bitAt(path: ByteString, index: Int): Int = {
        val byte = path.bytes(index / 8) & 0xff
        (byte >> (7 - index % 8)) & 1
    }

    /** Suffix encoding matching the on-chain `MerklePatriciaForestry2.suffix2`. */
    private[trie] def suffix2(fullPath: ByteString, cursor: Int): ByteString = {
        val byteOffset = cursor / 8
        val bitOffset = cursor % 8
        val remaining = ByteString.unsafeFromArray(fullPath.bytes.drop(byteOffset))
        if bitOffset == 0 then ByteString.unsafeFromArray(Array(0xff.toByte) ++ remaining.bytes)
        else ByteString.unsafeFromArray(Array(bitOffset.toByte) ++ remaining.bytes)
    }

    /** Extract bit range as one-byte-per-bit ByteString, matching on-chain `bits()`. */
    private def bitsFromPath(path: ByteString, start: Int, len: Int): ByteString =
        ByteString.unsafeFromArray(Array.tabulate(len)(i => bitAt(path, start + i).toByte))

    /** Unfused branch hash: `combine(bits_prefix, combine(child0, child1))`. Two blake2b calls. */
    private def branchHash(
        skipStart: Int,
        skipLen: Int,
        repPath: ByteString,
        children: Vector[UnfusedNode2]
    ): ByteString =
        combine(
          bitsFromPath(repPath, skipStart, skipLen),
          combine(children(0).hash, children(1).hash)
        )

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
        branch: UnfusedNode2.Branch,
        path: ByteString,
        cursor: Int
    ): Boolean = {
        var i = 0
        while i < branch.skipLen do
            if bitAt(branch.repPath, cursor + i) != bitAt(path, cursor + i) then return false
            i += 1
        true
    }

    private def mkLeaf(
        skipStart: Int,
        fullPath: ByteString,
        key: ByteString,
        value: ByteString
    ): UnfusedNode2.Leaf =
        UnfusedNode2.Leaf(skipStart, fullPath, key, value)

    private def mkBranch(
        skipStart: Int,
        skipLen: Int,
        repPath: ByteString,
        children: Vector[UnfusedNode2],
        size: Int
    ): UnfusedNode2.Branch =
        UnfusedNode2.Branch(
          skipStart,
          skipLen,
          repPath,
          children,
          size,
          () => branchHash(skipStart, skipLen, repPath, children)
        )

    private def doInsert(
        node: UnfusedNode2,
        path: ByteString,
        cursor: Int,
        key: ByteString,
        value: ByteString
    ): UnfusedNode2 = node match
        case UnfusedNode2.Empty =>
            mkLeaf(cursor, path, key, value)

        case leaf: UnfusedNode2.Leaf =>
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

        case branch: UnfusedNode2.Branch =>
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

    private def doGet(
        node: UnfusedNode2,
        path: ByteString,
        cursor: Int
    ): Option[ByteString] =
        node match
            case UnfusedNode2.Empty => None
            case leaf: UnfusedNode2.Leaf =>
                if leaf.fullPath == path then Some(leaf.value) else None
            case branch: UnfusedNode2.Branch =>
                if !branchSkipMatchesPath(branch, path, cursor) then None
                else
                    val childBit = bitAt(path, cursor + branch.skipLen)
                    doGet(branch.children(childBit), path, cursor + branch.skipLen + 1)

    private def doDelete(
        node: UnfusedNode2,
        path: ByteString,
        cursor: Int
    ): UnfusedNode2 = node match
        case UnfusedNode2.Empty =>
            throw new NoSuchElementException("Key not in trie")

        case leaf: UnfusedNode2.Leaf =>
            if leaf.fullPath == path then UnfusedNode2.Empty
            else throw new NoSuchElementException("Key not in trie")

        case branch: UnfusedNode2.Branch =>
            if !branchSkipMatchesPath(branch, path, cursor) then
                throw new NoSuchElementException("Key not in trie")

            val childBit = bitAt(path, cursor + branch.skipLen)
            val childCursor = cursor + branch.skipLen + 1
            val newChild = doDelete(branch.children(childBit), path, childCursor)
            val newChildren = branch.children.updated(childBit, newChild)

            val siblingIdx = 1 - childBit
            if newChild == UnfusedNode2.Empty then
                newChildren(siblingIdx) match
                    case leaf: UnfusedNode2.Leaf =>
                        mkLeaf(cursor, leaf.fullPath, leaf.key, leaf.value)
                    case inner: UnfusedNode2.Branch =>
                        mkBranch(
                          cursor,
                          branch.skipLen + 1 + inner.skipLen,
                          inner.repPath,
                          inner.children,
                          inner.size
                        )
                    case UnfusedNode2.Empty => UnfusedNode2.Empty
            else
                mkBranch(
                  branch.skipStart,
                  branch.skipLen,
                  branch.repPath,
                  newChildren,
                  branch.size - 1
                )

    // --- Proof generation ---

    private case class ProofStep2(skip: Int, siblingIdx: Int, sibling: UnfusedNode2)

    private def doProve(
        node: UnfusedNode2,
        path: ByteString,
        cursor: Int
    ): (Boolean, List[ProofStep2]) = node match
        case UnfusedNode2.Empty =>
            (false, Nil)

        case leaf: UnfusedNode2.Leaf =>
            (leaf.fullPath == path, Nil)

        case branch: UnfusedNode2.Branch =>
            val childBit = bitAt(path, cursor + branch.skipLen)
            val child = branch.children(childBit)
            val siblingIdx = 1 - childBit

            val (found, childSteps) = child match
                case UnfusedNode2.Empty => (false, Nil)
                case _                  => doProve(child, path, cursor + branch.skipLen + 1)

            val step = ProofStep2(branch.skipLen, siblingIdx, branch.children(siblingIdx))
            (found, step :: childSteps)

    private def makeProofStep(step: ProofStep2): ProofStep =
        step.sibling match
            case leaf: UnfusedNode2.Leaf =>
                ProofStep.Leaf(step.skip, leaf.fullPath, blake2b_256(leaf.value))
            case branch: UnfusedNode2.Branch =>
                ProofStep.Fork(
                  step.skip,
                  Neighbor(
                    bit = step.siblingIdx,
                    prefix = bitsFromPath(branch.repPath, branch.skipStart, branch.skipLen),
                    root = combine(branch.children(0).hash, branch.children(1).hash)
                  )
                )
            case UnfusedNode2.Empty =>
                throw new IllegalStateException("Empty sibling in proof")

    private def toProof(steps: List[ProofStep2]): Proof = {
        val len = steps.length
        var idx = 0
        val dataSteps = steps.map { step =>
            val isLast = idx == len - 1
            idx += 1
            if isLast then makeProofStep(step)
            else ProofStep.Branch(step.skip, step.sibling.hash)
        }
        dataSteps.foldRight(PList.Nil: Proof) { (step, acc) => PList.Cons(step, acc) }
    }
}
