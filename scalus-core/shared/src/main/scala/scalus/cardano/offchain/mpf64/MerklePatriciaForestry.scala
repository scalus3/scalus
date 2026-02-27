package scalus.cardano.offchain.mpf64

import scalus.cardano.onchain.plutus.mpf64.MerklePatriciaForestry as OnChainForestry
import scalus.cardano.onchain.plutus.mpf64.MerklePatriciaForestry.{Neighbor, Proof, ProofStep}
import scalus.cardano.onchain.plutus.mpf64.Merkling.*
import scalus.cardano.onchain.plutus.prelude.List as PList
import scalus.uplc.builtin.Builtins.*
import scalus.uplc.builtin.ByteString

private[mpf64] sealed trait Node {
    def hash: ByteString
    def size: Int
}

private[mpf64] object Node {

    case object Empty extends Node {
        val hash: ByteString = NullHash
        val size: Int = 0
    }

    case class Leaf(
        hash: ByteString,
        skip: Vector[Int],
        fullPath: ByteString,
        key: ByteString,
        value: ByteString
    ) extends Node {
        val size: Int = 1
    }

    /** A branch node with up to 64 children. */
    case class Branch(
        hash: ByteString,
        skip: Vector[Int],
        children: Vector[Node],
        size: Int
    ) extends Node
}

/** Off-chain Radix-64 Merkle Patricia Forestry implementation.
  *
  * Uses 6-bit path units (64 children per branch) extracted from 32-byte blake2b-256 paths. 32
  * bytes = 256 bits → 42 six-bit values (252 bits) + 4 leftover bits in suffix.
  */
case class MerklePatriciaForestry(root: Node) {
    import MerklePatriciaForestry.*

    def rootHash: ByteString = root.hash

    def size: Int = root.size

    def isEmpty: Boolean = root == Node.Empty

    def insert(key: ByteString, value: ByteString): MerklePatriciaForestry = {
        val path = blake2b_256(key)
        MerklePatriciaForestry(doInsert(root, path, 0, key, value))
    }

    def delete(key: ByteString): MerklePatriciaForestry = {
        val path = blake2b_256(key)
        MerklePatriciaForestry(doDelete(root, path, 0))
    }

    def get(key: ByteString): Option[ByteString] = {
        val path = blake2b_256(key)
        doGet(root, path, 0)
    }

    def proveExists(key: ByteString): Proof = {
        val path = blake2b_256(key)
        val (found, steps) = doProve(root, path, 0)
        if !found then throw new NoSuchElementException(s"Key not in trie: ${key.toHex}")
        toProof(steps)
    }

    def proveMissing(key: ByteString): Proof = {
        if get(key).isDefined then
            throw new IllegalArgumentException(s"Key already in trie: ${key.toHex}")
        val expanded = insert(key, ByteString.empty)
        expanded.proveExists(key)
    }

    /** Creates a binary-encoded proof of inclusion, suitable for the `mpf64b` on-chain verifier. */
    def proveExistsBinary(key: ByteString): ByteString = {
        val path = blake2b_256(key)
        val (found, steps) = MerklePatriciaForestry.doProve(root, path, 0)
        if !found then throw new NoSuchElementException(s"Key not in trie: ${key.toHex}")
        MerklePatriciaForestry.proofToBinary(steps)
    }

    /** Creates a binary-encoded proof of exclusion, suitable for the `mpf64b` on-chain verifier. */
    def proveMissingBinary(key: ByteString): ByteString = {
        if get(key).isDefined then
            throw new IllegalArgumentException(s"Key already in trie: ${key.toHex}")
        val expanded = insert(key, ByteString.empty)
        expanded.proveExistsBinary(key)
    }

    def toOnChain: OnChainForestry = OnChainForestry(rootHash)
}

object MerklePatriciaForestry {

    /** Number of 6-bit path units in a blake2b-256 path (252 bits / 6 = 42). */
    private val PathUnits = 42

    def empty: MerklePatriciaForestry = MerklePatriciaForestry(Node.Empty)

    def fromList(entries: Iterable[(ByteString, ByteString)]): MerklePatriciaForestry =
        entries.foldLeft(empty) { case (trie, (k, v)) => trie.insert(k, v) }

    /** Extract a 6-bit value (0-63) at the given index from a 32-byte path. */
    private def sixitAt(path: ByteString, index: Int): Int = {
        val base = (index / 4) * 3
        val pos = index % 4
        pos match
            case 0 => (path.bytes(base) & 0xff) >> 2
            case 1 => ((path.bytes(base) & 0x03) << 4) | ((path.bytes(base + 1) & 0xff) >> 4)
            case 2 => ((path.bytes(base + 1) & 0x0f) << 2) | ((path.bytes(base + 2) & 0xff) >> 6)
            case 3 => path.bytes(base + 2) & 0x3f
    }

    private def extractSixits(path: ByteString, start: Int, end: Int): Vector[Int] =
        (start until end).map(i => sixitAt(path, i)).toVector

    private def combine3(a: ByteString, b: ByteString, c: ByteString): ByteString =
        blake2b_256(appendByteString(a, appendByteString(b, c)))

    private def leafHash(fullPath: ByteString, cursor: Int, value: ByteString): ByteString =
        combine(suffix(fullPath, cursor), blake2b_256(value))

    /** Branch hash: `combine3(prefix, halfLeft, halfRight)` — matches on-chain doBranch. */
    private def branchHash(skip: Vector[Int], children: Vector[Node]): ByteString = {
        val prefix = consByteString(skip.length, ByteString.empty)
        val (hL, hR) = merkleHalves(children.map(_.hash))
        combine3(prefix, hL, hR)
    }

    /** Compute merkle root from pre-computed hashes. */
    private def merkleRootFromHashes(hashes: Vector[ByteString]): ByteString = {
        var current = hashes
        while current.length > 1 do
            current = current.grouped(2).map { pair => combine(pair(0), pair(1)) }.toVector
        current.head
    }

    /** Reduce hashes to two halves (left and right subtree roots). */
    private def merkleHalves(hashes: Vector[ByteString]): (ByteString, ByteString) = {
        var current = hashes
        while current.length > 2 do
            current = current.grouped(2).map { pair => combine(pair(0), pair(1)) }.toVector
        (current(0), current(1))
    }

    /** Extract 6 sibling hashes (192 bytes) for a Branch proof step. Largest level first
      * (consistent with on-chain merkle64 which peels from front).
      */
    private def extractSiblings(hashes: Vector[ByteString], idx: Int): ByteString = {
        if hashes.length <= 1 then ByteString.empty
        else
            val half = hashes.length / 2
            val (left, right) = hashes.splitAt(half)
            if idx < half then merkleRootFromHashes(right).concat(extractSiblings(left, idx))
            else merkleRootFromHashes(left).concat(extractSiblings(right, idx - half))
    }

    /** Extract 6 sibling hashes for a branch proof (192 bytes). */
    private def merkleProof6(children: Vector[Node], branch: Int): ByteString = {
        val hashes = children.map(_.hash)
        extractSiblings(hashes, branch)
    }

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

    private val emptyChildren: Vector[Node] = Vector.fill(64)(Node.Empty)

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
            mkLeaf(extractSixits(path, cursor, PathUnits), path, cursor, key, value)

        case leaf: Node.Leaf =>
            val remaining = extractSixits(path, cursor, PathUnits)
            val commonPrefix = commonPrefixLen(remaining, leaf.skip)

            if commonPrefix == remaining.length && commonPrefix == leaf.skip.length then
                throw new IllegalArgumentException(s"Key already in trie: ${key.toHex}")

            val newSixit = remaining(commonPrefix)
            val oldSixit = leaf.skip(commonPrefix)
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
                .updated(newSixit, newLeaf)
                .updated(oldSixit, oldLeaf)
            mkBranch(remaining.take(commonPrefix), children)

        case branch: Node.Branch =>
            val remaining = extractSixits(path, cursor, cursor + branch.skip.length + 1)
            val prefixPart = remaining.dropRight(1)
            val cp = commonPrefixLen(prefixPart, branch.skip)

            if cp < branch.skip.length then
                val splitCursor = cursor + cp + 1
                val newSixit = prefixPart(cp)
                val oldSixit = branch.skip(cp)

                val newLeaf =
                    mkLeaf(
                      extractSixits(path, splitCursor, PathUnits),
                      path,
                      splitCursor,
                      key,
                      value
                    )
                val oldBranch = mkBranch(branch.skip.drop(cp + 1), branch.children)

                val children = emptyChildren
                    .updated(newSixit, newLeaf)
                    .updated(oldSixit, oldBranch)
                mkBranch(branch.skip.take(cp), children)
            else
                val childSixit = remaining.last
                val childCursor = cursor + branch.skip.length + 1
                val newChild = doInsert(branch.children(childSixit), path, childCursor, key, value)
                mkBranch(branch.skip, branch.children.updated(childSixit, newChild))

    private def doGet(node: Node, path: ByteString, cursor: Int): Option[ByteString] =
        node match
            case Node.Empty => None
            case leaf: Node.Leaf =>
                if leaf.fullPath == path then Some(leaf.value) else None
            case branch: Node.Branch =>
                val remaining = extractSixits(path, cursor, cursor + branch.skip.length)
                if remaining != branch.skip then None
                else
                    val childSixit = sixitAt(path, cursor + branch.skip.length)
                    doGet(branch.children(childSixit), path, cursor + branch.skip.length + 1)

    private def doDelete(node: Node, path: ByteString, cursor: Int): Node = node match
        case Node.Empty =>
            throw new NoSuchElementException("Key not in trie")

        case leaf: Node.Leaf =>
            if leaf.fullPath == path then Node.Empty
            else throw new NoSuchElementException("Key not in trie")

        case branch: Node.Branch =>
            val remaining = extractSixits(path, cursor, cursor + branch.skip.length)
            if remaining != branch.skip then throw new NoSuchElementException("Key not in trie")

            val childSixit = sixitAt(path, cursor + branch.skip.length)
            val childCursor = cursor + branch.skip.length + 1
            val newChild = doDelete(branch.children(childSixit), path, childCursor)
            val newChildren = branch.children.updated(childSixit, newChild)

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

    private def doProve(
        node: Node,
        path: ByteString,
        cursor: Int
    ): (Boolean, scala.List[ProofStep]) = node match
        case Node.Empty =>
            (false, Nil)

        case leaf: Node.Leaf =>
            (leaf.fullPath == path, Nil)

        case branch: Node.Branch =>
            val skip = branch.skip.length
            val childSixit = sixitAt(path, cursor + skip)
            val child = branch.children(childSixit)

            val (found, childSteps) = child match
                case Node.Empty => (false, Nil)
                case _          => doProve(child, path, cursor + skip + 1)

            val step = makeProofStep(branch, childSixit, skip)
            (found, step :: childSteps)

    private def makeProofStep(branch: Node.Branch, targetIndex: Int, skip: Int): ProofStep = {
        val siblings = branch.children.zipWithIndex.filter { case (child, idx) =>
            child != Node.Empty && idx != targetIndex
        }

        if siblings.length >= 2 then
            ProofStep.Branch(skip, merkleProof6(branch.children, targetIndex))
        else if siblings.length == 1 then
            val (neighbor, neighborIdx) = siblings.head
            neighbor match
                case leaf: Node.Leaf =>
                    ProofStep.Leaf(skip, leaf.fullPath, blake2b_256(leaf.value))
                case inner: Node.Branch =>
                    val (hL, hR) = merkleHalves(inner.children.map(_.hash))
                    ProofStep.Fork(
                      skip,
                      Neighbor(
                        index = neighborIdx,
                        prefixLen = inner.skip.length,
                        halfLeft = hL,
                        halfRight = hR
                      )
                    )
                case Node.Empty =>
                    throw new IllegalStateException("Unexpected empty neighbor")
        else throw new IllegalStateException("Branch with fewer than 2 children")
    }

    private def toProof(steps: scala.List[ProofStep]): Proof =
        steps.foldRight(PList.Nil: Proof) { (step, acc) => PList.Cons(step, acc) }

    /** Serialize proof steps into the compact binary format used by `mpf64b`. */
    private def proofToBinary(steps: scala.List[ProofStep]): ByteString = {
        val buf = new java.io.ByteArrayOutputStream()
        for step <- steps do
            step match
                case ProofStep.Branch(skip, neighbors) =>
                    buf.write(0)
                    buf.write(skip.toInt)
                    buf.write(neighbors.bytes)
                case ProofStep.Fork(skip, Neighbor(index, prefixLen, halfLeft, halfRight)) =>
                    buf.write(1)
                    buf.write(skip.toInt)
                    buf.write(index.toInt)
                    buf.write(prefixLen.toInt)
                    buf.write(halfLeft.bytes)
                    buf.write(halfRight.bytes)
                case ProofStep.Leaf(skip, key, value) =>
                    buf.write(2)
                    buf.write(skip.toInt)
                    buf.write(key.bytes)
                    buf.write(value.bytes)
        ByteString.unsafeFromArray(buf.toByteArray)
    }
}
