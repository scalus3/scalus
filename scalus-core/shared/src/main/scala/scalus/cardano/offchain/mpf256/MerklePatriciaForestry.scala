package scalus.cardano.offchain.mpf256

import scalus.cardano.onchain.plutus.mpf256.MerklePatriciaForestry as OnChainForestry
import scalus.cardano.onchain.plutus.mpf256.MerklePatriciaForestry.{Neighbor, Proof, ProofStep}
import scalus.cardano.onchain.plutus.mpf256.Merkling.*
import scalus.cardano.onchain.plutus.prelude.List as PList
import scalus.uplc.builtin.Builtins.*
import scalus.uplc.builtin.ByteString

private[mpf256] sealed trait Node {
    def hash: ByteString
    def size: Int
}

private[mpf256] object Node {

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

    case class Branch(
        hash: ByteString,
        skip: Vector[Int],
        children: Vector[Node],
        size: Int
    ) extends Node
}

/** Off-chain Radix-256 Merkle Patricia Forestry implementation.
  *
  * Uses 8-bit bytes (256 children per branch) instead of 4-bit nibbles (16 children). This yields
  * 32 levels max (vs 64 for radix-16) for blake2b-256 paths.
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

    def toOnChain: OnChainForestry = OnChainForestry(rootHash)
}

object MerklePatriciaForestry {

    /** Number of bytes in a blake2b-256 path (32 bytes). */
    private val PathBytes = 32

    def empty: MerklePatriciaForestry = MerklePatriciaForestry(Node.Empty)

    def fromList(entries: Iterable[(ByteString, ByteString)]): MerklePatriciaForestry =
        entries.foldLeft(empty) { case (trie, (k, v)) => trie.insert(k, v) }

    private def leafHash(fullPath: ByteString, cursor: Int, value: ByteString): ByteString =
        combine(suffix(fullPath, cursor), blake2b_256(value))

    private def branchHash(skip: Vector[Int], children: Vector[Node]): ByteString =
        combine(consByteString(skip.length, ByteString.empty), merkleRoot256(children))

    /** Compute the merkle root of 256 child hashes using binary tree reduction. */
    private def merkleRoot256(children: Vector[Node]): ByteString = {
        var hashes = children.map(_.hash)
        while hashes.length > 1 do
            hashes = hashes.grouped(2).map { pair => combine(pair(0), pair(1)) }.toVector
        hashes.head
    }

    /** Compute merkle root from pre-computed hashes. */
    private def merkleRootFromHashes(hashes: Vector[ByteString]): ByteString = {
        var current = hashes
        while current.length > 1 do
            current = current.grouped(2).map { pair => combine(pair(0), pair(1)) }.toVector
        current.head
    }

    /** Extract 8 sibling hashes (256 bytes) for a Branch proof step. These are the sibling hashes
      * at each level of the binary tree over 256 slots. Largest level first (consistent with
      * on-chain merkle function which peels from front).
      */
    private def extractSiblings(hashes: Vector[ByteString], idx: Int): ByteString = {
        if hashes.length <= 1 then ByteString.empty
        else
            val half = hashes.length / 2
            val (left, right) = hashes.splitAt(half)
            if idx < half then merkleRootFromHashes(right).concat(extractSiblings(left, idx))
            else merkleRootFromHashes(left).concat(extractSiblings(right, idx - half))
    }

    /** Extract 8 sibling hashes for a branch proof. */
    private def merkleProof8(children: Vector[Node], branch: Int): ByteString = {
        val hashes = children.map(_.hash)
        extractSiblings(hashes, branch)
    }

    private def byteAtPath(path: ByteString, index: Int): Int =
        path.bytes(index) & 0xff

    private def extractBytes(path: ByteString, start: Int, end: Int): Vector[Int] =
        (start until end).map(i => byteAtPath(path, i)).toVector

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

    private val emptyChildren: Vector[Node] = Vector.fill(256)(Node.Empty)

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
            mkLeaf(extractBytes(path, cursor, PathBytes), path, cursor, key, value)

        case leaf: Node.Leaf =>
            val remaining = extractBytes(path, cursor, PathBytes)
            val commonPrefix = commonPrefixLen(remaining, leaf.skip)

            if commonPrefix == remaining.length && commonPrefix == leaf.skip.length then
                throw new IllegalArgumentException(s"Key already in trie: ${key.toHex}")

            val newByte = remaining(commonPrefix)
            val oldByte = leaf.skip(commonPrefix)
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
                .updated(newByte, newLeaf)
                .updated(oldByte, oldLeaf)
            mkBranch(remaining.take(commonPrefix), children)

        case branch: Node.Branch =>
            val remaining = extractBytes(path, cursor, cursor + branch.skip.length + 1)
            val prefixPart = remaining.dropRight(1)
            val cp = commonPrefixLen(prefixPart, branch.skip)

            if cp < branch.skip.length then
                val splitCursor = cursor + cp + 1
                val newByte = prefixPart(cp)
                val oldByte = branch.skip(cp)

                val newLeaf =
                    mkLeaf(
                      extractBytes(path, splitCursor, PathBytes),
                      path,
                      splitCursor,
                      key,
                      value
                    )
                val oldBranch = mkBranch(branch.skip.drop(cp + 1), branch.children)

                val children = emptyChildren
                    .updated(newByte, newLeaf)
                    .updated(oldByte, oldBranch)
                mkBranch(branch.skip.take(cp), children)
            else
                val childByte = remaining.last
                val childCursor = cursor + branch.skip.length + 1
                val newChild = doInsert(branch.children(childByte), path, childCursor, key, value)
                mkBranch(branch.skip, branch.children.updated(childByte, newChild))

    private def doGet(node: Node, path: ByteString, cursor: Int): Option[ByteString] =
        node match
            case Node.Empty => None
            case leaf: Node.Leaf =>
                if leaf.fullPath == path then Some(leaf.value) else None
            case branch: Node.Branch =>
                val remaining = extractBytes(path, cursor, cursor + branch.skip.length)
                if remaining != branch.skip then None
                else
                    val childByte = byteAtPath(path, cursor + branch.skip.length)
                    doGet(branch.children(childByte), path, cursor + branch.skip.length + 1)

    private def doDelete(node: Node, path: ByteString, cursor: Int): Node = node match
        case Node.Empty =>
            throw new NoSuchElementException("Key not in trie")

        case leaf: Node.Leaf =>
            if leaf.fullPath == path then Node.Empty
            else throw new NoSuchElementException("Key not in trie")

        case branch: Node.Branch =>
            val remaining = extractBytes(path, cursor, cursor + branch.skip.length)
            if remaining != branch.skip then throw new NoSuchElementException("Key not in trie")

            val childByte = byteAtPath(path, cursor + branch.skip.length)
            val childCursor = cursor + branch.skip.length + 1
            val newChild = doDelete(branch.children(childByte), path, childCursor)
            val newChildren = branch.children.updated(childByte, newChild)

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
            val childByte = byteAtPath(path, cursor + skip)
            val child = branch.children(childByte)

            val (found, childSteps) = child match
                case Node.Empty => (false, Nil)
                case _          => doProve(child, path, cursor + skip + 1)

            val step = makeProofStep(branch, childByte, skip)
            (found, step :: childSteps)

    private def makeProofStep(branch: Node.Branch, targetIndex: Int, skip: Int): ProofStep = {
        val siblings = branch.children.zipWithIndex.filter { case (child, idx) =>
            child != Node.Empty && idx != targetIndex
        }

        if siblings.length >= 2 then
            ProofStep.Branch(skip, merkleProof8(branch.children, targetIndex))
        else if siblings.length == 1 then
            val (neighbor, neighborIdx) = siblings.head
            neighbor match
                case leaf: Node.Leaf =>
                    ProofStep.Leaf(skip, leaf.fullPath, blake2b_256(leaf.value))
                case inner: Node.Branch =>
                    ProofStep.Fork(
                      skip,
                      Neighbor(
                        index = neighborIdx,
                        prefixLen = inner.skip.length,
                        root = merkleRoot256(inner.children)
                      )
                    )
                case Node.Empty =>
                    throw new IllegalStateException("Unexpected empty neighbor")
        else throw new IllegalStateException("Branch with fewer than 2 children")
    }

    private def toProof(steps: scala.List[ProofStep]): Proof =
        steps.foldRight(PList.Nil: Proof) { (step, acc) => PList.Cons(step, acc) }
}
