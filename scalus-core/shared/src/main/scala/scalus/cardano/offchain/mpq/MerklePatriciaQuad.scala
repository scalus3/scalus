package scalus.cardano.offchain.mpq

import scalus.cardano.onchain.plutus.mpq.MerklePatriciaQuad as OnChainQuad
import scalus.cardano.onchain.plutus.mpq.MerklePatriciaQuad.{Neighbor, Proof, ProofStep}
import scalus.cardano.onchain.plutus.mpq.QuaternaryMerkling.*
import scalus.cardano.onchain.plutus.prelude.List as PList
import scalus.uplc.builtin.Builtins.*
import scalus.uplc.builtin.ByteString

private[mpq] sealed trait QNode {
    def hash: ByteString
    def size: Int
}

private[mpq] object QNode {

    case object Empty extends QNode {
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
    ) extends QNode {
        val size: Int = 1
    }

    /** A branch node with up to 4 children. Stores the common prefix in [[skip]]. */
    case class Branch(
        hash: ByteString,
        skip: Vector[Int],
        children: Vector[QNode],
        size: Int
    ) extends QNode
}

/** Off-chain radix-4 Merkle Patricia Trie.
  *
  * Supports lookup, insertion, deletion, and generation of succinct proofs compatible with the
  * on-chain [[OnChainQuad]] verifier.
  */
case class MerklePatriciaQuad(root: QNode) {
    import MerklePatriciaQuad.*

    def rootHash: ByteString = root.hash

    def size: Int = root.size

    def isEmpty: Boolean = root == QNode.Empty

    def insert(key: ByteString, value: ByteString): MerklePatriciaQuad = {
        val path = blake2b_256(key)
        MerklePatriciaQuad(doInsert(root, path, 0, key, value))
    }

    def delete(key: ByteString): MerklePatriciaQuad = {
        val path = blake2b_256(key)
        MerklePatriciaQuad(doDelete(root, path, 0))
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

    def toOnChain: OnChainQuad = OnChainQuad(rootHash)
}

object MerklePatriciaQuad {

    /** Number of dibits in a blake2b-256 path (32 bytes = 128 dibits). */
    private val PathDibits = 128

    def empty: MerklePatriciaQuad = MerklePatriciaQuad(QNode.Empty)

    def fromList(entries: Iterable[(ByteString, ByteString)]): MerklePatriciaQuad =
        entries.foldLeft(empty) { case (trie, (k, v)) => trie.insert(k, v) }

    /** Extract a dibit (2-bit value, 0-3) at the given index from a ByteString. MSB-first. */
    private def dibitAt(path: ByteString, index: Int): Int = {
        val byte = path.bytes(index / 4) & 0xff
        val shift = 6 - 2 * (index % 4)
        (byte >> shift) & 3
    }

    private def extractDibits(path: ByteString, start: Int, end: Int): Vector[Int] =
        (start until end).map(i => dibitAt(path, i)).toVector

    /** Leaf hash: `combine(suffixDibit(path, cursor), blake2b_256(value))` */
    private def leafHash(fullPath: ByteString, cursor: Int, value: ByteString): ByteString =
        combine(suffixDibit(fullPath, cursor), blake2b_256(value))

    /** Branch hash: `combine(skipLenByte, merkleRoot4(children))` */
    private def branchHash(skip: Vector[Int], children: Vector[QNode]): ByteString =
        combine(consByteString(skip.length, ByteString.empty), merkleRoot4(children))

    /** Compute the merkle root of 4 child hashes: `combine(combine(h0, h1), combine(h2, h3))`
      */
    private def merkleRoot4(children: Vector[QNode]): ByteString = {
        val h = children.map(_.hash)
        combine(combine(h(0), h(1)), combine(h(2), h(3)))
    }

    /** Extract the 2 sibling hashes needed for a [[ProofStep.Branch]] step. Returns 64 bytes:
      * pair-mate (32) ++ opposite pair combined (32).
      */
    private def merkleProof2(children: Vector[QNode], branch: Int): ByteString = {
        val h = children.map(_.hash)
        val neighbor1 = if branch % 2 == 0 then h(branch + 1) else h(branch - 1)
        val level1 = Vector(combine(h(0), h(1)), combine(h(2), h(3)))
        val neighbor2 = if branch < 2 then level1(1) else level1(0)
        neighbor1.concat(neighbor2)
    }

    private def mkLeaf(
        skip: Vector[Int],
        fullPath: ByteString,
        cursor: Int,
        key: ByteString,
        value: ByteString
    ): QNode.Leaf =
        QNode.Leaf(leafHash(fullPath, cursor, value), skip, fullPath, key, value)

    private def mkBranch(skip: Vector[Int], children: Vector[QNode]): QNode.Branch =
        QNode.Branch(branchHash(skip, children), skip, children, children.map(_.size).sum)

    private val emptyChildren: Vector[QNode] = Vector.fill(4)(QNode.Empty)

    private def commonPrefixLen(a: Vector[Int], b: Vector[Int]): Int = {
        val len = math.min(a.length, b.length)
        var i = 0
        while i < len && a(i) == b(i) do i += 1
        i
    }

    private def doInsert(
        node: QNode,
        path: ByteString,
        cursor: Int,
        key: ByteString,
        value: ByteString
    ): QNode = node match
        case QNode.Empty =>
            mkLeaf(extractDibits(path, cursor, PathDibits), path, cursor, key, value)

        case leaf: QNode.Leaf =>
            val remaining = extractDibits(path, cursor, PathDibits)
            val commonPrefix = commonPrefixLen(remaining, leaf.skip)

            if commonPrefix == remaining.length && commonPrefix == leaf.skip.length then
                throw new IllegalArgumentException(s"Key already in trie: ${key.toHex}")

            val newDibit = remaining(commonPrefix)
            val oldDibit = leaf.skip(commonPrefix)
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
                .updated(newDibit, newLeaf)
                .updated(oldDibit, oldLeaf)
            mkBranch(remaining.take(commonPrefix), children)

        case branch: QNode.Branch =>
            val remaining = extractDibits(path, cursor, cursor + branch.skip.length + 1)
            val prefixPart = remaining.dropRight(1)
            val cp = commonPrefixLen(prefixPart, branch.skip)

            if cp < branch.skip.length then
                val splitCursor = cursor + cp + 1
                val newDibit = prefixPart(cp)
                val oldDibit = branch.skip(cp)

                val newLeaf = mkLeaf(
                  extractDibits(path, splitCursor, PathDibits),
                  path,
                  splitCursor,
                  key,
                  value
                )
                val oldBranch = mkBranch(branch.skip.drop(cp + 1), branch.children)

                val children = emptyChildren
                    .updated(newDibit, newLeaf)
                    .updated(oldDibit, oldBranch)
                mkBranch(branch.skip.take(cp), children)
            else
                val childDibit = remaining.last
                val childCursor = cursor + branch.skip.length + 1
                val newChild = doInsert(branch.children(childDibit), path, childCursor, key, value)
                mkBranch(branch.skip, branch.children.updated(childDibit, newChild))

    private def doGet(node: QNode, path: ByteString, cursor: Int): Option[ByteString] =
        node match
            case QNode.Empty => None
            case leaf: QNode.Leaf =>
                if leaf.fullPath == path then Some(leaf.value) else None
            case branch: QNode.Branch =>
                val remaining = extractDibits(path, cursor, cursor + branch.skip.length)
                if remaining != branch.skip then None
                else
                    val childDibit = dibitAt(path, cursor + branch.skip.length)
                    doGet(branch.children(childDibit), path, cursor + branch.skip.length + 1)

    private def doDelete(node: QNode, path: ByteString, cursor: Int): QNode = node match
        case QNode.Empty =>
            throw new NoSuchElementException("Key not in trie")

        case leaf: QNode.Leaf =>
            if leaf.fullPath == path then QNode.Empty
            else throw new NoSuchElementException("Key not in trie")

        case branch: QNode.Branch =>
            val remaining = extractDibits(path, cursor, cursor + branch.skip.length)
            if remaining != branch.skip then throw new NoSuchElementException("Key not in trie")

            val childDibit = dibitAt(path, cursor + branch.skip.length)
            val childCursor = cursor + branch.skip.length + 1
            val newChild = doDelete(branch.children(childDibit), path, childCursor)
            val newChildren = branch.children.updated(childDibit, newChild)

            val nonEmpty = newChildren.zipWithIndex.filter(_._1 != QNode.Empty)
            nonEmpty.size match
                case 0 => QNode.Empty
                case 1 =>
                    val (child, childIdx) = nonEmpty.head
                    child match
                        case leaf: QNode.Leaf =>
                            mkLeaf(
                              branch.skip :+ childIdx :++ leaf.skip,
                              leaf.fullPath,
                              cursor,
                              leaf.key,
                              leaf.value
                            )
                        case inner: QNode.Branch =>
                            mkBranch(branch.skip :+ childIdx :++ inner.skip, inner.children)
                        case QNode.Empty => QNode.Empty
                case _ =>
                    mkBranch(branch.skip, newChildren)

    /** Walk the trie root-to-leaf, collecting one [[ProofStep]] at each branch along the path. */
    private def doProve(
        node: QNode,
        path: ByteString,
        cursor: Int
    ): (Boolean, scala.List[ProofStep]) = node match
        case QNode.Empty =>
            (false, Nil)

        case leaf: QNode.Leaf =>
            (leaf.fullPath == path, Nil)

        case branch: QNode.Branch =>
            val skip = branch.skip.length
            val childDibit = dibitAt(path, cursor + skip)
            val child = branch.children(childDibit)

            val (found, childSteps) = child match
                case QNode.Empty => (false, Nil)
                case _           => doProve(child, path, cursor + skip + 1)

            val step = makeProofStep(branch, childDibit, skip)
            (found, step :: childSteps)

    /** Emit the most compact [[ProofStep]] for a branch, based on sibling count. */
    private def makeProofStep(branch: QNode.Branch, targetIndex: Int, skip: Int): ProofStep = {
        val siblings = branch.children.zipWithIndex.filter { case (child, idx) =>
            child != QNode.Empty && idx != targetIndex
        }

        if siblings.length >= 2 then
            ProofStep.Branch(skip, merkleProof2(branch.children, targetIndex))
        else if siblings.length == 1 then
            val (neighbor, neighborIdx) = siblings.head
            neighbor match
                case leaf: QNode.Leaf =>
                    ProofStep.Leaf(skip, leaf.fullPath, blake2b_256(leaf.value))
                case inner: QNode.Branch =>
                    ProofStep.Fork(
                      skip,
                      Neighbor(
                        dibit = neighborIdx,
                        prefixLen = inner.skip.length,
                        root = merkleRoot4(inner.children)
                      )
                    )
                case QNode.Empty =>
                    throw new IllegalStateException("Unexpected empty neighbor")
        else throw new IllegalStateException("Branch with fewer than 2 children")
    }

    private def toProof(steps: scala.List[ProofStep]): Proof =
        steps.foldRight(PList.Nil: Proof) { (step, acc) => PList.Cons(step, acc) }
}
