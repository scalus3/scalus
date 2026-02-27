package scalus.cardano.offchain.mpt

import scalus.cardano.onchain.plutus.mpt.MerklePatriciaTrie as OnChainTrie
import scalus.cardano.onchain.plutus.mpt.MerklePatriciaTrie.{Proof, ProofStep}
import scalus.cardano.onchain.plutus.mpt.BinaryMerkling.*
import scalus.cardano.onchain.plutus.prelude.List as PList
import scalus.uplc.builtin.Builtins.*
import scalus.uplc.builtin.ByteString

private[mpt] sealed trait BNode {
    def hash: ByteString
    def size: Int
}

private[mpt] object BNode {

    case object Empty extends BNode {
        val hash: ByteString = NullHash
        val size: Int = 0
    }

    case class Leaf(
        hash: ByteString,
        skip: Vector[Int],
        fullPath: ByteString,
        key: ByteString,
        value: ByteString
    ) extends BNode {
        val size: Int = 1
    }

    case class Branch(
        hash: ByteString,
        skip: Vector[Int],
        left: BNode,
        right: BNode,
        size: Int
    ) extends BNode
}

/** Off-chain binary Merkle Patricia Trie.
  *
  * Supports lookup, insertion, deletion, and generation of succinct proofs compatible with the
  * on-chain [[OnChainTrie]] verifier.
  */
case class MerklePatriciaTrie(root: BNode) {
    import MerklePatriciaTrie.*

    def rootHash: ByteString = root.hash

    def size: Int = root.size

    def isEmpty: Boolean = root == BNode.Empty

    def insert(key: ByteString, value: ByteString): MerklePatriciaTrie = {
        val path = blake2b_256(key)
        MerklePatriciaTrie(doInsert(root, path, 0, key, value))
    }

    def delete(key: ByteString): MerklePatriciaTrie = {
        val path = blake2b_256(key)
        MerklePatriciaTrie(doDelete(root, path, 0))
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

    def toOnChain: OnChainTrie = OnChainTrie(rootHash)
}

object MerklePatriciaTrie {

    private val PathBits = 256

    def empty: MerklePatriciaTrie = MerklePatriciaTrie(BNode.Empty)

    def fromList(entries: Iterable[(ByteString, ByteString)]): MerklePatriciaTrie =
        entries.foldLeft(empty) { case (trie, (k, v)) => trie.insert(k, v) }

    private def bitAt(path: ByteString, index: Int): Int = {
        val byte = path.bytes(index / 8) & 0xff
        (byte >> (7 - index % 8)) & 1
    }

    private def extractBits(path: ByteString, start: Int, end: Int): Vector[Int] =
        (start until end).map(i => bitAt(path, i)).toVector

    private def leafHash(fullPath: ByteString, cursor: Int, value: ByteString): ByteString =
        combine(suffixBit(fullPath, cursor), blake2b_256(value))

    private def branchHash(skip: Vector[Int], left: BNode, right: BNode): ByteString =
        combine(consByteString(skip.length, ByteString.empty), combine(left.hash, right.hash))

    private def mkLeaf(
        skip: Vector[Int],
        fullPath: ByteString,
        cursor: Int,
        key: ByteString,
        value: ByteString
    ): BNode.Leaf =
        BNode.Leaf(leafHash(fullPath, cursor, value), skip, fullPath, key, value)

    private def mkBranch(skip: Vector[Int], left: BNode, right: BNode): BNode.Branch =
        BNode.Branch(branchHash(skip, left, right), skip, left, right, left.size + right.size)

    private def commonPrefixLen(a: Vector[Int], b: Vector[Int]): Int = {
        val len = math.min(a.length, b.length)
        var i = 0
        while i < len && a(i) == b(i) do i += 1
        i
    }

    private def doInsert(
        node: BNode,
        path: ByteString,
        cursor: Int,
        key: ByteString,
        value: ByteString
    ): BNode = node match
        case BNode.Empty =>
            mkLeaf(extractBits(path, cursor, PathBits), path, cursor, key, value)

        case leaf: BNode.Leaf =>
            val remaining = extractBits(path, cursor, PathBits)
            val commonPrefix = commonPrefixLen(remaining, leaf.skip)

            if commonPrefix == remaining.length && commonPrefix == leaf.skip.length then
                throw new IllegalArgumentException(s"Key already in trie: ${key.toHex}")

            val newBit = remaining(commonPrefix)
            val oldBit = leaf.skip(commonPrefix)
            val splitCursor = cursor + commonPrefix + 1

            val newLeaf = mkLeaf(remaining.drop(commonPrefix + 1), path, splitCursor, key, value)
            val oldLeaf = mkLeaf(
              leaf.skip.drop(commonPrefix + 1),
              leaf.fullPath,
              splitCursor,
              leaf.key,
              leaf.value
            )

            if newBit == 0 then mkBranch(remaining.take(commonPrefix), newLeaf, oldLeaf)
            else mkBranch(remaining.take(commonPrefix), oldLeaf, newLeaf)

        case branch: BNode.Branch =>
            val remaining = extractBits(path, cursor, cursor + branch.skip.length + 1)
            val prefixPart = remaining.dropRight(1)
            val cp = commonPrefixLen(prefixPart, branch.skip)

            if cp < branch.skip.length then
                // Path diverges within the branch's prefix — split the branch
                val splitCursor = cursor + cp + 1
                val newBit = prefixPart(cp)

                val newLeaf = mkLeaf(
                  extractBits(path, splitCursor, PathBits),
                  path,
                  splitCursor,
                  key,
                  value
                )
                val oldBranch = mkBranch(branch.skip.drop(cp + 1), branch.left, branch.right)

                if newBit == 0 then mkBranch(branch.skip.take(cp), newLeaf, oldBranch)
                else mkBranch(branch.skip.take(cp), oldBranch, newLeaf)
            else
                // Prefix matches — descend into the appropriate child
                val childBit = remaining.last
                val childCursor = cursor + branch.skip.length + 1
                if childBit == 0 then
                    val newLeft = doInsert(branch.left, path, childCursor, key, value)
                    mkBranch(branch.skip, newLeft, branch.right)
                else
                    val newRight = doInsert(branch.right, path, childCursor, key, value)
                    mkBranch(branch.skip, branch.left, newRight)

    private def doGet(node: BNode, path: ByteString, cursor: Int): Option[ByteString] =
        node match
            case BNode.Empty => None
            case leaf: BNode.Leaf =>
                if leaf.fullPath == path then Some(leaf.value) else None
            case branch: BNode.Branch =>
                val remaining = extractBits(path, cursor, cursor + branch.skip.length)
                if remaining != branch.skip then None
                else
                    val childBit = bitAt(path, cursor + branch.skip.length)
                    val childCursor = cursor + branch.skip.length + 1
                    if childBit == 0 then doGet(branch.left, path, childCursor)
                    else doGet(branch.right, path, childCursor)

    private def doDelete(node: BNode, path: ByteString, cursor: Int): BNode = node match
        case BNode.Empty =>
            throw new NoSuchElementException("Key not in trie")

        case leaf: BNode.Leaf =>
            if leaf.fullPath == path then BNode.Empty
            else throw new NoSuchElementException("Key not in trie")

        case branch: BNode.Branch =>
            val remaining = extractBits(path, cursor, cursor + branch.skip.length)
            if remaining != branch.skip then throw new NoSuchElementException("Key not in trie")

            val childBit = bitAt(path, cursor + branch.skip.length)
            val childCursor = cursor + branch.skip.length + 1

            val (newLeft, newRight) =
                if childBit == 0 then (doDelete(branch.left, path, childCursor), branch.right)
                else (branch.left, doDelete(branch.right, path, childCursor))

            (newLeft, newRight) match
                case (BNode.Empty, BNode.Empty) => BNode.Empty
                case (BNode.Empty, only)        => collapseChild(branch.skip, 1, only, cursor)
                case (only, BNode.Empty)        => collapseChild(branch.skip, 0, only, cursor)
                case _                          => mkBranch(branch.skip, newLeft, newRight)

    private def collapseChild(
        parentSkip: Vector[Int],
        childBit: Int,
        child: BNode,
        cursor: Int
    ): BNode =
        child match
            case leaf: BNode.Leaf =>
                mkLeaf(
                  parentSkip :+ childBit :++ leaf.skip,
                  leaf.fullPath,
                  cursor,
                  leaf.key,
                  leaf.value
                )
            case inner: BNode.Branch =>
                mkBranch(parentSkip :+ childBit :++ inner.skip, inner.left, inner.right)
            case BNode.Empty => BNode.Empty

    private def doProve(
        node: BNode,
        path: ByteString,
        cursor: Int
    ): (Boolean, scala.List[ProofStep]) = node match
        case BNode.Empty =>
            (false, Nil)

        case leaf: BNode.Leaf =>
            (leaf.fullPath == path, Nil)

        case branch: BNode.Branch =>
            val skip = branch.skip.length
            val childBit = bitAt(path, cursor + skip)

            val (target, sibling) =
                if childBit == 0 then (branch.left, branch.right)
                else (branch.right, branch.left)

            val (found, childSteps) = target match
                case BNode.Empty => (false, Nil)
                case _           => doProve(target, path, cursor + skip + 1)

            val step = makeProofStep(sibling, skip)
            (found, step :: childSteps)

    private def makeProofStep(sibling: BNode, skip: Int): ProofStep =
        sibling match
            case leaf: BNode.Leaf =>
                ProofStep.Leaf(skip, leaf.fullPath, blake2b_256(leaf.value))
            case inner: BNode.Branch =>
                ProofStep.Fork(
                  skip,
                  neighborSkipLen = inner.skip.length,
                  neighborRoot = combine(inner.left.hash, inner.right.hash)
                )
            case BNode.Empty =>
                throw new IllegalStateException("Sibling cannot be empty in binary trie")

    private def toProof(steps: scala.List[ProofStep]): Proof =
        steps.foldRight(PList.Nil: Proof) { (step, acc) => PList.Cons(step, acc) }
}
