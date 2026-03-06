package scalus.crypto.trie

import scalus.cardano.onchain.plutus.crypto.trie.Merkling.combine
import scalus.cardano.onchain.plutus.crypto.trie.MerklePatriciaForestry64 as OnChain64
import scalus.cardano.onchain.plutus.crypto.trie.MerklePatriciaForestry64.{
    Neighbor,
    Proof,
    ProofStep
}
import scalus.cardano.onchain.plutus.prelude.List as PList
import scalus.uplc.builtin.Builtins.*
import scalus.uplc.builtin.ByteString

/** Off-chain radix-64 Merkle Patricia Forestry with Data-encoded proofs (unfused).
  *
  * Uses 6-bit path units (42 levels for 32-byte keys). Branch nodes have 64 children. Branch hash
  * uses two blake2b calls: `combine(sixits_prefix, merkle64Root(children))`.
  *
  * Proofs are `List[ProofStep]` encoded as Plutus Data, compatible with the on-chain
  * [[scalus.cardano.onchain.plutus.crypto.trie.MerklePatriciaForestry64]] verifier.
  */
case class MerklePatriciaForestry64(root: Node64) {
    import MerklePatriciaForestry64.*

    def rootHash: ByteString = root.hash
    def size: Int = root.size
    def isEmpty: Boolean = root == Node64.Empty

    def insert(key: ByteString, value: ByteString): MerklePatriciaForestry64 = {
        val path = blake2b_256(key)
        MerklePatriciaForestry64(doInsert(root, path, 0, key, value))
    }

    def delete(key: ByteString): MerklePatriciaForestry64 = {
        val path = blake2b_256(key)
        MerklePatriciaForestry64(doDelete(root, path, 0))
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

    def toOnChain: OnChain64 = OnChain64(rootHash)
}

object MerklePatriciaForestry64 {

    private val PathUnits = 42

    def empty: MerklePatriciaForestry64 = MerklePatriciaForestry64(Node64.Empty)

    def fromList(entries: Iterable[(ByteString, ByteString)]): MerklePatriciaForestry64 =
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

    /** Encode sixits from a path as a ByteString (one byte per 6-bit value, 0x00..0x3F). Matches
      * on-chain `Merkling64.sixits` output format.
      */
    private def sixitsAsBytesFromPath(
        path: ByteString,
        start: Int,
        len: Int
    ): ByteString =
        ByteString.unsafeFromArray(Array.tabulate(len)(i => sixitAt(path, start + i).toByte))

    /** Unfused branch hash: `combine(sixits_prefix, merkleRoot64(children))`. Two blake2b calls. */
    private def branchHash(
        skipStart: Int,
        skipLen: Int,
        repPath: ByteString,
        children: Vector[Node64]
    ): ByteString =
        combine(sixitsAsBytesFromPath(repPath, skipStart, skipLen), merkleRoot64(children))

    /** Compute the merkle root of 64 child hashes using the same binary tree structure as on-chain
      * `Merkling64.merkle64`.
      */
    private def merkleRoot64(children: Vector[Node64]): ByteString = {
        var hashes = children.map(_.hash)
        while hashes.length > 1 do
            hashes = hashes.grouped(2).map { pair => combine(pair(0), pair(1)) }.toVector
        hashes.head
    }

    /** Extract the 6 sibling hashes needed for a Branch proof step. These are the `neighbor32,
      * neighbor16, neighbor8, neighbor4, neighbor2, neighbor1` values expected by on-chain
      * `merkle64`.
      */
    private def merkleProof6(children: Vector[Node64], branch: Int): ByteString = {
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

        // sibling8: sibling octet in group of 16 (flip bit 3)
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

        // sibling16: sibling 16-group in group of 32 (flip bit 4)
        val h = ((branch >> 4) ^ 1) << 4
        val s16 = merkleRootRange(children, h, 16)

        // sibling32: sibling 32-group (flip bit 5)
        val g = ((branch >> 5) ^ 1) << 5
        val s32 = merkleRootRange(children, g, 32)

        // 192 bytes: n32 ++ n16 ++ n8 ++ n4 ++ n2 ++ n1
        s32.concat(s16).concat(s8).concat(s4).concat(s2).concat(s1)
    }

    /** Compute the merkle root of a contiguous range of children. */
    private def merkleRootRange(children: Vector[Node64], start: Int, count: Int): ByteString = {
        var hashes = (start until start + count).map(i => children(i).hash).toVector
        while hashes.length > 1 do
            hashes = hashes.grouped(2).map { pair => combine(pair(0), pair(1)) }.toVector
        hashes.head
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
          () => branchHash(skipStart, skipLen, repPath, children)
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
    ): (Boolean, List[ProofStep]) = node match
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
    ): ProofStep = {
        val siblings = branch.children.zipWithIndex.filter { case (child, idx) =>
            child != Node64.Empty && idx != targetIndex
        }

        if siblings.length >= 2 then
            ProofStep.Branch(skip, merkleProof6(branch.children, targetIndex))
        else if siblings.length == 1 then
            val (neighbor, neighborIdx) = siblings.head
            neighbor match
                case leaf: Node64.Leaf =>
                    ProofStep.Leaf(skip, leaf.fullPath, blake2b_256(leaf.value))
                case inner: Node64.Branch =>
                    ProofStep.Fork(
                      skip,
                      Neighbor(
                        sixit = neighborIdx,
                        prefix = sixitsAsBytesFromPath(
                          inner.repPath,
                          inner.skipStart,
                          inner.skipLen
                        ),
                        root = merkleRoot64(inner.children)
                      )
                    )
                case Node64.Empty =>
                    throw new IllegalStateException("Unexpected empty neighbor")
        else throw new IllegalStateException("Branch with fewer than 2 children")
    }

    private def toProof(steps: List[ProofStep]): Proof =
        steps.foldRight(PList.Nil: Proof) { (step, acc) => PList.Cons(step, acc) }
}
