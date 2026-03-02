package scalus.cardano.offchain.mpfo

import scalus.cardano.offchain.mpf.{MerklePatriciaTrieBase, Node}
import scalus.cardano.onchain.plutus.mpf.Merkling.*
import scalus.cardano.onchain.plutus.mpfo.MerklePatriciaForestry as OnChainForestry
import scalus.cardano.onchain.plutus.mpfo.MerklePatriciaForestry.{Neighbor, Proof, ProofStep}
import scalus.cardano.onchain.plutus.prelude.List as PList
import scalus.uplc.builtin.Builtins.*
import scalus.uplc.builtin.ByteString

/** Off-chain original Merkle Patricia Forestry implementation (full nibble prefix encoding).
  *
  * Allows lookup, insertion, deletion, and generation of succinct and on-chain MPF compatible
  * inclusion/exclusion proofs.
  */
case class MerklePatriciaForestry(root: Node) {
    import MerklePatriciaForestry.*

    /** The hash of this MPF */
    def rootHash: ByteString = root.hash

    /** The amount of elements in the tree. */
    def size: Int = root.size

    /** `true` if this trie has no elements, `false` otherwise */
    def isEmpty: Boolean = root == Node.Empty

    /** Inserts a new element into this trie */
    def insert(key: ByteString, value: ByteString): MerklePatriciaForestry = {
        val path = blake2b_256(key)
        MerklePatriciaForestry(doInsert(root, path, 0, key, value))
    }

    /** Deletes an element by the specified key from the trie. If this key is missing from the trie,
      * throws an exception.
      */
    def delete(key: ByteString): MerklePatriciaForestry = {
        val path = blake2b_256(key)
        MerklePatriciaForestry(doDelete(root, path, 0))
    }

    /** Returns the value stored by the specified key, or `None` */
    def get(key: ByteString): Option[ByteString] = {
        val path = blake2b_256(key)
        doGet(root, path, 0)
    }

    /** Creates a succinct, on-chain compatible proof of inclusion of an element by this [[key]].
      *
      * Proofs are compact because branch nodes with 2+ siblings use a sparse merkle tree encoding
      * (4 hashes / 128 bytes) rather than storing all 15 sibling hashes. Branches with a single
      * sibling are encoded as a [[ProofStep.Fork]] or [[ProofStep.Leaf]], requiring even less
      * space.
      *
      * If there's no element by this key, throws an exception.
      */
    def proveExists(key: ByteString): Proof = {
        val path = blake2b_256(key)
        val (found, steps) = doProve(root, path, 0, makeProofStep)
        if !found then throw new NoSuchElementException(s"Key not in trie: ${key.toHex}")
        toProof(steps)
    }

    def proveMissing(key: ByteString): Proof = {
        if get(key).isDefined then
            throw new IllegalArgumentException(s"Key already in trie: ${key.toHex}")
        // Insert with a dummy value, then prove in the expanded trie.
        // The proof's `excluding` mode will reconstruct the original root.
        val expanded = insert(key, ByteString.empty)
        expanded.proveExists(key)
    }

    /** Wrap the root hash as an on-chain [[OnChainForestry]] value. */
    def toOnChain: OnChainForestry = OnChainForestry(rootHash)
}

object MerklePatriciaForestry extends MerklePatriciaTrieBase {

    def empty: MerklePatriciaForestry = MerklePatriciaForestry(Node.Empty)

    def fromList(entries: Iterable[(ByteString, ByteString)]): MerklePatriciaForestry =
        entries.foldLeft(empty) { case (trie, (k, v)) => trie.insert(k, v) }

    /** Encode nibbles from a path as a ByteString (one byte per nibble, 0x00..0x0F). Matches
      * on-chain `Merkling.nibbles` output format.
      */
    private def nibblesAsBytesFromPath(
        path: ByteString,
        start: Int,
        len: Int
    ): ByteString =
        ByteString.unsafeFromArray(Array.tabulate(len)(i => nibbleAt(path, start + i).toByte))

    /** Branch hash: `combine(nibblesAsBytes(skip), merkleRoot16(children))` */
    private[offchain] def branchHash(
        skipStart: Int,
        skipLen: Int,
        repPath: ByteString,
        children: Vector[Node]
    ): ByteString =
        combine(nibblesAsBytesFromPath(repPath, skipStart, skipLen), merkleRoot16(children))

    /** Compute the merkle root of 16 child hashes using the same binary tree structure as on-chain
      * `Merkling.merkle16`.
      */
    private def merkleRoot16(children: Vector[Node]): ByteString = {
        var hashes = children.map(_.hash)
        while hashes.length > 1 do
            hashes = hashes.grouped(2).map { pair => combine(pair(0), pair(1)) }.toVector
        hashes.head
    }

    /** Emit the most compact [[ProofStep]] for a branch, based on sibling count. */
    private def makeProofStep(branch: Node.Branch, targetIndex: Int, skip: Int): ProofStep = {
        val siblings = branch.children.zipWithIndex.filter { case (child, idx) =>
            child != Node.Empty && idx != targetIndex
        }

        if siblings.length >= 2 then
            ProofStep.Branch(skip, merkleProof4(branch.children, targetIndex))
        else if siblings.length == 1 then
            val (neighbor, neighborIdx) = siblings.head
            neighbor match
                case leaf: Node.Leaf =>
                    ProofStep.Leaf(skip, leaf.fullPath, blake2b_256(leaf.value))
                case inner: Node.Branch =>
                    ProofStep.Fork(
                      skip,
                      Neighbor(
                        nibble = neighborIdx,
                        prefix = nibblesAsBytesFromPath(
                          inner.repPath,
                          inner.skipStart,
                          inner.skipLen
                        ),
                        root = merkleRoot16(inner.children)
                      )
                    )
                case Node.Empty =>
                    throw new IllegalStateException("Unexpected empty neighbor")
        else throw new IllegalStateException("Branch with fewer than 2 children")
    }

    private def toProof(steps: List[ProofStep]): Proof =
        steps.foldRight(PList.Nil: Proof) { (step, acc) => PList.Cons(step, acc) }
}
