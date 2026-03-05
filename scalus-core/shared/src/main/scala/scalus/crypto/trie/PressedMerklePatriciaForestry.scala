package scalus.crypto.trie
import scalus.cardano.onchain.plutus.crypto.trie.Merkling.*
import scalus.cardano.onchain.plutus.crypto.trie.PressedMerklePatriciaForestry as OnChainBinary
import scalus.uplc.builtin.Builtins.*
import scalus.uplc.builtin.ByteString

/** Internal proof step representation used only for proof generation before binary serialization.
  * Not the on-chain types — just intermediate representation.
  */
private[trie] sealed trait InternalProofStep

private[trie] object InternalProofStep {
    case class Branch(skip: Int, neighbors: ByteString) extends InternalProofStep
    case class Fork(
        skip: Int,
        nibble: Int,
        prefixLen: Int,
        halfLeft: ByteString,
        halfRight: ByteString
    ) extends InternalProofStep
    case class Leaf(skip: Int, key: ByteString, value: ByteString) extends InternalProofStep
}

/** Off-chain Merkle Patricia Forestry implementation producing binary proofs for the `mpfb`
  * on-chain verifier.
  */
case class PressedMerklePatriciaForestry(root: Node) {
    import PressedMerklePatriciaForestry.*

    /** The hash of this MPF */
    def rootHash: ByteString = root.hash

    /** The amount of elements in the tree. */
    def size: Int = root.size

    /** `true` if this trie has no elements, `false` otherwise */
    def isEmpty: Boolean = root == Node.Empty

    /** Inserts a new element into this trie */
    def insert(key: ByteString, value: ByteString): PressedMerklePatriciaForestry = {
        val path = blake2b_256(key)
        PressedMerklePatriciaForestry(doInsert(root, path, 0, key, value))
    }

    /** Deletes an element by the specified key from the trie. If this key is missing from the trie,
      * throws an exception.
      */
    def delete(key: ByteString): PressedMerklePatriciaForestry = {
        val path = blake2b_256(key)
        PressedMerklePatriciaForestry(doDelete(root, path, 0))
    }

    /** Returns the value stored by the specified key, or `None` */
    def get(key: ByteString): Option[ByteString] = {
        val path = blake2b_256(key)
        doGet(root, path, 0)
    }

    /** Creates a binary-encoded proof of inclusion, suitable for the `mpfb` on-chain verifier. */
    def proveMembership(key: ByteString): ByteString = {
        val path = blake2b_256(key)
        val (found, steps) = doProve(root, path, 0, makeProofStep)
        if !found then throw new NoSuchElementException(s"Key not in trie: ${key.toHex}")
        proofToBinary(steps)
    }

    /** Creates a binary-encoded proof of exclusion, suitable for the `mpfb` on-chain verifier. */
    def proveNonMembership(key: ByteString): ByteString = {
        if get(key).isDefined then
            throw new IllegalArgumentException(s"Key already in trie: ${key.toHex}")
        val expanded = insert(key, ByteString.empty)
        expanded.proveMembership(key)
    }

    /** Wrap the root hash as an on-chain `mpfb` value. */
    def toOnChain: OnChainBinary = OnChainBinary(rootHash)
}

object PressedMerklePatriciaForestry extends MerklePatriciaForestryBase {

    def empty: PressedMerklePatriciaForestry = PressedMerklePatriciaForestry(Node.Empty)

    def fromList(entries: Iterable[(ByteString, ByteString)]): PressedMerklePatriciaForestry =
        entries.foldLeft(empty) { case (trie, (k, v)) => trie.insert(k, v) }

    /** Branch hash: `combine3(prefix, halfLeft, halfRight)` — matches on-chain doBranch. */
    private[trie] def branchHash(
        skipStart: Int,
        skipLen: Int,
        repPath: ByteString,
        children: Vector[Node]
    ): ByteString = {
        val prefix = consByteString(skipLen, ByteString.empty)
        val (hL, hR) = merkleHalves(children.map(_.hash))
        combine3(prefix, hL, hR)
    }

    /** Reduce hashes to two halves (left and right subtree roots). */
    private def merkleHalves(hashes: Vector[ByteString]): (ByteString, ByteString) = {
        var current = hashes
        while current.length > 2 do
            current = current.grouped(2).map { pair => combine(pair(0), pair(1)) }.toVector
        (current(0), current(1))
    }

    /** Emit the most compact [[InternalProofStep]] for a branch, based on sibling count. */
    private def makeProofStep(
        branch: Node.Branch,
        targetIndex: Int,
        skip: Int
    ): InternalProofStep = {
        val siblings = branch.children.zipWithIndex.filter { case (child, idx) =>
            child != Node.Empty && idx != targetIndex
        }

        if siblings.length >= 2 then
            InternalProofStep.Branch(skip, merkleProof4(branch.children, targetIndex))
        else if siblings.length == 1 then
            val (neighbor, neighborIdx) = siblings.head
            neighbor match
                case leaf: Node.Leaf =>
                    InternalProofStep.Leaf(skip, leaf.fullPath, blake2b_256(leaf.value))
                case inner: Node.Branch =>
                    val (hL, hR) = merkleHalves(inner.children.map(_.hash))
                    InternalProofStep.Fork(skip, neighborIdx, inner.skipLen, hL, hR)
                case Node.Empty =>
                    throw new IllegalStateException("Unexpected empty neighbor")
        else throw new IllegalStateException("Branch with fewer than 2 children")
    }

    /** Serialize proof steps into the compact binary format used by `mpfb`. */
    private def proofToBinary(steps: List[InternalProofStep]): ByteString = {
        val buf = new java.io.ByteArrayOutputStream()
        for step <- steps do
            step match
                case InternalProofStep.Branch(skip, neighbors) =>
                    buf.write(0)
                    buf.write(skip)
                    buf.write(neighbors.bytes)
                case InternalProofStep.Fork(skip, nibble, prefixLen, halfLeft, halfRight) =>
                    buf.write(1)
                    buf.write(skip)
                    buf.write(nibble)
                    buf.write(prefixLen)
                    buf.write(halfLeft.bytes)
                    buf.write(halfRight.bytes)
                case InternalProofStep.Leaf(skip, key, value) =>
                    buf.write(2)
                    buf.write(skip)
                    buf.write(key.bytes)
                    buf.write(value.bytes)
        ByteString.unsafeFromArray(buf.toByteArray)
    }
}
