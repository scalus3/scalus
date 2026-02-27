package scalus.cardano.onchain.plutus.mpq

import scalus.compiler.Compile
import scalus.cardano.onchain.plutus.prelude.{require, List}
import scalus.uplc.builtin.Builtins.*
import scalus.uplc.builtin.{ByteString, FromData, ToData}

/** A radix-4 Merkle Patricia Trie. Each branch has up to 4 children (indexed by 2-bit dibits),
  * yielding ~8 proof steps for 32K elements — a middle ground between radix-2 (15 steps) and
  * radix-16 (4 steps).
  */
case class MerklePatriciaQuad(root: ByteString)

@Compile
object MerklePatriciaQuad:
    import QuaternaryMerkling.*

    private val Blake2b256DigestSize: BigInt = 32

    /** A neighbor node used in a sparse proof step. */
    case class Neighbor(
        dibit: BigInt,
        prefixLen: BigInt,
        root: ByteString
    ) derives FromData,
          ToData

    /** Proof step types.
      *   - Branch: 3-4 children occupied (dense). neighbors = 64 bytes (pair-mate ++ opposite
      *     pair).
      *   - Fork: exactly 2 children, sibling is a branch node.
      *   - Leaf: exactly 2 children, sibling is a leaf node.
      */
    enum ProofStep derives FromData, ToData:
        case Branch(skip: BigInt, neighbors: ByteString)
        case Fork(skip: BigInt, neighbor: Neighbor)
        case Leaf(skip: BigInt, key: ByteString, value: ByteString)

    type Proof = List[ProofStep]

    extension (self: MerklePatriciaQuad)
        def isEmpty: Boolean = self.root == NullHash

        def has(key: ByteString, value: ByteString, proof: Proof): Boolean =
            MerklePatriciaQuad.including(key, value, proof) == self.root

        def insert(key: ByteString, value: ByteString, proof: Proof): MerklePatriciaQuad =
            require(
              MerklePatriciaQuad.excluding(key, proof) == self.root,
              "Invalid proof or element exists"
            )
            MerklePatriciaQuad(MerklePatriciaQuad.including(key, value, proof))

        def delete(key: ByteString, value: ByteString, proof: Proof): MerklePatriciaQuad =
            require(
              MerklePatriciaQuad.including(key, value, proof) == self.root,
              "Invalid proof or element missing"
            )
            MerklePatriciaQuad(MerklePatriciaQuad.excluding(key, proof))

        def update(
            key: ByteString,
            proof: Proof,
            oldValue: ByteString,
            newValue: ByteString
        ): MerklePatriciaQuad =
            require(
              MerklePatriciaQuad.including(key, oldValue, proof) == self.root,
              "Invalid proof or old value missing"
            )
            MerklePatriciaQuad(MerklePatriciaQuad.including(key, newValue, proof))

    def empty: MerklePatriciaQuad = MerklePatriciaQuad(NullHash)

    def apply(root: ByteString): MerklePatriciaQuad =
        require(lengthOfByteString(root) == Blake2b256DigestSize, "Root must be 32 bytes")
        new MerklePatriciaQuad(root)

    /** Compute root hash assuming element IS present. */
    private def including(key: ByteString, value: ByteString, proof: Proof): ByteString =
        doIncluding(blake2b_256(key), blake2b_256(value), 0, proof)

    private def doIncluding(
        path: ByteString,
        value: ByteString,
        cursor: BigInt,
        proof: Proof
    ): ByteString = proof match
        case List.Nil =>
            combine(suffixDibit(path, cursor), value)

        case List.Cons(step, steps) =>
            step match
                case ProofStep.Branch(skip, neighbors) =>
                    val nextCursor = cursor + 1 + skip
                    val root = doIncluding(path, value, nextCursor, steps)
                    doBranch(path, cursor, nextCursor, root, neighbors)

                case ProofStep.Fork(skip, neighbor) =>
                    val nextCursor = cursor + 1 + skip
                    val root = doIncluding(path, value, nextCursor, steps)
                    val neighborHash = combine(
                      consByteString(neighbor.prefixLen, ByteString.empty),
                      neighbor.root
                    )
                    doFork(path, cursor, nextCursor, root, neighbor.dibit, neighborHash)

                case ProofStep.Leaf(skip, key, neighborValue) =>
                    val nextCursor = cursor + 1 + skip
                    val root = doIncluding(path, value, nextCursor, steps)
                    val neighborDibit = dibit(key, nextCursor - 1)
                    val neighborHash = combine(suffixDibit(key, nextCursor), neighborValue)
                    doFork(path, cursor, nextCursor, root, neighborDibit, neighborHash)

    /** Compute root hash assuming element is NOT present. */
    private def excluding(key: ByteString, proof: Proof): ByteString =
        doExcluding(blake2b_256(key), 0, proof)

    private def doExcluding(
        path: ByteString,
        cursor: BigInt,
        proof: Proof
    ): ByteString = proof match
        case List.Nil => NullHash
        case List.Cons(step, steps) =>
            step match
                case ProofStep.Branch(skip, neighbors) =>
                    val nextCursor = cursor + 1 + skip
                    val root = doExcluding(path, nextCursor, steps)
                    doBranch(path, cursor, nextCursor, root, neighbors)

                case ProofStep.Fork(skip, neighbor) =>
                    steps match
                        case List.Nil =>
                            combine(
                              consByteString(
                                skip + 1 + neighbor.prefixLen,
                                ByteString.empty
                              ),
                              neighbor.root
                            )
                        case _ =>
                            val nextCursor = cursor + 1 + skip
                            val root = doExcluding(path, nextCursor, steps)
                            val neighborHash = combine(
                              consByteString(neighbor.prefixLen, ByteString.empty),
                              neighbor.root
                            )
                            doFork(path, cursor, nextCursor, root, neighbor.dibit, neighborHash)

                case ProofStep.Leaf(skip, key, value) =>
                    steps match
                        case List.Nil => combine(suffixDibit(key, cursor), value)
                        case _ =>
                            val nextCursor = cursor + 1 + skip
                            val root = doExcluding(path, nextCursor, steps)
                            val neighborDibit = dibit(key, nextCursor - 1)
                            val neighborHash = combine(suffixDibit(key, nextCursor), value)
                            doFork(path, cursor, nextCursor, root, neighborDibit, neighborHash)

    /** Reconstruct hash at a dense Branch proof step (3-4 children occupied). */
    private def doBranch(
        path: ByteString,
        cursor: BigInt,
        nextCursor: BigInt,
        root: ByteString,
        neighbors: ByteString
    ): ByteString =
        val branch = dibit(path, nextCursor - 1)
        val prefix = consByteString(nextCursor - 1 - cursor, ByteString.empty)
        val neighbor1 = sliceByteString(0, Blake2b256DigestSize, neighbors)
        val neighbor2 = sliceByteString(32, Blake2b256DigestSize, neighbors)
        combine(prefix, merkle4(branch, root, neighbor2, neighbor1))

    /** Reconstruct hash at a sparse Fork/Leaf proof step (exactly 2 children occupied). */
    private def doFork(
        path: ByteString,
        cursor: BigInt,
        nextCursor: BigInt,
        root: ByteString,
        neighborDibit: BigInt,
        neighborHash: ByteString
    ): ByteString =
        val branch = dibit(path, nextCursor - 1)
        val prefix = consByteString(nextCursor - 1 - cursor, ByteString.empty)
        require(branch != neighborDibit)
        combine(prefix, sparseMerkle4(branch, root, neighborDibit, neighborHash))
