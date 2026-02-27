package scalus.cardano.onchain.plutus.mpf256

import scalus.compiler.Compile
import scalus.cardano.onchain.plutus.prelude.{require, List}
import scalus.uplc.builtin.Builtins.*
import scalus.uplc.builtin.{ByteString, FromData, ToData}

/** Radix-256 Merkle Patricia Forestry.
  *
  * Uses 8-bit bytes (0-255) instead of 4-bit nibbles (0-15), yielding 32 levels max (vs 64 for
  * radix-16). Branch proofs are 256 bytes (8 x 32) but there are half as many steps, so total proof
  * size stays ~log2(N) x 32 bytes. Total CPU drops ~10% because fewer steps dominate the slightly
  * larger per-step cost.
  *
  * Uses generic recursive merkling (merkle/singleMerkle/sparseMerkle) instead of specialized
  * merkle2/4/8/16 functions.
  */
case class MerklePatriciaForestry(root: ByteString)

@Compile
object MerklePatriciaForestry:
    import Merkling.*

    private val Blake2b256DigestSize: BigInt = 32

    /** A neighbor node used in a proof */
    case class Neighbor(
        index: BigInt,
        prefixLen: BigInt,
        root: ByteString
    ) derives FromData,
          ToData

    /** Types of proof steps. Each step contains a skip value indicating length of common prefix at
      * that level.
      */
    enum ProofStep derives FromData, ToData:
        case Branch(skip: BigInt, neighbors: ByteString)
        case Fork(skip: BigInt, neighbor: Neighbor)
        case Leaf(skip: BigInt, key: ByteString, value: ByteString)

    /** A proof is a list of steps processed left to right. */
    type Proof = List[ProofStep]

    extension (self: MerklePatriciaForestry)
        def isEmpty: Boolean = self.root == NullHash

        def has(key: ByteString, value: ByteString, proof: Proof): Boolean =
            MerklePatriciaForestry.including(key, value, proof) == self.root

        def insert(key: ByteString, value: ByteString, proof: Proof): MerklePatriciaForestry =
            require(
              MerklePatriciaForestry.excluding(key, proof) == self.root,
              "Invalid proof or element exists"
            )
            MerklePatriciaForestry(MerklePatriciaForestry.including(key, value, proof))

        def delete(key: ByteString, value: ByteString, proof: Proof): MerklePatriciaForestry =
            require(
              MerklePatriciaForestry.including(key, value, proof) == self.root,
              "Invalid proof or element missing"
            )
            MerklePatriciaForestry(MerklePatriciaForestry.excluding(key, proof))

        def update(
            key: ByteString,
            proof: Proof,
            oldValue: ByteString,
            newValue: ByteString
        ): MerklePatriciaForestry =
            require(
              MerklePatriciaForestry.including(key, oldValue, proof) == self.root,
              "Invalid proof or old value missing"
            )
            MerklePatriciaForestry(MerklePatriciaForestry.including(key, newValue, proof))

    def empty: MerklePatriciaForestry = MerklePatriciaForestry(NullHash)

    def apply(root: ByteString): MerklePatriciaForestry =
        require(lengthOfByteString(root) == Blake2b256DigestSize, "Root must be 32 bytes")
        new MerklePatriciaForestry(root)

    private def including(key: ByteString, value: ByteString, proof: Proof): ByteString =
        doIncluding(blake2b_256(key), blake2b_256(value), 0, proof)

    private def doIncluding(
        path: ByteString,
        value: ByteString,
        cursor: BigInt,
        proof: Proof
    ): ByteString = proof match
        case List.Nil =>
            combine(suffix(path, cursor), value)

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
                    doFork(path, cursor, nextCursor, root, neighbor.index, neighborHash)

                case ProofStep.Leaf(skip, key, neighborValue) =>
                    val nextCursor = cursor + 1 + skip
                    val root = doIncluding(path, value, nextCursor, steps)
                    val neighborByte = byteAt(key, nextCursor - 1)
                    val neighborHash = combine(suffix(key, nextCursor), neighborValue)
                    doFork(path, cursor, nextCursor, root, neighborByte, neighborHash)

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
                            doFork(path, cursor, nextCursor, root, neighbor.index, neighborHash)

                case ProofStep.Leaf(skip, key, value) =>
                    steps match
                        case List.Nil => combine(suffix(key, cursor), value)
                        case _ =>
                            val nextCursor = cursor + 1 + skip
                            val root = doExcluding(path, nextCursor, steps)
                            val neighborByte = byteAt(key, nextCursor - 1)
                            val neighborHash = combine(suffix(key, nextCursor), value)
                            doFork(path, cursor, nextCursor, root, neighborByte, neighborHash)

    private def doBranch(
        path: ByteString,
        cursor: BigInt,
        nextCursor: BigInt,
        root: ByteString,
        neighbors: ByteString
    ): ByteString =
        val branch = byteAt(path, nextCursor - 1)
        val prefix = consByteString(nextCursor - 1 - cursor, ByteString.empty)

        combine(
          prefix,
          merkle(128, branch, root, neighbors)
        )

    private def doFork(
        path: ByteString,
        cursor: BigInt,
        nextCursor: BigInt,
        root: ByteString,
        neighborIndex: BigInt,
        neighborHash: ByteString
    ): ByteString =
        val branch = byteAt(path, nextCursor - 1)
        val prefix = consByteString(nextCursor - 1 - cursor, ByteString.empty)

        require(branch != neighborIndex)

        combine(
          prefix,
          sparseMerkle(128, 7, branch, root, neighborIndex, neighborHash)
        )
