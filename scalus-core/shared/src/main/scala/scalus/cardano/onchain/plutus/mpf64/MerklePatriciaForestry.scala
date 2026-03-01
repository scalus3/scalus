package scalus.cardano.onchain.plutus.mpf64

import scalus.compiler.Compile
import scalus.cardano.onchain.plutus.prelude.{require, List}
import scalus.uplc.builtin.Builtins.*
import scalus.uplc.builtin.{ByteString, FromData, ToData}

/** Radix-64 Merkle Patricia Forestry.
  *
  * Uses 6-bit path units (0-63) extracted via arithmetic, yielding 42 levels max (vs 64 for
  * radix-16). Branch proofs are 192 bytes (6 x 32).
  *
  * Neighbor.root in Fork proofs is 32 bytes (merkle root of children).
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
        halfLeft: ByteString,
        halfRight: ByteString
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
                    val neighborHash = combine3(
                      consByteString(neighbor.prefixLen, ByteString.empty),
                      neighbor.halfLeft,
                      neighbor.halfRight
                    )
                    doFork(path, cursor, nextCursor, root, neighbor.index, neighborHash)

                case ProofStep.Leaf(skip, key, neighborValue) =>
                    val nextCursor = cursor + 1 + skip
                    val root = doIncluding(path, value, nextCursor, steps)
                    val neighborSixit = sixit(key, nextCursor - 1)
                    val neighborHash = combine(suffix(key, nextCursor), neighborValue)
                    doFork(path, cursor, nextCursor, root, neighborSixit, neighborHash)

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
                            combine3(
                              consByteString(
                                skip + 1 + neighbor.prefixLen,
                                ByteString.empty
                              ),
                              neighbor.halfLeft,
                              neighbor.halfRight
                            )
                        case _ =>
                            val nextCursor = cursor + 1 + skip
                            val root = doExcluding(path, nextCursor, steps)
                            val neighborHash = combine3(
                              consByteString(neighbor.prefixLen, ByteString.empty),
                              neighbor.halfLeft,
                              neighbor.halfRight
                            )
                            doFork(path, cursor, nextCursor, root, neighbor.index, neighborHash)

                case ProofStep.Leaf(skip, key, value) =>
                    steps match
                        case List.Nil => combine(suffix(key, cursor), value)
                        case _ =>
                            val nextCursor = cursor + 1 + skip
                            val root = doExcluding(path, nextCursor, steps)
                            val neighborSixit = sixit(key, nextCursor - 1)
                            val neighborHash = combine(suffix(key, nextCursor), value)
                            doFork(path, cursor, nextCursor, root, neighborSixit, neighborHash)

    private def doBranch(
        path: ByteString,
        cursor: BigInt,
        nextCursor: BigInt,
        root: ByteString,
        neighbors: ByteString
    ): ByteString =
        val branch = sixit(path, nextCursor - 1)
        val prefix = consByteString(nextCursor - 1 - cursor, ByteString.empty)
        val n32 = sliceByteString(0, Blake2b256DigestSize, neighbors)
        val n16 = sliceByteString(32, Blake2b256DigestSize, neighbors)
        val n8 = sliceByteString(64, Blake2b256DigestSize, neighbors)
        val n4 = sliceByteString(96, Blake2b256DigestSize, neighbors)
        val n2 = sliceByteString(128, Blake2b256DigestSize, neighbors)
        val n1 = sliceByteString(160, Blake2b256DigestSize, neighbors)
        if branch <= 31 then combine3(prefix, merkle32(branch, root, n16, n8, n4, n2, n1), n32)
        else combine3(prefix, n32, merkle32(branch - 32, root, n16, n8, n4, n2, n1))

    private def doFork(
        path: ByteString,
        cursor: BigInt,
        nextCursor: BigInt,
        root: ByteString,
        neighborIndex: BigInt,
        neighborHash: ByteString
    ): ByteString =
        val branch = sixit(path, nextCursor - 1)
        val prefix = consByteString(nextCursor - 1 - cursor, ByteString.empty)

        require(branch != neighborIndex)

        if branch < 32 then
            if neighborIndex < 32 then
                combine3(
                  prefix,
                  sparseMerkle32(branch, root, neighborIndex, neighborHash),
                  NullHash32
                )
            else
                combine3(
                  prefix,
                  merkle32(branch, root, NullHash16, NullHash8, NullHash4, NullHash2, NullHash),
                  merkle32(
                    neighborIndex - 32,
                    neighborHash,
                    NullHash16,
                    NullHash8,
                    NullHash4,
                    NullHash2,
                    NullHash
                  )
                )
        else if neighborIndex >= 32 then
            combine3(
              prefix,
              NullHash32,
              sparseMerkle32(branch - 32, root, neighborIndex - 32, neighborHash)
            )
        else
            combine3(
              prefix,
              merkle32(
                neighborIndex,
                neighborHash,
                NullHash16,
                NullHash8,
                NullHash4,
                NullHash2,
                NullHash
              ),
              merkle32(branch - 32, root, NullHash16, NullHash8, NullHash4, NullHash2, NullHash)
            )
