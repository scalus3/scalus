package scalus.cardano.onchain.plutus.crypto.trie

import scalus.compiler.Compile
import scalus.cardano.onchain.plutus.prelude.{require, List}
import scalus.uplc.builtin.Builtins.*
import scalus.uplc.builtin.{ByteString, FromData, ToData}

/** Merkle Patricia Forestry (radix-64) with full sixit prefix encoding.
  *
  * Unfused variant: uses `combine(prefix, merkleRoot)` (two blake2b calls per branch) and
  * Data-encoded proofs (`List[ProofStep]`). Same trie structure as [[FusedMerklePatriciaForestry64]]
  * but with different hashing and proof format.
  */
case class MerklePatriciaForestry64(root: ByteString)

@Compile
object MerklePatriciaForestry64:
    import Merkling.{NullHash, combine}
    import Merkling64.*

    private val Blake2b256DigestSize: BigInt = 32

    /** A neighbor node used in a proof */
    case class Neighbor(
        sixit: BigInt,
        prefix: ByteString,
        root: ByteString
    ) derives FromData,
          ToData

    /** Types of proof steps that can occur in a proof. Each step contains a skip value indicating
      * length of common prefix at that level.
      */
    enum ProofStep derives FromData, ToData:
        case Branch(skip: BigInt, neighbors: ByteString)
        case Fork(skip: BigInt, neighbor: Neighbor)
        case Leaf(skip: BigInt, key: ByteString, value: ByteString)

    type Proof = List[ProofStep]

    extension (self: MerklePatriciaForestry64)
        def isEmpty: Boolean = self.root == NullHash

        def has(key: ByteString, value: ByteString, proof: Proof): Boolean =
            MerklePatriciaForestry64.including(key, value, proof) == self.root

        def verifyMembership(key: ByteString, value: ByteString, proof: Proof): Unit =
            require(
              MerklePatriciaForestry64.including(key, value, proof) == self.root,
              "Membership verification failed"
            )

        def verifyNonMembership(key: ByteString, proof: Proof): Unit =
            val path = blake2b_256(key)
            val both = MerklePatriciaForestry64.doCombined(path, NullHash, 0, proof)
            val exclRoot = sliceByteString(0, Blake2b256DigestSize, both)
            require(exclRoot == self.root, "Non-membership verification failed")

        def insert(key: ByteString, value: ByteString, proof: Proof): MerklePatriciaForestry64 =
            val path = blake2b_256(key)
            val hValue = blake2b_256(value)
            val both = MerklePatriciaForestry64.doCombined(path, hValue, 0, proof)
            val exclRoot = sliceByteString(0, Blake2b256DigestSize, both)
            val inclRoot = sliceByteString(Blake2b256DigestSize, Blake2b256DigestSize, both)
            require(exclRoot == self.root, "Invalid proof or element exists")
            MerklePatriciaForestry64(inclRoot)

        def delete(key: ByteString, value: ByteString, proof: Proof): MerklePatriciaForestry64 =
            val path = blake2b_256(key)
            val hValue = blake2b_256(value)
            val both = MerklePatriciaForestry64.doCombined(path, hValue, 0, proof)
            val exclRoot = sliceByteString(0, Blake2b256DigestSize, both)
            val inclRoot = sliceByteString(Blake2b256DigestSize, Blake2b256DigestSize, both)
            require(inclRoot == self.root, "Invalid proof or element missing")
            MerklePatriciaForestry64(exclRoot)

    def empty: MerklePatriciaForestry64 = MerklePatriciaForestry64(NullHash)

    def apply(root: ByteString): MerklePatriciaForestry64 =
        require(lengthOfByteString(root) == Blake2b256DigestSize, "Root must be 32 bytes")
        new MerklePatriciaForestry64(root)

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
                    doFork(path, cursor, nextCursor, root, neighbor)

                case ProofStep.Leaf(skip, key, neighborValue) =>
                    val nextCursor = cursor + 1 + skip
                    val root = doIncluding(path, value, nextCursor, steps)
                    val neighbor = Neighbor(
                      sixit = sixit(key, nextCursor - 1),
                      prefix = suffix(key, nextCursor),
                      root = neighborValue
                    )
                    doFork(path, cursor, nextCursor, root, neighbor)

    private def doBranch(
        path: ByteString,
        cursor: BigInt,
        nextCursor: BigInt,
        root: ByteString,
        neighbors: ByteString
    ): ByteString =
        val branch = sixit(path, nextCursor - 1)
        val prefix = sixits(path, cursor, nextCursor - 1)
        val n32 = sliceByteString(0, Blake2b256DigestSize, neighbors)
        val n16 = sliceByteString(32, Blake2b256DigestSize, neighbors)
        val n8 = sliceByteString(64, Blake2b256DigestSize, neighbors)
        val n4 = sliceByteString(96, Blake2b256DigestSize, neighbors)
        val n2 = sliceByteString(128, Blake2b256DigestSize, neighbors)
        val n1 = sliceByteString(160, Blake2b256DigestSize, neighbors)
        combine(
          prefix,
          merkle64(branch, root, n32, n16, n8, n4, n2, n1)
        )

    private def doFork(
        path: ByteString,
        cursor: BigInt,
        nextCursor: BigInt,
        root: ByteString,
        neighbor: Neighbor
    ): ByteString =
        val branch = sixit(path, nextCursor - 1)
        val prefix = sixits(path, cursor, nextCursor - 1)
        require(branch != neighbor.sixit)
        combine(
          prefix,
          sparseMerkle64(
            branch,
            root,
            neighbor.sixit,
            combine(neighbor.prefix, neighbor.root)
          )
        )

    // --- Combined single-pass: compute excluding and including simultaneously ---
    // Returns a 64-byte ByteString: excl[32] || incl[32]

    private def doCombined(
        path: ByteString,
        value: ByteString,
        cursor: BigInt,
        proof: Proof
    ): ByteString = proof match
        case List.Nil =>
            appendByteString(NullHash, combine(suffix(path, cursor), value))

        case List.Cons(step, steps) =>
            step match
                case ProofStep.Branch(skip, neighbors) =>
                    val nextCursor = cursor + 1 + skip
                    val both = doCombined(path, value, nextCursor, steps)
                    doBranch2(path, cursor, nextCursor, both, neighbors)

                case ProofStep.Fork(skip, neighbor) =>
                    steps match
                        case List.Nil =>
                            val nextCursor = cursor + 1 + skip
                            val exclResult =
                                if skip == BigInt(0) then
                                    combine(
                                      consByteString(neighbor.sixit, neighbor.prefix),
                                      neighbor.root
                                    )
                                else
                                    combine(
                                      appendByteString(
                                        sixits(path, cursor, cursor + skip),
                                        consByteString(neighbor.sixit, neighbor.prefix)
                                      ),
                                      neighbor.root
                                    )
                            val inclChild = combine(suffix(path, nextCursor), value)
                            val inclResult =
                                doFork(path, cursor, nextCursor, inclChild, neighbor)
                            appendByteString(exclResult, inclResult)
                        case _ =>
                            val nextCursor = cursor + 1 + skip
                            val both = doCombined(path, value, nextCursor, steps)
                            doFork2(path, cursor, nextCursor, both, neighbor)

                case ProofStep.Leaf(skip, key, neighborValue) =>
                    steps match
                        case List.Nil =>
                            val exclResult = combine(suffix(key, cursor), neighborValue)
                            val nextCursor = cursor + 1 + skip
                            val inclChild = combine(suffix(path, nextCursor), value)
                            val neighbor = Neighbor(
                              sixit = sixit(key, nextCursor - 1),
                              prefix = suffix(key, nextCursor),
                              root = neighborValue
                            )
                            val inclResult =
                                doFork(path, cursor, nextCursor, inclChild, neighbor)
                            appendByteString(exclResult, inclResult)
                        case _ =>
                            val nextCursor = cursor + 1 + skip
                            val neighbor = Neighbor(
                              sixit = sixit(key, nextCursor - 1),
                              prefix = suffix(key, nextCursor),
                              root = neighborValue
                            )
                            val both = doCombined(path, value, nextCursor, steps)
                            doFork2(path, cursor, nextCursor, both, neighbor)

    private def doBranch2(
        path: ByteString,
        cursor: BigInt,
        nextCursor: BigInt,
        both: ByteString,
        neighbors: ByteString
    ): ByteString =
        val exclChild = sliceByteString(0, Blake2b256DigestSize, both)
        val inclChild = sliceByteString(Blake2b256DigestSize, Blake2b256DigestSize, both)
        val branch = sixit(path, nextCursor - 1)
        val prefix = sixits(path, cursor, nextCursor - 1)
        val n32 = sliceByteString(0, Blake2b256DigestSize, neighbors)
        val n16 = sliceByteString(32, Blake2b256DigestSize, neighbors)
        val n8 = sliceByteString(64, Blake2b256DigestSize, neighbors)
        val n4 = sliceByteString(96, Blake2b256DigestSize, neighbors)
        val n2 = sliceByteString(128, Blake2b256DigestSize, neighbors)
        val n1 = sliceByteString(160, Blake2b256DigestSize, neighbors)
        appendByteString(
          combine(prefix, merkle64(branch, exclChild, n32, n16, n8, n4, n2, n1)),
          combine(prefix, merkle64(branch, inclChild, n32, n16, n8, n4, n2, n1))
        )

    private def doFork2(
        path: ByteString,
        cursor: BigInt,
        nextCursor: BigInt,
        both: ByteString,
        neighbor: Neighbor
    ): ByteString =
        val exclChild = sliceByteString(0, Blake2b256DigestSize, both)
        val inclChild = sliceByteString(Blake2b256DigestSize, Blake2b256DigestSize, both)
        val branch = sixit(path, nextCursor - 1)
        val prefix = sixits(path, cursor, nextCursor - 1)
        require(branch != neighbor.sixit)
        val neighborHash = combine(neighbor.prefix, neighbor.root)
        appendByteString(
          combine(prefix, sparseMerkle64(branch, exclChild, neighbor.sixit, neighborHash)),
          combine(prefix, sparseMerkle64(branch, inclChild, neighbor.sixit, neighborHash))
        )
