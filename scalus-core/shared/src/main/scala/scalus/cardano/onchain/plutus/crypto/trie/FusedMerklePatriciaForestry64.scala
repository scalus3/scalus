package scalus.cardano.onchain.plutus.crypto.trie

import scalus.compiler.Compile
import scalus.cardano.onchain.plutus.prelude.require
import scalus.uplc.builtin.Builtins.*
import scalus.uplc.builtin.ByteString

/** Fused Merkle Patricia Forestry (radix-64).
  *
  * Proofs are encoded as a single flat ByteString instead of List[ProofStep]. This eliminates
  * per-step CBOR/Data overhead.
  *
  * Binary proof format (all fields fixed-size): Branch: 0x00 | skip[1] | neighbors[192] = 194
  * bytes Fork: 0x01 | skip[1] | index[1] | prefixLen[1] | halfLeft[32] | halfRight[32] = 68 bytes
  * Leaf: 0x02 | skip[1] | key[32] | value[32] = 66 bytes
  */
case class FusedMerklePatriciaForestry64(root: ByteString)

@Compile
object FusedMerklePatriciaForestry64:
    import Merkling.{NullHash, NullHash2, NullHash4, NullHash8, combine, combine3}
    import Merkling64.*

    private val Blake2b256DigestSize: BigInt = 32
    private val NeighborSize: BigInt = 192

    type Proof = ByteString

    extension (self: FusedMerklePatriciaForestry64)
        def isEmpty: Boolean = self.root == NullHash

        def has(key: ByteString, value: ByteString, proof: Proof): Boolean =
            FusedMerklePatriciaForestry64.including(key, value, proof) == self.root

        def insert(
            key: ByteString,
            value: ByteString,
            proof: Proof
        ): FusedMerklePatriciaForestry64 =
            val path = blake2b_256(key)
            val hValue = blake2b_256(value)
            val both = FusedMerklePatriciaForestry64.doCombined(path, hValue, 0, proof, 0)
            val exclRoot = sliceByteString(0, Blake2b256DigestSize, both)
            val inclRoot = sliceByteString(Blake2b256DigestSize, Blake2b256DigestSize, both)
            require(exclRoot == self.root, "Invalid proof or element exists")
            FusedMerklePatriciaForestry64(inclRoot)

        def delete(
            key: ByteString,
            value: ByteString,
            proof: Proof
        ): FusedMerklePatriciaForestry64 =
            val path = blake2b_256(key)
            val hValue = blake2b_256(value)
            val both = FusedMerklePatriciaForestry64.doCombined(path, hValue, 0, proof, 0)
            val exclRoot = sliceByteString(0, Blake2b256DigestSize, both)
            val inclRoot = sliceByteString(Blake2b256DigestSize, Blake2b256DigestSize, both)
            require(inclRoot == self.root, "Invalid proof or element missing")
            FusedMerklePatriciaForestry64(exclRoot)

    def empty: FusedMerklePatriciaForestry64 = FusedMerklePatriciaForestry64(NullHash)

    def apply(root: ByteString): FusedMerklePatriciaForestry64 =
        require(lengthOfByteString(root) == Blake2b256DigestSize, "Root must be 32 bytes")
        new FusedMerklePatriciaForestry64(root)

    private def including(key: ByteString, value: ByteString, proof: Proof): ByteString =
        doIncluding(blake2b_256(key), blake2b_256(value), 0, proof, 0)

    private def doIncluding(
        path: ByteString,
        value: ByteString,
        cursor: BigInt,
        proof: ByteString,
        offset: BigInt
    ): ByteString =
        if offset >= lengthOfByteString(proof) then combine(suffix(path, cursor), value)
        else
            val stepType = indexByteString(proof, offset)
            val skip = indexByteString(proof, offset + 1)
            val nextCursor = cursor + 1 + skip
            if stepType == BigInt(0) then // Branch
                val neighbors = sliceByteString(offset + 2, NeighborSize, proof)
                val root = doIncluding(path, value, nextCursor, proof, offset + 2 + NeighborSize)
                doBranch(path, cursor, nextCursor, root, neighbors)
            else if stepType == BigInt(1) then // Fork
                val neighborIndex = indexByteString(proof, offset + 2)
                val root = doIncluding(path, value, nextCursor, proof, offset + 68)
                val neighborHash = blake2b_256(sliceByteString(offset + 3, 65, proof))
                doFork(path, cursor, nextCursor, root, neighborIndex, neighborHash)
            else // Leaf (stepType == 2)
                val neighborKey = sliceByteString(offset + 2, Blake2b256DigestSize, proof)
                val neighborValue = sliceByteString(offset + 34, Blake2b256DigestSize, proof)
                val root = doIncluding(path, value, nextCursor, proof, offset + 66)
                val neighborSixit = sixit(neighborKey, nextCursor - 1)
                val neighborHash = combine(suffix(neighborKey, nextCursor), neighborValue)
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

    // --- Combined single-pass: compute excluding and including simultaneously ---

    private def doCombined(
        path: ByteString,
        value: ByteString,
        cursor: BigInt,
        proof: ByteString,
        offset: BigInt
    ): ByteString =
        if offset >= lengthOfByteString(proof) then
            appendByteString(NullHash, combine(suffix(path, cursor), value))
        else
            val stepType = indexByteString(proof, offset)
            val skip = indexByteString(proof, offset + 1)
            val nextCursor = cursor + 1 + skip
            if stepType == BigInt(0) then // Branch
                val neighbors = sliceByteString(offset + 2, NeighborSize, proof)
                val both = doCombined(path, value, nextCursor, proof, offset + 2 + NeighborSize)
                doBranch2(path, cursor, nextCursor, both, neighbors)
            else if stepType == BigInt(1) then // Fork
                val neighborIndex = indexByteString(proof, offset + 2)
                val nextOffset = offset + 68
                val neighborHash = blake2b_256(sliceByteString(offset + 3, 65, proof))
                if nextOffset >= lengthOfByteString(proof) then
                    val neighborPrefixLen = indexByteString(proof, offset + 3)
                    val neighborData = sliceByteString(offset + 4, 64, proof)
                    val exclResult = combine(
                      consByteString(skip + 1 + neighborPrefixLen, ByteString.empty),
                      neighborData
                    )
                    val inclChild = combine(suffix(path, nextCursor), value)
                    val inclResult =
                        doFork(path, cursor, nextCursor, inclChild, neighborIndex, neighborHash)
                    appendByteString(exclResult, inclResult)
                else
                    val both = doCombined(path, value, nextCursor, proof, nextOffset)
                    doFork2(path, cursor, nextCursor, both, neighborIndex, neighborHash)
            else // Leaf (stepType == 2)
                val neighborKey = sliceByteString(offset + 2, Blake2b256DigestSize, proof)
                val neighborValue = sliceByteString(offset + 34, Blake2b256DigestSize, proof)
                val nextOffset = offset + 66
                if nextOffset >= lengthOfByteString(proof) then
                    val exclResult = combine(suffix(neighborKey, cursor), neighborValue)
                    val neighborSixit = sixit(neighborKey, nextCursor - 1)
                    val neighborHash = combine(suffix(neighborKey, nextCursor), neighborValue)
                    val inclChild = combine(suffix(path, nextCursor), value)
                    val inclResult =
                        doFork(path, cursor, nextCursor, inclChild, neighborSixit, neighborHash)
                    appendByteString(exclResult, inclResult)
                else
                    val neighborSixit = sixit(neighborKey, nextCursor - 1)
                    val neighborHash = combine(suffix(neighborKey, nextCursor), neighborValue)
                    val both = doCombined(path, value, nextCursor, proof, nextOffset)
                    doFork2(path, cursor, nextCursor, both, neighborSixit, neighborHash)

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
        val prefix = consByteString(nextCursor - 1 - cursor, ByteString.empty)
        val n32 = sliceByteString(0, Blake2b256DigestSize, neighbors)
        val n16 = sliceByteString(32, Blake2b256DigestSize, neighbors)
        val n8 = sliceByteString(64, Blake2b256DigestSize, neighbors)
        val n4 = sliceByteString(96, Blake2b256DigestSize, neighbors)
        val n2 = sliceByteString(128, Blake2b256DigestSize, neighbors)
        val n1 = sliceByteString(160, Blake2b256DigestSize, neighbors)
        if branch <= 31 then
            appendByteString(
              combine3(prefix, merkle32(branch, exclChild, n16, n8, n4, n2, n1), n32),
              combine3(prefix, merkle32(branch, inclChild, n16, n8, n4, n2, n1), n32)
            )
        else
            appendByteString(
              combine3(prefix, n32, merkle32(branch - 32, exclChild, n16, n8, n4, n2, n1)),
              combine3(prefix, n32, merkle32(branch - 32, inclChild, n16, n8, n4, n2, n1))
            )

    private def doFork2(
        path: ByteString,
        cursor: BigInt,
        nextCursor: BigInt,
        both: ByteString,
        neighborIndex: BigInt,
        neighborHash: ByteString
    ): ByteString =
        val exclChild = sliceByteString(0, Blake2b256DigestSize, both)
        val inclChild = sliceByteString(Blake2b256DigestSize, Blake2b256DigestSize, both)
        val branch = sixit(path, nextCursor - 1)
        val prefix = consByteString(nextCursor - 1 - cursor, ByteString.empty)
        require(branch != neighborIndex)
        if branch < 32 then
            if neighborIndex < 32 then
                appendByteString(
                  combine3(
                    prefix,
                    sparseMerkle32(branch, exclChild, neighborIndex, neighborHash),
                    NullHash32
                  ),
                  combine3(
                    prefix,
                    sparseMerkle32(branch, inclChild, neighborIndex, neighborHash),
                    NullHash32
                  )
                )
            else
                appendByteString(
                  combine3(
                    prefix,
                    merkle32(
                      branch,
                      exclChild,
                      NullHash16,
                      NullHash8,
                      NullHash4,
                      NullHash2,
                      NullHash
                    ),
                    merkle32(
                      neighborIndex - 32,
                      neighborHash,
                      NullHash16,
                      NullHash8,
                      NullHash4,
                      NullHash2,
                      NullHash
                    )
                  ),
                  combine3(
                    prefix,
                    merkle32(
                      branch,
                      inclChild,
                      NullHash16,
                      NullHash8,
                      NullHash4,
                      NullHash2,
                      NullHash
                    ),
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
                )
        else if neighborIndex >= 32 then
            appendByteString(
              combine3(
                prefix,
                NullHash32,
                sparseMerkle32(branch - 32, exclChild, neighborIndex - 32, neighborHash)
              ),
              combine3(
                prefix,
                NullHash32,
                sparseMerkle32(branch - 32, inclChild, neighborIndex - 32, neighborHash)
              )
            )
        else
            appendByteString(
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
                merkle32(
                  branch - 32,
                  exclChild,
                  NullHash16,
                  NullHash8,
                  NullHash4,
                  NullHash2,
                  NullHash
                )
              ),
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
                merkle32(
                  branch - 32,
                  inclChild,
                  NullHash16,
                  NullHash8,
                  NullHash4,
                  NullHash2,
                  NullHash
                )
              )
            )
