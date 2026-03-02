package scalus.cardano.onchain.plutus.mpf64b

import scalus.compiler.Compile
import scalus.cardano.onchain.plutus.prelude.require
import scalus.uplc.builtin.Builtins.*
import scalus.uplc.builtin.ByteString

/** Binary-proof Merkle Patricia Forestry (radix-64).
  *
  * Identical to `mpf64.MerklePatriciaForestry` in verification logic, but proofs are encoded as a
  * single flat ByteString instead of List[ProofStep]. This eliminates per-step CBOR/Data overhead.
  * Insert/delete use a combined single-pass proof traversal that computes both excluding and
  * including roots simultaneously, saving one full proof parse.
  *
  * Binary proof format (all fields fixed-size): Branch: 0x00 | skip[1] | neighbors[192] = 194 bytes
  * Fork: 0x01 | skip[1] | index[1] | prefixLen[1] | halfLeft[32] | halfRight[32] = 68 bytes Leaf:
  * 0x02 | skip[1] | key[32] | value[32] = 66 bytes
  */
case class MerklePatriciaForestry(root: ByteString)

@Compile
object MerklePatriciaForestry:
    import scalus.cardano.onchain.plutus.mpf64.Merkling.*

    private val Blake2b256DigestSize: BigInt = 32
    private val NeighborSize: BigInt = 192

    type Proof = ByteString

    extension (self: MerklePatriciaForestry)
        def isEmpty: Boolean = self.root == NullHash

        def has(key: ByteString, value: ByteString, proof: Proof): Boolean =
            MerklePatriciaForestry.including(key, value, proof) == self.root

        /** Insert key/value using combined single-pass: parse proof once, compute both excluding
          * (verify absent) and including (new root) simultaneously.
          */
        def insert(key: ByteString, value: ByteString, proof: Proof): MerklePatriciaForestry =
            val path = blake2b_256(key)
            val hValue = blake2b_256(value)
            val both = MerklePatriciaForestry.doCombined(path, hValue, 0, proof, 0)
            val exclRoot = sliceByteString(0, Blake2b256DigestSize, both)
            val inclRoot = sliceByteString(Blake2b256DigestSize, Blake2b256DigestSize, both)
            require(exclRoot == self.root, "Invalid proof or element exists")
            MerklePatriciaForestry(inclRoot)

        /** Delete key/value using combined single-pass: parse proof once, compute both including
          * (verify present) and excluding (new root) simultaneously.
          */
        def delete(key: ByteString, value: ByteString, proof: Proof): MerklePatriciaForestry =
            val path = blake2b_256(key)
            val hValue = blake2b_256(value)
            val both = MerklePatriciaForestry.doCombined(path, hValue, 0, proof, 0)
            val exclRoot = sliceByteString(0, Blake2b256DigestSize, both)
            val inclRoot = sliceByteString(Blake2b256DigestSize, Blake2b256DigestSize, both)
            require(inclRoot == self.root, "Invalid proof or element missing")
            MerklePatriciaForestry(exclRoot)

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
                val neighborPrefixLen = indexByteString(proof, offset + 3)
                val neighborHalfLeft = sliceByteString(offset + 4, Blake2b256DigestSize, proof)
                val neighborHalfRight = sliceByteString(offset + 36, Blake2b256DigestSize, proof)
                val root = doIncluding(path, value, nextCursor, proof, offset + 68)
                val neighborHash = combine3(
                  consByteString(neighborPrefixLen, ByteString.empty),
                  neighborHalfLeft,
                  neighborHalfRight
                )
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
    // Returns a 64-byte ByteString: excl[32] || incl[32]

    /** Combined proof traversal: parse each proof step once, compute both excluding and including
      * hashes simultaneously. Saves one full proof traversal (proof parsing, recursion, path
      * computations).
      */
    private def doCombined(
        path: ByteString,
        value: ByteString,
        cursor: BigInt,
        proof: ByteString,
        offset: BigInt
    ): ByteString =
        if offset >= lengthOfByteString(proof) then
            // Base case: excl = NullHash, incl = combine(suffix, value)
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
                val neighborPrefixLen = indexByteString(proof, offset + 3)
                val neighborHalfLeft = sliceByteString(offset + 4, Blake2b256DigestSize, proof)
                val neighborHalfRight = sliceByteString(offset + 36, Blake2b256DigestSize, proof)
                val nextOffset = offset + 68
                val neighborHash = combine3(
                  consByteString(neighborPrefixLen, ByteString.empty),
                  neighborHalfLeft,
                  neighborHalfRight
                )
                if nextOffset >= lengthOfByteString(proof) then
                    // Last step: excluding has special prefix adjustment
                    val exclResult = combine3(
                      consByteString(skip + 1 + neighborPrefixLen, ByteString.empty),
                      neighborHalfLeft,
                      neighborHalfRight
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
                    // Last step: excluding returns neighbor moved up
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

    /** Branch step for combined pass. Computes both excl and incl branch hashes, sharing branch,
      * prefix, and neighbor slice computations.
      */
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

    /** Fork step for combined pass. Computes both excl and incl fork hashes, sharing branch,
      * prefix, and neighbor computations.
      */
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
