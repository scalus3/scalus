package scalus.cardano.onchain.plutus.mpfb

import scalus.compiler.Compile
import scalus.cardano.onchain.plutus.prelude.require
import scalus.uplc.builtin.Builtins.*
import scalus.uplc.builtin.ByteString

/** Binary-proof Merkle Patricia Forestry (radix-16).
  *
  * Identical to `mpf.MerklePatriciaForestry` in verification logic, but proofs are encoded as a
  * single flat ByteString instead of List[ProofStep]. This eliminates per-step CBOR/Data overhead.
  *
  * Binary proof format (all fields fixed-size): Branch: 0x00 | skip[1] | neighbors[128] = 130 bytes
  * Fork: 0x01 | skip[1] | nibble[1] | prefixLen[1] | halfLeft[32] | halfRight[32] = 68 bytes Leaf:
  * 0x02 | skip[1] | key[32] | value[32] = 66 bytes
  */
case class MerklePatriciaForestry(root: ByteString)

@Compile
object MerklePatriciaForestry:
    import scalus.cardano.onchain.plutus.mpf.Merkling.*

    private val Blake2b256DigestSize: BigInt = 32
    private val NeighborSize: BigInt = 128

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
                val neighborNibble = indexByteString(proof, offset + 2)
                val neighborPrefixLen = indexByteString(proof, offset + 3)
                val neighborHalfLeft = sliceByteString(offset + 4, Blake2b256DigestSize, proof)
                val neighborHalfRight = sliceByteString(offset + 36, Blake2b256DigestSize, proof)
                val root = doIncluding(path, value, nextCursor, proof, offset + 68)
                val neighborHash = combine3(
                  consByteString(neighborPrefixLen, ByteString.empty),
                  neighborHalfLeft,
                  neighborHalfRight
                )
                doFork(path, cursor, nextCursor, root, neighborNibble, neighborHash)
            else // Leaf (stepType == 2)
                val neighborKey = sliceByteString(offset + 2, Blake2b256DigestSize, proof)
                val neighborValue = sliceByteString(offset + 34, Blake2b256DigestSize, proof)
                val root = doIncluding(path, value, nextCursor, proof, offset + 66)
                val neighborNibble = nibble(neighborKey, nextCursor - 1)
                val neighborHash = combine(suffix(neighborKey, nextCursor), neighborValue)
                doFork(path, cursor, nextCursor, root, neighborNibble, neighborHash)

    private def excluding(key: ByteString, proof: Proof): ByteString =
        doExcluding(blake2b_256(key), 0, proof, 0)

    private def doExcluding(
        path: ByteString,
        cursor: BigInt,
        proof: ByteString,
        offset: BigInt
    ): ByteString =
        if offset >= lengthOfByteString(proof) then NullHash
        else
            val stepType = indexByteString(proof, offset)
            val skip = indexByteString(proof, offset + 1)
            val nextCursor = cursor + 1 + skip
            if stepType == BigInt(0) then // Branch
                val neighbors = sliceByteString(offset + 2, NeighborSize, proof)
                val root = doExcluding(path, nextCursor, proof, offset + 2 + NeighborSize)
                doBranch(path, cursor, nextCursor, root, neighbors)
            else if stepType == BigInt(1) then // Fork
                val neighborNibble = indexByteString(proof, offset + 2)
                val neighborPrefixLen = indexByteString(proof, offset + 3)
                val neighborHalfLeft = sliceByteString(offset + 4, Blake2b256DigestSize, proof)
                val neighborHalfRight = sliceByteString(offset + 36, Blake2b256DigestSize, proof)
                val nextOffset = offset + 68
                if nextOffset >= lengthOfByteString(proof) then
                    combine3(
                      consByteString(skip + 1 + neighborPrefixLen, ByteString.empty),
                      neighborHalfLeft,
                      neighborHalfRight
                    )
                else
                    val root = doExcluding(path, nextCursor, proof, nextOffset)
                    val neighborHash = combine3(
                      consByteString(neighborPrefixLen, ByteString.empty),
                      neighborHalfLeft,
                      neighborHalfRight
                    )
                    doFork(path, cursor, nextCursor, root, neighborNibble, neighborHash)
            else // Leaf (stepType == 2)
                val neighborKey = sliceByteString(offset + 2, Blake2b256DigestSize, proof)
                val neighborValue = sliceByteString(offset + 34, Blake2b256DigestSize, proof)
                val nextOffset = offset + 66
                if nextOffset >= lengthOfByteString(proof) then
                    combine(suffix(neighborKey, cursor), neighborValue)
                else
                    val root = doExcluding(path, nextCursor, proof, nextOffset)
                    val neighborNibble = nibble(neighborKey, nextCursor - 1)
                    val neighborHash = combine(suffix(neighborKey, nextCursor), neighborValue)
                    doFork(path, cursor, nextCursor, root, neighborNibble, neighborHash)

    private def doBranch(
        path: ByteString,
        cursor: BigInt,
        nextCursor: BigInt,
        root: ByteString,
        neighbors: ByteString
    ): ByteString =
        val branch = nibble(path, nextCursor - 1)
        val prefix = consByteString(nextCursor - 1 - cursor, ByteString.empty)
        val n8 = sliceByteString(0, Blake2b256DigestSize, neighbors)
        val n4 = sliceByteString(32, Blake2b256DigestSize, neighbors)
        val n2 = sliceByteString(64, Blake2b256DigestSize, neighbors)
        val n1 = sliceByteString(96, Blake2b256DigestSize, neighbors)
        if branch <= 7 then combine3(prefix, merkle8(branch, root, n4, n2, n1), n8)
        else combine3(prefix, n8, merkle8(branch - 8, root, n4, n2, n1))

    private def doFork(
        path: ByteString,
        cursor: BigInt,
        nextCursor: BigInt,
        root: ByteString,
        neighborNibble: BigInt,
        neighborHash: ByteString
    ): ByteString =
        val branch = nibble(path, nextCursor - 1)
        val prefix = consByteString(nextCursor - 1 - cursor, ByteString.empty)
        require(branch != neighborNibble)
        if branch < 8 then
            if neighborNibble < 8 then
                combine3(
                  prefix,
                  sparseMerkle8(branch, root, neighborNibble, neighborHash),
                  NullHash8
                )
            else
                combine3(
                  prefix,
                  merkle8(branch, root, NullHash4, NullHash2, NullHash),
                  merkle8(neighborNibble - 8, neighborHash, NullHash4, NullHash2, NullHash)
                )
        else if neighborNibble >= 8 then
            combine3(
              prefix,
              NullHash8,
              sparseMerkle8(branch - 8, root, neighborNibble - 8, neighborHash)
            )
        else
            combine3(
              prefix,
              merkle8(neighborNibble, neighborHash, NullHash4, NullHash2, NullHash),
              merkle8(branch - 8, root, NullHash4, NullHash2, NullHash)
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
                val neighborNibble = indexByteString(proof, offset + 2)
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
                        doFork(path, cursor, nextCursor, inclChild, neighborNibble, neighborHash)
                    appendByteString(exclResult, inclResult)
                else
                    val both = doCombined(path, value, nextCursor, proof, nextOffset)
                    doFork2(path, cursor, nextCursor, both, neighborNibble, neighborHash)
            else // Leaf (stepType == 2)
                val neighborKey = sliceByteString(offset + 2, Blake2b256DigestSize, proof)
                val neighborValue = sliceByteString(offset + 34, Blake2b256DigestSize, proof)
                val nextOffset = offset + 66
                if nextOffset >= lengthOfByteString(proof) then
                    // Last step: excluding returns neighbor moved up
                    val exclResult = combine(suffix(neighborKey, cursor), neighborValue)
                    val neighborNibble = nibble(neighborKey, nextCursor - 1)
                    val neighborHash = combine(suffix(neighborKey, nextCursor), neighborValue)
                    val inclChild = combine(suffix(path, nextCursor), value)
                    val inclResult =
                        doFork(path, cursor, nextCursor, inclChild, neighborNibble, neighborHash)
                    appendByteString(exclResult, inclResult)
                else
                    val neighborNibble = nibble(neighborKey, nextCursor - 1)
                    val neighborHash = combine(suffix(neighborKey, nextCursor), neighborValue)
                    val both = doCombined(path, value, nextCursor, proof, nextOffset)
                    doFork2(path, cursor, nextCursor, both, neighborNibble, neighborHash)

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
        val branch = nibble(path, nextCursor - 1)
        val prefix = consByteString(nextCursor - 1 - cursor, ByteString.empty)
        val n8 = sliceByteString(0, Blake2b256DigestSize, neighbors)
        val n4 = sliceByteString(32, Blake2b256DigestSize, neighbors)
        val n2 = sliceByteString(64, Blake2b256DigestSize, neighbors)
        val n1 = sliceByteString(96, Blake2b256DigestSize, neighbors)
        if branch <= 7 then
            appendByteString(
              combine3(prefix, merkle8(branch, exclChild, n4, n2, n1), n8),
              combine3(prefix, merkle8(branch, inclChild, n4, n2, n1), n8)
            )
        else
            appendByteString(
              combine3(prefix, n8, merkle8(branch - 8, exclChild, n4, n2, n1)),
              combine3(prefix, n8, merkle8(branch - 8, inclChild, n4, n2, n1))
            )

    /** Fork step for combined pass. Computes both excl and incl fork hashes, sharing branch,
      * prefix, and neighbor computations.
      */
    private def doFork2(
        path: ByteString,
        cursor: BigInt,
        nextCursor: BigInt,
        both: ByteString,
        neighborNibble: BigInt,
        neighborHash: ByteString
    ): ByteString =
        val exclChild = sliceByteString(0, Blake2b256DigestSize, both)
        val inclChild = sliceByteString(Blake2b256DigestSize, Blake2b256DigestSize, both)
        val branch = nibble(path, nextCursor - 1)
        val prefix = consByteString(nextCursor - 1 - cursor, ByteString.empty)
        require(branch != neighborNibble)
        if branch < 8 then
            if neighborNibble < 8 then
                appendByteString(
                  combine3(
                    prefix,
                    sparseMerkle8(branch, exclChild, neighborNibble, neighborHash),
                    NullHash8
                  ),
                  combine3(
                    prefix,
                    sparseMerkle8(branch, inclChild, neighborNibble, neighborHash),
                    NullHash8
                  )
                )
            else
                appendByteString(
                  combine3(
                    prefix,
                    merkle8(branch, exclChild, NullHash4, NullHash2, NullHash),
                    merkle8(neighborNibble - 8, neighborHash, NullHash4, NullHash2, NullHash)
                  ),
                  combine3(
                    prefix,
                    merkle8(branch, inclChild, NullHash4, NullHash2, NullHash),
                    merkle8(neighborNibble - 8, neighborHash, NullHash4, NullHash2, NullHash)
                  )
                )
        else if neighborNibble >= 8 then
            appendByteString(
              combine3(
                prefix,
                NullHash8,
                sparseMerkle8(branch - 8, exclChild, neighborNibble - 8, neighborHash)
              ),
              combine3(
                prefix,
                NullHash8,
                sparseMerkle8(branch - 8, inclChild, neighborNibble - 8, neighborHash)
              )
            )
        else
            appendByteString(
              combine3(
                prefix,
                merkle8(neighborNibble, neighborHash, NullHash4, NullHash2, NullHash),
                merkle8(branch - 8, exclChild, NullHash4, NullHash2, NullHash)
              ),
              combine3(
                prefix,
                merkle8(neighborNibble, neighborHash, NullHash4, NullHash2, NullHash),
                merkle8(branch - 8, inclChild, NullHash4, NullHash2, NullHash)
              )
            )
