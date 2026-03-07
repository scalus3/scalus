package scalus.cardano.onchain.plutus.crypto.trie

import scalus.compiler.Compile
import scalus.cardano.onchain.plutus.prelude.require
import scalus.uplc.builtin.Builtins.*
import scalus.uplc.builtin.ByteString
import scalus.uplc.builtin.ByteString.hex

/** Fused Merkle Patricia Forestry (radix-2).
  *
  * Each branch step consumes 1 bit of the 256-bit key path. The Forestry binary tree at each branch
  * is trivial (depth 1, just 2 children), so there are no unrolled `merkle4/8/16/32` functions.
  *
  * Branch hash = `combine3(skipLen_byte, child0_hash, child1_hash)` — a single blake2b call per
  * step instead of two in the unfused variant.
  *
  * With fusing, the total blake2b calls per proof = 256 (constant, independent of radix).
  *
  * Binary proof format:
  *   - Branch: `0x00 | skip[1] | neighbor[32]` = 34 bytes
  *   - Fork:   `0x01 | skip[1] | prefixLen[1] | child0[32] | child1[32]` = 67 bytes
  *   - Leaf:   `0x02 | skip[1] | key[32] | value[32]` = 66 bytes
  */
case class FusedMerklePatriciaForestry2(root: ByteString)

@Compile
object FusedMerklePatriciaForestry2:
    import Merkling.{NullHash, combine, combine3}

    private val Blake2b256DigestSize: BigInt = 32

    /** Lookup table for 2^(7-i), i=0..7: [128, 64, 32, 16, 8, 4, 2, 1] */
    private val BitDivisors: ByteString = hex"8040201008040201"

    type Proof = ByteString

    /** Extract bit at position `index` (0-255) from a 32-byte path. Bit 0 = MSB of byte 0. */
    def bit(path: ByteString, index: BigInt): BigInt =
        val byteVal = indexByteString(path, quotientInteger(index, 8))
        val divisor = indexByteString(BitDivisors, modInteger(index, 8))
        modInteger(quotientInteger(byteVal, divisor), 2)

    /** Suffix encoding for bit-aligned cursor. */
    def suffix(path: ByteString, cursor: BigInt): ByteString =
        val byteOffset = quotientInteger(cursor, 8)
        val bitOffset = modInteger(cursor, 8)
        val remaining = sliceByteString(byteOffset, lengthOfByteString(path), path)
        if bitOffset == BigInt(0) then consByteString(0xff, remaining)
        else consByteString(bitOffset, remaining)

    extension (self: FusedMerklePatriciaForestry2)
        def isEmpty: Boolean = self.root == NullHash

        def has(key: ByteString, value: ByteString, proof: Proof): Boolean =
            FusedMerklePatriciaForestry2.including(key, value, proof) == self.root

        def insert(
            key: ByteString,
            value: ByteString,
            proof: Proof
        ): FusedMerklePatriciaForestry2 =
            val path = blake2b_256(key)
            val hValue = blake2b_256(value)
            val both = FusedMerklePatriciaForestry2.doCombined(path, hValue, 0, proof, 0)
            val exclRoot = sliceByteString(0, Blake2b256DigestSize, both)
            val inclRoot = sliceByteString(Blake2b256DigestSize, Blake2b256DigestSize, both)
            require(exclRoot == self.root, "Invalid proof or element exists")
            FusedMerklePatriciaForestry2(inclRoot)

        def delete(
            key: ByteString,
            value: ByteString,
            proof: Proof
        ): FusedMerklePatriciaForestry2 =
            val path = blake2b_256(key)
            val hValue = blake2b_256(value)
            val both = FusedMerklePatriciaForestry2.doCombined(path, hValue, 0, proof, 0)
            val exclRoot = sliceByteString(0, Blake2b256DigestSize, both)
            val inclRoot = sliceByteString(Blake2b256DigestSize, Blake2b256DigestSize, both)
            require(inclRoot == self.root, "Invalid proof or element missing")
            FusedMerklePatriciaForestry2(exclRoot)

    def empty: FusedMerklePatriciaForestry2 = FusedMerklePatriciaForestry2(NullHash)

    def apply(root: ByteString): FusedMerklePatriciaForestry2 =
        require(lengthOfByteString(root) == Blake2b256DigestSize, "Root must be 32 bytes")
        new FusedMerklePatriciaForestry2(root)

    private def including(key: ByteString, value: ByteString, proof: Proof): ByteString =
        doIncluding(blake2b_256(key), blake2b_256(value), 0, proof, 0)

    /** Place two hashes as (child0, child1) based on our bit position. */
    private inline def ordered(
        ourBit: BigInt,
        ourHash: ByteString,
        neighborHash: ByteString
    ): (ByteString, ByteString) =
        if ourBit == BigInt(0) then (ourHash, neighborHash)
        else (neighborHash, ourHash)

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
            val prefix = consByteString(skip, ByteString.empty)
            val b = bit(path, nextCursor - 1)
            if stepType == BigInt(0) then // Branch
                val neighborHash = sliceByteString(offset + 2, Blake2b256DigestSize, proof)
                val root = doIncluding(path, value, nextCursor, proof, offset + 34)
                val (left, right) = ordered(b, root, neighborHash)
                combine3(prefix, left, right)
            else if stepType == BigInt(1) then // Fork
                val root = doIncluding(path, value, nextCursor, proof, offset + 67)
                // prefixLen[1] ++ halfLeft[32] ++ halfRight[32] = 65 contiguous bytes
                val neighborHash = blake2b_256(sliceByteString(offset + 2, 65, proof))
                val (left, right) = ordered(b, root, neighborHash)
                combine3(prefix, left, right)
            else // Leaf (stepType == 2)
                val neighborKey = sliceByteString(offset + 2, Blake2b256DigestSize, proof)
                val neighborValue = sliceByteString(offset + 34, Blake2b256DigestSize, proof)
                val root = doIncluding(path, value, nextCursor, proof, offset + 66)
                val neighborHash = combine(suffix(neighborKey, nextCursor), neighborValue)
                val (left, right) = ordered(b, root, neighborHash)
                combine3(prefix, left, right)

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
            val prefix = consByteString(skip, ByteString.empty)
            val b = bit(path, nextCursor - 1)
            if stepType == BigInt(0) then // Branch
                val neighborHash = sliceByteString(offset + 2, Blake2b256DigestSize, proof)
                val both = doCombined(path, value, nextCursor, proof, offset + 34)
                val exclChild = sliceByteString(0, Blake2b256DigestSize, both)
                val inclChild = sliceByteString(Blake2b256DigestSize, Blake2b256DigestSize, both)
                val (exL, exR) = ordered(b, exclChild, neighborHash)
                val (inL, inR) = ordered(b, inclChild, neighborHash)
                appendByteString(
                  combine3(prefix, exL, exR),
                  combine3(prefix, inL, inR)
                )
            else if stepType == BigInt(1) then // Fork
                val nextOffset = offset + 67
                val neighborHash = blake2b_256(sliceByteString(offset + 2, 65, proof))
                if nextOffset >= lengthOfByteString(proof) then
                    // Last step: excluding = neighbor promoted up with combined skip
                    val neighborPrefixLen = indexByteString(proof, offset + 2)
                    val neighborData = sliceByteString(offset + 3, 64, proof)
                    val exclResult = combine(
                      consByteString(skip + 1 + neighborPrefixLen, ByteString.empty),
                      neighborData
                    )
                    val inclChild = combine(suffix(path, nextCursor), value)
                    val (inL, inR) = ordered(b, inclChild, neighborHash)
                    appendByteString(exclResult, combine3(prefix, inL, inR))
                else
                    val both = doCombined(path, value, nextCursor, proof, nextOffset)
                    val exclChild = sliceByteString(0, Blake2b256DigestSize, both)
                    val inclChild =
                        sliceByteString(Blake2b256DigestSize, Blake2b256DigestSize, both)
                    val (exL, exR) = ordered(b, exclChild, neighborHash)
                    val (inL, inR) = ordered(b, inclChild, neighborHash)
                    appendByteString(
                      combine3(prefix, exL, exR),
                      combine3(prefix, inL, inR)
                    )
            else // Leaf (stepType == 2)
                val neighborKey = sliceByteString(offset + 2, Blake2b256DigestSize, proof)
                val neighborValue = sliceByteString(offset + 34, Blake2b256DigestSize, proof)
                val nextOffset = offset + 66
                if nextOffset >= lengthOfByteString(proof) then
                    // Last step: excluding = neighbor leaf promoted up
                    val exclResult = combine(suffix(neighborKey, cursor), neighborValue)
                    val inclChild = combine(suffix(path, nextCursor), value)
                    val neighborHash =
                        combine(suffix(neighborKey, nextCursor), neighborValue)
                    val (inL, inR) = ordered(b, inclChild, neighborHash)
                    appendByteString(exclResult, combine3(prefix, inL, inR))
                else
                    val neighborHash =
                        combine(suffix(neighborKey, nextCursor), neighborValue)
                    val both = doCombined(path, value, nextCursor, proof, nextOffset)
                    val exclChild = sliceByteString(0, Blake2b256DigestSize, both)
                    val inclChild =
                        sliceByteString(Blake2b256DigestSize, Blake2b256DigestSize, both)
                    val (exL, exR) = ordered(b, exclChild, neighborHash)
                    val (inL, inR) = ordered(b, inclChild, neighborHash)
                    appendByteString(
                      combine3(prefix, exL, exR),
                      combine3(prefix, inL, inR)
                    )
