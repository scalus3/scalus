package scalus.cardano.onchain.plutus.crypto.trie

import scalus.compiler.Compile
import scalus.cardano.onchain.plutus.prelude.{require, List}
import scalus.uplc.builtin.Builtins.*
import scalus.uplc.builtin.{ByteString, FromData, ToData}
import scalus.uplc.builtin.ByteString.hex

/** Merkle Patricia Forestry (radix-2) with full bit prefix encoding.
  *
  * Unfused variant: uses `combine(bits_prefix, combine(child0, child1))` (two blake2b calls per
  * branch) and Data-encoded proofs (`List[ProofStep]`). Same trie structure as
  * [[FusedMerklePatriciaForestry2]] but with different hashing and proof format.
  */
case class MerklePatriciaForestry2(root: ByteString)

@Compile
object MerklePatriciaForestry2:
    import Merkling.{NullHash, combine}

    private val Blake2b256DigestSize: BigInt = 32

    /** Lookup table for 2^(7-i), i=0..7: [128, 64, 32, 16, 8, 4, 2, 1] */
    private val BitDivisors: ByteString = hex"8040201008040201"

    case class Neighbor(
        bit: BigInt,
        prefix: ByteString,
        root: ByteString
    ) derives FromData,
          ToData

    enum ProofStep derives FromData, ToData:
        case Branch(skip: BigInt, neighbor: ByteString)
        case Fork(skip: BigInt, neighbor: Neighbor)
        case Leaf(skip: BigInt, key: ByteString, value: ByteString)

    type Proof = List[ProofStep]

    /** Extract bit at position `index` (0-255) from a 32-byte path. Bit 0 = MSB of byte 0. */
    private def bit(path: ByteString, index: BigInt): BigInt =
        val byteVal = indexByteString(path, quotientInteger(index, 8))
        val divisor = indexByteString(BitDivisors, modInteger(index, 8))
        modInteger(quotientInteger(byteVal, divisor), 2)

    /** Suffix encoding for bit-aligned cursor (same as FusedMPF2). */
    private def suffix2(path: ByteString, cursor: BigInt): ByteString =
        val byteOffset = quotientInteger(cursor, 8)
        val bitOffset = modInteger(cursor, 8)
        val remaining = sliceByteString(byteOffset, lengthOfByteString(path), path)
        if bitOffset == BigInt(0) then consByteString(0xff, remaining)
        else consByteString(bitOffset, remaining)

    /** Extract bit range [start, end) as one-byte-per-bit ByteString. */
    private def bits(path: ByteString, start: BigInt, end: BigInt): ByteString =
        if start >= end then ByteString.empty
        else consByteString(bit(path, start), bits(path, addInteger(start, 1), end))

    extension (self: MerklePatriciaForestry2)
        def isEmpty: Boolean = self.root == NullHash

        def has(key: ByteString, value: ByteString, proof: Proof): Boolean =
            MerklePatriciaForestry2.including(key, value, proof) == self.root

        def verifyMembership(key: ByteString, value: ByteString, proof: Proof): Unit =
            require(
              MerklePatriciaForestry2.including(key, value, proof) == self.root,
              "Membership verification failed"
            )

        def insert(
            key: ByteString,
            value: ByteString,
            proof: Proof
        ): MerklePatriciaForestry2 =
            val path = blake2b_256(key)
            val hValue = blake2b_256(value)
            val both = MerklePatriciaForestry2.doCombined(path, hValue, 0, proof)
            val exclRoot = sliceByteString(0, Blake2b256DigestSize, both)
            val inclRoot = sliceByteString(Blake2b256DigestSize, Blake2b256DigestSize, both)
            require(exclRoot == self.root, "Invalid proof or element exists")
            MerklePatriciaForestry2(inclRoot)

        def delete(
            key: ByteString,
            value: ByteString,
            proof: Proof
        ): MerklePatriciaForestry2 =
            val path = blake2b_256(key)
            val hValue = blake2b_256(value)
            val both = MerklePatriciaForestry2.doCombined(path, hValue, 0, proof)
            val exclRoot = sliceByteString(0, Blake2b256DigestSize, both)
            val inclRoot = sliceByteString(Blake2b256DigestSize, Blake2b256DigestSize, both)
            require(inclRoot == self.root, "Invalid proof or element missing")
            MerklePatriciaForestry2(exclRoot)

    def empty: MerklePatriciaForestry2 = MerklePatriciaForestry2(NullHash)

    def apply(root: ByteString): MerklePatriciaForestry2 =
        require(lengthOfByteString(root) == Blake2b256DigestSize, "Root must be 32 bytes")
        new MerklePatriciaForestry2(root)

    private def including(key: ByteString, value: ByteString, proof: Proof): ByteString =
        doIncluding(blake2b_256(key), blake2b_256(value), 0, proof)

    private def doIncluding(
        path: ByteString,
        value: ByteString,
        cursor: BigInt,
        proof: Proof
    ): ByteString = proof match
        case List.Nil =>
            combine(suffix2(path, cursor), value)

        case List.Cons(step, steps) =>
            step match
                case ProofStep.Branch(skip, neighborHash) =>
                    val nextCursor = cursor + 1 + skip
                    val root = doIncluding(path, value, nextCursor, steps)
                    val b = bit(path, nextCursor - 1)
                    val prefix = bits(path, cursor, nextCursor - 1)
                    if b == BigInt(0) then combine(prefix, combine(root, neighborHash))
                    else combine(prefix, combine(neighborHash, root))

                case ProofStep.Fork(skip, neighbor) =>
                    val nextCursor = cursor + 1 + skip
                    val root = doIncluding(path, value, nextCursor, steps)
                    val b = bit(path, nextCursor - 1)
                    val prefix = bits(path, cursor, nextCursor - 1)
                    val neighborHash = combine(neighbor.prefix, neighbor.root)
                    if b == BigInt(0) then combine(prefix, combine(root, neighborHash))
                    else combine(prefix, combine(neighborHash, root))

                case ProofStep.Leaf(skip, key, neighborValue) =>
                    val nextCursor = cursor + 1 + skip
                    val root = doIncluding(path, value, nextCursor, steps)
                    val b = bit(path, nextCursor - 1)
                    val prefix = bits(path, cursor, nextCursor - 1)
                    val neighborHash = combine(suffix2(key, nextCursor), neighborValue)
                    if b == BigInt(0) then combine(prefix, combine(root, neighborHash))
                    else combine(prefix, combine(neighborHash, root))

    // --- Combined single-pass: compute excluding and including simultaneously ---
    // Returns a 64-byte ByteString: excl[32] || incl[32]

    private def doCombined(
        path: ByteString,
        value: ByteString,
        cursor: BigInt,
        proof: Proof
    ): ByteString = proof match
        case List.Nil =>
            appendByteString(NullHash, combine(suffix2(path, cursor), value))

        case List.Cons(step, steps) =>
            step match
                case ProofStep.Branch(skip, neighborHash) =>
                    val nextCursor = cursor + 1 + skip
                    val both = doCombined(path, value, nextCursor, steps)
                    val exclChild = sliceByteString(0, Blake2b256DigestSize, both)
                    val inclChild =
                        sliceByteString(Blake2b256DigestSize, Blake2b256DigestSize, both)
                    val b = bit(path, nextCursor - 1)
                    val prefix = bits(path, cursor, nextCursor - 1)
                    if b == BigInt(0) then
                        appendByteString(
                          combine(prefix, combine(exclChild, neighborHash)),
                          combine(prefix, combine(inclChild, neighborHash))
                        )
                    else
                        appendByteString(
                          combine(prefix, combine(neighborHash, exclChild)),
                          combine(prefix, combine(neighborHash, inclChild))
                        )

                case ProofStep.Fork(skip, neighbor) =>
                    val nextCursor = cursor + 1 + skip
                    val b = bit(path, nextCursor - 1)
                    val prefix = bits(path, cursor, nextCursor - 1)
                    val neighborHash = combine(neighbor.prefix, neighbor.root)
                    steps match
                        case List.Nil =>
                            // Last step: excluding = neighbor promoted up
                            val exclResult =
                                if skip == BigInt(0) then
                                    combine(
                                      consByteString(neighbor.bit, neighbor.prefix),
                                      neighbor.root
                                    )
                                else
                                    combine(
                                      appendByteString(
                                        bits(path, cursor, cursor + skip),
                                        consByteString(neighbor.bit, neighbor.prefix)
                                      ),
                                      neighbor.root
                                    )
                            val inclChild = combine(suffix2(path, nextCursor), value)
                            val inclResult =
                                if b == BigInt(0) then
                                    combine(prefix, combine(inclChild, neighborHash))
                                else combine(prefix, combine(neighborHash, inclChild))
                            appendByteString(exclResult, inclResult)
                        case _ =>
                            val both = doCombined(path, value, nextCursor, steps)
                            val exclChild = sliceByteString(0, Blake2b256DigestSize, both)
                            val inclChild =
                                sliceByteString(Blake2b256DigestSize, Blake2b256DigestSize, both)
                            if b == BigInt(0) then
                                appendByteString(
                                  combine(prefix, combine(exclChild, neighborHash)),
                                  combine(prefix, combine(inclChild, neighborHash))
                                )
                            else
                                appendByteString(
                                  combine(prefix, combine(neighborHash, exclChild)),
                                  combine(prefix, combine(neighborHash, inclChild))
                                )

                case ProofStep.Leaf(skip, key, neighborValue) =>
                    val nextCursor = cursor + 1 + skip
                    val b = bit(path, nextCursor - 1)
                    val prefix = bits(path, cursor, nextCursor - 1)
                    val neighborHash = combine(suffix2(key, nextCursor), neighborValue)
                    steps match
                        case List.Nil =>
                            val exclResult = combine(suffix2(key, cursor), neighborValue)
                            val inclChild = combine(suffix2(path, nextCursor), value)
                            val inclResult =
                                if b == BigInt(0) then
                                    combine(prefix, combine(inclChild, neighborHash))
                                else combine(prefix, combine(neighborHash, inclChild))
                            appendByteString(exclResult, inclResult)
                        case _ =>
                            val both = doCombined(path, value, nextCursor, steps)
                            val exclChild = sliceByteString(0, Blake2b256DigestSize, both)
                            val inclChild =
                                sliceByteString(Blake2b256DigestSize, Blake2b256DigestSize, both)
                            if b == BigInt(0) then
                                appendByteString(
                                  combine(prefix, combine(exclChild, neighborHash)),
                                  combine(prefix, combine(inclChild, neighborHash))
                                )
                            else
                                appendByteString(
                                  combine(prefix, combine(neighborHash, exclChild)),
                                  combine(prefix, combine(neighborHash, inclChild))
                                )
