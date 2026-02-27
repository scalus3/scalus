package scalus.cardano.onchain.plutus.mpq

import scalus.compiler.Compile
import scalus.uplc.builtin.Builtins.*
import scalus.uplc.builtin.ByteString
import scalus.uplc.builtin.ByteString.hex

@Compile
object QuaternaryMerkling:
    val NullHash: ByteString = hex"0000000000000000000000000000000000000000000000000000000000000000"
    val NullHash2: ByteString = combine(NullHash, NullHash)

    /** Combines two ByteStrings by concatenating them and taking their Blake2b-256 hash. */
    def combine(left: ByteString, right: ByteString): ByteString =
        blake2b_256(appendByteString(left, right))

    /** Extracts a dibit (2-bit value, 0-3) at the given index (MSB-first).
      *
      * Index 0 is the top 2 bits of the first byte, index 127 is the bottom 2 bits of the last byte
      * (for 32-byte paths). Each byte contains 4 dibits.
      */
    def dibit(path: ByteString, index: BigInt): BigInt =
        val byte = indexByteString(path, quotientInteger(index, 4))
        val pos = modInteger(index, 4)
        if pos == BigInt(0) then quotientInteger(byte, 64)
        else if pos == BigInt(1) then modInteger(quotientInteger(byte, 16), 4)
        else if pos == BigInt(2) then modInteger(quotientInteger(byte, 4), 4)
        else modInteger(byte, 4)

    /** Encodes the remaining path from a dibit cursor position. Used to create unique leaf hashes.
      *
      * Format:
      *   - Byte-aligned (cursor % 4 == 0): marker `0xff` followed by remaining bytes from cursor/4
      *   - Non-aligned (cursor % 4 in 1..3): marker = cursor % 4 followed by remaining bytes from
      *     cursor/4
      */
    def suffixDibit(path: ByteString, cursor: BigInt): ByteString =
        val byteOffset = quotientInteger(cursor, 4)
        val dibitOffset = modInteger(cursor, 4)
        val remaining = sliceByteString(byteOffset, lengthOfByteString(path), path)
        if dibitOffset == BigInt(0) then consByteString(0xff, remaining)
        else consByteString(dibitOffset, remaining)

    /** Merkle root for 2 elements. */
    def merkle2(branch: BigInt, root: ByteString, neighbor: ByteString): ByteString =
        if branch <= BigInt(0) then combine(root, neighbor)
        else combine(neighbor, root)

    /** Merkle root for 4 elements.
      *
      * @param branch
      *   Branch index (0-3)
      * @param root
      *   Current node hash at position `branch`
      * @param neighbor2
      *   Opposite pair combined hash
      * @param neighbor1
      *   Pair-mate hash
      */
    def merkle4(
        branch: BigInt,
        root: ByteString,
        neighbor2: ByteString,
        neighbor1: ByteString
    ): ByteString =
        if branch <= BigInt(1) then combine(merkle2(branch, root, neighbor1), neighbor2)
        else combine(neighbor2, merkle2(branch - 2, root, neighbor1))

    /** Sparse merkle root for exactly 2 occupied positions out of 4. */
    def sparseMerkle4(
        me: BigInt,
        meHash: ByteString,
        neighbor: BigInt,
        neighborHash: ByteString
    ): ByteString =
        if me < BigInt(2) then
            if neighbor < BigInt(2) then combine(merkle2(me, meHash, neighborHash), NullHash2)
            else
                combine(
                  merkle2(me, meHash, NullHash),
                  merkle2(neighbor - 2, neighborHash, NullHash)
                )
        else if neighbor >= BigInt(2) then combine(NullHash2, merkle2(me - 2, meHash, neighborHash))
        else
            combine(
              merkle2(neighbor, neighborHash, NullHash),
              merkle2(me - 2, meHash, NullHash)
            )
