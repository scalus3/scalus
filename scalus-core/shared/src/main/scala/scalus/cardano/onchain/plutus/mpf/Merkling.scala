package scalus.cardano.onchain.plutus.mpf

import scalus.compiler.Compile
import scalus.uplc.builtin.Builtins.*
import scalus.uplc.builtin.ByteString
import scalus.uplc.builtin.ByteString.hex

@Compile
object Merkling:
    /** By convention, the hash of a null tree/trie is the null hash. Note that we also cache trees
      * made of empty trees to short-circuit the neighbor sparse-merkle tree calculation.
      */
    val NullHash: ByteString = hex"0000000000000000000000000000000000000000000000000000000000000000"

    // Pre-computed null hashes for optimization
    val NullHash2: ByteString = combine(NullHash, NullHash)
    val NullHash4: ByteString = combine(NullHash2, NullHash2)
    val NullHash8: ByteString = combine(NullHash4, NullHash4)

    /** Combines two ByteStrings by concatenating them and taking their Blake2b hash */
    def combine(left: ByteString, right: ByteString): ByteString =
        blake2b_256(appendByteString(left, right))

    /** Creates a suffix path based on cursor position
      *
      * @param path
      *   The original path
      * @param cursor
      *   Position in the path
      * @return
      *   A new path with suffix appended according to cursor position
      */
    def suffix(path: ByteString, cursor: BigInt): ByteString =
        if cursor % 2 == BigInt(0) then
            // Even cursor: drop cursor/2 bytes and append 0xff
            val dropped = sliceByteString(cursor / 2, lengthOfByteString(path), path)
            consByteString(0xff, dropped)
        else
            // Odd cursor: drop (cursor+1)/2 bytes, append nibble and 0
            val dropped = sliceByteString((cursor + 1) / 2, lengthOfByteString(path), path)
            val withNibble = consByteString(nibble(path, cursor), dropped)
            consByteString(0, withNibble)

    /** Computes nibbles for a given branch node between start and end positions
      *
      * @param path
      *   The source ByteString to extract nibbles from
      * @param start
      *   Starting position
      * @param end
      *   Ending position (exclusive)
      * @return
      *   ByteString containing the extracted nibbles
      */
    def nibbles(path: ByteString, start: BigInt, end: BigInt): ByteString =
        if start >= end then ByteString.empty
        else
            consByteString(
              nibble(path, start),
              nibbles(path, addInteger(start, 1), end)
            )

    /** Extracts a nibble (4-bit value) from a specific position in the ByteString
      *
      * @param self
      *   Source ByteString
      * @param index
      *   Position to extract nibble from
      * @return
      *   The nibble value as BigInt (0-15)
      */
    def nibble(self: ByteString, index: BigInt): BigInt =
        if modInteger(index, 2) == BigInt(0) then
            // Even index: take high nibble (divide by 16)
            quotientInteger(indexByteString(self, index / 2), 16)
        else
            // Odd index: take low nibble (modulo 16)
            modInteger(indexByteString(self, index / 2), 16)

    /** Calculates Merkle root for 16 elements
      *
      * @param branch
      *   Branch index (0-15)
      * @param root
      *   Current node hash
      * @param neighbor_8
      *   8-element neighbor hash
      * @param neighbor_4
      *   4-element neighbor hash
      * @param neighbor_2
      *   2-element neighbor hash
      * @param neighbor_1
      *   1-element neighbor hash
      */
    def merkle16(
        branch: BigInt,
        root: ByteString,
        neighbor8: ByteString,
        neighbor4: ByteString,
        neighbor2: ByteString,
        neighbor1: ByteString
    ): ByteString =
        if branch <= 7 then
            combine(
              merkle8(branch, root, neighbor4, neighbor2, neighbor1),
              neighbor8
            )
        else
            combine(
              neighbor8,
              merkle8(branch - 8, root, neighbor4, neighbor2, neighbor1)
            )

    /** Calculates Merkle root for 8 elements */
    def merkle8(
        branch: BigInt,
        root: ByteString,
        neighbor4: ByteString,
        neighbor2: ByteString,
        neighbor1: ByteString
    ): ByteString =
        if branch <= 3 then combine(merkle4(branch, root, neighbor2, neighbor1), neighbor4)
        else combine(neighbor4, merkle4(branch - 4, root, neighbor2, neighbor1))

    /** Calculates Merkle root for 4 elements */
    def merkle4(
        branch: BigInt,
        root: ByteString,
        neighbor2: ByteString,
        neighbor1: ByteString
    ): ByteString =
        if branch <= 1 then combine(merkle2(branch, root, neighbor1), neighbor2)
        else combine(neighbor2, merkle2(branch - 2, root, neighbor1))

    /** Calculates Merkle root for 2 elements */
    def merkle2(
        branch: BigInt,
        root: ByteString,
        neighbor: ByteString
    ): ByteString =
        if branch <= 0 then combine(root, neighbor)
        else combine(neighbor, root)

    /** Calculates sparse Merkle root for 16 elements */
    def sparseMerkle16(
        me: BigInt,
        meHash: ByteString,
        neighbor: BigInt,
        neighborHash: ByteString
    ): ByteString =
        if me < 8 then
            if neighbor < 8 then
                combine(sparseMerkle8(me, meHash, neighbor, neighborHash), NullHash8)
            else
                combine(
                  merkle8(me, meHash, NullHash4, NullHash2, NullHash),
                  merkle8(neighbor - 8, neighborHash, NullHash4, NullHash2, NullHash)
                )
        else if neighbor >= 8 then
            combine(
              NullHash8,
              sparseMerkle8(me - 8, meHash, neighbor - 8, neighborHash)
            )
        else
            combine(
              merkle8(neighbor, neighborHash, NullHash4, NullHash2, NullHash),
              merkle8(me - 8, meHash, NullHash4, NullHash2, NullHash)
            )

    /** Calculates sparse Merkle root for 8 elements */
    def sparseMerkle8(
        me: BigInt,
        meHash: ByteString,
        neighbor: BigInt,
        neighborHash: ByteString
    ): ByteString =
        if me < 4 then
            if neighbor < 4 then
                combine(sparseMerkle4(me, meHash, neighbor, neighborHash), NullHash4)
            else
                combine(
                  merkle4(me, meHash, NullHash2, NullHash),
                  merkle4(neighbor - 4, neighborHash, NullHash2, NullHash)
                )
        else if neighbor >= 4 then
            combine(
              NullHash4,
              sparseMerkle4(me - 4, meHash, neighbor - 4, neighborHash)
            )
        else
            combine(
              merkle4(neighbor, neighborHash, NullHash2, NullHash),
              merkle4(me - 4, meHash, NullHash2, NullHash)
            )

    /** Calculates sparse Merkle root for 4 elements */
    def sparseMerkle4(
        me: BigInt,
        meHash: ByteString,
        neighbor: BigInt,
        neighborHash: ByteString
    ): ByteString =
        if me < 2 then
            if neighbor < 2 then combine(merkle2(me, meHash, neighborHash), NullHash2)
            else
                combine(
                  merkle2(me, meHash, NullHash),
                  merkle2(neighbor - 2, neighborHash, NullHash)
                )
        else if neighbor >= 2 then combine(NullHash2, merkle2(me - 2, meHash, neighborHash))
        else
            combine(
              merkle2(neighbor, neighborHash, NullHash),
              merkle2(me - 2, meHash, NullHash)
            )
