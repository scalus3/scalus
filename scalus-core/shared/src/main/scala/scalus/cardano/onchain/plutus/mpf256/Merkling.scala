package scalus.cardano.onchain.plutus.mpf256

import scalus.compiler.Compile
import scalus.uplc.builtin.Builtins.*
import scalus.uplc.builtin.ByteString
import scalus.uplc.builtin.ByteString.hex

@Compile
object Merkling:
    /** By convention, the hash of a null tree/trie is the null hash. */
    val NullHash: ByteString = hex"0000000000000000000000000000000000000000000000000000000000000000"

    // Pre-computed null hashes: NullHash_k = merkle root of 2^k null leaves
    val NullHash2: ByteString = combine(NullHash, NullHash)
    val NullHash4: ByteString = combine(NullHash2, NullHash2)
    val NullHash8: ByteString = combine(NullHash4, NullHash4)
    val NullHash16: ByteString = combine(NullHash8, NullHash8)
    val NullHash32: ByteString = combine(NullHash16, NullHash16)
    val NullHash64: ByteString = combine(NullHash32, NullHash32)
    val NullHash128: ByteString = combine(NullHash64, NullHash64)

    /** Table of 8 null hashes, 32 bytes each (256 bytes total). Index 0 = NullHash (2^0 leaves),
      * index 7 = NullHash128 (2^7 leaves). Used by singleMerkle/sparseMerkle to fill empty
      * subtrees.
      */
    val NullHashTable: ByteString =
        appendByteString(
          appendByteString(
            appendByteString(NullHash, NullHash2),
            appendByteString(NullHash4, NullHash8)
          ),
          appendByteString(
            appendByteString(NullHash16, NullHash32),
            appendByteString(NullHash64, NullHash128)
          )
        )

    /** Lookup null hash at the given level (0..7) from the table. */
    def nullHashAt(level: BigInt): ByteString = sliceByteString(level * 32, 32, NullHashTable)

    /** Combines two ByteStrings by concatenating them and taking their Blake2b hash */
    def combine(left: ByteString, right: ByteString): ByteString =
        blake2b_256(appendByteString(left, right))

    /** Creates a suffix path for byte-aligned cursor. Since cursor is always byte-aligned,
      * different cursor positions produce different-length suffixes, so no markers are needed.
      */
    def suffix(path: ByteString, cursor: BigInt): ByteString =
        sliceByteString(cursor, 32 - cursor, path)

    /** Extracts a byte (0-255) from a specific position in the ByteString */
    def byteAt(self: ByteString, index: BigInt): BigInt =
        indexByteString(self, index)

    /** Generic merkle: reconstruct root of 2^depth binary tree given element at `branch` and depth
      * sibling hashes.
      *
      * @param half
      *   starting half-size (128 for 256 slots)
      * @param branch
      *   element position (0..255)
      * @param root
      *   hash of the element at `branch`
      * @param neighbors
      *   depth x 32 bytes, largest level first
      */
    def merkle(half: BigInt, branch: BigInt, root: ByteString, neighbors: ByteString): ByteString =
        if half <= 0 then root
        else
            val sibling = sliceByteString(0, 32, neighbors)
            val rest = sliceByteString(32, lengthOfByteString(neighbors) - 32, neighbors)
            if branch < half then combine(merkle(half / 2, branch, root, rest), sibling)
            else combine(sibling, merkle(half / 2, branch - half, root, rest))

    /** Place a single element at `pos` in a 2^depth tree, all other slots NullHash.
      *
      * @param half
      *   starting half-size (128 for 256 slots)
      * @param level
      *   current level index into NullHashTable (7 for 128 slots)
      * @param pos
      *   element position
      * @param hash
      *   element hash
      */
    def singleMerkle(half: BigInt, level: BigInt, pos: BigInt, hash: ByteString): ByteString =
        if half <= 0 then hash
        else
            val nh = nullHashAt(level)
            if pos < half then combine(singleMerkle(half / 2, level - 1, pos, hash), nh)
            else combine(nh, singleMerkle(half / 2, level - 1, pos - half, hash))

    /** Place 2 elements in a 2^depth tree, all other slots NullHash.
      *
      * @param half
      *   starting half-size (128 for 256 slots)
      * @param level
      *   current level index into NullHashTable (7 for 128 slots)
      * @param me
      *   first element position
      * @param meH
      *   first element hash
      * @param nb
      *   second element position
      * @param nbH
      *   second element hash
      */
    def sparseMerkle(
        half: BigInt,
        level: BigInt,
        me: BigInt,
        meH: ByteString,
        nb: BigInt,
        nbH: ByteString
    ): ByteString =
        if half <= 0 then meH
        else
            val nh = nullHashAt(level)
            if me < half then
                if nb < half then combine(sparseMerkle(half / 2, level - 1, me, meH, nb, nbH), nh)
                else
                    combine(
                      singleMerkle(half / 2, level - 1, me, meH),
                      singleMerkle(half / 2, level - 1, nb - half, nbH)
                    )
            else if nb >= half then
                combine(nh, sparseMerkle(half / 2, level - 1, me - half, meH, nb - half, nbH))
            else
                combine(
                  singleMerkle(half / 2, level - 1, nb, nbH),
                  singleMerkle(half / 2, level - 1, me - half, meH)
                )
