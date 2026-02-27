package scalus.cardano.onchain.plutus.mpf64

import scalus.compiler.Compile
import scalus.uplc.builtin.Builtins.*
import scalus.uplc.builtin.ByteString
import scalus.uplc.builtin.ByteString.hex

/** Merkling primitives for radix-64 Merkle Patricia Forestry.
  *
  * Uses 6-bit path units (0-63) extracted via lookup tables, yielding 42 levels for 32-byte blake2b
  * paths (252 bits / 6 = 42, with 4 leftover bits in suffix).
  *
  * The merkle functions are unrolled (merkle2/4/8/16/32/64) for minimal overhead, matching the
  * style of MPF-16's unrolled merkle2/4/8/16.
  */
@Compile
object Merkling:
    val NullHash: ByteString = hex"0000000000000000000000000000000000000000000000000000000000000000"

    val NullHash2: ByteString = combine(NullHash, NullHash)
    val NullHash4: ByteString = combine(NullHash2, NullHash2)
    val NullHash8: ByteString = combine(NullHash4, NullHash4)
    val NullHash16: ByteString = combine(NullHash8, NullHash8)
    val NullHash32: ByteString = combine(NullHash16, NullHash16)

    def combine(left: ByteString, right: ByteString): ByteString =
        blake2b_256(appendByteString(left, right))

    /** Combines three ByteStrings by concatenating them and taking their Blake2b hash */
    def combine3(a: ByteString, b: ByteString, c: ByteString): ByteString =
        blake2b_256(appendByteString(a, appendByteString(b, c)))

    /** Single zero byte for padding path to 33 bytes (boundary fix for indices 40-41) */
    val ZeroByte: ByteString = hex"00"

    /** Extracts a 6-bit value (0-63) at the given index from a 32-byte path.
      *
      * 32 bytes = 256 bits → 42 six-bit values (252 bits) + 4 remaining bits. Every 3 bytes contain
      * 4 six-bit values (LCM(6,8) = 24 bits): Byte 0: [aaaaaa|bb] Byte 1: [bbbb|cccc] Byte 2:
      * [cc|dddddd]
      */
    def sixit(path: ByteString, index: BigInt): BigInt =
        val base = quotientInteger(index, 4) * 3
        val pos = modInteger(index, 4)
        if pos == BigInt(0) then quotientInteger(indexByteString(path, base), 4)
        else if pos == BigInt(1) then
            modInteger(indexByteString(path, base), 4) * 16 +
                quotientInteger(indexByteString(path, base + 1), 16)
        else if pos == BigInt(2) then
            modInteger(indexByteString(path, base + 1), 16) * 4 +
                quotientInteger(indexByteString(path, base + 2), 64)
        else modInteger(indexByteString(path, base + 2), 64)

    /** Suffix encoding for 6-bit-aligned cursor.
      *
      * The cursor is an index into 6-bit values. We encode the remaining path as:
      *   - Byte-aligned (cursor % 4 == 0): 0xff || remaining bytes from byteOffset
      *   - Non-aligned: (cursor % 4) || remaining bytes from byteOffset
      *
      * The marker byte distinguishes alignment: 0xff cannot be a valid pos (0-3).
      */
    def suffix(path: ByteString, cursor: BigInt): ByteString =
        val byteOffset = quotientInteger(cursor, 4) * 3
        val pos = modInteger(cursor, 4)
        val remaining = sliceByteString(byteOffset, lengthOfByteString(path), path)
        if pos == BigInt(0) then consByteString(0xff, remaining)
        else consByteString(pos, remaining)

    // --- Unrolled merkle functions (6 levels for 64 slots) ---

    inline def merkle2(branch: BigInt, root: ByteString, neighbor: ByteString): ByteString =
        if branch <= 0 then combine(root, neighbor)
        else combine(neighbor, root)

    def merkle4(
        branch: BigInt,
        root: ByteString,
        neighbor2: ByteString,
        neighbor1: ByteString
    ): ByteString =
        if branch <= 1 then combine(merkle2(branch, root, neighbor1), neighbor2)
        else combine(neighbor2, merkle2(branch - 2, root, neighbor1))

    def merkle8(
        branch: BigInt,
        root: ByteString,
        neighbor4: ByteString,
        neighbor2: ByteString,
        neighbor1: ByteString
    ): ByteString =
        if branch <= 3 then combine(merkle4(branch, root, neighbor2, neighbor1), neighbor4)
        else combine(neighbor4, merkle4(branch - 4, root, neighbor2, neighbor1))

    def merkle16(
        branch: BigInt,
        root: ByteString,
        neighbor8: ByteString,
        neighbor4: ByteString,
        neighbor2: ByteString,
        neighbor1: ByteString
    ): ByteString =
        if branch <= 7 then
            combine(merkle8(branch, root, neighbor4, neighbor2, neighbor1), neighbor8)
        else combine(neighbor8, merkle8(branch - 8, root, neighbor4, neighbor2, neighbor1))

    def merkle32(
        branch: BigInt,
        root: ByteString,
        neighbor16: ByteString,
        neighbor8: ByteString,
        neighbor4: ByteString,
        neighbor2: ByteString,
        neighbor1: ByteString
    ): ByteString =
        if branch <= 15 then
            combine(
              merkle16(branch, root, neighbor8, neighbor4, neighbor2, neighbor1),
              neighbor16
            )
        else
            combine(
              neighbor16,
              merkle16(branch - 16, root, neighbor8, neighbor4, neighbor2, neighbor1)
            )

    /** Merkle root for 64 elements (6 levels of binary tree).
      *
      * @param branch
      *   position (0-63) of the element with hash `root`
      * @param root
      *   hash of the element at `branch`
      * @param neighbor32..neighbor1
      *   sibling subtree hashes, largest first
      */
    def merkle64(
        branch: BigInt,
        root: ByteString,
        neighbor32: ByteString,
        neighbor16: ByteString,
        neighbor8: ByteString,
        neighbor4: ByteString,
        neighbor2: ByteString,
        neighbor1: ByteString
    ): ByteString =
        if branch <= 31 then
            combine(
              merkle32(branch, root, neighbor16, neighbor8, neighbor4, neighbor2, neighbor1),
              neighbor32
            )
        else
            combine(
              neighbor32,
              merkle32(branch - 32, root, neighbor16, neighbor8, neighbor4, neighbor2, neighbor1)
            )

    // --- Unrolled sparse merkle functions ---

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

    def sparseMerkle32(
        me: BigInt,
        meHash: ByteString,
        neighbor: BigInt,
        neighborHash: ByteString
    ): ByteString =
        if me < 16 then
            if neighbor < 16 then
                combine(sparseMerkle16(me, meHash, neighbor, neighborHash), NullHash16)
            else
                combine(
                  merkle16(me, meHash, NullHash8, NullHash4, NullHash2, NullHash),
                  merkle16(neighbor - 16, neighborHash, NullHash8, NullHash4, NullHash2, NullHash)
                )
        else if neighbor >= 16 then
            combine(
              NullHash16,
              sparseMerkle16(me - 16, meHash, neighbor - 16, neighborHash)
            )
        else
            combine(
              merkle16(neighbor, neighborHash, NullHash8, NullHash4, NullHash2, NullHash),
              merkle16(me - 16, meHash, NullHash8, NullHash4, NullHash2, NullHash)
            )

    /** Sparse merkle root for exactly 2 occupied positions out of 64. */
    def sparseMerkle64(
        me: BigInt,
        meHash: ByteString,
        neighbor: BigInt,
        neighborHash: ByteString
    ): ByteString =
        if me < 32 then
            if neighbor < 32 then
                combine(sparseMerkle32(me, meHash, neighbor, neighborHash), NullHash32)
            else
                combine(
                  merkle32(me, meHash, NullHash16, NullHash8, NullHash4, NullHash2, NullHash),
                  merkle32(
                    neighbor - 32,
                    neighborHash,
                    NullHash16,
                    NullHash8,
                    NullHash4,
                    NullHash2,
                    NullHash
                  )
                )
        else if neighbor >= 32 then
            combine(
              NullHash32,
              sparseMerkle32(me - 32, meHash, neighbor - 32, neighborHash)
            )
        else
            combine(
              merkle32(
                neighbor,
                neighborHash,
                NullHash16,
                NullHash8,
                NullHash4,
                NullHash2,
                NullHash
              ),
              merkle32(me - 32, meHash, NullHash16, NullHash8, NullHash4, NullHash2, NullHash)
            )
