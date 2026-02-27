package scalus.cardano.onchain.plutus.mpt

import scalus.compiler.Compile
import scalus.uplc.builtin.Builtins.*
import scalus.uplc.builtin.ByteString
import scalus.uplc.builtin.ByteString.hex

@Compile
object BinaryMerkling:
    val NullHash: ByteString = hex"0000000000000000000000000000000000000000000000000000000000000000"

    /** Combines two ByteStrings by concatenating them and taking their Blake2b-256 hash. */
    def combine(left: ByteString, right: ByteString): ByteString =
        blake2b_256(appendByteString(left, right))

    /** Extracts a single bit from a path at the given index (MSB-first).
      *
      * Index 0 is the MSB of the first byte, index 255 is the LSB of the last byte (for 32-byte
      * paths). Uses the `readBit` builtin (CIP-122) which uses LSB-first indexing internally.
      */
    def bit(path: ByteString, index: BigInt): BigInt =
        if readBit(path, lengthOfByteString(path) * 8 - 1 - index) then BigInt(1)
        else BigInt(0)

    /** Encodes the remaining path from a bit cursor position. Used to create unique leaf hashes.
      *
      * Format:
      *   - Byte-aligned (cursor % 8 == 0): marker `0xff` followed by remaining bytes from cursor/8
      *   - Non-aligned (cursor % 8 in 1..7): marker = cursor % 8 followed by remaining bytes from
      *     cursor/8
      */
    def suffixBit(path: ByteString, cursor: BigInt): ByteString =
        val byteOffset = quotientInteger(cursor, 8)
        val bitOffset = modInteger(cursor, 8)
        val remaining = sliceByteString(byteOffset, lengthOfByteString(path), path)
        if bitOffset == BigInt(0) then consByteString(0xff, remaining)
        else consByteString(bitOffset, remaining)
