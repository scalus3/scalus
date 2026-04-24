package scalus.cardano.node.stream.engine.snapshot.immutabledb

import java.nio.ByteBuffer
import java.nio.ByteOrder

/** Binary-format decoders for cardano-node's ImmutableDB chunk indexes.
  *
  * The on-disk format is the one defined by `ouroboros-consensus` (see
  * `Ouroboros.Consensus.Storage.ImmutableDB.Impl.Index.{Primary, Secondary}`). Each immutable chunk
  * has three files under `immutable/`:
  *
  *   - `{N}.chunk` — raw block bytes concatenated, no framing.
  *   - `{N}.primary` — `u8 version` (= 1) followed by `u32be` secondary-offsets; the i-th offset is
  *     the byte-offset into the secondary file for relative-slot i. Equal consecutive offsets mean
  *     "slot empty".
  *   - `{N}.secondary` — fixed-size `Entry`s, one per filled slot: `u64be blockOffset, u16be
  *     headerOffset, u16be headerSize, u32be checksum, u8[hashSize] headerHash, u64be blockOrEBB`.
  *
  * We target Babbage+ only, so `hashSize` is 32 and `blockOrEBB` is always a slot number. EBBs
  * (Byron-only epoch boundary blocks) aren't expected and aren't handled.
  */
object ImmutableDb {

    val CurrentPrimaryVersion: Byte = 1

    /** Shelley+ header hash length (Blake2b-256). */
    val ShelleyHashSize: Int = 32

    /** Fixed size of one `.secondary` entry for Shelley+ chunks. */
    val ShelleySecondaryEntrySize: Int = 8 + 2 + 2 + 4 + ShelleyHashSize + 8

    /** One row of `.secondary`. `slot` is the absolute slot number (Babbage+ has no EBBs, so the
      * Haskell-side `blockOrEBB` variant is always `Block slotNo`).
      */
    final case class SecondaryEntry(
        blockOffset: Long,
        headerOffset: Int,
        headerSize: Int,
        checksum: Int,
        headerHash: Array[Byte],
        slot: Long
    )

    /** One decoded block: absolute slot, header hash, raw CBOR bytes. `chunkNo` identifies the
      * `.chunk` file it came from — useful for error messages and for restorer progress reporting.
      */
    final case class ImmutableBlock(
        chunkNo: Int,
        slot: Long,
        headerHash: Array[Byte],
        blockBytes: Array[Byte]
    )

    /** Decode `.primary` — returns one secondary-offset per (filled-or-empty) relative slot.
      *
      * The first offset is always 0 by spec. Equal consecutive offsets mark empty slots; we keep
      * them in the returned vector so callers can detect emptiness by `offsets(i) == offsets(i+1)`.
      *
      * Fails on unknown version byte — we intentionally don't try to guess future formats.
      */
    def parsePrimary(bytes: Array[Byte]): IndexedSeq[Int] = {
        require(bytes.nonEmpty, "empty .primary file")
        val version = bytes(0)
        require(
          version == CurrentPrimaryVersion,
          s".primary version=$version, expected=$CurrentPrimaryVersion"
        )
        val body = bytes.length - 1
        require(body % 4 == 0, s".primary body length $body not a multiple of 4")
        val n = body / 4
        val bb = ByteBuffer.wrap(bytes, 1, body).order(ByteOrder.BIG_ENDIAN)
        val out = Array.ofDim[Int](n)
        var i = 0
        while i < n do {
            out(i) = bb.getInt
            i += 1
        }
        require(out.head == 0, s".primary first offset must be 0, got ${out.head}")
        out.toIndexedSeq
    }

    /** Decode `.secondary` as a vector of entries. Expects the file length to be a multiple of the
      * Shelley entry size; chunks that still hold Byron-era entries would fail here (28-byte hash)
      * — deliberately unsupported.
      */
    def parseSecondary(bytes: Array[Byte]): IndexedSeq[SecondaryEntry] = {
        require(
          bytes.length % ShelleySecondaryEntrySize == 0,
          s".secondary length ${bytes.length} not a multiple of $ShelleySecondaryEntrySize " +
              "(Byron-era chunks with 28-byte hashes aren't supported)"
        )
        val n = bytes.length / ShelleySecondaryEntrySize
        val bb = ByteBuffer.wrap(bytes).order(ByteOrder.BIG_ENDIAN)
        val out = Array.ofDim[SecondaryEntry](n)
        var i = 0
        while i < n do {
            val blockOffset = bb.getLong
            val headerOffset = java.lang.Short.toUnsignedInt(bb.getShort)
            val headerSize = java.lang.Short.toUnsignedInt(bb.getShort)
            val checksum = bb.getInt
            val hash = Array.ofDim[Byte](ShelleyHashSize)
            bb.get(hash)
            val slot = bb.getLong
            out(i) = SecondaryEntry(blockOffset, headerOffset, headerSize, checksum, hash, slot)
            i += 1
        }
        out.toIndexedSeq
    }

    /** Compute `(blockOffset, blockSize)` pairs from a secondary vector and the `.chunk` file size.
      * The last entry's size is derived from `chunkSize - lastBlockOffset`.
      */
    def blockRanges(
        entries: IndexedSeq[SecondaryEntry],
        chunkSize: Long
    ): IndexedSeq[(Long, Int)] = {
        if entries.isEmpty then IndexedSeq.empty
        else {
            val out = Array.ofDim[(Long, Int)](entries.size)
            var i = 0
            while i < entries.size - 1 do {
                val size = entries(i + 1).blockOffset - entries(i).blockOffset
                require(
                  size > 0 && size <= Int.MaxValue,
                  s"invalid block size $size at entry $i"
                )
                out(i) = (entries(i).blockOffset, size.toInt)
                i += 1
            }
            val lastSize = chunkSize - entries.last.blockOffset
            require(
              lastSize > 0 && lastSize <= Int.MaxValue,
              s"invalid last block size $lastSize"
            )
            out(entries.size - 1) = (entries.last.blockOffset, lastSize.toInt)
            out.toIndexedSeq
        }
    }
}
