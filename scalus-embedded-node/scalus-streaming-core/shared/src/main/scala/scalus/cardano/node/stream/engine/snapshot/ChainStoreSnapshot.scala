package scalus.cardano.node.stream.engine.snapshot

import scalus.cardano.ledger.{TransactionInput, TransactionOutput}
import scalus.cardano.node.stream.ChainTip

/** The [[ChainStoreSnapshot]] interchange format is a streaming CBOR-framed sequence owned by
  * Scalus. One [[Header]], then any number of [[Record]]s in order (blocks before utxos by
  * convention — the reader doesn't care), then one [[Footer]]. See
  * `docs/local/claude/indexer/snapshot-bootstrap-m10.md` § *ChainStoreSnapshot interchange format*
  * for the rationale.
  *
  * The format is uncompressed; users who care wrap the stream in `GZIPOutputStream` /
  * `GZIPInputStream`. Compressed-seekable formats are a future follow-up.
  */
object ChainStoreSnapshot {

    /** Current on-disk schema version. Readers refuse snapshots at other versions. */
    val SchemaVersion: Int = 1

    /** Sentinel written in the footer so truncation / concatenation accidents are caught. */
    val FooterSentinel: Int = 0xec1057a1

    /** Bit flags advertising what the body contains. Useful for block-only snapshots (M7 replay
      * backfill) vs. full Heavy-mode snapshots (M10's UTxO-set restore).
      */
    object ContentFlag {
        val Blocks: Int = 1 << 0
        val UtxoSet: Int = 1 << 1
    }

    /** Top-of-stream envelope. Advisory counts cross-check against the observed body records;
      * mismatch triggers [[SnapshotCorrupted]] in the reader.
      */
    final case class Header(
        schemaVersion: Int,
        networkMagic: Long,
        tip: ChainTip,
        blockCount: Long,
        utxoCount: Long,
        contentFlags: Int
    ) {
        def hasBlocks: Boolean = (contentFlags & ContentFlag.Blocks) != 0
        def hasUtxoSet: Boolean = (contentFlags & ContentFlag.UtxoSet) != 0
    }

    /** End-of-stream envelope. `sha256` is the SHA-256 of every body-record's raw CBOR bytes in
      * stream order; the reader hashes independently and compares. `sentinel` guards against
      * premature EOF or concatenation.
      */
    final case class Footer(sha256: Array[Byte], sentinel: Int)

    /** A single body record. Two concrete shapes, same CBOR-tagged structure. */
    sealed trait Record
    object Record {
        final case class BlockRecord(block: scalus.cardano.node.stream.engine.AppliedBlock)
            extends Record
        final case class UtxoEntry(input: TransactionInput, output: TransactionOutput)
            extends Record
    }
}
