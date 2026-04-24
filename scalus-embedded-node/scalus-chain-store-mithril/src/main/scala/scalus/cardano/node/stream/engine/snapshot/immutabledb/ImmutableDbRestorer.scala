package scalus.cardano.node.stream.engine.snapshot.immutabledb

import scalus.cardano.ledger.BlockHash
import scalus.cardano.node.stream.engine.{AppliedBlock, ChainStore}
import scalus.cardano.node.stream.{ChainPoint, ChainTip}
import scalus.uplc.builtin.ByteString

import java.nio.file.Path

/** Load blocks from a Mithril-downloaded cardano-node `immutable/` directory into a [[ChainStore]].
  *
  * Walks the directory via [[ImmutableDbReader]], peels each block's HFC wrapper via
  * [[HfcDiskBlockDecoder]], projects to [[AppliedBlock]] via [[AppliedBlock.fromRaw]], and calls
  * `store.appendBlock(...)` in slot order. Returns a [[Stats]] record with counts and the restored
  * tip.
  *
  * ==WARNING: UTxO set is NOT restored correctly by this pass alone==
  *
  * `KvChainStore.appendBlock` maintains the UTxO set via per-block deltas: for each input a block
  * spends, it looks up the prior output in the local store; if absent, the delta entry is silently
  * dropped (the "Missing-input tolerance" documented in `snapshot-bootstrap-m10.md`). When the
  * restore does not start from genesis — which is the normal Mithril case, because aggregator
  * CDNs retain only a rolling window of recent immutables — every input referencing a UTxO created
  * before the earliest retained chunk is missing, so its spend is dropped. The resulting UTxO set
  * contains all outputs created within the restored range with no spend pruning, which is wrong.
  *
  * The intended snapshot-bootstrap flow — not implemented yet — is:
  *
  *   1. Parse the ancillary `ledger/` directory's `NewEpochState` CBOR into
  *      `Iterator[(TransactionInput, TransactionOutput)]`.
  *   2. Call [[ChainStoreUtxoSet.restoreUtxoSet]] to seed the authoritative UTxO set at the
  *      snapshot's tip.
  *   3. *Then* run this restorer to populate the block history for checkpoint-replay coverage.
  *
  * Without step 1, the block history loaded here is still useful for
  * [[scalus.cardano.node.stream.engine.replay.ChainStoreReplaySource]] (checkpoint replay only
  * reads blocks, never the UTxO set), but Heavy-mode `findUtxos` queries will return a
  * silently-wrong set.
  *
  * Use directly (without step 1) only when:
  *   - You are starting from genesis (full mainnet Mithril with complete chunk retention, or a
  *     local full-history archive). In that case `appendBlock` sees every input's creation first
  *     and the delta machinery is correct; or
  *   - You only need the block history (replay), not the UTxO set.
  *
  * Not crash-safe across partial runs — if a restore fails mid-way, the store has a partial history
  * and needs to be wiped before a fresh attempt. Callers restore into an empty store.
  */
final class ImmutableDbRestorer(store: ChainStore) {

    def restore(
        immutableDir: Path,
        onProgress: ImmutableDbRestorer.Progress => Unit = _ => ()
    ): ImmutableDbRestorer.Stats = {
        val reader = new ImmutableDbReader(immutableDir)
        val chunks = reader.chunkNumbers
        require(chunks.nonEmpty, s"no immutable chunks found in $immutableDir")

        val started = System.nanoTime()
        var totalBlocks = 0L
        var totalBytes = 0L
        var skippedByron = 0L
        var lastTip: Option[ChainTip] = None

        for (chunkNo, chunkIdx) <- chunks.zipWithIndex do {
            var chunkBlocks = 0L
            reader.readChunk(chunkNo).foreach { raw =>
                HfcDiskBlockDecoder.decode(raw) match {
                    case Right(decoded) =>
                        val block = decoded.block.value
                        val tip = ChainTip(
                          point = ChainPoint(
                            slot = decoded.slot,
                            blockHash = BlockHash.fromByteString(
                              ByteString.fromArray(decoded.headerHash)
                            )
                          ),
                          blockNo = block.header.blockNumber
                        )
                        store.appendBlock(AppliedBlock.fromRaw(tip, decoded.block))
                        lastTip = Some(tip)
                        totalBlocks += 1
                        totalBytes += raw.blockBytes.length
                        chunkBlocks += 1
                    case Left(HfcDiskBlockDecoder.Error.ByronEra(_)) =>
                        // Byron chunks don't fit `scalus-cardano-ledger`'s Babbage+ block shape;
                        // skipping them is consistent with the project scope, not a silent error
                        // (they're counted so the caller can log).
                        skippedByron += 1
                    case Left(err) =>
                        throw new ImmutableDbRestorer.RestoreError(
                          s"decode failed in chunk=$chunkNo slot=${raw.slot}: $err"
                        )
                }
            }
            onProgress(
              ImmutableDbRestorer.Progress(
                chunkNo = chunkNo,
                chunkIndex = chunkIdx,
                totalChunks = chunks.size,
                blocksInChunk = chunkBlocks,
                cumulativeBlocks = totalBlocks,
                cumulativeBytes = totalBytes
              )
            )
        }

        val elapsed = (System.nanoTime() - started) / 1_000_000L
        ImmutableDbRestorer.Stats(
          chunksProcessed = chunks.size,
          blocksApplied = totalBlocks,
          bytesApplied = totalBytes,
          skippedByron = skippedByron,
          tip = lastTip,
          elapsedMillis = elapsed
        )
    }
}

object ImmutableDbRestorer {

    /** Per-chunk progress event. Fires after the chunk has been fully applied to the store. */
    final case class Progress(
        chunkNo: Int,
        chunkIndex: Int,
        totalChunks: Int,
        blocksInChunk: Long,
        cumulativeBlocks: Long,
        cumulativeBytes: Long
    )

    final case class Stats(
        chunksProcessed: Int,
        blocksApplied: Long,
        bytesApplied: Long,
        skippedByron: Long,
        tip: Option[ChainTip],
        elapsedMillis: Long
    )

    /** Thrown on a LedgerDecode / Malformed error — not on Byron, which is counted and skipped. */
    final class RestoreError(msg: String) extends RuntimeException(msg)
}
