package scalus.cardano.node.stream.engine.snapshot

import scalus.cardano.node.stream.engine.snapshot.immutabledb.ImmutableDbRestorer
import scalus.cardano.node.stream.engine.snapshot.ledgerstate.{
    LedgerStateLayout,
    LedgerStateRestorer,
    UnsupportedLedgerSnapshotFormat
}
import scalus.cardano.node.stream.engine.{ChainStore, ChainStoreUtxoSet}

import java.nio.file.{Files, Path}

/** Top-level orchestrator that restores a mithril-downloaded cardano-node snapshot directory
  * into a [[ChainStore]] + [[ChainStoreUtxoSet]].
  *
  * Input: a directory with `immutable/` (ImmutableDB chunks) and `ledger/<slot>/` (UTxO-HD V2
  * InMemory snapshot) subdirs — the layout Mithril produces after unpacking its V2 artefact.
  *
  * ==Tip alignment==
  *
  * cardano-node snapshots the ledger state periodically, while the ImmutableDB grows
  * continuously. A Mithril-signed artefact therefore typically has `immutable/` chunks that
  * extend a few hundred slots past the most recent `ledger/<Y>/` checkpoint. The orchestrator
  * sniffs `ledger/` first to learn Y, then caps the ImmutableDbRestorer at slot Y so the
  * block-history tip lines up with the ledger-state tip. Blocks after Y are ignored here;
  * they'll come from live N2N sync once the provider is up.
  *
  * ==Order==
  *
  *   1. Sniff `ledger/` → expected tip slot Y.
  *   2. Block-history via [[ImmutableDbRestorer]] stopped at slot Y. The UTxO-delta machinery
  *      in `KvChainStore.appendBlock` produces a silently-wrong UTxO set here — fine, step 3
  *      overwrites it.
  *   3. Ledger state via [[LedgerStateRestorer]]. Wipes the UTxO / utxo-by-key keyspaces and
  *      bulk-replaces them with the authoritative set stored in `ledger/<Y>/tables`, pinned at
  *      the tip established by step 2.
  */
final class SnapshotDirRestorer(store: ChainStore & ChainStoreUtxoSet) {

    def restore(
        snapshotDir: Path,
        onBlockProgress: ImmutableDbRestorer.Progress => Unit = _ => (),
        onUtxoProgress: LedgerStateRestorer.Progress => Unit = _ => ()
    ): SnapshotDirRestorer.Stats = {
        val immutableDir = snapshotDir.resolve("immutable")
        val ledgerDir = snapshotDir.resolve("ledger")
        if !Files.isDirectory(immutableDir) then
            throw new SnapshotDirRestorer.RestoreError(
              s"no immutable/ subdirectory under $snapshotDir"
            )
        if !Files.isDirectory(ledgerDir) then
            throw new SnapshotDirRestorer.RestoreError(
              s"no ledger/ subdirectory under $snapshotDir"
            )

        val layout = LedgerStateLayout
            .highestSlotIn(ledgerDir)
            .getOrElse(
              throw new SnapshotDirRestorer.RestoreError(
                s"ledger/ at $ledgerDir contains no recognisable snapshot entries"
              )
            )
        val expectedSlot = layout match {
            case LedgerStateLayout.InMemoryV2(_, s) => s
            case unsupported                        => throw UnsupportedLedgerSnapshotFormat(unsupported)
        }

        val started = System.nanoTime()

        val blockStats = new ImmutableDbRestorer(store).restore(
          immutableDir,
          onBlockProgress,
          stopAtSlotInclusive = Some(expectedSlot)
        )
        val tip = blockStats.tip.getOrElse(
          throw new SnapshotDirRestorer.RestoreError(
            s"ImmutableDB at $immutableDir had no decodable blocks up to slot $expectedSlot; " +
                s"cannot pin UTxO tip"
          )
        )
        if tip.point.slot != expectedSlot then
            throw new SnapshotDirRestorer.RestoreError(
              s"ImmutableDB tip slot ${tip.point.slot} did not reach the ledger-snapshot slot " +
                  s"$expectedSlot — likely a mid-chain gap; aborting rather than producing an " +
                  s"inconsistent store"
            )

        val ledgerStats =
            new LedgerStateRestorer(store).restore(ledgerDir, tip, onUtxoProgress)

        val elapsed = (System.nanoTime() - started) / 1_000_000L
        SnapshotDirRestorer.Stats(blockStats, ledgerStats, elapsed)
    }
}

object SnapshotDirRestorer {

    final case class Stats(
        blocks: ImmutableDbRestorer.Stats,
        utxos: LedgerStateRestorer.Stats,
        elapsedMillis: Long
    )

    final class RestoreError(msg: String) extends RuntimeException(msg)
}
