package scalus.cardano.node.stream.engine.snapshot.ledgerstate

import java.nio.file.{Files, Path}

/** Classification of the on-disk shape of a cardano-node `db/ledger/<slot>/` directory. cardano-node
  * writes one of three layouts depending on its `LedgerDbFlavorArgs` setting (see
  * `ouroboros-consensus/.../LedgerDB/Args.hs`):
  *
  *   - [[LedgerStateLayout.InMemoryV2]]: the UTxO-HD V2 InMemory default — `meta` + `state` +
  *     `tables/tvar`. Our decoder supports only this variant today.
  *   - [[LedgerStateLayout.LegacyPreUtxoHd]]: a single flat CBOR file whose name is the
  *     slot number. Predates UTxO-HD; no aggregator produces this on a post-Chang chain.
  *   - [[LedgerStateLayout.LsmTables]]: `meta` + `state` + `tables/…` (LSM-tree files, not
  *     `tvar`). Used by stake-pool-scale deployments that can't hold the UTxO set in RAM.
  */
enum LedgerStateLayout {

    /** UTxO-HD V2 InMemory snapshot — three files under a slot-numbered directory:
      * `meta` (JSON), `state` (CBOR ExtLedgerState), and `tables` (CBOR-wrapped map of
      * MemPack-packed UTxO entries). Confirmed against real preview aggregator output at
      * `/tmp/mithrill-preview/ledger/110350526/` (2026-04-24): `tables` is a plain file, not
      * a `tables/tvar` subdir as the Mithril Rust detector might suggest — the Haskell
      * consensus write path at `LedgerDB/V2/InMemory.hs:162-168` `implTakeHandleSnapshot`
      * writes `mkFsPath [snapshotName, "tables"]` as a file.
      */
    case InMemoryV2(dir: Path, slotFromDirName: Long)

    /** Pre-UTxO-HD legacy snapshot — a single flat file at `ledger/<slot>` holding the whole
      * `ExtLedgerState` CBOR. Unsupported; our restorer rejects these.
      */
    case LegacyPreUtxoHd(file: Path, slotFromFileName: Long)

    /** UTxO-HD V1 LMDB or V2 LSM-tree snapshot — same `meta` + `state` prelude as InMemoryV2,
      * but `tables` is a directory (holding LMDB or LSM-tree files) rather than a single
      * serialised map file. Unsupported (tracked as M10d); our restorer rejects these.
      */
    case LsmTables(dir: Path, slotFromDirName: Long)
}

object LedgerStateLayout {

    /** Filenames inside a UTxO-HD InMemory snapshot directory. `tables` is a plain file here
      * (the serialised UTxO map), matching what cardano-node's Haskell write path actually
      * produces; an LMDB / LSM snapshot uses a directory of the same name instead.
      */
    val InMemoryMeta = "meta"
    val InMemoryState = "state"
    val InMemoryTables = "tables"

    /** Scan a cardano-node `ledger/` directory and return the highest-slot entry along with its
      * classification. Returns `None` if no ledger entries are present (empty `ledger/`). Does
      * not throw on unsupported variants — the caller decides whether to accept or reject them.
      */
    def highestSlotIn(ledgerDir: Path): Option[LedgerStateLayout] = {
        if !Files.isDirectory(ledgerDir) then return None
        val stream = Files.list(ledgerDir)
        try {
            val iter = stream.iterator()
            var best: Option[LedgerStateLayout] = None
            while iter.hasNext do {
                val entry = iter.next()
                classify(entry).foreach { classified =>
                    val slotA = slotOf(classified)
                    val slotB = best.map(slotOf).getOrElse(Long.MinValue)
                    if slotA > slotB then best = Some(classified)
                }
            }
            best
        } finally stream.close()
    }

    /** Classify a single `ledger/<x>` entry. Returns `None` for entries whose name doesn't
      * parse as a slot number (e.g. `README`, stray files), or whose shape doesn't match any
      * of the three recognised layouts.
      */
    def classify(entry: Path): Option[LedgerStateLayout] = {
        val name = entry.getFileName.toString
        slotFromName(name).map { slot =>
            if Files.isDirectory(entry) then classifyDir(entry, slot)
            else LedgerStateLayout.LegacyPreUtxoHd(entry, slot)
        }
    }

    /** Within a slot-numbered directory, decide between InMemoryV2 (`tables` is a file) and
      * LsmTables (`tables` is a directory of LMDB / LSM files). A directory missing `meta` or
      * `state` is treated as LSM — both get rejected at the next layer; the sniffer's job is
      * just to classify, not to validate.
      */
    private def classifyDir(dir: Path, slot: Long): LedgerStateLayout = {
        val hasMeta = Files.isRegularFile(dir.resolve(InMemoryMeta))
        val hasState = Files.isRegularFile(dir.resolve(InMemoryState))
        val tables = dir.resolve(InMemoryTables)
        val tablesIsFile = Files.isRegularFile(tables)
        if hasMeta && hasState && tablesIsFile then LedgerStateLayout.InMemoryV2(dir, slot)
        else LedgerStateLayout.LsmTables(dir, slot)
    }

    /** Parse a slot number from an entry's file name. The cardano-node convention is that
      * every legitimate `ledger/<x>` entry (file or folder) is named with a base-10 slot
      * number. Entries that don't parse as `Long` are ignored.
      */
    private def slotFromName(name: String): Option[Long] =
        try Some(java.lang.Long.parseLong(name))
        catch case _: NumberFormatException => None

    private def slotOf(layout: LedgerStateLayout): Long = layout match {
        case LedgerStateLayout.InMemoryV2(_, s)      => s
        case LedgerStateLayout.LegacyPreUtxoHd(_, s) => s
        case LedgerStateLayout.LsmTables(_, s)       => s
    }
}

/** Thrown when a ledger-state directory has a layout our restorer does not support
  * ([[LedgerStateLayout.LegacyPreUtxoHd]] or [[LedgerStateLayout.LsmTables]]). The restorer
  * fails loud rather than silently degrading — the alternative ("skip the UTxO set") would
  * hand the caller a silently-wrong Heavy-mode store.
  */
final class UnsupportedLedgerSnapshotFormat(val layout: LedgerStateLayout, msg: String)
    extends RuntimeException(msg)

object UnsupportedLedgerSnapshotFormat {
    def apply(layout: LedgerStateLayout): UnsupportedLedgerSnapshotFormat = layout match {
        case LedgerStateLayout.LegacyPreUtxoHd(file, slot) =>
            new UnsupportedLedgerSnapshotFormat(
              layout,
              s"legacy pre-UTxO-HD snapshot at slot $slot ($file) — not supported; " +
                  "aggregators serving post-Chang chains do not produce this format."
            )
        case LedgerStateLayout.LsmTables(dir, slot) =>
            new UnsupportedLedgerSnapshotFormat(
              layout,
              s"UTxO-HD LSM-tree snapshot at slot $slot ($dir) — not supported yet " +
                  "(tracked as M10d). Users can convert via " +
                  "`mithril-client tools utxo-hd snapshot-converter --target in-memory`."
            )
        case LedgerStateLayout.InMemoryV2(_, _) =>
            throw new IllegalArgumentException(
              "InMemoryV2 is supported; should never be wrapped in UnsupportedLedgerSnapshotFormat"
            )
    }
}
