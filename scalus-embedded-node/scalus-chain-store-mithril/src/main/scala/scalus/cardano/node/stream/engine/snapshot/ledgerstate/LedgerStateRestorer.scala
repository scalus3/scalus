package scalus.cardano.node.stream.engine.snapshot.ledgerstate

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, readFromArray}
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}
import scalus.cardano.node.stream.ChainTip
import scalus.cardano.node.stream.engine.{ChainStore, ChainStoreUtxoSet}

import java.io.{BufferedInputStream, FileInputStream}
import java.nio.file.{Files, Path}

/** Drive a UTxO-HD V2 InMemory `ledger/<slot>/` directory into a [[ChainStoreUtxoSet]].
  *
  * The restorer owns the end-to-end flow that makes Heavy-mode `findUtxos` answerable from the
  * local store:
  *   1. Sniff the `ledger/` directory via [[LedgerStateLayout.highestSlotIn]]; fail loud with
  *      [[UnsupportedLedgerSnapshotFormat]] on a legacy or LSM layout.
  *   2. Verify the `meta` file's `backend` is `"utxohd-mem"` — guards against an InMemory-shaped
  *      dir whose metadata says otherwise (corrupt download, version mismatch).
  *   3. Cross-check the caller-supplied tip's slot against the directory name (cardano-node
  *      always names the directory by the snapshot slot).
  *   4. Open `tables/tvar`, stream `(TxIn, TxOut)` pairs through [[TvarTableDecoder]], and
  *      forward them to `store.restoreUtxoSet(tip, iter)`.
  *
  * The caller supplies the tip (slot + block hash + block number) because extracting it from
  * the `state` file would require decoding the outer `ExtLedgerState` CBOR — a big detour for
  * information the caller already has from the paired `ImmutableDbRestorer` pass. The restorer
  * still verifies the slot against the on-disk directory name so a mistyped tip doesn't
  * silently produce a bad store.
  *
  * ==Not idempotent across partial runs==
  *
  * `ChainStoreUtxoSet.restoreUtxoSet` is not required to be atomic (the trait docs call out
  * that mainnet-sized UTxO sets span many write batches). A crash mid-restore leaves the store
  * in a partially-populated state; callers should treat that as "wipe and retry" — on the next
  * cold start the tip is absent, which triggers a fresh bootstrap.
  */
final class LedgerStateRestorer(store: ChainStore & ChainStoreUtxoSet) {

    def restore(
        ledgerDir: Path,
        expectedTip: ChainTip,
        onProgress: LedgerStateRestorer.Progress => Unit = _ => ()
    ): LedgerStateRestorer.Stats = {
        val layout = LedgerStateLayout
            .highestSlotIn(ledgerDir)
            .getOrElse(
              throw new LedgerStateRestorer.RestoreError(
                s"ledger directory $ledgerDir contains no recognisable snapshot entries"
              )
            )

        layout match {
            case LedgerStateLayout.InMemoryV2(dir, slot) =>
                restoreInMemory(dir, slot, expectedTip, onProgress)
            case unsupported =>
                throw UnsupportedLedgerSnapshotFormat(unsupported)
        }
    }

    private def restoreInMemory(
        dir: Path,
        dirSlot: Long,
        expectedTip: ChainTip,
        onProgress: LedgerStateRestorer.Progress => Unit
    ): LedgerStateRestorer.Stats = {
        if dirSlot != expectedTip.point.slot then
            throw new LedgerStateRestorer.RestoreError(
              s"ledger dir slot $dirSlot does not match expected tip slot ${expectedTip.point.slot}"
            )

        verifyMetaBackend(dir.resolve(LedgerStateLayout.InMemoryMeta))

        val tablesPath = dir.resolve(LedgerStateLayout.InMemoryTables)
        val started = System.nanoTime()
        var applied: Long = 0L
        val handle = TvarTableDecoder.stream(
          new BufferedInputStream(new FileInputStream(tablesPath.toFile), 128 * 1024)
        )
        try {
            val instrumented = new Iterator[(scalus.cardano.ledger.TransactionInput, scalus.cardano.ledger.TransactionOutput)] {
                override def hasNext: Boolean = handle.iterator.hasNext
                override def next(): (scalus.cardano.ledger.TransactionInput, scalus.cardano.ledger.TransactionOutput) = {
                    val kv = handle.iterator.next()
                    applied += 1
                    if (applied & 0xfff) == 0 then
                        onProgress(LedgerStateRestorer.Progress(applied, handle.expectedCount))
                    kv
                }
            }
            store.restoreUtxoSet(expectedTip, instrumented)
        } finally handle.close()

        onProgress(LedgerStateRestorer.Progress(applied, handle.expectedCount))
        val elapsed = (System.nanoTime() - started) / 1_000_000L
        LedgerStateRestorer.Stats(
          utxosRestored = applied,
          expectedCount = handle.expectedCount,
          tip = expectedTip,
          elapsedMillis = elapsed
        )
    }

    /** The `meta` file is JSON: `{"backend": "utxohd-mem"|"utxohd-lmdb"|"utxohd-lsm",
      * "checksum": <u64>, "tablesCodecVersion": 1}`. Only `backend` matters to us — the
      * checksum is verified upstream by Mithril's digest manifest.
      */
    private def verifyMetaBackend(metaFile: Path): Unit = {
        val bytes =
            try Files.readAllBytes(metaFile)
            catch
                case ex: java.io.IOException =>
                    throw new LedgerStateRestorer.RestoreError(
                      s"meta file $metaFile not readable: ${ex.getMessage}"
                    )
        val meta =
            try readFromArray[LedgerStateRestorer.MetaFile](bytes)
            catch
                case ex: RuntimeException =>
                    throw new LedgerStateRestorer.RestoreError(
                      s"meta file $metaFile is malformed JSON: ${ex.getMessage}"
                    )
        if meta.backend != "utxohd-mem" then
            throw new LedgerStateRestorer.RestoreError(
              s"meta file $metaFile declares backend=\"${meta.backend}\" but only \"utxohd-mem\" " +
                  s"is supported — aborting rather than mis-decoding the tables."
            )
    }
}

object LedgerStateRestorer {

    /** Progress tick fired every ~4096 UTxOs, once at the end. `expectedCount` is -1 for
      * indefinite-length tvar maps (rare — the bulk snapshot path always uses finite length).
      */
    final case class Progress(utxosApplied: Long, expectedCount: Long)

    final case class Stats(
        utxosRestored: Long,
        expectedCount: Long,
        tip: ChainTip,
        elapsedMillis: Long
    )

    /** Thrown for all restore-time failures that aren't classified by
      * [[UnsupportedLedgerSnapshotFormat]] — missing files, slot mismatches, malformed meta.
      */
    final class RestoreError(msg: String) extends RuntimeException(msg)

    /** Shape of the cardano-node-written `meta` file inside a UTxO-HD V2 InMemory snapshot.
      * Mirrors `ouroboros-consensus/.../LedgerDB/Snapshots.hs` `SnapshotMetadata`'s ToJSON.
      */
    private final case class MetaFile(
        backend: String,
        checksum: Long,
        tablesCodecVersion: Int
    )

    private given JsonValueCodec[MetaFile] =
        JsonCodecMaker.make(CodecMakerConfig.withSkipUnexpectedFields(true))
}
