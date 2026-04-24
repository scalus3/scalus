package scalus.cardano.node.stream.engine.snapshot

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.Address
import scalus.cardano.ledger.{
    TransactionHash,
    TransactionInput,
    TransactionOutput,
    Value
}
import scalus.cardano.node.{UtxoQuery, UtxoSource}
import scalus.cardano.node.stream.engine.KvChainStore
import scalus.cardano.node.stream.engine.kvstore.InMemoryKvStore
import scalus.cardano.node.stream.engine.snapshot.immutabledb.{
    ImmutableDbReader,
    ImmutableDbRealFixtureSuite
}

import java.io.{ByteArrayOutputStream, FileOutputStream}
import java.nio.file.{Files, Path}

/** End-to-end: stage the real-fixture `immutable/` chunks alongside a hand-built
  * `ledger/<slot>/` directory whose tip matches the ImmutableDB's last block. Run
  * [[SnapshotDirRestorer]] and assert the UTxO set matches what we seeded (not the
  * delta-derived set that `ImmutableDbRestorer` alone would have produced).
  */
final class SnapshotDirRestorerSuite extends AnyFunSuite {

    private val ShelleyEnterpriseAddrBech =
        "addr_test1vzpwq95z3xyum8vqndgdd9mdnmafh3djcxnc6jemlgdmswcve6tkw"

    test("snapshot dir restore: immutable blocks + ledger UTxO set at matching tip") {
        val (immutableDir, _) = ImmutableDbRealFixtureSuite.stageFixture()
        val snapshotRoot = immutableDir.getParent
        try {
            // Derive the ImmutableDB tip slot from the last block so we can name the ledger
            // dir to match; the restorer cross-checks these.
            val lastSlot = new ImmutableDbReader(immutableDir).blocks().map(_.slot).max
            val ledgerSlotDir = snapshotRoot.resolve("ledger").resolve(lastSlot.toString)
            val tablesFile = ledgerSlotDir.resolve("tables")
            Files.createDirectories(ledgerSlotDir)
            Files.write(
              ledgerSlotDir.resolve("meta"),
              """{"backend":"utxohd-mem","checksum":0,"tablesCodecVersion":1}""".getBytes
            )
            Files.write(ledgerSlotDir.resolve("state"), Array[Byte](0x00))

            val addr = Address.fromBech32(ShelleyEnterpriseAddrBech)
            val seededUtxos = Seq(
              mkTxIn(0x11.toByte, 0) -> TransactionOutput
                  .Shelley(addr, Value.lovelace(123_456_789L), datumHash = None),
              mkTxIn(0x22.toByte, 3) -> TransactionOutput
                  .Shelley(addr, Value.lovelace(100L), datumHash = None)
            )
            writeTablesFile(tablesFile, seededUtxos)

            val store = new KvChainStore(InMemoryKvStore())
            try {
                val stats = new SnapshotDirRestorer(store).restore(snapshotRoot)

                assert(stats.blocks.blocksApplied > 0, s"blocks: ${stats.blocks}")
                assert(stats.blocks.tip.exists(_.point.slot == lastSlot))
                assert(stats.utxos.utxosRestored == seededUtxos.size)
                assert(stats.utxos.tip == stats.blocks.tip.get)

                // Store tip reflects the post-restore point (same as block tip since the ledger
                // state restore pins its tip to the caller-supplied ImmutableDB tip).
                assert(store.tip == stats.blocks.tip)

                // UTxO queries see ONLY the seeded entries — the delta-derived set from the
                // immutable-block replay was wiped by `restoreUtxoSet`.
                val (seededIn1, seededOut1) = seededUtxos.head
                val (seededIn2, seededOut2) = seededUtxos.last
                val q = UtxoQuery.Simple(
                  source = UtxoSource.FromInputs(Set(seededIn1, seededIn2))
                )
                val found = store.findUtxosFromStore(q).getOrElse(Map.empty)
                assert(found.get(seededIn1).contains(seededOut1))
                assert(found.get(seededIn2).contains(seededOut2))
            } finally store.close()
        } finally ImmutableDbRealFixtureSuite.cleanup(snapshotRoot)
    }

    test("fails loud when `immutable/` is missing") {
        val tmp = Files.createTempDirectory("snap-missing-immutable-")
        try {
            Files.createDirectories(tmp.resolve("ledger"))
            val store = new KvChainStore(InMemoryKvStore())
            val ex = intercept[SnapshotDirRestorer.RestoreError](
              new SnapshotDirRestorer(store).restore(tmp)
            )
            assert(ex.getMessage.contains("immutable"))
            store.close()
        } finally TestUtils.deleteRecursively(tmp)
    }

    test("fails loud when `ledger/` is missing") {
        val tmp = Files.createTempDirectory("snap-missing-ledger-")
        try {
            Files.createDirectories(tmp.resolve("immutable"))
            val store = new KvChainStore(InMemoryKvStore())
            val ex = intercept[SnapshotDirRestorer.RestoreError](
              new SnapshotDirRestorer(store).restore(tmp)
            )
            assert(ex.getMessage.contains("ledger"))
            store.close()
        } finally TestUtils.deleteRecursively(tmp)
    }

    // -----------------------------------------------------------------------
    // Fixture helpers (mirror the ledger-state suite; we don't reuse its private helpers
    // because the two suites live in different packages and this one is a one-off).

    private def mkTxIn(firstByte: Byte, index: Int): TransactionInput = {
        val hashBytes = new Array[Byte](32)
        hashBytes(0) = firstByte
        TransactionInput(TransactionHash.fromArray(hashBytes), index)
    }

    private def writeTablesFile(
        path: Path,
        entries: Seq[(TransactionInput, TransactionOutput)]
    ): Unit = {
        val buf = new ByteArrayOutputStream()
        buf.write(0x81) // CBOR array header of length 1
        require(entries.size < 24, "fixture size must be < 24 for single-byte map header")
        buf.write(0xa0 | entries.size)
        entries.foreach { case (txIn, txOut) =>
            writeCborBytes(buf, encodeTxInMemPack(txIn))
            writeCborBytes(buf, encodeTxOutMemPack(txOut))
        }
        val os = new FileOutputStream(path.toFile)
        try os.write(buf.toByteArray)
        finally os.close()
    }

    private def encodeTxInMemPack(txIn: TransactionInput): Array[Byte] = {
        val buf = new ByteArrayOutputStream(34)
        buf.write(txIn.transactionId.bytes.toArray)
        val ix = txIn.index
        buf.write(ix & 0xff)
        buf.write((ix >>> 8) & 0xff)
        buf.toByteArray
    }

    /** Tag 0 only: Shelley + ada-only. Matches what the orchestrator test seeds. */
    private def encodeTxOutMemPack(txOut: TransactionOutput): Array[Byte] = {
        val buf = new ByteArrayOutputStream()
        buf.write(0x00)
        txOut match {
            case TransactionOutput.Shelley(addr, value, None) =>
                val addrBytes = addr.toBytes.bytes
                writeVarLen(buf, addrBytes.length.toLong)
                buf.write(addrBytes)
                buf.write(0x00)
                writeVarLen(buf, value.coin.value)
            case other => fail(s"fixture encoder only supports Shelley + ada-only, got $other")
        }
        buf.toByteArray
    }

    private def writeCborBytes(out: ByteArrayOutputStream, payload: Array[Byte]): Unit = {
        val len = payload.length
        if len < 24 then out.write(0x40 | len)
        else if len < 0x100 then { out.write(0x58); out.write(len & 0xff) }
        else if len < 0x10000 then {
            out.write(0x59); out.write((len >>> 8) & 0xff); out.write(len & 0xff)
        } else {
            out.write(0x5a)
            out.write((len >>> 24) & 0xff)
            out.write((len >>> 16) & 0xff)
            out.write((len >>> 8) & 0xff)
            out.write(len & 0xff)
        }
        out.write(payload)
    }

    private def writeVarLen(out: ByteArrayOutputStream, value: Long): Unit = {
        require(value >= 0)
        val bits = if value == 0 then 1 else 64 - java.lang.Long.numberOfLeadingZeros(value)
        val groups = (bits + 6) / 7
        var i = groups - 1
        while i >= 0 do {
            val groupVal = ((value >>> (i * 7)) & 0x7fL).toInt
            if i == 0 then out.write(groupVal) else out.write(groupVal | 0x80)
            i -= 1
        }
    }
}
