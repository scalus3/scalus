package scalus.cardano.node.stream.engine.snapshot.ledgerstate

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.Address
import scalus.cardano.ledger.{BlockHash, TransactionHash, TransactionInput, TransactionOutput, Value}
import scalus.cardano.node.{UtxoQuery, UtxoSource}
import scalus.cardano.node.stream.engine.KvChainStore
import scalus.cardano.node.stream.engine.kvstore.InMemoryKvStore
import scalus.cardano.node.stream.engine.snapshot.TestUtils
import scalus.cardano.node.stream.{ChainPoint, ChainTip}
import scalus.uplc.builtin.ByteString

import java.io.{ByteArrayOutputStream, FileOutputStream}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

/** End-to-end: build a synthetic `ledger/<slot>/{meta, state, tables/tvar}` directory where the
  * `tables/tvar` file is a valid CBOR list-of-length-1 wrapping a finite map of MemPack-packed
  * UTxO entries. Restore into a `KvChainStore(InMemoryKvStore())` and assert the UTxO set
  * matches what we wrote.
  */
final class LedgerStateRestorerSuite extends AnyFunSuite {

    private val TipSlot: Long = 12345L
    private val ShelleyEnterpriseAddrBech =
        "addr_test1vzpwq95z3xyum8vqndgdd9mdnmafh3djcxnc6jemlgdmswcve6tkw"

    test("synthetic ledger-state: round-trip three UTxOs via LedgerStateRestorer") {
        val tmp = Files.createTempDirectory("ledgerstate-restore-")
        try {
            val ledgerDir = tmp.resolve("ledger")
            val slotDir = ledgerDir.resolve(TipSlot.toString)
            Files.createDirectories(slotDir)

            // meta: must say backend=utxohd-mem
            Files.write(
              slotDir.resolve("meta"),
              """{"backend":"utxohd-mem","checksum":0,"tablesCodecVersion":1}""".getBytes(
                StandardCharsets.UTF_8
              )
            )
            // state: opaque — restorer doesn't parse it
            Files.write(slotDir.resolve("state"), Array[Byte](0x00))

            val addr = Address.fromBech32(ShelleyEnterpriseAddrBech)

            // Three synthetic UTxOs — distinct txIds, ascending.
            val txIn1 = mkTxIn(0x01.toByte, 0)
            val txIn2 = mkTxIn(0x02.toByte, 7)
            val txIn3 = mkTxIn(0x03.toByte, 42)
            val out1 = TransactionOutput.Shelley(addr, Value.lovelace(1_000_000L), datumHash = None)
            val out2 = TransactionOutput.Shelley(addr, Value.lovelace(2_500_000L), datumHash = None)
            val out3 = TransactionOutput.Shelley(addr, Value.lovelace(42_000L), datumHash = None)

            writeTablesFile(
              slotDir.resolve("tables"),
              Seq(txIn1 -> out1, txIn2 -> out2, txIn3 -> out3)
            )

            val tip = ChainTip(
              ChainPoint(TipSlot, BlockHash.fromByteString(ByteString.fromArray(new Array[Byte](32)))),
              blockNo = 7L
            )

            val store = new KvChainStore(InMemoryKvStore())
            try {
                val progress = scala.collection.mutable.ArrayBuffer
                    .empty[LedgerStateRestorer.Progress]
                val stats =
                    new LedgerStateRestorer(store).restore(
                      ledgerDir,
                      expectedTip = tip,
                      onProgress = p => progress += p
                    )
                assert(stats.utxosRestored == 3L)
                assert(stats.expectedCount == 3L)
                assert(stats.tip == tip)
                assert(progress.nonEmpty && progress.last.utxosApplied == 3L)

                // Store tip reflects the restored point.
                assert(store.tip.contains(tip))

                // findUtxosFromStore answers queries against restored entries.
                val q = UtxoQuery.Simple(source = UtxoSource.FromInputs(Set(txIn1, txIn3)))
                val found = store.findUtxosFromStore(q).getOrElse(Map.empty)
                assert(found(txIn1) == out1)
                assert(found(txIn3) == out3)
                assert(!found.contains(txIn2), "query didn't ask for txIn2")
            } finally store.close()
        } finally TestUtils.deleteRecursively(tmp)
    }

    test("rejects mismatched tip slot") {
        val tmp = Files.createTempDirectory("ledgerstate-restore-bad-")
        try {
            val ledgerDir = tmp.resolve("ledger")
            val slotDir = ledgerDir.resolve(TipSlot.toString)
            Files.createDirectories(slotDir)
            Files.write(
              slotDir.resolve("meta"),
              """{"backend":"utxohd-mem","checksum":0,"tablesCodecVersion":1}""".getBytes
            )
            Files.write(slotDir.resolve("state"), Array[Byte](0x00))
            writeTablesFile(slotDir.resolve("tables"), Seq.empty)

            val wrongSlotTip = ChainTip(
              ChainPoint(TipSlot + 1, BlockHash.fromByteString(ByteString.fromArray(new Array[Byte](32)))),
              blockNo = 0L
            )
            val store = new KvChainStore(InMemoryKvStore())
            val ex = intercept[LedgerStateRestorer.RestoreError](
              new LedgerStateRestorer(store).restore(ledgerDir, wrongSlotTip)
            )
            assert(ex.getMessage.contains("slot"))
            store.close()
        } finally TestUtils.deleteRecursively(tmp)
    }

    test("rejects backend != utxohd-mem") {
        val tmp = Files.createTempDirectory("ledgerstate-bad-backend-")
        try {
            val ledgerDir = tmp.resolve("ledger")
            val slotDir = ledgerDir.resolve(TipSlot.toString)
            Files.createDirectories(slotDir)
            Files.write(
              slotDir.resolve("meta"),
              """{"backend":"utxohd-lmdb","checksum":0,"tablesCodecVersion":1}""".getBytes
            )
            Files.write(slotDir.resolve("state"), Array[Byte](0x00))
            writeTablesFile(slotDir.resolve("tables"), Seq.empty)

            val tip = ChainTip(
              ChainPoint(TipSlot, BlockHash.fromByteString(ByteString.fromArray(new Array[Byte](32)))),
              blockNo = 0L
            )
            val store = new KvChainStore(InMemoryKvStore())
            val ex = intercept[LedgerStateRestorer.RestoreError](
              new LedgerStateRestorer(store).restore(ledgerDir, tip)
            )
            assert(ex.getMessage.contains("utxohd"))
            store.close()
        } finally TestUtils.deleteRecursively(tmp)
    }

    test("rejects legacy flat-file ledger layout") {
        val tmp = Files.createTempDirectory("ledgerstate-legacy-")
        try {
            val ledgerDir = tmp.resolve("ledger")
            Files.createDirectories(ledgerDir)
            // Pre-UTxO-HD legacy: a single file named with the slot number
            Files.write(ledgerDir.resolve(TipSlot.toString), Array[Byte](0x00))

            val tip = ChainTip(
              ChainPoint(TipSlot, BlockHash.fromByteString(ByteString.fromArray(new Array[Byte](32)))),
              blockNo = 0L
            )
            val store = new KvChainStore(InMemoryKvStore())
            val ex = intercept[UnsupportedLedgerSnapshotFormat](
              new LedgerStateRestorer(store).restore(ledgerDir, tip)
            )
            assert(ex.layout.isInstanceOf[LedgerStateLayout.LegacyPreUtxoHd])
            assert(ex.getMessage.contains("legacy"))
            store.close()
        } finally TestUtils.deleteRecursively(tmp)
    }

    // -----------------------------------------------------------------------
    // Fixture helpers

    private def mkTxIn(firstByte: Byte, index: Int): TransactionInput = {
        val hashBytes = new Array[Byte](32)
        hashBytes(0) = firstByte
        TransactionInput(TransactionHash.fromArray(hashBytes), index)
    }

    /** Produce a `tables` file mirroring the bulk-snapshot writer
      * (`ouroboros-consensus/.../LedgerDB/V2/InMemory.hs` `implTakeHandleSnapshot`):
      * `[1, mapHeader(n), (CBOR-bytes(memPack(TxIn)), CBOR-bytes(memPack(TxOut)))*]`.
      *
      * Hand-written byte layout so we don't need a Borer encoder for the outer wrapper.
      */
    private def writeTablesFile(
        path: Path,
        entries: Seq[(TransactionInput, TransactionOutput)]
    ): Unit = {
        val buf = new ByteArrayOutputStream()
        // Outer: CBOR array header of length 1 = major type 4 (array), small-count encoding:
        //   0x80 | 1 = 0x81
        buf.write(0x81)
        // Map header of size n. If n < 24 → 0xA0 | n. Our fixtures stay small.
        require(entries.size < 24, "fixture map size must be < 24 for single-byte map header")
        buf.write(0xa0 | entries.size)
        entries.foreach { case (txIn, txOut) =>
            writeCborBytes(buf, encodeTxInMemPack(txIn))
            writeCborBytes(buf, encodeTxOutMemPack(txOut))
        }
        val os = new FileOutputStream(path.toFile)
        try os.write(buf.toByteArray)
        finally os.close()
    }

    /** Write a CBOR `bytes` item. For payloads < 24 bytes: single-byte header 0x40 | len.
      * Otherwise use 0x58 + u8 len, or 0x59 + u16 len (BE), etc.
      */
    private def writeCborBytes(out: ByteArrayOutputStream, payload: Array[Byte]): Unit = {
        val len = payload.length
        if len < 24 then out.write(0x40 | len)
        else if len < 0x100 then { out.write(0x58); out.write(len & 0xff) }
        else if len < 0x10000 then {
            out.write(0x59)
            out.write((len >>> 8) & 0xff)
            out.write(len & 0xff)
        } else {
            out.write(0x5a)
            out.write((len >>> 24) & 0xff)
            out.write((len >>> 16) & 0xff)
            out.write((len >>> 8) & 0xff)
            out.write(len & 0xff)
        }
        out.write(payload)
    }

    private def encodeTxInMemPack(txIn: TransactionInput): Array[Byte] = {
        // TxIn MemPack = 32 raw hash bytes + Word16 LE (txIx).
        val buf = new ByteArrayOutputStream(34)
        buf.write(txIn.transactionId.bytes.toArray)
        val ix = txIn.index
        buf.write(ix & 0xff)
        buf.write((ix >>> 8) & 0xff)
        buf.toByteArray
    }

    private def encodeTxOutMemPack(txOut: TransactionOutput): Array[Byte] = {
        // Only tag-0 (TxOutCompact' = CompactAddr + ada-only CompactValue) is built here — the
        // synthetic fixture UTxOs are Shelley enterprise + ada-only.
        val buf = new ByteArrayOutputStream()
        buf.write(0x00) // tag 0
        txOut match {
            case TransactionOutput.Shelley(addr, value, None) =>
                val addrBytes = addr.toBytes.bytes
                writeVarLen(buf, addrBytes.length.toLong) // CompactAddr Length prefix
                buf.write(addrBytes)
                buf.write(0x00) // CompactValue ada-only tag
                writeVarLen(buf, value.coin.value)
            case other =>
                fail(s"fixture encoder only supports Shelley + ada-only, got $other")
        }
        buf.toByteArray
    }

    private def writeVarLen(out: ByteArrayOutputStream, value: Long): Unit = {
        require(value >= 0)
        val bits = if value == 0 then 1 else 64 - java.lang.Long.numberOfLeadingZeros(value)
        val groups = (bits + 6) / 7
        var i = groups - 1
        while i >= 0 do {
            val groupVal = ((value >>> (i * 7)) & 0x7fL).toInt
            if i == 0 then out.write(groupVal)
            else out.write(groupVal | 0x80)
            i -= 1
        }
    }

}
