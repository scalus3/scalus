package scalus.cardano.node.stream.engine.snapshot

import org.scalatest.concurrent.ScalaFutures
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.time.{Millis, Seconds, Span}
import scalus.cardano.node.stream.SnapshotSource
import scalus.cardano.node.stream.engine.kvstore.InMemoryKvStore
import scalus.cardano.node.stream.engine.{EngineTestFixtures, KvChainStore}
import scalus.cardano.node.{UtxoQuery, UtxoSource}

import java.io.ByteArrayOutputStream
import java.nio.file.{Files, Path}
import scala.concurrent.ExecutionContext

import ChainStoreSnapshot.{ContentFlag, Header, Record}
import EngineTestFixtures.*

class ChainStoreRestorerSuite extends AnyFunSuite with ScalaFutures {

    implicit override val patienceConfig: PatienceConfig =
        PatienceConfig(timeout = Span(5, Seconds), interval = Span(20, Millis))

    private given ExecutionContext = ExecutionContext.global

    private def writeSnapshot(
        file: Path,
        tipT: scalus.cardano.node.stream.ChainTip,
        blocks: Seq[scalus.cardano.node.stream.engine.AppliedBlock],
        utxos: Seq[
          (scalus.cardano.ledger.TransactionInput, scalus.cardano.ledger.TransactionOutput)
        ]
    ): Unit = {
        val out = Files.newOutputStream(file)
        val writer = SnapshotWriter(out)
        try {
            writer.writeHeader(
              Header(
                schemaVersion = ChainStoreSnapshot.SchemaVersion,
                networkMagic = 42L,
                tip = tipT,
                blockCount = blocks.size.toLong,
                utxoCount = utxos.size.toLong,
                contentFlags = ContentFlag.Blocks | ContentFlag.UtxoSet
              )
            )
            blocks.foreach(b => writer.writeRecord(Record.BlockRecord(b)))
            utxos.foreach((i, o) => writer.writeRecord(Record.UtxoEntry(i, o)))
            writer.finish()
        } finally writer.close()
    }

    test("restore from a File source populates blocks + utxo set + tip") {
        val dir = Files.createTempDirectory("scalus-restorer-test-")
        val file = dir.resolve("bundle.scsnap")
        val blocks =
            (1L to 3L).map(n => block(n, tx(100 + n, producing = IndexedSeq(output(addressA, n)))))
        val utxos = Seq(
          input(1000L, 0) -> output(addressA, 10L),
          input(1000L, 1) -> output(addressB, 20L)
        )
        writeSnapshot(file, blocks.last.tip, blocks, utxos)

        val store = new KvChainStore(InMemoryKvStore())
        val restorer = ChainStoreRestorer(store)
        val tip = restorer.restore(SnapshotSource.File(file)).futureValue
        assert(tip == blocks.last.tip)

        val restoredBlocks =
            store
                .blocksBetween(scalus.cardano.node.stream.ChainPoint.origin, tip.point)
                .toOption
                .get
                .toList
        assert(restoredBlocks.map(_.point) == blocks.map(_.point))

        val byA = store
            .findUtxosFromStore(UtxoQuery(UtxoSource.FromAddress(addressA)))
            .getOrElse(fail())
        assert(byA.exists((i, _) => i == input(1000L, 0)))

        // Cleanup
        Files.deleteIfExists(file)
        Files.deleteIfExists(dir)
    }

    test("Mithril source raises UnsupportedSourceException with a pointer to M10b") {
        val store = new KvChainStore(InMemoryKvStore())
        val restorer = ChainStoreRestorer(store)
        val cause = restorer
            .restore(SnapshotSource.Mithril("https://example.test", "abcd"))
            .failed
            .futureValue
        assert(cause.isInstanceOf[scalus.cardano.node.stream.UnsupportedSourceException])
        assert(cause.getMessage.contains("M10b"))
    }

    test("body sha256 mismatch surfaces as SnapshotCorrupted") {
        val dir = Files.createTempDirectory("scalus-restorer-corrupt-")
        val file = dir.resolve("bad.scsnap")
        writeSnapshot(file, tip(1L, 1L), Seq(block(1, tx(101))), Seq.empty)
        // Corrupt a byte near the end of the file.
        val bytes = Files.readAllBytes(file)
        bytes(bytes.length - 10) = (bytes(bytes.length - 10) ^ 0xff).toByte
        Files.write(file, bytes)

        val store = new KvChainStore(InMemoryKvStore())
        val restorer = ChainStoreRestorer(store)
        val cause = restorer.restore(SnapshotSource.File(file)).failed.futureValue
        assert(cause.isInstanceOf[SnapshotError.SnapshotCorrupted])

        Files.deleteIfExists(file)
        Files.deleteIfExists(dir)
    }

    test("header count mismatch surfaces as SnapshotCorrupted") {
        val dir = Files.createTempDirectory("scalus-restorer-mismatch-")
        val file = dir.resolve("mismatch.scsnap")
        val out = new ByteArrayOutputStream()
        val writer = SnapshotWriter(out)
        writer.writeHeader(
          Header(
            schemaVersion = ChainStoreSnapshot.SchemaVersion,
            networkMagic = 42L,
            tip = tip(1L, 1L),
            blockCount = 99L, // lies — we write only 1
            utxoCount = 0L,
            contentFlags = ContentFlag.Blocks
          )
        )
        writer.writeRecord(Record.BlockRecord(block(1, tx(101))))
        writer.finish()
        writer.close()
        Files.write(file, out.toByteArray)

        val store = new KvChainStore(InMemoryKvStore())
        val restorer = ChainStoreRestorer(store)
        val cause = restorer.restore(SnapshotSource.File(file)).failed.futureValue
        assert(cause.isInstanceOf[SnapshotError.SnapshotCorrupted])
        assert(cause.getMessage.contains("blockCount"))

        Files.deleteIfExists(file)
        Files.deleteIfExists(dir)
    }
}
