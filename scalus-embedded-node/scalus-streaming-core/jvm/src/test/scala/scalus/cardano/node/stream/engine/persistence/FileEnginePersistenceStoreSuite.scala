package scalus.cardano.node.stream.engine.persistence

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.CardanoInfo
import scalus.cardano.node.stream.{ChainPoint, ChainTip}
import scalus.cardano.node.stream.engine.{Bucket, Engine, EngineTestFixtures, UtxoKey}

import java.nio.ByteBuffer
import java.nio.file.{Files, Path, StandardOpenOption}
import scala.concurrent.duration.*
import scala.concurrent.{Await, ExecutionContext}

import EngineTestFixtures.*

class FileEnginePersistenceStoreSuite extends AnyFunSuite {

    private given ExecutionContext = ExecutionContext.global
    private val timeout: FiniteDuration = 5.seconds
    private val ci = CardanoInfo.preview

    private def withTempDir[A](f: Path => A): A = {
        val dir = Files.createTempDirectory("scalus-stream-test-")
        try f(dir)
        finally {
            // Recursively delete for cleanup.
            Files
                .walk(dir)
                .sorted(java.util.Comparator.reverseOrder())
                .forEach(p => { val _ = Files.deleteIfExists(p) })
        }
    }

    test("first-run load returns None") {
        withTempDir { dir =>
            val store = FileEnginePersistenceStore.file(dir, "test.app")
            try assert(Await.result(store.load(), timeout).isEmpty)
            finally Await.result(store.close(), timeout)
        }
    }

    test("append + close + reopen yields the same journal") {
        withTempDir { dir =>
            val appId = "roundtrip"
            val store1 = FileEnginePersistenceStore.file(dir, appId)
            store1.appendSync(JournalRecord.OwnSubmitted(txHash(1)))
            store1.appendSync(JournalRecord.Backward(point(3)))
            Await.result(store1.flush(), timeout)
            Await.result(store1.close(), timeout)

            val store2 = FileEnginePersistenceStore.file(dir, appId)
            try {
                val loaded = Await.result(store2.load(), timeout)
                assert(loaded.isDefined)
                val journal = loaded.get.journal
                assert(journal.size == 2)
                assert(journal(0) == JournalRecord.OwnSubmitted(txHash(1)))
                assert(journal(1) == JournalRecord.Backward(point(3)))
            } finally Await.result(store2.close(), timeout)
        }
    }

    test("compact writes snapshot and truncates the log") {
        withTempDir { dir =>
            val appId = "compact"
            val store = FileEnginePersistenceStore.file(dir, appId)
            try {
                store.appendSync(JournalRecord.OwnSubmitted(txHash(42)))
                val snap = EngineSnapshotFile(
                  schemaVersion = EngineSnapshotFile.CurrentSchemaVersion,
                  appId = appId,
                  networkMagic = 1L,
                  tip = Some(ChainTip(point(5), 5L)),
                  ownSubmissions = Set(txHash(42)),
                  volatileTail = Seq.empty,
                  buckets = Map.empty
                )
                Await.result(store.compact(snap), timeout)
            } finally Await.result(store.close(), timeout)

            val reopened = FileEnginePersistenceStore.file(dir, appId)
            try {
                val loaded = Await.result(reopened.load(), timeout)
                assert(loaded.isDefined)
                // Post-compact: log is empty, snapshot carries the state.
                assert(loaded.get.journal.isEmpty)
                assert(loaded.get.snapshot.exists(_.ownSubmissions.contains(txHash(42))))
            } finally Await.result(reopened.close(), timeout)
        }
    }

    test("corrupt log tail is truncated at the last good record") {
        withTempDir { dir =>
            val appId = "corrupt"
            val store = FileEnginePersistenceStore.file(dir, appId)
            store.appendSync(JournalRecord.OwnSubmitted(txHash(10)))
            Await.result(store.flush(), timeout)
            Await.result(store.close(), timeout)

            // Append garbage: a plausible length prefix followed by random bytes the CBOR
            // decoder cannot interpret. The loader should truncate and keep the good record.
            val logPath = dir.resolve(s"$appId.log")
            val ch = java.nio.channels.FileChannel.open(logPath, StandardOpenOption.APPEND)
            try {
                val bb = ByteBuffer.allocate(4 + 10)
                bb.putInt(10)
                bb.put(Array.fill[Byte](10)(0xff.toByte))
                bb.flip()
                while bb.hasRemaining do { val _ = ch.write(bb) }
            } finally ch.close()

            val reopened = FileEnginePersistenceStore.file(dir, appId)
            try {
                val loaded = Await.result(reopened.load(), timeout)
                assert(loaded.isDefined)
                assert(loaded.get.journal == Seq(JournalRecord.OwnSubmitted(txHash(10))))
            } finally Await.result(reopened.close(), timeout)
        }
    }

    test("two stores on the same path — the second fails with Locked") {
        withTempDir { dir =>
            val appId = "locked"
            val first = FileEnginePersistenceStore.file(dir, appId)
            try {
                val thrown = intercept[EnginePersistenceError.Locked](
                  FileEnginePersistenceStore.file(dir, appId)
                )
                assert(thrown.getMessage.contains(appId))
            } finally Await.result(first.close(), timeout)

            // After closing, a fresh store can acquire the lock again.
            val second = FileEnginePersistenceStore.file(dir, appId)
            Await.result(second.close(), timeout)
        }
    }

    test("engine + file store round-trip through shutdown + rebuild") {
        withTempDir { dir =>
            val appId = "engine-roundtrip"
            val store1 = FileEnginePersistenceStore.file(dir, appId)
            val engine1 = new Engine(ci, None, Engine.DefaultSecurityParam, store1)
            Await.result(
              engine1.onRollForward(block(1, tx(1, producing = IndexedSeq(output(addressA, 5L))))),
              timeout
            )
            Await.result(engine1.notifySubmit(txHash(99)), timeout)
            val snap = Await.result(engine1.takeSnapshot(appId, 42L), timeout)
            Await.result(store1.compact(snap), timeout)
            Await.result(store1.close(), timeout)

            val store2 = FileEnginePersistenceStore.file(dir, appId)
            try {
                val loaded = Await.result(store2.load(), timeout)
                assert(loaded.isDefined)
                val engine2 = Await.result(
                  Engine.rebuildFrom(loaded.get, ci, None, Engine.DefaultSecurityParam, store2),
                  timeout
                )
                assert(engine2.currentTip.map(_.point) == Some(point(1)))
                val status = Await.result(engine2.txStatus(txHash(99)), timeout)
                assert(status.contains(scalus.cardano.node.TransactionStatus.Pending))
            } finally Await.result(store2.close(), timeout)
        }
    }
}
