package scalus.cardano.node.stream.engine.snapshot

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.node.stream.engine.EngineTestFixtures

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}

import ChainStoreSnapshot.{ContentFlag, Header, Record}
import EngineTestFixtures.*

class SnapshotRoundTripSuite extends AnyFunSuite {

    private def mkHeader(
        blockCount: Long = 0L,
        utxoCount: Long = 0L,
        flags: Int = ContentFlag.Blocks | ContentFlag.UtxoSet
    ): Header =
        Header(
          schemaVersion = ChainStoreSnapshot.SchemaVersion,
          networkMagic = 42L,
          tip = tip(10L, blockNo = 10L),
          blockCount = blockCount,
          utxoCount = utxoCount,
          contentFlags = flags
        )

    private def roundTrip(
        header: Header,
        records: Seq[Record]
    ): (Header, Seq[Record]) = {
        val baos = new ByteArrayOutputStream()
        val writer = SnapshotWriter(baos)
        writer.writeHeader(header)
        records.foreach(writer.writeRecord)
        writer.finish()
        writer.close()

        val reader = SnapshotReader(new ByteArrayInputStream(baos.toByteArray))
        val h = reader.readHeader()
        val body = reader.records().toList
        reader.finish()
        reader.close()
        (h, body)
    }

    test("empty snapshot round-trips") {
        val (h, body) = roundTrip(mkHeader(), Seq.empty)
        assert(h.schemaVersion == ChainStoreSnapshot.SchemaVersion)
        assert(body.isEmpty)
    }

    test("blocks-only snapshot round-trips in chain order") {
        val blocks = (1L to 3L).map(n => block(n, tx(100 + n)))
        val records = blocks.map(Record.BlockRecord.apply)
        val (_, body) = roundTrip(mkHeader(blockCount = blocks.size), records)
        val got = body.collect { case Record.BlockRecord(b) => b.point }
        assert(got == blocks.map(_.point))
    }

    test("utxo-only snapshot round-trips") {
        val entries = (1 to 5).map { n =>
            Record.UtxoEntry(input(100L + n, 0), output(addressA, n.toLong))
        }
        val (_, body) = roundTrip(mkHeader(utxoCount = entries.size), entries)
        assert(body == entries)
    }

    test("mixed snapshot (blocks + utxos) round-trips and preserves order") {
        val mixed: Seq[Record] = Seq(
          Record.BlockRecord(block(1, tx(101))),
          Record.UtxoEntry(input(101L, 0), output(addressA, 1L)),
          Record.BlockRecord(block(2, tx(102))),
          Record.UtxoEntry(input(102L, 0), output(addressB, 2L))
        )
        val (_, body) = roundTrip(mkHeader(blockCount = 2L, utxoCount = 2L), mixed)
        assert(body == mixed)
    }

    test("reader refuses an unsupported schema version") {
        val baos = new ByteArrayOutputStream()
        val writer = SnapshotWriter(baos)
        writer.writeHeader(mkHeader().copy(schemaVersion = 999))
        writer.finish()
        writer.close()
        val reader = SnapshotReader(new ByteArrayInputStream(baos.toByteArray))
        val err = intercept[SnapshotError.SnapshotSchemaMismatch](reader.readHeader())
        assert(err.actual == 999)
    }

    test("reader refuses a truncated snapshot (missing footer)") {
        val baos = new ByteArrayOutputStream()
        val writer = SnapshotWriter(baos)
        writer.writeHeader(mkHeader(blockCount = 1L))
        writer.writeRecord(Record.BlockRecord(block(1, tx(101))))
        // Do NOT call finish() — stream ends without footer. Close the underlying stream by
        // draining the writer's finish() but we cheat by just closing baos.
        baos.close()
        // Take the bytes-without-footer (truncate last few bytes to simulate real truncation).
        val bytes = baos.toByteArray
        val truncated = bytes.take(bytes.length - 1) // arbitrary truncation
        val reader = SnapshotReader(new ByteArrayInputStream(truncated))
        // Any body-iteration attempt fails on SnapshotCorrupted or EOF.
        reader.readHeader()
        val it = reader.records()
        val err = intercept[Throwable](it.toList)
        assert(
          err.isInstanceOf[SnapshotError.SnapshotCorrupted] ||
              err.isInstanceOf[java.io.EOFException]
        )
    }

    test("reader detects sha256 corruption in the footer") {
        val baos = new ByteArrayOutputStream()
        val writer = SnapshotWriter(baos)
        writer.writeHeader(mkHeader(blockCount = 1L))
        writer.writeRecord(Record.BlockRecord(block(1, tx(101))))
        writer.finish()
        writer.close()
        val bytes = baos.toByteArray
        // Flip a byte near the end — the footer carries the sha256 and sentinel; flipping a
        // byte inside the sha256 region must trip the hash check.
        bytes(bytes.length - 10) = (bytes(bytes.length - 10) ^ 0xff).toByte
        val reader = SnapshotReader(new ByteArrayInputStream(bytes))
        reader.readHeader()
        val _ = reader.records().toList
        val err = intercept[SnapshotError.SnapshotCorrupted](reader.finish())
        assert(err.getMessage.contains("sha256") || err.getMessage.contains("sentinel"))
    }

    test("writer requires writeHeader before writeRecord") {
        val baos = new ByteArrayOutputStream()
        val writer = SnapshotWriter(baos)
        val err = intercept[IllegalArgumentException](
          writer.writeRecord(Record.BlockRecord(block(1)))
        )
        assert(err.getMessage.contains("writeHeader"))
    }
}
