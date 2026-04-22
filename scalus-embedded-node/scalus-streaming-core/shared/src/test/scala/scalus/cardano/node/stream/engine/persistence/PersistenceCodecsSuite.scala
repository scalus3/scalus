package scalus.cardano.node.stream.engine.persistence

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.node.stream.{ChainPoint, ChainTip}
import scalus.cardano.node.stream.engine.{Bucket, EngineTestFixtures, UtxoKey}

import EngineTestFixtures.*

class PersistenceCodecsSuite extends AnyFunSuite {

    test("JournalRecord.Forward round-trips through CBOR") {
        val addrKey = UtxoKey.Addr(addressA)
        val rec = JournalRecord.Forward(
          tip = ChainTip(point(5), 5L),
          txIds = Set(txHash(100), txHash(101)),
          bucketDeltas = Map(
            addrKey -> BucketDelta(
              added = Seq(
                Bucket.CreatedRec(input(100, 0), output(addressA, 10L), txHash(100))
              ),
              removed = Seq.empty
            )
          )
        )
        val bytes = PersistenceCodecs.encodeRecord(rec)
        val decoded = PersistenceCodecs.decodeRecord(bytes)
        assert(decoded == rec)
    }

    test("JournalRecord.Backward round-trips through CBOR") {
        val rec = JournalRecord.Backward(point(3))
        assert(PersistenceCodecs.decodeRecord(PersistenceCodecs.encodeRecord(rec)) == rec)
    }

    test("JournalRecord.OwnSubmitted round-trips through CBOR") {
        val rec = JournalRecord.OwnSubmitted(txHash(999))
        assert(PersistenceCodecs.decodeRecord(PersistenceCodecs.encodeRecord(rec)) == rec)
    }

    test("UtxoKey covers every variant") {
        val keys: Seq[UtxoKey] = Seq(
          UtxoKey.Addr(addressA),
          UtxoKey.Asset(policyId(7), assetName("LP")),
          UtxoKey.TxOuts(txHash(50)),
          UtxoKey.Inputs(Set(input(60, 0), input(60, 1)))
        )
        keys.foreach { k =>
            // Roundtrip by wrapping into a minimal snapshot that uses the codec path.
            val snap = EngineSnapshotFile(
              schemaVersion = EngineSnapshotFile.CurrentSchemaVersion,
              appId = "test",
              networkMagic = 1L,
              tip = None,
              ownSubmissions = Set.empty,
              volatileTail = Seq.empty,
              buckets = Map(k -> BucketState(k, Map.empty))
            )
            val decoded = PersistenceCodecs.decodeSnapshot(PersistenceCodecs.encodeSnapshot(snap))
            assert(decoded.buckets.keys.head == k, s"roundtrip failed for $k")
        }
    }

    test("EngineSnapshotFile round-trips with non-empty tail + buckets") {
        val addrKey = UtxoKey.Addr(addressA)
        val snap = EngineSnapshotFile(
          schemaVersion = EngineSnapshotFile.CurrentSchemaVersion,
          appId = "com.test.app",
          networkMagic = 42L,
          tip = Some(ChainTip(point(10), 10L)),
          ownSubmissions = Set(txHash(1), txHash(2)),
          volatileTail = Seq(
            AppliedBlockSummary(
              tip = ChainTip(point(9), 9L),
              txIds = Set(txHash(90)),
              bucketDeltas = Map(
                addrKey -> BucketDelta(
                  added = Seq(
                    Bucket.CreatedRec(input(90, 0), output(addressA, 5L), txHash(90))
                  ),
                  removed = Seq.empty
                )
              )
            )
          ),
          buckets = Map(
            addrKey -> BucketState(addrKey, Map(input(90, 0) -> output(addressA, 5L)))
          )
        )
        val bytes = PersistenceCodecs.encodeSnapshot(snap)
        val decoded = PersistenceCodecs.decodeSnapshot(bytes)
        assert(decoded == snap)
    }
}
