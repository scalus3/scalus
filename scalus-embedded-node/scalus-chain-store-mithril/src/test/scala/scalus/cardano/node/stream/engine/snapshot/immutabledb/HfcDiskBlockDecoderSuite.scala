package scalus.cardano.node.stream.engine.snapshot.immutabledb

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.node.stream.{ChainPoint, ChainTip}
import scalus.cardano.node.stream.engine.AppliedBlock
import scalus.uplc.builtin.ByteString
import scalus.cardano.ledger.BlockHash

/** Decode real preview blocks from the committed fixture end-to-end: ImmutableDB trio bytes →
  * `HfcDiskBlockDecoder.decode` → `AppliedBlock.fromRaw`.
  *
  * The three fixture chunks (22449 / 22477 / 22481) sit well past the Conway/Dijkstra hard fork on
  * preview, so era tag is 7 (Dijkstra) — this exercises the "accept any Shelley+ era" policy.
  */
final class HfcDiskBlockDecoderSuite extends AnyFunSuite {

    test("real fixture: HFC wrapper split yields era>=1 and inner block shape") {
        val (immutableDir, _) = ImmutableDbRealFixtureSuite.stageFixture()
        try {
            val reader = new ImmutableDbReader(immutableDir)
            val blocks = reader.blocks().toVector
            assert(blocks.nonEmpty)
            blocks.foreach { b =>
                HfcDiskBlockDecoder.decode(b) match {
                    case Right(d) =>
                        assert(d.era >= 1, s"Shelley+ expected, got era ${d.era}")
                        assert(d.slot == b.slot)
                        assert(d.headerHash.sameElements(b.headerHash))
                        // Shelley+ block CBOR starts with an array(5) marker (0x85).
                        assert(
                          d.block.raw(0) == 0x85.toByte,
                          f"inner not array(5): ${d.block.raw(0)}%02x"
                        )
                    case Left(err) =>
                        fail(s"decode failed for slot=${b.slot} chunk=${b.chunkNo}: $err")
                }
            }
        } finally ImmutableDbRealFixtureSuite.cleanup(immutableDir.getParent)
    }

    test("real fixture: project to AppliedBlock and assert header-hash integrity") {
        val (immutableDir, _) = ImmutableDbRealFixtureSuite.stageFixture()
        try {
            val reader = new ImmutableDbReader(immutableDir)
            val raws = reader.blocks().toVector
            val decoded = raws.map(HfcDiskBlockDecoder.decode(_).toOption.getOrElse(fail("decode")))

            decoded.foreach { d =>
                val tip = ChainTip(
                  ChainPoint(d.slot, BlockHash.fromByteString(ByteString.fromArray(d.headerHash))),
                  blockNo = 0L
                )
                val applied = AppliedBlock.fromRaw(tip, d.block)
                assert(applied.tip.point.slot == d.slot)
                // The header's inner slot field should match the secondary-reported slot — proves
                // the era-specific ledger decoder and cardano-node's index agree on slot.
                assert(
                  d.block.value.header.headerBody.slot == d.slot,
                  s"header slot ${d.block.value.header.headerBody.slot} != secondary slot ${d.slot}"
                )
            }
        } finally ImmutableDbRealFixtureSuite.cleanup(immutableDir.getParent)
    }

    test("synthetic: HFC wrapper rejects non-array leading byte") {
        val bad = Array[Byte](0x20.toByte, 0x00, 0x00)
        val fakeBlock = ImmutableDb.ImmutableBlock(
          chunkNo = 0,
          slot = 0L,
          headerHash = Array.fill(32)(0),
          blockBytes = bad
        )
        HfcDiskBlockDecoder.decode(fakeBlock) match {
            case Left(HfcDiskBlockDecoder.Error.Malformed(msg, _)) =>
                assert(msg.contains("listLen") || msg.contains("0x82"))
            case other => fail(s"expected Malformed, got $other")
        }
    }

    test("synthetic: byron era surfaces as ByronEra") {
        // Valid [listLen=2, era=0, ...dummy inner...] — we never run the ledger decoder since
        // the era==0 check comes first.
        val byron = Array[Byte](0x82.toByte, 0x00, 0x85.toByte)
        val fake = ImmutableDb.ImmutableBlock(0, 100L, Array.fill(32)(0), byron)
        HfcDiskBlockDecoder.decode(fake) match {
            case Left(HfcDiskBlockDecoder.Error.ByronEra(s)) => assert(s == 100L)
            case other => fail(s"expected ByronEra, got $other")
        }
    }
}
