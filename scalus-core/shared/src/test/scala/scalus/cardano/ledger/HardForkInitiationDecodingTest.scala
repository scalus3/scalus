package scalus.cardano.ledger

import io.bullet.borer.{Cbor, Encoder, Writer}
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.{Network, StakeAddress, StakePayload}
import scalus.uplc.builtin.ByteString

import scala.util.Try

/** The protocol version of a `HardForkInitiation` must decode as the node decodes it.
  *
  * The ledger reads it with `decodeProtVer @ConwayEra` (Conway/Governance/Procedures.hs), which
  * fails when the major exceeds `succ (ProtVerHigh ConwayEra)` = 12 (BaseTypes.hs, Era.hs). The
  * minor is not checked. `conway.cddl` states the same bound: `major_protocol_version = 0 .. 12`.
  */
class HardForkInitiationDecodingTest extends AnyFunSuite {
    private def hash(byte: String, size: Int) = ByteString.fromHex(byte * size)
    private val prevActionId = GovActionId(Hash(hash("0a", 32)), 3)

    private def decode(hex: String): Try[GovAction] =
        Cbor.decode(ByteString.fromHex(hex).bytes).to[GovAction].valueTry

    private def hex(action: GovAction): String =
        ByteString.fromArray(Cbor.encode(action).toByteArray).toHex

    /** Asserts that decoding failed, and that `reason` is among the messages of the failure. */
    private def assertRejected(result: Try[?], reason: String): Unit = {
        assert(result.isFailure, s"decoded: $result")
        val messages = Iterator
            .iterate(result.failed.get)(_.getCause)
            .takeWhile(_ != null)
            .map(e => String.valueOf(e.getMessage))
            .toList
        assert(messages.exists(_.contains(reason)), messages)
    }

    private val exceeds = "Protocol version 13 exceeds the maximum expected version of 12"

    test("major 12 decodes and encodes to the same bytes") {
        // [1, null, [12, 0]]
        val bytes = "8301f6820c00"
        val action = GovAction.HardForkInitiation(None, ProtocolVersion(12, 0))
        assert(decode(bytes).get == action)
        assert(hex(action) == bytes)
    }

    test("major 13 is rejected, as the node rejects it") {
        // [1, null, [13, 0]]
        assertRejected(decode("8301f6820d00"), exceeds)
        assertRejected(
          decode(hex(GovAction.HardForkInitiation(None, ProtocolVersion(13, 0)))),
          exceeds
        )
    }

    test("a previous action id does not change the bound") {
        val ok = GovAction.HardForkInitiation(Some(prevActionId), ProtocolVersion(12, 7))
        assert(decode(hex(ok)).get == ok)
        val tooHigh = GovAction.HardForkInitiation(Some(prevActionId), ProtocolVersion(13, 0))
        assertRejected(decode(hex(tooHigh)), exceeds)
    }

    test("the minor is not checked") {
        val action = GovAction.HardForkInitiation(None, ProtocolVersion(12, Int.MaxValue))
        assert(decode(hex(action)).get == action)
        // [1, null, [11, 0]]: the current major also decodes
        assert(
          decode("8301f6820b00").get == GovAction.HardForkInitiation(
            None,
            ProtocolVersion.vanRossemPV
          )
        )
    }

    /** A transaction body with one input, no outputs, a zero fee and one proposal. */
    private def bodyWithProposal(action: GovAction): Try[TransactionBody] = {
        val proposal = ProposalProcedure(
          Coin(1),
          RewardAccount(StakeAddress(Network.Mainnet, StakePayload.Script(Hash(hash("05", 28))))),
          action,
          Anchor("https://example.org", Hash(hash("00", 32)))
        )
        final class Raw(val write: Writer => Unit)
        given Encoder[Raw] = (w, raw) => { raw.write(w); w }
        val bytes = Cbor
            .encode(Raw { w =>
                w.writeMapOpen(4)
                w.writeInt(0).writeArrayOpen(1).write(TransactionInput(Hash(hash("01", 32)), 0))
                w.writeArrayClose()
                w.writeInt(1).writeArrayOpen(0).writeArrayClose()
                w.writeInt(2).write(Coin(0))
                w.writeInt(20).writeArrayOpen(1).write(proposal).writeArrayClose()
                w.writeMapClose()
            })
            .toByteArray
        given OriginalCborByteArray = OriginalCborByteArray(bytes)
        given ProtocolVersion = ProtocolVersion.vanRossemPV
        Cbor.decode(bytes).to[TransactionBody].valueTry
    }

    test("a transaction body proposing a hard fork to major 13 is rejected") {
        val ok = GovAction.HardForkInitiation(None, ProtocolVersion(12, 0))
        assert(bodyWithProposal(ok).get.proposalProcedures.toSeq.map(_.govAction) == Seq(ok))
        assertRejected(
          bodyWithProposal(GovAction.HardForkInitiation(None, ProtocolVersion(13, 0))),
          exceeds
        )
    }
}
