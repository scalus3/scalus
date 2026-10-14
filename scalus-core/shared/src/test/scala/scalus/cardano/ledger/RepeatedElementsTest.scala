package scalus.cardano.ledger

import io.bullet.borer.{Cbor, Encoder, Tag, Writer}
import io.bullet.borer.NullOptions.given
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.{Network, StakeAddress, StakePayload}
import scalus.uplc.builtin.ByteString

import scala.util.Try

/** A repeated element or key in a CBOR set or map must decode as the ledger decodes it.
  *
  * From protocol version 9 the ledger's `decodeSet`, `decodeMap` and `decodeMapByKey`
  * (cardano-ledger-binary, Decoding/Decoder.hs) fail on a repeat and accept any order; before it
  * they merge repeats. `decodeOSet` (proposals) and the sparse decoder of a protocol-parameter
  * update always fail on a repeat. The witness set's key and bootstrap witnesses stay lenient until
  * protocol version 12 (`addrWitsSetDecoder` in Alonzo/TxWits.hs).
  */
class RepeatedElementsTest extends AnyFunSuite {
    private def hash(byte: String, size: Int = 28) = ByteString.fromHex(byte * size)
    private def key(byte: String): Credential = Credential.KeyHash(Hash(hash(byte)))
    private def script(byte: String): Credential = Credential.ScriptHash(Hash(hash(byte)))
    private def account(byte: String) =
        RewardAccount(StakeAddress(Network.Mainnet, StakePayload.Script(Hash(hash(byte)))))
    private def input(byte: String) = TransactionInput(Hash(hash(byte, 32)), 0)
    private val proposal = ProposalProcedure(
      Coin(1),
      account("05"),
      GovAction.InfoAction,
      Anchor("https://example.org", Hash(hash("00", 32)))
    )

    private val repeatedSetElement = "does not match the total count"
    private val repeatedMapKey = "Repeated map key"

    /** Writes CBOR by hand, so that a test can repeat what a Scala `Set` or `Map` would merge. */
    private final class Raw(val write: Writer => Unit)
    private given Encoder[Raw] = (w, raw) => { raw.write(w); w }
    private def cbor(write: Writer => Unit): Array[Byte] = Cbor.encode(Raw(write)).toByteArray

    private def cborArray(items: (Writer => Unit)*): Writer => Unit = { w =>
        w.writeArrayOpen(items.size)
        items.foreach(_(w))
        w.writeArrayClose()
    }
    private def cborMap(entries: (Writer => Unit, Writer => Unit)*): Writer => Unit = { w =>
        w.writeMapOpen(entries.size)
        entries.foreach((k, v) => { k(w); v(w) })
        w.writeMapClose()
    }
    private def value[A: Encoder](a: A): Writer => Unit = _.write(a)

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

    private def decodeAction(write: Writer => Unit) =
        Cbor.decode(cbor(write)).to[GovAction].valueTry

    /** `UpdateCommittee Nothing removed added 1/2`. */
    private def updateCommittee(removed: Writer => Unit, added: Writer => Unit) =
        decodeAction(
          cborArray(
            value(4),
            value(Option.empty[GovActionId]),
            removed,
            added,
            value(UnitInterval(1, 2))
          )
        )

    /** `ParameterChange Nothing update Nothing`. */
    private def parameterChange(update: Writer => Unit) =
        decodeAction(
          cborArray(
            value(0),
            value(Option.empty[GovActionId]),
            update,
            value(Option.empty[ScriptHash])
          )
        )

    /** A transaction body with one input, no outputs and a zero fee, with `fields` added or
      * replacing those, decoded at protocol version `pv`.
      */
    private def body(pv: ProtocolVersion)(fields: (Int, Writer => Unit)*): Try[TransactionBody] = {
        val all = (Map[Int, Writer => Unit](
          0 -> cborArray(value(input("01"))),
          1 -> cborArray(),
          2 -> value(Coin(0))
        ) ++ fields).toSeq.sortBy(_._1)
        val bytes = cbor(cborMap(all.map((k, v) => (value(k), v))*))
        given OriginalCborByteArray = OriginalCborByteArray(bytes)
        given ProtocolVersion = pv
        Cbor.decode(bytes).to[TransactionBody].valueTry
    }

    test("a committee member removed twice is rejected, with or without the set tag") {
        val twice = Seq(value(key("01")), value(key("01")))
        assertRejected(updateCommittee(cborArray(twice*), cborMap()), repeatedSetElement)
        val tagged: Writer => Unit = w => cborArray(twice*)(w.writeTag(Tag.Other(258)))
        assertRejected(updateCommittee(tagged, cborMap()), repeatedSetElement)
    }

    test("a committee member added twice is rejected, in a definite or an indefinite map") {
        assertRejected(
          updateCommittee(
            cborArray(),
            cborMap(value(key("01")) -> value(5L), value(key("01")) -> value(6L))
          ),
          repeatedMapKey
        )
        val indefinite: Writer => Unit = w => {
            w.writeMapStart()
            w.write(key("01")).write(5L).write(key("01")).write(6L)
            w.writeBreak()
        }
        assertRejected(updateCommittee(cborArray(), indefinite), repeatedMapKey)
    }

    test("a reward account withdrawn from the treasury twice is rejected") {
        val twice =
            cborMap(value(account("05")) -> value(Coin(1)), value(account("05")) -> value(Coin(2)))
        assertRejected(
          decodeAction(cborArray(value(2), twice, value(Option.empty[ScriptHash]))),
          repeatedMapKey
        )
    }

    test("a protocol-parameter update with a repeated key or cost-model language is rejected") {
        assertRejected(
          parameterChange(cborMap(value(0) -> value(Coin(5)), value(0) -> value(Coin(6)))),
          "Repeated key 0"
        )
        val costs =
            cborMap(value(1) -> value(IndexedSeq(1L, 2L)), value(1) -> value(IndexedSeq(3L, 4L)))
        assertRejected(parameterChange(cborMap(value(18) -> costs)), repeatedMapKey)
    }

    test("members in any order, an empty set to remove and an indefinite map are accepted") {
        val unordered = updateCommittee(
          cborArray(value(key("02")), value(script("01"))),
          cborMap(value(key("04")) -> value(7L))
        )
        assert(
          unordered.get == GovAction.UpdateCommittee(
            None,
            Set(key("02"), script("01")),
            Map(key("04") -> 7L),
            UnitInterval(1, 2)
          )
        )
        val indefinite: Writer => Unit = w => {
            w.writeMapStart()
            w.write(key("04")).write(1L).write(script("03")).write(2L)
            w.writeBreak()
        }
        assert(
          updateCommittee(cborArray(), indefinite).get == GovAction.UpdateCommittee(
            None,
            Set.empty,
            Map(key("04") -> 1L, script("03") -> 2L),
            UnitInterval(1, 2)
          )
        )
    }

    test("a repeated input, collateral, reference input or signer is rejected from version 9") {
        val twice = cborArray(value(input("02")), value(input("02")))
        for field <- Seq(0, 13, 18) do
            assertRejected(body(ProtocolVersion.conwayPV)(field -> twice), repeatedSetElement)
        val signer: AddrKeyHash = Hash(hash("03"))
        assertRejected(
          body(ProtocolVersion.conwayPV)(14 -> cborArray(value(signer), value(signer))),
          repeatedSetElement
        )
    }

    test("before version 9 a repeated input is merged, as the ledger merges it") {
        val decoded = body(ProtocolVersion.valentinePV)(
          0 -> cborArray(value(input("02")), value(input("02")))
        )
        assert(decoded.get.inputs.toSet == Set(input("02")))
    }

    test(
      "a repeated withdrawal account is rejected from version 9, and keeps its last amount before"
    ) {
        val twice =
            cborMap(value(account("05")) -> value(Coin(5)), value(account("05")) -> value(Coin(1)))
        assertRejected(body(ProtocolVersion.conwayPV)(5 -> twice), repeatedMapKey)
        val babbage = body(ProtocolVersion.valentinePV)(5 -> twice).get
        assert(babbage.withdrawals.map(_.withdrawals.toMap) == Some(Map(account("05") -> Coin(1))))
    }

    test("a repeated voter, or a repeated action under one voter, is rejected") {
        val voter = Voter.DRepKey(Hash(hash("0a")))
        val action = GovActionId(Hash(hash("0b", 32)), 0)
        val vote = value(VotingProcedure(Vote.Yes, None))
        val oneVote = cborMap(value(action) -> vote)
        assertRejected(
          body(ProtocolVersion.conwayPV)(
            19 -> cborMap(value(voter) -> oneVote, value(voter) -> oneVote)
          ),
          repeatedMapKey
        )
        assertRejected(
          body(ProtocolVersion.conwayPV)(
            19 -> cborMap(value(voter) -> cborMap(value(action) -> vote, value(action) -> vote))
          ),
          repeatedMapKey
        )
    }

    test("a repeated proposal is rejected") {
        assertRejected(
          body(ProtocolVersion.conwayPV)(20 -> cborArray(value(proposal), value(proposal))),
          repeatedSetElement
        )
    }

    test("a repeated key witness is still merged at version 9, as the ledger merges it until 12") {
        val witness = VKeyWitness(hash("0c", 32), hash("0d", 64))
        val bytes = cbor(cborMap(value(0) -> cborArray(value(witness), value(witness))))
        given OriginalCborByteArray = OriginalCborByteArray(bytes)
        given ProtocolVersion = ProtocolVersion.conwayPV
        val decoded = Cbor.decode(bytes).to[TransactionWitnessSet].value
        assert(decoded.vkeyWitnesses.toSet == Set(witness))
    }
}
