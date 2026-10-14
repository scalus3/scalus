package scalus.cardano.ledger

import io.bullet.borer.{Cbor, Encoder, Writer}
import io.bullet.borer.NullOptions.given
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.{Network, StakeAddress, StakePayload}
import scalus.uplc.builtin.ByteString

/** From protocol version 9, the ledger's `decodeSet` and `decodeMap` (cardano-ledger-binary,
  * Decoding/Decoder.hs) reject a repeated element or key, and accept any order. A governance action
  * exists only from version 9, so its sets and maps must decode the same way: a transaction with a
  * repeat fails to deserialize on a node.
  */
class GovActionDuplicatesTest extends AnyFunSuite {
    private def hash(byte: String) = ByteString.fromHex(byte * 28)
    private def key(byte: String): Credential = Credential.KeyHash(Hash(hash(byte)))
    private def script(byte: String): Credential = Credential.ScriptHash(Hash(hash(byte)))
    private def account(byte: String) =
        RewardAccount(StakeAddress(Network.Mainnet, StakePayload.Script(Hash(hash(byte)))))

    /** Writes CBOR by hand, so that a test can repeat what a Scala `Set` or `Map` would merge. */
    private final class Raw(val write: Writer => Unit)
    private given Encoder[Raw] = (w, raw) => { raw.write(w); w }
    private def cbor(write: Writer => Unit): Array[Byte] = Cbor.encode(Raw(write)).toByteArray

    private def decode(bytes: Array[Byte]) = Cbor.decode(bytes).to[GovAction].valueTry

    /** `UpdateCommittee` with its members written exactly as given. */
    private def updateCommittee(removed: Seq[Credential], added: Seq[(Credential, Long)]) =
        cbor { w =>
            w.writeArrayOpen(5).writeInt(4).write(Option.empty[GovActionId])
            w.writeArrayOpen(removed.size)
            removed.foreach(w.write(_))
            w.writeArrayClose()
            w.writeMapOpen(added.size)
            added.foreach((credential, epoch) => w.write(credential).write(epoch))
            w.writeMapClose()
            w.write(UnitInterval(1, 2)).writeArrayClose()
        }

    /** `TreasuryWithdrawals` with its withdrawals written exactly as given. */
    private def treasuryWithdrawals(withdrawals: Seq[(RewardAccount, Coin)]) =
        cbor { w =>
            w.writeArrayOpen(3).writeInt(2)
            w.writeMapOpen(withdrawals.size)
            withdrawals.foreach((account, coin) => w.write(account).write(coin))
            w.writeMapClose()
            w.write(Option.empty[ScriptHash]).writeArrayClose()
        }

    test("a committee member removed twice is rejected") {
        assert(decode(updateCommittee(Seq(key("01"), key("01")), Seq.empty)).isFailure)
    }

    test("a committee member added twice is rejected, whatever the two terms") {
        assert(decode(updateCommittee(Seq.empty, Seq(key("01") -> 5L, key("01") -> 6L))).isFailure)
    }

    test("a reward account withdrawn from twice is rejected") {
        val twice = Seq(account("05") -> Coin(1), account("05") -> Coin(2))
        assert(decode(treasuryWithdrawals(twice)).isFailure)
    }

    test("members in any order, and an empty set to remove, are accepted") {
        val unordered = updateCommittee(Seq(key("02"), script("01")), Seq(key("04") -> 7L))
        val decoded = decode(unordered).get
        assert(
          decoded == GovAction.UpdateCommittee(
            None,
            Set(key("02"), script("01")),
            Map(key("04") -> 7L),
            UnitInterval(1, 2)
          )
        )
        assert(decode(updateCommittee(Seq.empty, Seq(script("03") -> 1L))).isSuccess)
        val withdrawals = Seq(account("06") -> Coin(1), account("05") -> Coin(2))
        assert(decode(treasuryWithdrawals(withdrawals)).isSuccess)
    }
}
