package scalus.examples.auction

import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.builtin.ByteString.utf8
import scalus.uplc.builtin.{ByteString, Data}
import scalus.uplc.builtin.Data.toData
import scalus.cardano.onchain.plutus.v1.{Address, Credential, PosixTime, PubKeyHash, Value}
import scalus.cardano.onchain.plutus.v2.OutputDatum
import scalus.cardano.onchain.plutus.v3.*
import scalus.cardano.onchain.plutus.v3.ScriptInfo.SpendingScript
import scalus.cardano.onchain.plutus.prelude.{List as SList, Option as SOption}
import scalus.testing.kit.{ScalusTest, TestUtil}

/** Cross-instance double-satisfaction test using two *genuinely distinct* auction instances.
  *
  * Each auction is one-shot-parameterized, so it has its own script hash and address. The per-hash
  * "exactly one auction NFT spent" guard in `handleEnd` only sees its own input and cannot detect a
  * sibling auction at a different address. The seller-output id tag (the auction's scriptHash) is
  * what blocks the attack: a single shared seller output can carry only one auction's tag.
  *
  * Before the fix, both validators passed this attack (each saw one NFT under its own hash and a
  * seller output >= its own bid), paying the seller once for two auctions.
  */
class CrossInstanceDoubleSatisfactionTest extends AnyFunSuite, ScalusTest {

    // Two distinct one-shot params => two distinct script hashes/addresses.
    private val oneShotA = TxOutRef(TxId(ByteString.fromHex("aa" * 32)), 0)
    private val oneShotB = TxOutRef(TxId(ByteString.fromHex("bb" * 32)), 0)
    private val instA = AuctionContract.withErrorTraces.apply(oneShotA.toData)
    private val instB = AuctionContract.withErrorTraces.apply(oneShotB.toData)
    private val hashA: ByteString = instA.script.scriptHash
    private val hashB: ByteString = instB.script.scriptHash

    private val seller = TestUtil.mockPubKeyHash(1)
    private val buyer = TestUtil.mockPubKeyHash(2)
    private val itemIdA = utf8"item-A"
    private val itemIdB = utf8"item-B"
    private val bid = BigInt(10_000_000)
    private val endTime: PosixTime = BigInt(1000)

    private def wonDatum(itemId: ByteString) =
        Datum(seller, SOption.Some(buyer), bid, endTime, itemId)

    test("cross-instance double satisfaction is blocked across distinct one-shot auctions") {
        val outRefA = TxOutRef(TxId(ByteString.fromHex("01" * 32)), 0)
        val outRefB = TxOutRef(TxId(ByteString.fromHex("02" * 32)), 0)

        val inputA = TxInInfo(
          outRefA,
          TxOut(
            address = Address.fromScriptHash(hashA),
            value = Value.lovelace(bid + 2_000_000) + Value(hashA, itemIdA, 1),
            datum = OutputDatum.OutputDatum(wonDatum(itemIdA).toData)
          )
        )
        val inputB = TxInInfo(
          outRefB,
          TxOut(
            address = Address.fromScriptHash(hashB),
            value = Value.lovelace(bid + 2_000_000) + Value(hashB, itemIdB, 1),
            datum = OutputDatum.OutputDatum(wonDatum(itemIdB).toData)
          )
        )

        // ATTACK: a single shared seller output (tagged with auction A's id only), and the winner
        // collects both NFTs. The attacker keeps auction B's bid.
        val sellerOut = TxOut(
          address = Address.fromPubKeyHash(seller),
          value = Value.lovelace(bid),
          datum = OutputDatum.OutputDatum(hashA.toData)
        )
        val winnerOut = TxOut(
          address = Address.fromPubKeyHash(buyer),
          value = Value.lovelace(2_000_000) + Value(hashA, itemIdA, 1) + Value(hashB, itemIdB, 1)
        )

        val txInfo = TxInfo.placeholder.copy(
          inputs = SList(inputA, inputB),
          outputs = SList(sellerOut, winnerOut),
          validRange = Interval.after(endTime + 1),
          id = random[TxId]
        )

        // Auction A is ended pointing at the shared seller output (idx 0) and winner output (idx 1).
        val ctxA = ScriptContext(
          txInfo,
          Action.End(BigInt(0), BigInt(0), BigInt(1)).toData,
          SpendingScript(outRefA)
        )
        // Auction B points at the SAME seller output (idx 0) — the double-satisfaction attempt.
        val ctxB = ScriptContext(
          txInfo,
          Action.End(BigInt(1), BigInt(0), BigInt(1)).toData,
          SpendingScript(outRefB)
        )

        val resultA = instA.program.runWithDebug(ctxA)
        val resultB = instB.program.runWithDebug(ctxB)

        // A is satisfied (the shared output carries A's tag), but B must reject it — the seller
        // output is not tagged with B's id, so the two auctions cannot share one seller payout.
        assert(resultA.isSuccess, s"Auction A should accept its own tagged output: ${resultA.logs}")
        assert(
          resultB.isFailure,
          s"Auction B must reject the shared seller output (double satisfaction): ${resultB.logs}"
        )
        assert(
          resultB.logs.exists(_.contains("tagged with this auction")),
          s"Expected the id-tag error, got: ${resultB.logs.mkString(", ")}"
        )
    }
}
