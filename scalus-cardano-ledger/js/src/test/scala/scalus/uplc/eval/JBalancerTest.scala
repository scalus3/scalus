package scalus.uplc.eval

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.Network.Mainnet
import scalus.cardano.ledger.*
import scalus.testing.kit.Party.{Alice, Bob, Charles}
import scalus.utils.Hex
import scalus.utils.scalajs.internal.*

import scala.collection.immutable.SortedMap
import scala.scalajs.js

class JBalancerTest extends AnyFunSuite {

    private val params = CardanoInfo.mainnet.protocolParams

    private val slotConfig = js.Dynamic
        .literal(
          zeroTime = SlotConfig.mainnet.zeroTime.toDouble,
          zeroSlot = SlotConfig.mainnet.zeroSlot.toDouble,
          slotLength = SlotConfig.mainnet.slotLength.toDouble
        )
        .asInstanceOf[JSlotConfigLike]

    private val input = TransactionInput(TransactionHash.fromHex("0" * 64), 0)
    private val utxo = JsUtxo.wrap(input, Output(Alice.address(Mainnet), Value.ada(1000)))

    private val registration = Certificate.PoolRegistration(
      operator = Bob.addrKeyHash,
      vrfKeyHash = VrfKeyHash.fromHex("a" * 64),
      pledge = Coin.ada(100),
      cost = Coin(params.minPoolCost),
      margin = UnitInterval.one,
      rewardAccount = RewardAccount(Bob.account.stakeAddress(Mainnet)),
      poolOwners = Set(Bob.addrKeyHash, Charles.addrKeyHash),
      relays = IndexedSeq.empty,
      poolMetadata = None
    )

    /** Spends Alice's 1000 ADA into one change output, the output at index 0. */
    private def draft(body: TransactionBody => TransactionBody): Transaction =
        Transaction(
          body(
            TransactionBody(
              inputs = TaggedSortedSet(input),
              outputs = IndexedSeq(Sized(Output(Alice.address(Mainnet), Value.ada(1000)))),
              fee = Coin.zero
            )
          )
        )

    private def balance(
        tx: Transaction,
        extraSigners: Seq[String]
    ): Transaction = {
        val balanced = JBalancer.balanceTx(
          Hex.bytesToHex(tx.toCbor),
          js.Array[js.Any](utxo),
          slotConfig,
          JsProtocolParams.wrap(params).asInstanceOf[JProtocolParamsLike],
          0,
          js.Array(extraSigners*)
        )
        Transaction.fromCbor(balanced.toByteString.bytes)
    }

    private def fee(tx: Transaction): Coin = tx.body.value.fee

    test("prices the signatures of a pool's operator and owners without extra signers") {
        val tx = draft(_.copy(certificates = TaggedOrderedStrictSet(registration)))
        val named = Seq(Bob.addrKeyHash, Charles.addrKeyHash).map(_.toHex)
        assert(fee(balance(tx, Seq.empty)) == fee(balance(tx, named)))
    }

    test("prices the signature of a withdrawal's stake key without extra signers") {
        val account = RewardAccount(Bob.account.stakeAddress(Mainnet))
        val tx = draft(_.copy(withdrawals = Some(Withdrawals(SortedMap(account -> Coin.zero)))))
        val named = Seq(Bob.account.stakeKeyHash.toHex)
        assert(fee(balance(tx, Seq.empty)) == fee(balance(tx, named)))
    }
}
