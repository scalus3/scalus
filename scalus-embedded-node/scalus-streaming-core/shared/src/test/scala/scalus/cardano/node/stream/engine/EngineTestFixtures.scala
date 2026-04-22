package scalus.cardano.node.stream.engine

import scalus.uplc.builtin.ByteString
import scalus.cardano.address.Address
import scalus.cardano.ledger.{AssetName, BlockHash, Coin, MultiAsset, PolicyId, ScriptHash, TransactionHash, TransactionInput, TransactionOutput, Value}
import scalus.cardano.node.stream.ChainPoint

import scala.collection.immutable.SortedMap
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.Await
import scala.concurrent.duration.{FiniteDuration, *}

/** Shared fixtures for engine unit tests. Keeps per-suite prelude short and ensures each test uses
  * the same deterministic hash scheme.
  */
object EngineTestFixtures {

    private def hexN(n: Long, len: Int): String = {
        val s = n.toHexString
        "0" * (len - s.length) + s
    }

    def txHash(n: Long): TransactionHash =
        TransactionHash.fromHex(hexN(n, 64))

    def blockHash(n: Long): BlockHash =
        BlockHash.fromHex(hexN(n, 64))

    def policyId(n: Long): PolicyId =
        ScriptHash.fromHex(hexN(n, 56))

    def assetName(name: String): AssetName =
        AssetName(ByteString.fromString(name))

    def point(slot: Long): ChainPoint = ChainPoint(slot, blockHash(slot))

    def tip(slot: Long, blockNo: Long = 0L): scalus.cardano.node.stream.ChainTip =
        scalus.cardano.node.stream.ChainTip(point(slot), blockNo)

    val addressA: Address = Address.fromBech32(
      "addr1qx2fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3jhsydzer3n0d3vllmyqwsx5wktcd8cc3sq835lu7drv2xwl2wywfgse35a3x"
    )

    val addressB: Address = Address.fromBech32(
      "addr1z8phkx6acpnf78fuvxn0mkew3l0fd058hzquvz7w36x4gten0d3vllmyqwsx5wktcd8cc3sq835lu7drv2xwl2wywfgs9yc0hh"
    )

    def output(address: Address, lovelace: Long): TransactionOutput =
        TransactionOutput.Babbage(address, Value(Coin(lovelace), MultiAsset.zero))

    def outputWithAsset(
        address: Address,
        lovelace: Long,
        pid: PolicyId,
        name: AssetName,
        quantity: Long
    ): TransactionOutput = {
        val multi = MultiAsset(SortedMap(pid -> SortedMap(name -> quantity)))
        TransactionOutput.Babbage(address, Value(Coin(lovelace), multi))
    }

    def input(txHashN: Long, index: Int): TransactionInput =
        TransactionInput(txHash(txHashN), index)

    /** Build an AppliedBlock at `slotN` from the given transactions. `blockNo` defaults to `slotN`
      * so tests don't have to track it separately — arbitrary monotonic value will do.
      */
    def block(slotN: Long, txs: AppliedTransaction*): AppliedBlock =
        AppliedBlock(tip(slotN, slotN), txs.toSeq)

    def tx(
        idN: Long,
        spending: Set[TransactionInput] = Set.empty,
        producing: IndexedSeq[TransactionOutput] = IndexedSeq.empty
    ): AppliedTransaction =
        AppliedTransaction(txHash(idN), spending, producing)

    /** Drain up to `count` events from a mailbox, awaiting each pull synchronously. Stops early on
      * a clean close (mailbox returned `None`). Default 5-second per-pull timeout is generous
      * enough for CI; individual tests can override via [[drainWithTimeout]].
      */
    def drain[A](mailbox: Mailbox[A], count: Int): Seq[A] =
        drainWithTimeout(mailbox, count, 5.seconds)

    def drainWithTimeout[A](mailbox: Mailbox[A], count: Int, timeout: FiniteDuration): Seq[A] = {
        val buf = ArrayBuffer.empty[A]
        while buf.size < count do {
            val next = Await.result(mailbox.pull(), timeout)
            next match {
                case Some(a) => buf += a
                case None    => return buf.toSeq
            }
        }
        buf.toSeq
    }
}
