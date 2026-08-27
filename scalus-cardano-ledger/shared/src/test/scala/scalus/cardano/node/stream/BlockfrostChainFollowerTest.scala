package scalus.cardano.node.stream

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.Address
import scalus.cardano.infra.ResyncRequiredException
import scalus.cardano.ledger.*
import scalus.cardano.node.UtxoSource
import scalus.cardano.node.stream.internal.*
import scalus.testing.kit.Party
import scalus.uplc.builtin.ByteString

import scala.concurrent.duration.*
import scala.concurrent.{ExecutionContext, Future}

/** The polling loop, driven against a fake Blockfrost.
  *
  * This is what the follower seam is for: the behaviour worth pinning here — how many requests a
  * poll costs, that every height is reported whether or not it matched, and what happens when the
  * chain forks under us — is all reachable without HTTP, a key or real time.
  */
class BlockfrostChainFollowerTest extends AnyFunSuite {

    private given CardanoInfo = CardanoInfo.mainnet
    private given ExecutionContext = ExecutionContext.parasitic

    private val alice = Party.Alice.address

    private def point(n: Long): ChainPoint = {
        val bytes = new Array[Byte](32)
        bytes(31) = n.toByte
        ChainPoint(n, BlockHash.fromByteString(ByteString.fromArray(bytes)))
    }

    private def ref(n: Long): BlockRef = BlockRef(point(n), n)

    /** A scripted Blockfrost. `chain` is what `/blocks/{hash}/next` will hand back; `forked` makes
      * it answer 404, which is how a reorg reaches the follower.
      */
    private class FakeApi(
        tip: BlockRef,
        var chain: Seq[BlockRef] = Seq.empty,
        var forked: Boolean = false
    ) extends BlockfrostChainApi {
        var addressQueries: List[(Address, BlockNo)] = Nil
        var txQueries: List[TransactionHash] = Nil

        override def latestBlock(): Future[BlockRef] = Future.successful(tip)

        override def blocksAfter(block: BlockRef): Future[Option[Seq[BlockRef]]] =
            if forked then Future.successful(None)
            else
                val next = chain.filter(_.blockNo > block.blockNo)
                Future.successful(Some(next))

        override def addressTransactionsIn(
            address: Address,
            from: BlockNo,
            to: BlockNo
        ): Future[Seq[TransactionHash]] = {
            addressQueries = (address, from) :: addressQueries
            Future.successful(Seq.empty)
        }

        override def transaction(hash: TransactionHash): Future[ObservedTransaction] = {
            txQueries = hash :: txQueries
            Future.failed(new AssertionError("no transactions in these fixtures"))
        }
    }

    /** Runs the loop synchronously: every future is already completed, so `delay` returning a
      * completed unit makes the whole poll cycle run inline on `start()`.
      */
    private def follower(api: BlockfrostChainApi, polls: Int): BlockfrostChainFollower = {
        var remaining = polls
        val f = new BlockfrostChainFollower(
          api,
          1.second,
          _ =>
              if remaining <= 0 then Future.never
              else
                  remaining -= 1
                  Future.unit
        )
        f
    }

    private def drain(source: ScalusAsyncSource[ChainEvent]): List[ChainEvent] = {
        val buf = List.newBuilder[ChainEvent]
        var more = true
        while more do
            source.pull().value.flatMap(_.toOption).flatten match
                case Some(e) => buf += e
                case None    => more = false
        buf.result()
    }

    test("a poll that finds no new block reports nothing") {
        val api = new FakeApi(ref(10L))
        val f = follower(api, polls = 1)
        f.start()
        assert(drain(f.events).isEmpty, "no block, no event — Idle is the hub's business, not ours")
        assert(api.addressQueries.isEmpty, "an unchanged tip must not cost a per-address request")
    }

    test("every new height is reported, matched or not") {
        val api = new FakeApi(ref(10L), chain = Seq(ref(11L), ref(12L)))
        val f = follower(api, polls = 1)
        f.watch(Set(UtxoSource.FromAddress(alice)))
        f.start()

        val forwarded = drain(f.events).collect { case ChainEvent.RollForward(b) => b }
        assert(
          forwarded.map(_.blockNo) == List(11L, 12L),
          "a height with no matches still has to be reported, or subscriptions never advance"
        )
        assert(
          forwarded.forall(_.coverage == BlockCoverage.Sources(Set(UtxoSource.FromAddress(alice)))),
          "coverage names every source probed at that height"
        )
    }

    test("cost is one address request per watched address per block") {
        val api = new FakeApi(ref(10L), chain = Seq(ref(11L), ref(12L)))
        val f = follower(api, polls = 1)
        f.watch(Set(UtxoSource.FromAddress(alice)))
        f.start()

        assert(
          api.addressQueries.map(_._2).sorted == List(11L, 12L),
          "one query per address per block, scoped to that block — not a scan of the address's " +
              "whole history, which is what makes this affordable"
        )
    }

    test("a fork under the last reported block fails the stream, and says to resync") {
        val api = new FakeApi(ref(10L), forked = true)
        val f = follower(api, polls = 1)
        f.start()

        val failure = f.events.pull().value.get
        assert(failure.isFailure)
        assert(
          failure.failed.get.isInstanceOf[ResyncRequiredException],
          "the light driver detects reorgs but does not reconcile them; a subscriber must be told " +
              "its view is untrustworthy rather than left to diverge silently"
        )
    }

    test("watching nothing still tracks the chain") {
        val api = new FakeApi(ref(10L), chain = Seq(ref(11L)))
        val f = follower(api, polls = 1)
        f.start()

        val forwarded = drain(f.events).collect { case ChainEvent.RollForward(b) => b }
        assert(forwarded.map(_.blockNo) == List(11L))
        assert(
          api.addressQueries.isEmpty,
          "no subscriptions means no per-address cost, but the tip must still advance"
        )
    }
}
