package scalus.cardano.node.stream.internal

import scalus.cardano.address.{Address, ShelleyAddress}
import scalus.cardano.blockfrost.BlockInfo
import scalus.cardano.ledger.{BlockHash, Transaction, TransactionHash}
import scalus.cardano.node.BlockfrostProvider
import scalus.cardano.node.stream.{BlockNo, ChainPoint}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Success

/** [[BlockfrostChainApi]] over the real REST endpoints.
  *
  * Nothing but translation: HTTP and JSON live in [[BlockfrostProvider]], polling and ordering live
  * in [[BlockfrostChainFollower]], and this maps between their vocabularies. Keeping it that thin is
  * what lets the follower's failure modes — reorg detection, request ordering, the watch/observe
  * race — be tested against a fake without a server.
  */
private[stream] final class BlockfrostRestChainApi(provider: BlockfrostProvider)(using
    ec: ExecutionContext
) extends BlockfrostChainApi {

    override def latestBlock(): Future[BlockRef] = provider.fetchLatestBlock.map(toRef)

    override def blocksAfter(block: BlockRef): Future[Option[Seq[BlockRef]]] =
        provider
            .fetchBlockNextOrGone(block.point.blockHash.toHex)
            .map(_.map(_.map(toRef)))

    override def addressTransactionsIn(
        address: Address,
        from: BlockNo,
        to: BlockNo
    ): Future[Seq[TransactionHash]] =
        provider
            .fetchAddressTransactionsInRange(BlockfrostRestChainApi.bech32(address), from, to)
            .map(_.map(tx => TransactionHash.fromHex(tx.txHash)))

    /** Two requests: the body, and what it did to the UTxO set.
      *
      * The body comes from `/txs/{hash}/cbor` rather than being reassembled from the JSON, because
      * a subscriber receives the `Transaction` itself and anything short of the original bytes
      * would be a re-encoding — with a different hash, and different script data.
      */
    override def transaction(hash: TransactionHash): Future[ObservedTransaction] =
        for
            cbor <- provider.fetchTransactionCbor(hash.toHex)
            effects <- provider.fetchTransactionEffects(hash)
        yield ObservedTransaction(
          Transaction.fromCbor(cbor.bytes),
          effects.created,
          effects.spent
        )

    /** A block Blockfrost reports as being on the chain always has a height and a slot; the fields
      * are optional in the API because the Byron-era boundary blocks it also serves have neither.
      * Those cannot appear here — the follower starts at the current tip and walks forward — so an
      * absent one is a broken response rather than a case to paper over with a default, which would
      * put a block at height 0 and drag every watermark back with it.
      */
    private def toRef(info: BlockInfo): BlockRef = {
        val height = info.height.getOrElse(
          throw new IllegalStateException(
            s"Blockfrost reported block ${info.hash} with no height; it cannot be placed on the chain"
          )
        )
        val slot = info.slot.getOrElse(
          throw new IllegalStateException(
            s"Blockfrost reported block ${info.hash} with no slot; it cannot be placed on the chain"
          )
        )
        BlockRef(ChainPoint(slot, BlockHash.fromHex(info.hash)), height)
    }
}

private[stream] object BlockfrostRestChainApi {

    /** Whether an address can be watched at all — i.e. whether [[bech32]] will succeed.
      *
      * Checked by the provider at `subscribe`, where the caller can still do something about it.
      * The alternative is discovering it in the poll loop, where the failure is not one
      * subscription's problem: the follower feeds every subscriber, so one unwatchable address
      * would fail all of them, repeatedly, for a mistake only one of them made.
      */
    def isWatchable(address: Address): Boolean = bech32Opt(address).isDefined

    /** The Bech32 form Blockfrost addresses are keyed by.
      *
      * Throws for anything [[isWatchable]] rejects. Reaching that means the provider admitted a
      * subscription it should have refused, which is a defect here rather than a caller error.
      */
    def bech32(address: Address): String =
        bech32Opt(address).getOrElse(
          throw new IllegalArgumentException(
            s"$address cannot be addressed on Blockfrost; the provider should have refused this " +
                "subscription rather than letting the follower poll for it"
          )
        )

    private def bech32Opt(address: Address): Option[String] = address match
        case sh: ShelleyAddress =>
            sh.toBech32 match
                case Success(b) => Some(b)
                case _          => None
        case _ => None
}
