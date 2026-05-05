package scalus.examples.auction

import scalus.cardano.address.{Network, ShelleyAddress, ShelleyDelegationPart, ShelleyPaymentPart}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.onchain.plutus.v1.PosixTime
import scalus.cardano.onchain.plutus.v3.{TxId, TxOutRef}
import scalus.cardano.txbuilder.TransactionSigner
import scalus.cardano.wallet.hd.HdAccount
import scalus.uplc.builtin.ByteString.*
import scalus.crypto.ed25519.given_Ed25519Signer

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

/** Auction benchmark: end-to-end Cardano smart contract transactions on Hydrozoa L2.
  *
  * Demonstrates that the same scalus code (validators, tx builders, wallets) that targets
  * Cardano L1 works unchanged on Hydrozoa — the only difference is the BlockchainProvider.
  *
  * Usage: scalusExamplesJVM/runMain scalus.examples.auction.AuctionBench [url] [rounds]
  */
object AuctionBench {
    private given ExecutionContext = ExecutionContext.global

    private val TestMnemonic =
        "test test test test test test test test " +
            "test test test test test test test test " +
            "test test test test test test test sauce"

    case class Wallet(account: HdAccount) {
        val keyHash: AddrKeyHash = account.paymentKeyHash
        def address(net: Network): ShelleyAddress =
            ShelleyAddress(net, ShelleyPaymentPart.Key(keyHash), ShelleyDelegationPart.Null)
        val signer: TransactionSigner = new TransactionSigner(Set(account.paymentKeyPair))
    }

    private def wallet(index: Int) = Wallet(HdAccount.fromMnemonic(TestMnemonic, "", index))

    def main(args: Array[String]): Unit = {
        val baseUrl = args.headOption.getOrElse("http://localhost:8080")
        val rounds = if args.length > 1 then args(1).toInt else 50

        // --- Setup: wallets, provider, auction factory ---

        val info = CardanoInfo.preview
        val seller = wallet(0)
        val bidders = Seq(wallet(1), wallet(2))
        val addresses = (seller +: bidders).map(_.address(info.network))

        val seedUtxos: Utxos = Map(
          Input(GenesisHash, 0) -> Babbage(addresses(0), Value.lovelace(10_000_000L)),
          Input(GenesisHash, 1) -> Babbage(addresses(0), Value.lovelace(10_000_000_000L)),
          Input(GenesisHash, 2) -> Babbage(addresses(1), Value.lovelace(10_000_000_000L)),
          Input(GenesisHash, 3) -> Babbage(addresses(2), Value.lovelace(10_000_000_000L))
        )

        val headInfo = HydrozoaProvider.fetchHeadInfo(baseUrl)
        val provider = new HydrozoaProvider(
          baseUrl = baseUrl,
          info = info,
          initialUtxos = seedUtxos,
          keyPair = seller.account.paymentKeyPair,
          signerAddress = addresses(0),
          headId = headInfo.headId
        )

        val oneShotInput = seedUtxos.head._1
        val oneShot = TxOutRef(TxId(oneShotInput.transactionId), BigInt(oneShotInput.index))
        val auction = AuctionFactory(provider, withErrorTraces = true).createInstance(oneShot)

        // --- Run auction lifecycle ---

        val itemId = utf8"bench-item-001"
        val startingBid = 2_000_000L
        val auctionEnd: PosixTime = BigInt(info.slotConfig.slotToTime(1000))

        println(s"Auction Bench — $baseUrl, $rounds bid rounds")

        val t0 = System.nanoTime()
        var txCount = 0

        def submit[T](f: Future[T]): T = { val r = Await.result(f, Duration.Inf); txCount += 1; r }

        submit(auction.startAuction(
          sellerAddress = addresses(0),
          oneShotUtxo = Utxo(oneShotInput, seedUtxos(oneShotInput)),
          itemId = itemId,
          startingBid = startingBid,
          auctionEndTime = auctionEnd,
          initialValue = Coin(5_000_000L),
          signer = seller.signer
        ))

        var currentBid = startingBid
        for round <- 1 to rounds do
            for (bidder, i) <- bidders.zipWithIndex do
                currentBid += 1_000_000L
                submit(auction.bid(
                  bidderAddress = addresses(i + 1),
                  bidAmount = currentBid,
                  itemId = itemId,
                  signer = bidder.signer
                ))
                if round % 10 == 0 && i == 0 then
                    println(s"  round $round/$rounds ...")

        provider.setSlot(1001)
        submit(auction.endAuction(
          sponsorAddress = addresses(0),
          itemId = itemId,
          signer = seller.signer
        ))

        val elapsedMs = (System.nanoTime() - t0) / 1_000_000
        println(s"  $txCount transactions in ${elapsedMs}ms — ${"%.1f".format(txCount * 1000.0 / elapsedMs)} tx/s")
    }

    private val GenesisHash: TransactionHash =
        TransactionHash.fromHex("0000000000000000000000000000000000000000000000000000000000000000")
}
