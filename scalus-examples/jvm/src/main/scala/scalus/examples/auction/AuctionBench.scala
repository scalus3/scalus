//noinspection ZeroIndexToHead
package scalus.examples.auction

import scalus.cardano.address.{Network, ShelleyAddress, ShelleyDelegationPart, ShelleyPaymentPart}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.rules.{Context, UtxoEnv}
import scalus.cardano.node.{BlockchainProvider, Emulator}
import scalus.cardano.onchain.plutus.v1.PosixTime
import scalus.cardano.onchain.plutus.v3.{TxId, TxOutRef}
import scalus.cardano.txbuilder.TransactionSigner
import scalus.cardano.wallet.hd.HdAccount
import scalus.uplc.builtin.ByteString.*
import scalus.crypto.ed25519.given_Ed25519Signer

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

/** Auction benchmark: end-to-end Cardano smart contract transactions.
  *
  * Demonstrates that the same scalus code (validators, tx builders, wallets) works unchanged
  * on both a local Emulator (L1) and Hydrozoa L2 — the only difference is the BlockchainProvider.
  *
  * Usage: scalusExamplesJVM/runMain scalus.examples.auction.AuctionBench
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

    // --- Provider factories ---

    /** Create an L1 emulator provider — runs full Cardano ledger rules locally, no network needed. */
    private def createL1Provider(info: CardanoInfo, seedUtxos: Utxos): Emulator = {
        val env = UtxoEnv(slot = 0, params = info.protocolParams, certState = CertState.empty, network = info.network)
        val ctx = Context(env = env, slotConfig = info.slotConfig)
        new Emulator(initialUtxos = seedUtxos, initialContext = ctx)
    }

    /** Create an L2 Hydrozoa provider — submits transactions to a running Hydrozoa instance.
      * In this example, hydrozoa runs with 1 head.
      * This talks to the local Hydrozoa over HTTP and uses its headInfo endpoint.
      */
    private def createL2Provider(
        baseUrl: String,
        info: CardanoInfo,
        seedUtxos: Utxos,
        seller: Wallet,
        sellerAddress: ShelleyAddress
    ): HydrozoaProvider = {
        val headInfo = HydrozoaProvider.fetchHeadInfo(baseUrl)
        println(s"  Connected to Hydrozoa head: ${headInfo.headId.take(16)}...")
        // The `Provider` trait allows users to submit transactions and query UTxOs.
        // When interacting with a regular L1 network, a blockfrost-like interface is enough.
        // Hydrozoa requires some additional input wrapping, so we extract that logic into a respective provider.
        new HydrozoaProvider(
          baseUrl = baseUrl,
          info = info,
          initialUtxos = seedUtxos,
          keyPair = seller.account.paymentKeyPair,
          signerAddress = sellerAddress,
          headId = headInfo.headId
        )
    }

    def main(args: Array[String]): Unit = {
        val mode = "l2"
        val baseUrl = "http://localhost:8080"
        val rounds = 50

        // --- Setup ---

        // First, set up the general network info and initialize the wallets of the bidders.
        val info = CardanoInfo.preprod
        val seller = wallet(0)
        val bidders = Seq(wallet(1), wallet(2))
        val addresses = (seller +: bidders).map(_.address(info.network))

        // Pre-initialize the UTxOs such that bidders have enough ADA to participate in the auction.
        val seedUtxos: Utxos = Map(
          Input(GenesisHash, 0) -> Babbage(addresses(0), Value.ada(10)),
          Input(GenesisHash, 1) -> Babbage(addresses(0), Value.ada(10_000)),
          Input(GenesisHash, 2) -> Babbage(addresses(1), Value.ada(10_000)),
          Input(GenesisHash, 3) -> Babbage(addresses(2), Value.ada(10_000))
        )

        // --- The only line that differs between L1 and L2 --
        val provider: BlockchainProvider = mode match
            case "l1" => createL1Provider(info, seedUtxos)
            case _    => createL2Provider(baseUrl, info, seedUtxos, seller, addresses(0))

        println(s"Auction Bench — mode=$mode, $rounds bid rounds")

        // --- From here on, the code is identical regardless of provider ---

        // The auction depends on a one-shot UTxO, so we specify it here.
        val oneShotInput = seedUtxos.head._1
        val oneShot = TxOutRef(TxId(oneShotInput.transactionId), BigInt(oneShotInput.index))

        // Then, pass the provider into the transaction factory.
        // The transaction factory knows how to assemble the transactions related to auction,
        // which are later submitted by the specified `provider`.
        val auction = AuctionTransactionFactory(provider, withErrorTraces = true).createInstance(oneShot)

        // An item that's being auctioned
        val itemId = utf8"bench-item-001"
        val startingBid = 2_000_000L
        val auctionEnd: PosixTime = BigInt(info.slotConfig.slotToTime(1000))

        val t0 = System.nanoTime()
        var txCount = 0

        // A simple helper that also increments the tx count.
        // The real submit is done from the respective `auction` methods.
        def submit[T](f: Future[T]): T = { val r = Await.result(f, Duration.Inf); txCount += 1; r }

        // First, start the auction.
        submit(
          auction.startAuction(
            sellerAddress = addresses(0),
            oneShotUtxo = Utxo(oneShotInput, seedUtxos(oneShotInput)),
            itemId = itemId,
            startingBid = startingBid,
            auctionEndTime = auctionEnd,
            initialValue = Coin.ada(5),
            signer = seller.signer
          )
        )

        // Then, begin the bid war
        var currentBid = startingBid
        for round <- 1 to rounds do
            for (bidder, i) <- bidders.zipWithIndex do
                currentBid += 1_000_000L
                submit(
                  auction.bid(
                    bidderAddress = addresses(i + 1),
                    bidAmount = currentBid,
                    itemId = itemId,
                    signer = bidder.signer
                  )
                )
                if round % 10 == 0 && i == 0 then println(s"  round $round/$rounds ...")

        provider match
            case e: Emulator         => e.setSlot(1001)
            case h: HydrozoaProvider => h.setSlot(1001)
            case _                   => ()

        submit(
          auction.endAuction(
            sponsorAddress = addresses(0),
            itemId = itemId,
            signer = seller.signer
          )
        )

        val elapsedMs = (System.nanoTime() - t0) / 1_000_000
        println(
          s"  $txCount transactions in ${elapsedMs}ms — ${"%.1f".format(txCount * 1000.0 / elapsedMs)} tx/s"
        )
    }

    private val GenesisHash: TransactionHash =
        TransactionHash.fromHex("0000000000000000000000000000000000000000000000000000000000000000")
}
