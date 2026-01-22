package scalus.examples.auction

import scalus.Compile
import scalus.builtin.Data.toData
import scalus.builtin.{ByteString, Data, ToData}
import scalus.cardano.address.{Address as CardanoAddress, ShelleyAddress, ShelleyDelegationPart, ShelleyPaymentPart}
import scalus.cardano.blueprint.Blueprint
import scalus.cardano.ledger.{AddrKeyHash, AssetName, CardanoInfo, Coin, Transaction, Utxo, Value as LedgerValue}
import scalus.cardano.node.BlockchainProvider
import scalus.cardano.txbuilder.{TransactionSigner, TxBuilder}
import scalus.compiler.Options
import scalus.ledger.api.v1.{Address, Credential, PubKeyHash}
import scalus.ledger.api.v2.OutputDatum
import scalus.ledger.api.v3.*
import scalus.prelude.*
import scalus.uplc.PlutusV3

import java.time.Instant
import scala.concurrent.Future

/** Auction datum representing the state of an auction
  * @param seller
  *   The public key hash of the seller
  * @param highestBidder
  *   The current highest bidder (None if no bids yet)
  * @param highestBid
  *   The current highest bid amount in lovelace
  * @param auctionEndTime
  *   The POSIX time when the auction ends
  * @param itemId
  *   The token name of the auction NFT
  */
case class Datum(
    seller: PubKeyHash,
    highestBidder: Option[PubKeyHash],
    highestBid: BigInt,
    auctionEndTime: PosixTime,
    itemId: ByteString
) derives Data.FromData,
      Data.ToData

@Compile
object Datum {
    given Eq[Datum] = Eq.derived
}

/** Auction, as described in Rosetta Smart Contracts:
  * https://github.com/blockchain-unica/rosetta-smart-contracts/tree/main/contracts/auction
  */

/** Actions that can be performed on the auction contract
  *
  * Bid and End actions include index parameters for O(1) UTxO lookups (indexed UTxO pattern). The
  * indices are computed off-chain using delayed redeemer pattern.
  */
enum Action derives Data.FromData, Data.ToData:
    case Start(
        itemId: ByteString,
        seller: PubKeyHash,
        startingBid: BigInt,
        auctionEndTime: PosixTime
    )

    /** Place a bid on the auction
      * @param amount
      *   The bid amount in lovelace
      * @param bidder
      *   The bidder's public key hash
      * @param inputIdx
      *   Index of the auction input in txInfo.inputs
      * @param outputIdx
      *   Index of the continuing auction output in txInfo.outputs
      * @param refundOutputIdx
      *   Index of refund output for previous bidder (-1 if no previous bidder)
      */
    case Bid(
        amount: BigInt,
        bidder: PubKeyHash,
        inputIdx: BigInt,
        outputIdx: BigInt,
        refundOutputIdx: BigInt
    )

    /** End the auction and transfer item to highest bidder, funds to seller
      * @param inputIdx
      *   Index of the auction input in txInfo.inputs
      * @param sellerOutputIdx
      *   Index of seller's payment output in txInfo.outputs
      * @param winnerOutputIdx
      *   Index of winner's NFT output (-1 if no winner, seller reclaims)
      */
    case End(inputIdx: BigInt, sellerOutputIdx: BigInt, winnerOutputIdx: BigInt)

@Compile
object Action

/** Auction validator parameterized by a one-shot UTxO reference.
  *
  * The oneShot TxOutRef must be spent when starting the auction, ensuring each auction instance has
  * a unique policyId/script hash. This prevents UTXO discovery confusion attacks where multiple
  * auctions could share the same itemId.
  *
  * Uses DataParameterizedValidator so the parameter is passed as Data and can be applied at
  * runtime.
  */
@Compile
object AuctionValidator extends DataParameterizedValidator {

    inline override def spend(
        oneShotData: Data,
        @annotation.unused datum: Option[Data],
        redeemer: Data,
        txInfo: TxInfo,
        txOutRef: TxOutRef
    ): Unit =
        val oneShot = oneShotData.to[TxOutRef]
        // Match on redeemer action and extract input using provided index
        redeemer.to[Action] match
            case Action.Bid(bidAmount, bidder, inputIdx, outputIdx, refundOutputIdx) =>
                // Use indexed lookup instead of searching
                val input = txInfo.inputs.at(inputIdx)
                require(input.outRef === txOutRef, "Input index does not match txOutRef")

                val (scriptHash, inputValue, currentDatum) = input.resolved match
                    case TxOut(
                          Address(Credential.ScriptCredential(sh), _),
                          value,
                          OutputDatum.OutputDatum(inlineDatum),
                          _
                        ) =>
                        (sh, value, inlineDatum.to[Datum])
                    case _ => fail("Auction input must have script credential and inline datum")

                handleBid(
                  txInfo,
                  scriptHash,
                  currentDatum,
                  bidAmount,
                  bidder,
                  outputIdx,
                  refundOutputIdx
                )

            case Action.End(inputIdx, sellerOutputIdx, winnerOutputIdx) =>
                // Use indexed lookup instead of searching
                val input = txInfo.inputs.at(inputIdx)
                require(input.outRef === txOutRef, "Input index does not match txOutRef")

                val (scriptHash, inputValue, currentDatum) = input.resolved match
                    case TxOut(
                          Address(Credential.ScriptCredential(sh), _),
                          value,
                          OutputDatum.OutputDatum(inlineDatum),
                          _
                        ) =>
                        (sh, value, inlineDatum.to[Datum])
                    case _ => fail("Auction input must have script credential and inline datum")

                handleEnd(txInfo, scriptHash, currentDatum, sellerOutputIdx, winnerOutputIdx)

            case Action.Start(_, _, _, _) =>
                fail("Start action is only valid for minting")

    /** Handle bid action using indexed UTxO pattern for O(1) lookups */
    private inline def handleBid(
        txInfo: TxInfo,
        scriptHash: ValidatorHash,
        datum: Datum,
        bidAmount: BigInt,
        bidder: PubKeyHash,
        outputIdx: BigInt,
        refundOutputIdx: BigInt
    ): Unit =
        val Datum(seller, currentHighestBidder, currentHighestBid, auctionEndTime, itemId) = datum

        // 1. Time validation: bid must be before auction end
        require(
          txInfo.validRange.isEntirelyBefore(auctionEndTime),
          "Bid must be placed before auction ends"
        )

        // 2. Bidder must sign the transaction
        require(
          txInfo.isSignedBy(bidder),
          "Bidder must sign the transaction"
        )

        // 3. Bidder cannot be the seller (prevents self-bidding manipulation)
        require(
          !(bidder === seller),
          "Seller cannot bid on their own auction"
        )

        // 4. New bid must be higher than current highest bid
        require(
          bidAmount > currentHighestBid,
          "Bid must be higher than current highest bid"
        )

        // 5. Use indexed lookup for continuing output (O(1) instead of O(n))
        val continuingOutput = txInfo.outputs.at(outputIdx)

        // 6. Verify continuing output goes to the same script address (prevents redirect attack)
        require(
          continuingOutput.address === Address.fromScriptHash(scriptHash),
          "Continuing output must go to auction script address"
        )

        val newDatum = continuingOutput.datum match
            case OutputDatum.OutputDatum(newDatumData) => newDatumData.to[Datum]
            case _ => fail("Continuing auction output must have inline datum")

        // 7. Verify the new datum is correct
        val expectedNewDatum = Datum(
          seller = seller,
          highestBidder = Option.Some(bidder),
          highestBid = bidAmount,
          auctionEndTime = auctionEndTime,
          itemId = itemId
        )
        require(
          newDatum === expectedNewDatum,
          "New datum must reflect the new bid"
        )

        // 8. Verify the auction NFT is preserved in the continuing output
        require(
          continuingOutput.value.quantityOf(scriptHash, itemId) === BigInt(1),
          "Auction NFT must be preserved"
        )

        // 9. Verify the continuing output has at least the bid amount in lovelace
        require(
          continuingOutput.value.getLovelace >= bidAmount,
          "Continuing output must contain at least the bid amount"
        )

        // 10. If there was a previous bidder, verify they get refunded using indexed lookup
        currentHighestBidder match
            case Option.Some(previousBidder) =>
                // refundOutputIdx >= 0 means there should be a refund output
                require(
                  refundOutputIdx >= BigInt(0),
                  "Refund output index required when previous bidder exists"
                )
                val refundOutput = txInfo.outputs.at(refundOutputIdx)
                require(
                  refundOutput.address === Address.fromPubKeyHash(previousBidder),
                  "Refund output must go to previous bidder"
                )
                require(
                  refundOutput.value.getLovelace === currentHighestBid,
                  "Previous bidder must receive exactly their bid amount"
                )
            case Option.None =>
                // No previous bidder, no refund needed
                ()

    /** Handle end action using indexed UTxO pattern for O(1) lookups */
    private inline def handleEnd(
        txInfo: TxInfo,
        scriptHash: ValidatorHash,
        datum: Datum,
        sellerOutputIdx: BigInt,
        winnerOutputIdx: BigInt
    ): Unit =
        val Datum(seller, currentHighestBidder, currentHighestBid, auctionEndTime, itemId) = datum

        // 1. Time validation: must be after auction end
        require(
          txInfo.validRange.isEntirelyAfter(auctionEndTime),
          "Auction can only end after the end time"
        )

        // 2. Verify only one auction NFT is being spent from this script (prevents double satisfaction)
        // This ensures each End action corresponds to exactly one auction
        val scriptAddress = Address.fromScriptHash(scriptHash)
        val totalAuctionNftsSpent = txInfo.inputs.foldLeft(BigInt(0)) { (count, input) =>
            if input.resolved.address === scriptAddress then
                count + input.resolved.value.tokens(scriptHash).values.foldLeft(BigInt(0))(_ + _)
            else count
        }
        require(
          totalAuctionNftsSpent === BigInt(1),
          "Only one auction can be ended per transaction (prevents double satisfaction)"
        )

        currentHighestBidder match
            case Option.Some(winner) =>
                // 3. Winner cannot be the seller (defense in depth - also checked in handleBid)
                require(
                  !(winner === seller),
                  "Seller cannot be the winner"
                )

                // 3. Winner must receive the NFT (the auctioned item) - use indexed lookup
                require(
                  winnerOutputIdx >= BigInt(0),
                  "Winner output index required when there is a winner"
                )
                val winnerOutput = txInfo.outputs.at(winnerOutputIdx)
                require(
                  winnerOutput.address === Address.fromPubKeyHash(winner),
                  "Winner output must go to the winner"
                )
                // Verify winner receives exactly this auction's NFT (prevents double satisfaction)
                // If multiple auctions shared this output, it would have multiple NFTs
                val totalNftsInWinnerOutput =
                    winnerOutput.value.tokens(scriptHash).values.foldLeft(BigInt(0))(_ + _)
                require(
                  totalNftsInWinnerOutput === BigInt(1),
                  "Winner output must have exactly one auction NFT (no bundling)"
                )
                require(
                  winnerOutput.value.quantityOf(scriptHash, itemId) === BigInt(1),
                  "Winner must receive this auction's NFT"
                )

                // 3. Seller must receive the highest bid amount - use indexed lookup
                val sellerOutput = txInfo.outputs.at(sellerOutputIdx)
                require(
                  sellerOutput.address === Address.fromPubKeyHash(seller),
                  "Seller output must go to the seller"
                )
                require(
                  sellerOutput.value.getLovelace >= currentHighestBid,
                  "Seller must receive at least the highest bid amount"
                )

            case Option.None =>
                // No bidders - seller can reclaim the item
                // Seller must sign to end without bids
                require(
                  txInfo.isSignedBy(seller),
                  "Seller must sign to end auction without bids"
                )
                // NFT goes back to seller - use indexed lookup
                val sellerOutput = txInfo.outputs.at(sellerOutputIdx)
                require(
                  sellerOutput.address === Address.fromPubKeyHash(seller),
                  "Seller output must go to the seller"
                )
                // Verify seller receives exactly this auction's NFT (prevents double satisfaction)
                val totalNftsInSellerOutput =
                    sellerOutput.value.tokens(scriptHash).values.foldLeft(BigInt(0))(_ + _)
                require(
                  totalNftsInSellerOutput === BigInt(1),
                  "Seller output must have exactly one auction NFT (no bundling)"
                )
                require(
                  sellerOutput.value.quantityOf(scriptHash, itemId) === BigInt(1),
                  "Seller must receive back this auction's NFT"
                )

    inline override def mint(
        oneShotData: Data,
        redeemer: Data,
        policyId: PolicyId,
        txInfo: TxInfo
    ): Unit =
        val oneShot = oneShotData.to[TxOutRef]
        val action = redeemer.to[Action]

        action match
            case Action.Start(itemId, seller, startingBid, auctionEndTime) =>
                handleMint(oneShot, policyId, txInfo, itemId, seller, startingBid, auctionEndTime)
            case _ =>
                // For End action - burning is allowed
                handleBurn(policyId, txInfo)

    private inline def handleMint(
        oneShot: TxOutRef,
        policyId: PolicyId,
        txInfo: TxInfo,
        itemId: ByteString,
        seller: PubKeyHash,
        startingBid: BigInt,
        auctionEndTime: PosixTime
    ): Unit =
        // 1. Verify the one-shot UTxO is being spent (ensures unique policyId per auction)
        require(
          txInfo.inputs.exists(_.outRef === oneShot),
          "Must spend the one-shot UTxO to create auction"
        )

        // 2. Seller must sign the transaction
        require(
          txInfo.isSignedBy(seller),
          "Seller must sign to start auction"
        )

        // 3. Validate ALL tokens minted under this policy (prevents Other Token Name Attack)
        val mintedTokens = txInfo.mint.tokens(policyId)
        require(
          mintedTokens.size === BigInt(1),
          "Only one token name allowed per auction start"
        )
        val (mintedTokenName, mintedQuantity) = mintedTokens.toList.head
        require(
          mintedTokenName === itemId && mintedQuantity === BigInt(1),
          "Must mint exactly one auction NFT with the specified itemId"
        )

        // 4. The auction end time must be in the future
        require(
          txInfo.validRange.isEntirelyBefore(auctionEndTime),
          "Auction end time must be in the future"
        )

        // 5. Starting bid must be positive
        require(
          startingBid > BigInt(0),
          "Starting bid must be positive"
        )

        // 6. Find the output going to the script address
        val auctionOutput = txInfo.outputs.filter { out =>
            out.address === Address.fromScriptHash(policyId)
        }.match
            case List.Cons(out, List.Nil) => out
            case _ => fail("There must be exactly one output to the auction script")

        // 7. Verify the output contains the minted NFT
        require(
          auctionOutput.value.quantityOf(policyId, itemId) === BigInt(1),
          "Auction output must contain the minted NFT"
        )

        // 8. Verify the datum is correct
        val expectedDatum = Datum(
          seller = seller,
          highestBidder = Option.None,
          highestBid = startingBid,
          auctionEndTime = auctionEndTime,
          itemId = itemId
        )
        auctionOutput.datum match
            case OutputDatum.OutputDatum(datumData) =>
                require(
                  datumData.to[Datum] === expectedDatum,
                  "Initial auction datum must be correct"
                )
            case _ => fail("Auction output must have inline datum")

    private inline def handleBurn(
        policyId: PolicyId,
        txInfo: TxInfo
    ): Unit =
        // For burning, verify all tokens of this policy are burned (negative quantity)
        val mintedTokens = txInfo.mint.tokens(policyId)
        require(
          mintedTokens.forall { case (_, amount) => amount < BigInt(0) },
          "Only burning is allowed (all amounts must be negative)"
        )
}

private given Options = Options.release

/** Compiled parameterized auction validator. Apply a TxOutRef (as Data) to get a unique auction
  * instance.
  */
lazy val AuctionContract: PlutusV3[Data => Data => Unit] =
    PlutusV3.compile(AuctionValidator.validate)

/** Factory for creating auction instances with unique policyIds.
  *
  * Each auction is parameterized by a one-shot UTxO reference, ensuring globally unique policyIds.
  * This prevents UTXO discovery confusion attacks where multiple auctions could share the same
  * itemId.
  *
  * @param provider
  *   Node provider for queries and submission
  * @param withErrorTraces
  *   If true, include error traces for debugging (default: false for production)
  */
class AuctionFactory(provider: BlockchainProvider, withErrorTraces: Boolean = false) {

    private val baseContract =
        if withErrorTraces then AuctionContract.withErrorTraces else AuctionContract

    /** Creates a new auction instance parameterized by the given one-shot UTxO.
      *
      * @param oneShot
      *   UTxO reference that will be spent to create the auction (ensures unique policyId)
      * @return
      *   AuctionInstance with unique policyId and script address
      */
    def createInstance(oneShot: TxOutRef): AuctionInstance = {
        val appliedContract = baseContract.apply(Data.toData(oneShot))
        AuctionInstance(
          provider = provider,
          oneShot = oneShot,
          compiledContract = appliedContract
        )
    }
}

/** A specific auction instance with a unique policyId derived from the one-shot UTxO.
  *
  * @param provider
  *   Node provider for queries and submission
  * @param oneShot
  *   The UTxO reference that parameterizes this auction (must be spent on creation)
  * @param compiledContract
  *   The compiled contract with oneShot applied
  */
class AuctionInstance(
    provider: BlockchainProvider,
    val oneShot: TxOutRef,
    compiledContract: PlutusV3[Data => Unit]
) {
    private def env: CardanoInfo = provider.cardanoInfo
    private val script = compiledContract.script
    private val scriptHash: scalus.cardano.ledger.ScriptHash = script.scriptHash
    def scriptAddress: CardanoAddress = compiledContract.address(env.network)

    /** Extract PubKeyHash from a ShelleyAddress */
    private def extractPkh(address: ShelleyAddress): PubKeyHash =
        address.payment match
            case ShelleyPaymentPart.Key(hash) => PubKeyHash(hash)
            case _ => throw IllegalArgumentException("Address must have key payment credential")

    /** Create a ShelleyAddress from a PubKeyHash */
    private def addressFromPkh(pkh: PubKeyHash): ShelleyAddress =
        ShelleyAddress(
          env.network,
          ShelleyPaymentPart.Key(AddrKeyHash.fromByteString(pkh.hash)),
          ShelleyDelegationPart.Null
        )

    /** Starts an auction for the given itemId by minting an NFT representing the item.
      *
      * The oneShot UTxO (used to parameterize this auction instance) must be owned by the seller
      * and will be spent in this transaction to ensure the auction can only be created once.
      *
      * @param sellerAddress
      *   The seller's address for receiving funds and signing
      * @param oneShotUtxo
      *   The UTxO to spend as one-shot (must match the oneShot used to create this instance)
      * @param itemId
      *   Unique identifier for the auctioned item (becomes token name)
      * @param startingBid
      *   Minimum bid amount in lovelace
      * @param auctionEndTime
      *   POSIX timestamp when the auction ends
      * @param initialValue
      *   Initial ADA locked with the auction (for min UTxO requirements)
      * @param signer
      *   Transaction signer with seller's keys
      * @return
      *   The submitted transaction
      */
    def startAuction(
        sellerAddress: ShelleyAddress,
        oneShotUtxo: Utxo,
        itemId: ByteString,
        startingBid: Long,
        auctionEndTime: PosixTime,
        initialValue: Coin,
        signer: TransactionSigner
    ): Future[Transaction] =
        given scala.concurrent.ExecutionContext = provider.executionContext
        // Verify the provided UTxO matches the oneShot parameter
        require(
          oneShotUtxo.input.transactionId == oneShot.id.hash &&
              oneShotUtxo.input.index == oneShot.idx.toInt,
          s"Provided UTxO ${oneShotUtxo.input} does not match oneShot ${oneShot}"
        )

        val sellerPkh = extractPkh(sellerAddress)
        for
            _ <- Future.unit
            datum = Datum(
              seller = sellerPkh,
              highestBidder = Option.None,
              highestBid = BigInt(startingBid),
              auctionEndTime = auctionEndTime,
              itemId = itemId
            )

            redeemer = Action.Start(
              itemId = itemId,
              seller = sellerPkh,
              startingBid = BigInt(startingBid),
              auctionEndTime = auctionEndTime
            )

            nftAsset = AssetName(itemId)
            mintedValue = LedgerValue.asset(scriptHash, nftAsset, 1L)
            sellerAddrKeyHash = AddrKeyHash.fromByteString(sellerPkh.hash)

            // Spend the oneShot UTxO and mint the auction NFT
            tx <- TxBuilder(env)
                .spend(oneShotUtxo) // Spend the one-shot UTxO (pubkey-protected)
                .mint(script, Map(nftAsset -> 1L), redeemer, Set(sellerAddrKeyHash))
                .payTo(scriptAddress, LedgerValue(initialValue) + mintedValue, datum)
                .validTo(Instant.ofEpochMilli(auctionEndTime.toLong - 1000))
                .complete(provider, sellerAddress)
                .map(_.sign(signer).transaction)

            _ <- provider.submit(tx).map {
                case Right(_)    => ()
                case Left(error) => throw RuntimeException(s"Failed to submit: $error")
            }
        yield tx

    /** Places a bid on this auction.
      *
      * Since each auction instance has a unique policyId, we can directly query by script address
      * without risk of UTXO discovery confusion.
      *
      * @param bidderAddress
      *   The bidder's address
      * @param bidAmount
      *   The bid amount in lovelace
      * @param signer
      *   Transaction signer with bidder's keys
      * @return
      *   The submitted transaction
      */
    def bid(
        bidderAddress: ShelleyAddress,
        bidAmount: Long,
        signer: TransactionSigner
    ): Future[Transaction] =
        given scala.concurrent.ExecutionContext = provider.executionContext
        val bidderPkh = extractPkh(bidderAddress)
        for
            auctionUtxo <- findAuctionUtxo().map(
              _.getOrElse(throw RuntimeException(s"No active auction found at $scriptAddress"))
            )
            currentDatum = auctionUtxo.output.inlineDatum
                .getOrElse(throw IllegalStateException("Auction UTxO must have inline datum"))
                .to[Datum]

            newDatum = currentDatum.copy(
              highestBidder = Option.Some(bidderPkh),
              highestBid = BigInt(bidAmount)
            )

            nftAsset = AssetName(currentDatum.itemId)
            nftValue = LedgerValue.asset(scriptHash, nftAsset, 1L)
            newAuctionValue = LedgerValue.lovelace(bidAmount) + nftValue

            // Calculate previous bidder address for refund output index computation
            prevBidderAddr: scala.Option[ShelleyAddress] = currentDatum.highestBidder match
                case Option.Some(prevBidder) => scala.Some(addressFromPkh(prevBidder))
                case Option.None             => scala.None

            // Build transaction with delayed redeemer that computes indices
            // The redeemerBuilder receives the complete transaction and computes indices
            builder = TxBuilder(env)
                .spend(
                  auctionUtxo,
                  redeemerBuilder = (tx: Transaction) => {
                      // Compute input index - find our auction input
                      val inputIdx = tx.body.value.inputs.toSeq.indexOf(auctionUtxo.input)

                      // Compute output index - find the continuing auction output
                      val outputIdx = tx.body.value.outputs.indexWhere { sized =>
                          sized.value.address == scriptAddress
                      }

                      // Compute refund output index if there was a previous bidder
                      val refundOutputIdx = prevBidderAddr match
                          case scala.Some(addr) =>
                              tx.body.value.outputs.indexWhere { sized =>
                                  sized.value.address == addr
                              }
                          case scala.None => -1

                      Action
                          .Bid(
                            BigInt(bidAmount),
                            bidderPkh,
                            BigInt(inputIdx),
                            BigInt(outputIdx),
                            BigInt(refundOutputIdx)
                          )
                          .toData
                  },
                  script,
                  Set(AddrKeyHash.fromByteString(bidderPkh.hash))
                )
                .payTo(scriptAddress, newAuctionValue, newDatum)
                .validTo(Instant.ofEpochMilli(currentDatum.auctionEndTime.toLong - 1000))

            builderWithRefund = prevBidderAddr match
                case scala.Some(addr) =>
                    builder.payTo(addr, LedgerValue.lovelace(currentDatum.highestBid.toLong))
                case scala.None => builder

            tx <- builderWithRefund
                .complete(provider, bidderAddress)
                .map(_.sign(signer).transaction)

            _ <- provider.submit(tx).map {
                case Right(_)    => ()
                case Left(error) => throw RuntimeException(s"Failed to submit: $error")
            }
        yield tx

    /** Ends this auction.
      *
      * Transfers the NFT to the winner and funds to the seller. If no bids were placed, the seller
      * reclaims the NFT (seller must sign).
      *
      * Since each auction instance has a unique policyId, we can directly query by script address
      * without risk of UTXO discovery confusion.
      *
      * @param sponsorAddress
      *   Address to pay transaction fees from
      * @param signer
      *   Transaction signer (seller must sign if no bids)
      * @return
      *   The submitted transaction
      */
    def endAuction(
        sponsorAddress: ShelleyAddress,
        signer: TransactionSigner
    ): Future[Transaction] =
        given scala.concurrent.ExecutionContext = provider.executionContext
        for
            auctionUtxo <- findAuctionUtxo().map(
              _.getOrElse(throw RuntimeException(s"No active auction found at $scriptAddress"))
            )
            currentDatum = auctionUtxo.output.inlineDatum
                .getOrElse(throw IllegalStateException("Auction UTxO must have inline datum"))
                .to[Datum]

            nftAsset = AssetName(currentDatum.itemId)
            nftValue = LedgerValue.asset(scriptHash, nftAsset, 1L)

            sellerAddr = addressFromPkh(currentDatum.seller)
            sellerAddrKeyHash = AddrKeyHash.fromByteString(currentDatum.seller.hash)

            // Determine required signers based on whether there are bids
            // If no bids, seller must sign to reclaim NFT
            spendRequiredSigners = currentDatum.highestBidder match
                case Option.Some(_) => Set.empty[AddrKeyHash]
                case Option.None    => Set(sellerAddrKeyHash)

            // Calculate winner address for output index computation
            winnerAddr: scala.Option[ShelleyAddress] = currentDatum.highestBidder match
                case Option.Some(winner) => scala.Some(addressFromPkh(winner))
                case Option.None         => scala.None

            // Build transaction with delayed redeemer that computes indices
            builder = TxBuilder(env)
                .spend(
                  auctionUtxo,
                  redeemerBuilder = (tx: Transaction) => {
                      // Compute input index - find our auction input
                      val inputIdx = tx.body.value.inputs.toSeq.indexOf(auctionUtxo.input)

                      // Compute seller output index
                      val sellerOutputIdx = tx.body.value.outputs.indexWhere { sized =>
                          sized.value.address == sellerAddr
                      }

                      // Compute winner output index if there is a winner
                      val winnerOutputIdx = winnerAddr match
                          case scala.Some(addr) =>
                              tx.body.value.outputs.indexWhere { sized =>
                                  sized.value.address == addr
                              }
                          case scala.None => -1

                      Action
                          .End(BigInt(inputIdx), BigInt(sellerOutputIdx), BigInt(winnerOutputIdx))
                          .toData
                  },
                  script,
                  spendRequiredSigners
                )
                .validFrom(Instant.ofEpochMilli(currentDatum.auctionEndTime.toLong + 1000))

            builderWithOutputs = winnerAddr match
                case scala.Some(addr) =>
                    // Winner gets the NFT (auctioned item), seller gets the bid amount
                    builder
                        .payTo(addr, LedgerValue.lovelace(2_000_000L) + nftValue)
                        .payTo(sellerAddr, LedgerValue.lovelace(currentDatum.highestBid.toLong))
                case scala.None =>
                    // No bids - seller reclaims the NFT (auctioned item)
                    builder.payTo(sellerAddr, LedgerValue.lovelace(2_000_000L) + nftValue)

            tx <- builderWithOutputs
                .complete(provider, sponsorAddress)
                .map(_.sign(signer).transaction)

            _ <- provider.submit(tx).map {
                case Right(_)    => ()
                case Left(error) => throw RuntimeException(s"Failed to submit: $error")
            }
        yield tx

    /** Finds the auction UTxO at this auction's script address.
      *
      * Since each auction instance has a unique policyId (derived from the one-shot UTxO), there
      * can only be one active UTxO at this script address. This eliminates UTXO discovery
      * confusion.
      *
      * @return
      *   The auction UTxO if found
      */
    def findAuctionUtxo(): Future[scala.Option[Utxo]] =
        given scala.concurrent.ExecutionContext = provider.executionContext
        provider
            .queryUtxos { u =>
                u.output.address == scriptAddress
            }
            .limit(1)
            .execute()
            .map {
                case Right(utxos) =>
                    utxos.headOption.map { case (input, output) => Utxo(input, output) }
                case Left(_) => scala.None
            }
}
