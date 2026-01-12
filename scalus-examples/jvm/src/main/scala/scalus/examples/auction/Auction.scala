package scalus.examples.auction

import scalus.builtin.Data.toData
import scalus.builtin.{ByteString, Data, ToData}
import scalus.cardano.address.{Address as CardanoAddress, ShelleyAddress, ShelleyDelegationPart, ShelleyPaymentPart}
import scalus.cardano.blueprint.Blueprint
import scalus.cardano.ledger.{AddrKeyHash, AssetName, CardanoInfo, Coin, DatumOption, Transaction, Utxo, Value as LedgerValue}
import scalus.cardano.node.Provider
import scalus.cardano.txbuilder.{TransactionSigner, TxBuilder}
import scalus.compiler.Options
import scalus.ledger.api.v1.{Address, Credential, PubKeyHash}
import scalus.ledger.api.v2.OutputDatum
import scalus.ledger.api.v3.*
import scalus.prelude.*
import scalus.uplc.PlutusV3
import scalus.Compile

import java.time.Instant
import scala.concurrent.{ExecutionContext, Future}

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
    given Eq[Datum] = (a: Datum, b: Datum) =>
        a.seller === b.seller &&
            a.highestBidder === b.highestBidder &&
            a.highestBid === b.highestBid &&
            a.auctionEndTime === b.auctionEndTime &&
            a.itemId === b.itemId
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

@Compile
object AuctionValidator extends Validator {

    inline override def spend(
        @annotation.unused datum: Option[Data],
        redeemer: Data,
        txInfo: TxInfo,
        txOutRef: TxOutRef
    ): Unit =
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

        currentHighestBidder match
            case Option.Some(winner) =>
                // 2. Winner cannot be the seller (defense in depth - also checked in handleBid)
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
                require(
                  winnerOutput.value.quantityOf(scriptHash, itemId) === BigInt(1),
                  "Winner must receive the auction NFT"
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
                require(
                  sellerOutput.value.quantityOf(scriptHash, itemId) === BigInt(1),
                  "Seller must receive back the auction NFT"
                )

    inline override def mint(
        redeemer: Data,
        policyId: PolicyId,
        txInfo: TxInfo
    ): Unit =
        val action = redeemer.to[Action]

        action match
            case Action.Start(itemId, seller, startingBid, auctionEndTime) =>
                handleMint(policyId, txInfo, itemId, seller, startingBid, auctionEndTime)
            case _ =>
                // For End action - burning is allowed
                handleBurn(policyId, txInfo)

    private inline def handleMint(
        policyId: PolicyId,
        txInfo: TxInfo,
        itemId: ByteString,
        seller: PubKeyHash,
        startingBid: BigInt,
        auctionEndTime: PosixTime
    ): Unit =
        // 1. Seller must sign the transaction
        require(
          txInfo.isSignedBy(seller),
          "Seller must sign to start auction"
        )

        // 2. Only one NFT token should be minted with this itemId
        require(
          txInfo.mint.quantityOf(policyId, itemId) === BigInt(1),
          "Exactly one auction NFT must be minted"
        )

        // 3. The auction end time must be in the future
        require(
          txInfo.validRange.isEntirelyBefore(auctionEndTime),
          "Auction end time must be in the future"
        )

        // 4. Starting bid must be positive
        require(
          startingBid > BigInt(0),
          "Starting bid must be positive"
        )

        // 5. Find the output going to the script address
        val auctionOutput = txInfo.outputs.filter { out =>
            out.address === Address.fromScriptHash(policyId)
        }.match
            case List.Cons(out, List.Nil) => out
            case _ => fail("There must be exactly one output to the auction script")

        // 6. Verify the output contains the minted NFT
        require(
          auctionOutput.value.quantityOf(policyId, itemId) === BigInt(1),
          "Auction output must contain the minted NFT"
        )

        // 7. Verify the datum is correct
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
lazy val AuctionContract = PlutusV3.compile(AuctionValidator.validate)
lazy val AuctionBlueprint = Blueprint.plutusV3[Datum, Action](
  title = "Auction validator",
  description = "English auction smart contract for Cardano",
  version = "1.0.0",
  license = Some("Apache License Version 2.0"),
  compiled = AuctionContract
)

class AuctionEndpoints(
    env: CardanoInfo,
    provider: Provider,
    compiledContract: PlutusV3[Data => Unit]
) {
    private val script = compiledContract.script
    private val policyId = script.scriptHash
    val scriptAddress: CardanoAddress = compiledContract.address(env.network)

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
      * @param sellerAddress
      *   The seller's address for receiving funds and signing
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
        itemId: ByteString,
        startingBid: Long,
        auctionEndTime: PosixTime,
        initialValue: Coin,
        signer: TransactionSigner
    )(using ExecutionContext): Future[Transaction] =
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
            mintedValue = LedgerValue.asset(policyId, nftAsset, 1L)
            sellerAddrKeyHash = AddrKeyHash.fromByteString(sellerPkh.hash)

            tx <- TxBuilder(env)
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

    /** Places a bid on the auction identified by itemId.
      *
      * @param itemId
      *   The auction item identifier (token name)
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
        itemId: ByteString,
        bidderAddress: ShelleyAddress,
        bidAmount: Long,
        signer: TransactionSigner
    )(using ExecutionContext): Future[Transaction] =
        val bidderPkh = extractPkh(bidderAddress)
        for
            auctionUtxo <- findActiveUtxo(itemId).map(
              _.getOrElse(throw RuntimeException(s"No active auction found for itemId: $itemId"))
            )
            currentDatum = extractDatum(auctionUtxo)

            newDatum = currentDatum.copy(
              highestBidder = Option.Some(bidderPkh),
              highestBid = BigInt(bidAmount)
            )

            nftAsset = AssetName(currentDatum.itemId)
            nftValue = LedgerValue.asset(policyId, nftAsset, 1L)
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

    /** Ends the auction identified by itemId.
      *
      * Transfers the NFT to the winner and funds to the seller. If no bids were placed, the seller
      * reclaims the NFT (seller must sign).
      *
      * @param itemId
      *   The auction item identifier (token name)
      * @param sponsorAddress
      *   Address to pay transaction fees from
      * @param signer
      *   Transaction signer (seller must sign if no bids)
      * @return
      *   The submitted transaction
      */
    def endAuction(
        itemId: ByteString,
        sponsorAddress: ShelleyAddress,
        signer: TransactionSigner
    )(using ExecutionContext): Future[Transaction] =
        for
            auctionUtxo <- findActiveUtxo(itemId).map(
              _.getOrElse(throw RuntimeException(s"No active auction found for itemId: $itemId"))
            )
            currentDatum = extractDatum(auctionUtxo)

            nftAsset = AssetName(currentDatum.itemId)
            nftValue = LedgerValue.asset(policyId, nftAsset, 1L)

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

    /** Finds the active auction UTxO containing the auction token with the given itemId.
      *
      * The auction is identified by the NFT token with policyId equal to the script hash and
      * tokenName equal to the itemId.
      *
      * @param itemId
      *   The auction item identifier (token name of the auction NFT)
      * @return
      *   The auction UTxO if found
      */
    def findActiveUtxo(itemId: ByteString)(using ExecutionContext): Future[scala.Option[Utxo]] =
        for utxos <- provider
                .findUtxos(scriptAddress)
                .map(_.getOrElse(Map.empty))
        yield
            val nftAsset = AssetName(itemId)
            utxos
                .find { case (_, output) =>
                    // Check if this UTxO contains the auction NFT
                    output.value.assets.assets.get(policyId).exists(_.get(nftAsset).exists(_ > 0))
                }
                .map { case (input, output) =>
                    Utxo(input, output)
                }

    private def extractDatum(utxo: Utxo): Datum =
        utxo.output.datumOption match
            case Some(DatumOption.Inline(data)) =>
                scalus.builtin.Data.fromData[Datum](data)
            case _ =>
                throw IllegalStateException("Expected inline datum in auction UTxO")
}
