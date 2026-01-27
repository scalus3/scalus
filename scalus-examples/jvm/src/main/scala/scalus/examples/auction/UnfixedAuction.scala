package scalus.examples.auction

import scalus.Compile
import scalus.uplc.builtin.{ByteString, Data}
import scalus.cardano.onchain.plutus.v1.{Address, Credential, PubKeyHash}
import scalus.cardano.onchain.plutus.v2.OutputDatum
import scalus.cardano.onchain.plutus.v3.{Datum as _, *}
import scalus.cardano.onchain.plutus.prelude.*
import scalus.cardano.onchain.plutus.v3.Validator

/** VULNERABLE Auction Validator - DO NOT USE IN PRODUCTION
  *
  * This validator demonstrates the Double Satisfaction vulnerability (V005). When multiple auctions
  * with the same seller end in a single transaction, a single seller output can satisfy validation
  * for all of them.
  *
  * Attack scenario:
  *   1. Bob wins AuctionA (seller=Alice, highestBid=10 ADA) and AuctionB (seller=Alice,
  *      highestBid=10 ADA)
  *   2. Bob ends both auctions in one transaction
  *   3. Bob creates only ONE output of 10 ADA to Alice (instead of 20 ADA)
  *   4. Both validators pass because they share the same sellerOutputIdx
  *   5. Alice loses 10 ADA
  *
  * The fix is to require the auction NFT to be burned, creating a 1:1 mapping between each auction
  * End and its unique NFT burn operation.
  */
@Compile
object UnfixedAuctionValidator extends Validator {

    inline override def spend(
        @annotation.unused datum: Option[Data],
        redeemer: Data,
        txInfo: TxInfo,
        txOutRef: TxOutRef
    ): Unit =
        redeemer.to[Action] match
            case Action.Bid(bidAmount, bidder, inputIdx, outputIdx, refundOutputIdx) =>
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

        require(
          txInfo.validRange.isEntirelyBefore(auctionEndTime),
          "Bid must be placed before auction ends"
        )

        require(
          txInfo.isSignedBy(bidder),
          "Bidder must sign the transaction"
        )

        require(
          !(bidder === seller),
          "Seller cannot bid on their own auction"
        )

        require(
          bidAmount > currentHighestBid,
          "Bid must be higher than current highest bid"
        )

        val continuingOutput = txInfo.outputs.at(outputIdx)

        require(
          continuingOutput.address === Address.fromScriptHash(scriptHash),
          "Continuing output must go to auction script address"
        )

        val newDatum = continuingOutput.datum match
            case OutputDatum.OutputDatum(newDatumData) => newDatumData.to[Datum]
            case _ => fail("Continuing auction output must have inline datum")

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

        require(
          continuingOutput.value.quantityOf(scriptHash, itemId) === BigInt(1),
          "Auction NFT must be preserved"
        )

        require(
          continuingOutput.value.getLovelace >= bidAmount,
          "Continuing output must contain at least the bid amount"
        )

        currentHighestBidder match
            case Option.Some(previousBidder) =>
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
                ()

    /** VULNERABLE: This function has a Double Satisfaction vulnerability.
      *
      * The problem is at the seller output validation:
      *   - It uses `>=` instead of `===` for the payment amount
      *   - Multiple auction Ends can share the same sellerOutputIdx
      *   - No unique linking between this auction and its output
      *
      * FIX: Add `require(txInfo.mint.quantityOf(scriptHash, itemId) === BigInt(-1))` to ensure each
      * auction's NFT is burned, creating 1:1 input-burn mapping.
      */
    private inline def handleEnd(
        txInfo: TxInfo,
        scriptHash: ValidatorHash,
        datum: Datum,
        sellerOutputIdx: BigInt,
        winnerOutputIdx: BigInt
    ): Unit =
        val Datum(seller, currentHighestBidder, currentHighestBid, auctionEndTime, itemId) = datum

        require(
          txInfo.validRange.isEntirelyAfter(auctionEndTime),
          "Auction can only end after the end time"
        )

        // VULNERABILITY: No check that this auction's NFT is being burned!
        // This allows multiple auctions to share the same seller output.

        currentHighestBidder match
            case Option.Some(winner) =>
                require(
                  !(winner === seller),
                  "Seller cannot be the winner"
                )

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

                // VULNERABLE: Uses >= and no unique linking to this specific auction
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
                require(
                  txInfo.isSignedBy(seller),
                  "Seller must sign to end auction without bids"
                )
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
                handleBurn(policyId, txInfo)

    private inline def handleMint(
        policyId: PolicyId,
        txInfo: TxInfo,
        itemId: ByteString,
        seller: PubKeyHash,
        startingBid: BigInt,
        auctionEndTime: PosixTime
    ): Unit =
        require(
          txInfo.isSignedBy(seller),
          "Seller must sign to start auction"
        )

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

        require(
          txInfo.validRange.isEntirelyBefore(auctionEndTime),
          "Auction end time must be in the future"
        )

        require(
          startingBid > BigInt(0),
          "Starting bid must be positive"
        )

        val auctionOutput = txInfo.outputs.filter { out =>
            out.address === Address.fromScriptHash(policyId)
        }.match
            case List.Cons(out, List.Nil) => out
            case _ => fail("There must be exactly one output to the auction script")

        require(
          auctionOutput.value.quantityOf(policyId, itemId) === BigInt(1),
          "Auction output must contain the minted NFT"
        )

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
        val mintedTokens = txInfo.mint.tokens(policyId)
        require(
          mintedTokens.forall { case (_, amount) => amount < BigInt(0) },
          "Only burning is allowed (all amounts must be negative)"
        )
}
