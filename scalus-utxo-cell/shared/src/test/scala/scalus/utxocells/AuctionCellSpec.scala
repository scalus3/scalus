package scalus.utxocells

import org.scalatest.funsuite.AnyFunSuite
import scalus.Compile
import scalus.compiler.Options
import scalus.uplc.PlutusV3
import scalus.uplc.builtin.ByteString
import scalus.uplc.builtin.ByteString.*
import scalus.uplc.builtin.Data
import scalus.uplc.builtin.Data.toData
import scalus.uplc.builtin.{FromData, ToData}
import scalus.cardano.ledger.{CardanoInfo, Value as LedgerValue, *}
import scalus.cardano.node.Emulator
import scalus.cardano.txbuilder.{TxBuilder, TxBuilderException}
import scalus.cardano.onchain.plutus.v1.{Address, Credential, PolicyId, PosixTime, PubKeyHash, Value}
import scalus.cardano.onchain.plutus.v3.*
import scalus.cardano.onchain.plutus.prelude.*
import scalus.testing.kit.Party.{Alice, Bob, Charles}

import UtxoCellBuilder.*

// -- Data types --

case class AuctionState(
    seller: PubKeyHash,
    highestBidder: Option[PubKeyHash],
    highestBid: BigInt,
    auctionEndTime: PosixTime
) derives FromData,
      ToData

@Compile
object AuctionState {
    given Eq[AuctionState] = Eq.derived
}

enum AuctionAction derives FromData, ToData:
    case Bid(bidder: PubKeyHash, amount: BigInt)
    case Close

@Compile
object AuctionAction

// -- Validator --

@Compile
object AuctionCell extends CellValidator {

    val beaconName: ByteString = utf8"auction"

    def transition(
        state: AuctionState,
        action: AuctionAction,
        ctx: CellContext
    ): Option[AuctionState] =
        action match
            case AuctionAction.Bid(bidder, amount) =>
                ctx.txInfo.requireValidBefore(state.auctionEndTime)
                ctx.txInfo.requireSignedBy(bidder)
                require(amount > state.highestBid, "Auction: bid too low")
                require(!(bidder === state.seller), "Auction: seller cannot bid")
                // Set the continuing output value to the new bid amount
                // (beacon token is auto-included by UtxoCellDef)
                ctx.setContinuingValue(Value.lovelace(amount))
                // Refund previous bidder
                state.highestBidder match
                    case Option.Some(prev) =>
                        ctx.txInfo.outputs.add(
                          Address.fromPubKeyHash(prev),
                          Value.lovelace(state.highestBid)
                        )
                    case Option.None => ()
                Option.Some(
                  AuctionState(state.seller, Option.Some(bidder), amount, state.auctionEndTime)
                )
            case AuctionAction.Close =>
                ctx.txInfo.requireValidAfter(state.auctionEndTime)
                state.highestBidder match
                    case Option.Some(_) =>
                        // Seller gets the bid amount
                        ctx.txInfo.outputs.add(
                          Address.fromPubKeyHash(state.seller),
                          Value.lovelace(state.highestBid)
                        )
                    case Option.None =>
                        // No bids — seller must sign to reclaim
                        ctx.txInfo.requireSignedBy(state.seller)
                Option.None // terminal

    inline override def spendCell(
        datum: Option[Data],
        redeemer: Data,
        sc: ScriptContext,
        ownRef: TxOutRef
    ): Unit = {
        val state = datum.getOrFail("Auction: missing datum").to[AuctionState]
        val action = redeemer.to[AuctionAction]
        val ctx: CellContext = sc.toData.asInstanceOf[CellContext]
        val nextState = transition(state, action, ctx)
        UtxoCellLib.verifyContinuingOutput(nextState, sc.txInfo, ownRef)
        nextState match
            case Option.None =>
                UtxoCellLib.verifyBurnBeacon(beaconName, ctx.ownPolicyId, sc.txInfo)
            case _ => ()
    }

    inline override def mintCell(
        redeemer: Data,
        policyId: PolicyId,
        sc: ScriptContext
    ): Unit = {
        val qty = sc.txInfo.mint.quantityOf(policyId, beaconName)
        if qty === BigInt(1) then
            // Mint redeemer is Data.I(0) — just verify initial state
            UtxoCellLib.verifyMintResult(
              redeemer.to[AuctionState],
              beaconName,
              policyId,
              sc.txInfo
            )
        else if qty === BigInt(-1) then
            UtxoCellLib.verifyBurnBeacon(beaconName, policyId, sc.txInfo)
        else fail("Auction: invalid beacon mint quantity")
    }
}

object AuctionCellCompilation {
    given Options = Options.debug
    lazy val compiled = PlutusV3.compile(AuctionCell.validate)
}

// -- Emulator tests --

class AuctionCellSpec extends AnyFunSuite {

    given testEnv: CardanoInfo = CardanoInfo.mainnet

    val genesisHash: TransactionHash =
        TransactionHash.fromByteString(ByteString.fromHex("0" * 64))

    val auctionCell = new UtxoCellDef[AuctionState, AuctionAction](
      compiled = AuctionCellCompilation.compiled,
      tokenName = AuctionCell.beaconName,
      transition = AuctionCell.transition
    )

    private val sellerPkh = PubKeyHash(Alice.addrKeyHash)
    private val bidder1Pkh = PubKeyHash(Bob.addrKeyHash)
    private val bidder2Pkh = PubKeyHash(Charles.addrKeyHash)

    // Auction end time: slot 100 in mainnet config
    private val auctionEndTime: PosixTime = BigInt(
      testEnv.slotConfig.slotToTime(100).toLong
    )

    private def createEmulator(): Emulator = Emulator(
      initialUtxos = Map(
        Input(genesisHash, 0) -> Output(Alice.address, LedgerValue.ada(1000)),
        Input(genesisHash, 1) -> Output(Bob.address, LedgerValue.ada(1000)),
        Input(genesisHash, 2) -> Output(Charles.address, LedgerValue.ada(1000))
      )
    )

    private def initialState = AuctionState(
      seller = sellerPkh,
      highestBidder = Option.None,
      highestBid = BigInt(2_000_000),
      auctionEndTime = auctionEndTime
    )

    test("AuctionCell compiles to UPLC") {
        val compiled = AuctionCellCompilation.compiled
        info(s"Auction script size: ${compiled.script.script.size} bytes")
        assert(compiled.script.script.size > 0)
    }

    test("Transaction witness contains the AuctionCell script") {
        val emulator = createEmulator()
        emulator.setSlot(80)

        // Init: mint tx should contain the script
        val initTx = TxBuilder(testEnv)
            .initUtxoCell(auctionCell, initialState, LedgerValue.ada(5))
            .validTo(java.time.Instant.ofEpochMilli(auctionEndTime.toLong - 1000))
            .complete(emulator.utxos, Alice.address)
            .sign(Alice.signer)
            .transaction

        val scriptHash = auctionCell.scriptHash
        assert(
          initTx.witnessSet.plutusV3Scripts.toMap.contains(scriptHash),
          "Init transaction should contain the AuctionCell PlutusV3 script"
        )
        assert(emulator.submitSync(initTx).isRight)

        // Spend: bid tx should contain the script
        val bidTx = TxBuilder(testEnv)
            .spendUtxoCell(auctionCell, AuctionAction.Bid(bidder1Pkh, BigInt(3_000_000)))
            .complete(emulator.utxos, Bob.address)
            .sign(Bob.signer)
            .transaction

        assert(
          bidTx.witnessSet.plutusV3Scripts.toMap.contains(scriptHash),
          "Bid transaction should contain the AuctionCell PlutusV3 script"
        )
        assert(
          bidTx.witnessSet.redeemers.isDefined,
          "Bid transaction should have redeemers"
        )
    }

    test("Auction lifecycle: init -> bid -> outbid -> close with winner") {
        val emulator = createEmulator()
        emulator.setSlot(80) // before auction end

        // 1. Init auction
        val initTx = TxBuilder(testEnv)
            .initUtxoCell(auctionCell, initialState, LedgerValue.ada(5))
            .validTo(java.time.Instant.ofEpochMilli(auctionEndTime.toLong - 1000))
            .complete(emulator.utxos, Alice.address)
            .sign(Alice.signer)
            .transaction

        val initResult = emulator.submitSync(initTx)
        assert(initResult.isRight, s"Init should succeed: $initResult")

        // Verify initial state
        val cell0 = auctionCell.findUtxo(emulator.utxos)
        assert(cell0.isDefined, "Should find auction cell")
        assert(auctionCell.currentState(cell0.get) == initialState)

        // 2. First bid: Bob bids 3 ADA
        //    validTo is set automatically by ctx.txInfo.requireValidBefore(auctionEndTime)
        val bidTx = TxBuilder(testEnv)
            .spendUtxoCell(auctionCell, AuctionAction.Bid(bidder1Pkh, BigInt(3_000_000)))
            .complete(emulator.utxos, Bob.address)
            .sign(Bob.signer)
            .transaction

        val bidResult = emulator.submitSync(bidTx)
        assert(bidResult.isRight, s"First bid should succeed: $bidResult")

        // Verify state after bid
        val cell1 = auctionCell.findUtxo(emulator.utxos)
        assert(cell1.isDefined)
        val state1 = auctionCell.currentState(cell1.get)
        assert(state1.highestBid == BigInt(3_000_000))
        assert(state1.highestBidder == Option.Some(bidder1Pkh))

        // 3. Outbid: Charles bids 5 ADA (Bob gets refunded)
        val outbidTx = TxBuilder(testEnv)
            .spendUtxoCell(auctionCell, AuctionAction.Bid(bidder2Pkh, BigInt(5_000_000)))
            .complete(emulator.utxos, Charles.address)
            .sign(Charles.signer)
            .transaction

        val outbidResult = emulator.submitSync(outbidTx)
        assert(outbidResult.isRight, s"Outbid should succeed: $outbidResult")

        // Verify state after outbid
        val cell2 = auctionCell.findUtxo(emulator.utxos)
        assert(cell2.isDefined)
        val state2 = auctionCell.currentState(cell2.get)
        assert(state2.highestBid == BigInt(5_000_000))
        assert(state2.highestBidder == Option.Some(bidder2Pkh))

        // 4. Close auction (after end time)
        //    validFrom is set automatically by ctx.txInfo.requireValidAfter(auctionEndTime)
        emulator.setSlot(110)
        val closeTx = TxBuilder(testEnv)
            .spendUtxoCell(auctionCell, AuctionAction.Close)
            .complete(emulator.utxos, Alice.address)
            .sign(Alice.signer)
            .transaction

        val closeResult = emulator.submitSync(closeTx)
        assert(closeResult.isRight, s"Close should succeed: $closeResult")

        // Verify cell is gone (terminal)
        val cell3 = auctionCell.findUtxo(emulator.utxos)
        assert(cell3.isEmpty, "Auction cell should be consumed")
    }

    test("Auction close without bids (seller reclaims)") {
        val emulator = createEmulator()
        emulator.setSlot(80)

        // Init
        val initTx = TxBuilder(testEnv)
            .initUtxoCell(auctionCell, initialState, LedgerValue.ada(5))
            .validTo(java.time.Instant.ofEpochMilli(auctionEndTime.toLong - 1000))
            .complete(emulator.utxos, Alice.address)
            .sign(Alice.signer)
            .transaction

        assert(emulator.submitSync(initTx).isRight)

        // Close without bids (seller must sign)
        emulator.setSlot(110)
        val closeTx = TxBuilder(testEnv)
            .spendUtxoCell(auctionCell, AuctionAction.Close)
            .complete(emulator.utxos, Alice.address)
            .sign(Alice.signer)
            .transaction

        val closeResult = emulator.submitSync(closeTx)
        assert(closeResult.isRight, s"Close without bids should succeed: $closeResult")

        val cell = auctionCell.findUtxo(emulator.utxos)
        assert(cell.isEmpty, "Auction cell should be consumed")
    }

    // -- Negative tests --

    /** Helper: creates an emulator and initializes the auction cell. */
    private def initAuction(): Emulator = {
        val emulator = createEmulator()
        emulator.setSlot(80)
        val initTx = TxBuilder(testEnv)
            .initUtxoCell(auctionCell, initialState, LedgerValue.ada(5))
            .validTo(java.time.Instant.ofEpochMilli(auctionEndTime.toLong - 1000))
            .complete(emulator.utxos, Alice.address)
            .sign(Alice.signer)
            .transaction
        assert(emulator.submitSync(initTx).isRight)
        emulator
    }

    test("Bid too low is rejected") {
        val emulator = initAuction()

        // Initial highest bid is 2 ADA, try bidding 1 ADA
        val ex = intercept[TxBuilderException.DeferredResolutionException] {
            TxBuilder(testEnv)
                .spendUtxoCell(auctionCell, AuctionAction.Bid(bidder1Pkh, BigInt(1_000_000)))
                .complete(emulator.utxos, Bob.address)
        }
        assert(ex.getMessage.contains("bid too low"))
        assert(ex.getCause != null, "Should preserve underlying cause")
    }

    test("Seller cannot bid on own auction") {
        val emulator = initAuction()

        val ex = intercept[TxBuilderException.DeferredResolutionException] {
            TxBuilder(testEnv)
                .spendUtxoCell(auctionCell, AuctionAction.Bid(sellerPkh, BigInt(5_000_000)))
                .complete(emulator.utxos, Alice.address)
        }
        assert(ex.getMessage.contains("seller cannot bid"))
        assert(ex.getCause != null, "Should preserve underlying cause")
    }

    test("Bid after auction end is rejected by emulator") {
        val emulator = initAuction()
        emulator.setSlot(110) // past auction end

        // Off-chain transition succeeds (just adds ValidityEndSlot),
        // but emulator rejects because tx validity range is expired
        val bidTx = TxBuilder(testEnv)
            .spendUtxoCell(auctionCell, AuctionAction.Bid(bidder1Pkh, BigInt(3_000_000)))
            .complete(emulator.utxos, Bob.address)
            .sign(Bob.signer)
            .transaction

        val result = emulator.submitSync(bidTx)
        assert(result.isLeft, s"Bid after auction end should fail: $result")
    }

    test("Close before auction end is rejected by emulator") {
        val emulator = initAuction()
        // emulator is at slot 80, before auction end at slot 100

        val closeTx = TxBuilder(testEnv)
            .spendUtxoCell(auctionCell, AuctionAction.Close)
            .complete(emulator.utxos, Alice.address)
            .sign(Alice.signer)
            .transaction

        val result = emulator.submitSync(closeTx)
        assert(result.isLeft, s"Close before auction end should fail: $result")
    }

    test("Close without bids fails without seller signature") {
        val emulator = initAuction()
        emulator.setSlot(110)

        // Bob tries to close, but seller (Alice) must sign
        val closeTx = TxBuilder(testEnv)
            .spendUtxoCell(auctionCell, AuctionAction.Close)
            .complete(emulator.utxos, Bob.address)
            .sign(Bob.signer) // only Bob signs, Alice required
            .transaction

        val result = emulator.submitSync(closeTx)
        assert(result.isLeft, s"Close without seller signature should fail: $result")
    }
}
