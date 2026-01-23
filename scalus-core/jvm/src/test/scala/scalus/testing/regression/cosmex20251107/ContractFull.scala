package scalus.testing.regression.cosmex20251107.full
import scalus.*
import scalus.uplc.builtin.*
import scalus.uplc.builtin.Builtins.*
import scalus.uplc.builtin.Data.toData
import scalus.compiler.sir.TargetLoweringBackend
import scalus.compiler.{compile, Options}
import scalus.cardano.onchain.plutus.v1.IntervalBound
import scalus.cardano.onchain.plutus.v1.IntervalBoundType.Finite
import scalus.cardano.onchain.plutus.v2
import scalus.cardano.onchain.plutus.v2.Value.*
import scalus.cardano.onchain.plutus.v3.*
import scalus.cardano.onchain.plutus.prelude.*
import scalus.uplc.Program

type DiffMilliSeconds = BigInt
type Signature = ByteString
type OrderId = BigInt
type AssetClass = (PolicyId, TokenName)
type PubKey = ByteString

case class Trade(orderId: OrderId, tradeAmount: BigInt, tradePrice: BigInt)

type TxOutIndex = BigInt

enum Action:
    case Update
    case ClientAbort
    case Close(party: Party, signedSnapshot: SignedSnapshot)
    case Trades(actionTrades: List[Trade], actionCancelOthers: Boolean)
    case Payout
    case Transfer(txOutIndex: TxOutIndex, value: Value)
    case Timeout

enum Party:
    case Client
    case Exchange

type Pair = (AssetClass, AssetClass)

case class LimitOrder(orderPair: Pair, orderAmount: BigInt, orderPrice: BigInt)

enum PendingTxType:
    case PendingIn
    case PendingOut(txOutIndex: TxOutIndex)
    case PendingTransfer(txOutIndex: TxOutIndex)

case class PendingTx(
    pendingTxValue: Value,
    pendingTxType: PendingTxType,
    pendingTxSpentTxOutRef: TxOutRef
)

case class TradingState(
    tsClientBalance: Value,
    tsExchangeBalance: Value,
    tsOrders: AssocMap[OrderId, LimitOrder]
)

case class Snapshot(
    snapshotTradingState: TradingState,
    snapshotPendingTx: Option[PendingTx],
    snapshotVersion: BigInt
)

case class SignedSnapshot(
    signedSnapshot: Snapshot,
    snapshotClientSignature: Signature,
    snapshotExchangeSignature: Signature
)

enum OnChainChannelState:
    case OpenState
    case SnapshotContestState(
        contestSnapshot: Snapshot,
        contestSnapshotStart: PosixTime,
        contestInitiator: Party,
        contestChannelTxOutRef: TxOutRef
    )
    case TradesContestState(latestTradingState: TradingState, tradeContestStart: PosixTime)
    case PayoutState(clientBalance: Value, exchangeBalance: Value)

case class OnChainState(
    clientPkh: PubKeyHash,
    clientPubKey: ByteString,
    clientTxOutRef: TxOutRef,
    channelState: OnChainChannelState
)

case class ExchangeParams(
    exchangePkh: PubKeyHash,
    exchangePubKey: ByteString,
    contestationPeriodInMilliseconds: DiffMilliSeconds
) derives FromData,
      ToData

@Compile
object ExchangeParams

@Compile
object CosmexToDataInstances {
    given Data.ToData[Party] = ToData.derived
    given Data.ToData[LimitOrder] = ToData.derived
    given Data.ToData[TradingState] = ToData.derived
    given Data.ToData[PendingTxType] = ToData.derived
    given Data.ToData[PendingTx] = ToData.derived
    given Data.ToData[Snapshot] = ToData.derived
    given Data.ToData[SignedSnapshot] = ToData.derived
    given Data.ToData[OnChainChannelState] = ToData.derived
    given Data.ToData[OnChainState] = ToData.derived
    given Data.ToData[Trade] = ToData.derived
    given Data.ToData[Action] = ToData.derived
}

@Compile
object CosmexFromDataInstances {
    given Data.FromData[Party] = FromData.derived

    given Data.FromData[LimitOrder] = FromData.derived

    given Data.FromData[TradingState] = FromData.derived

    given Data.FromData[PendingTxType] = FromData.derived

    given Data.FromData[PendingTx] = FromData.derived

    given Data.FromData[Snapshot] = FromData.derived

    given Data.FromData[SignedSnapshot] = FromData.derived

    given Data.FromData[OnChainChannelState] = FromData.derived

    given Data.FromData[Trade] = FromData.derived

    given Data.FromData[Action] = FromData.derived

    given Data.FromData[OnChainState] = FromData.derived
}

@Compile
object CosmexContract extends DataParameterizedValidator {

    import CosmexFromDataInstances.given
    import CosmexToDataInstances.given

    def findOwnInputAndIndex(
        inputs: List[TxInInfo],
        spendingTxOutRef: TxOutRef
    ): (TxInInfo, BigInt) = {
        def go(i: BigInt, txIns: List[TxInInfo]): (TxInInfo, BigInt) = txIns match
            case List.Nil => fail("Own input not found")
            case List.Cons(txInInfo, tail) =>
                if txInInfo.outRef === spendingTxOutRef then (txInInfo, i)
                else go(i + 1, tail)

        go(0, inputs)
    }

    def expectNewState(
        ownOutput: TxOut,
        ownInputAddress: Address,
        newState: OnChainState,
        newValue: Value
    ): Boolean = {

        ownOutput match
            case TxOut(address, value, datum, referenceScript) =>
                val newStateData = newState.toData
                val expectedNewDatum = datum === v2.OutputDatum.OutputDatum(newStateData)
                val sameAddress = address === ownInputAddress
                val preserveValue = value === newValue
                expectedNewDatum.? &&
                sameAddress.? && preserveValue.?
    }

    def txSignedBy(signatories: List[PubKeyHash], k: PubKeyHash): Boolean =
        List.exists(signatories)(k.hash === _.hash)

    inline def handleUpdate(
        ownInputAddress: Address,
        ownOutput: TxOut,
        state: OnChainState,
        signatories: List[PubKeyHash],
        exchangePkh: PubKeyHash
    ) = {
        val newValue = ownOutput.value
        // both parties must sign the transaction,
        // thus it's validated by them, so no need to check anything else
        // NOTE: this allows parties to change the channel funds by mutual agreement
        val clientSigned = txSignedBy(signatories, state.clientPkh)
        val exchangeSigned = txSignedBy(signatories, exchangePkh)
        val validNewState = expectNewState(ownOutput, ownInputAddress, state, newValue)
        clientSigned.? && exchangeSigned.? && validNewState.?
    }

    inline def handleClientAbort(
        ownTxInResolvedTxOut: TxOut,
        contestSnapshotStart: PosixTime,
        signatories: List[PubKeyHash],
        state: OnChainState,
        spendingTxOutRef: TxOutRef,
        ownOutput: TxOut
    ) = {
        state match
            case OnChainState(clientPkh, clientPubKey, clientTxOutRef, channelState) =>
                val tradingState =
                    TradingState(
                      tsClientBalance = Value.zero,
                      tsExchangeBalance = Value.zero,
                      tsOrders = AssocMap.empty
                    )
                val snapshot =
                    Snapshot(
                      snapshotTradingState = tradingState,
                      snapshotPendingTx = Option.None,
                      snapshotVersion = 0
                    )
                val contestSnapshotState = OnChainChannelState.SnapshotContestState(
                  contestSnapshot = snapshot,
                  contestSnapshotStart = contestSnapshotStart,
                  contestInitiator = Party.Client,
                  contestChannelTxOutRef = spendingTxOutRef
                )
                val snapshotContestState =
                    OnChainState(clientPkh, clientPubKey, clientTxOutRef, contestSnapshotState)
                ownTxInResolvedTxOut match
                    case TxOut(ownInputAddress, ownInputValue, _, _) =>
                        val clientSigned = txSignedBy(signatories, clientPkh)
                        val correctNewState =
                            expectNewState(
                              ownOutput,
                              ownInputAddress,
                              snapshotContestState,
                              ownInputValue
                            )
                        clientSigned.?
                        && correctNewState.?
    }

    def lockedInOrders(orders: AssocMap[BigInt, LimitOrder]): Value = {
        orders.toList.foldLeft(Value.zero) { (acc, pair) =>
            pair match
                case (_, LimitOrder((base, quote), orderAmount, orderPrice)) =>
                    val orderValue =
                        if orderAmount < 0 then
                            assetClassValue(base, orderAmount) // Sell base asset
                        else assetClassValue(quote, orderAmount * orderPrice) // Buy quote asset
                    acc + orderValue
        }
    }

    def validSignedSnapshot(
        signedSnapshot: SignedSnapshot,
        clientTxOutRef: TxOutRef,
        clientPubKey: PubKey,
        exchangePubKey: PubKey
    ): Boolean = {
        signedSnapshot match
            case SignedSnapshot(
                  signedSnapshot,
                  snapshotClientSignature,
                  snapshotExchangeSignature
                ) =>
                val signedInfo = (clientTxOutRef, signedSnapshot)
                val msg = serialiseData(
                  signedInfo.toData
                )
                val validExchangeSig =
                    verifyEd25519Signature(exchangePubKey, msg, snapshotExchangeSignature)
                val validClientSig =
                    verifyEd25519Signature(clientPubKey, msg, snapshotClientSignature)
                validClientSig && validExchangeSig
    }

    def balancedSnapshot(ts: TradingState, locked: Value): Boolean = {
        ts match
            case TradingState(tsClientBalance, tsExchangeBalance, tsOrders) =>
                val allFunds = tsClientBalance + tsExchangeBalance + lockedInOrders(tsOrders)
                allFunds === locked
    }

    inline def handleClose(
        initiator: Party,
        ownTxInResolvedTxOut: TxOut,
        contestSnapshotStart: PosixTime,
        signatories: List[PubKeyHash],
        params: ExchangeParams,
        state: OnChainState,
        newSignedSnapshot: SignedSnapshot,
        spendingTxOutRef: TxOutRef,
        ownOutput: TxOut
    ) = {
        state match
            case OnChainState(clientPkh, clientPubKey, clientTxOutRef, _) =>
                val validInitiator = initiator match
                    case Party.Client   => txSignedBy(signatories, clientPkh)
                    case Party.Exchange => txSignedBy(signatories, params.exchangePkh)
                newSignedSnapshot match
                    case SignedSnapshot(
                          signedSnapshot,
                          snapshotClientSignature,
                          snapshotExchangeSignature
                        ) =>
                        val newChannelState =
                            OnChainChannelState.SnapshotContestState(
                              contestSnapshot = signedSnapshot,
                              contestSnapshotStart = contestSnapshotStart,
                              contestInitiator = initiator,
                              // save the channel tx out ref so that we can check it in the contest
                              contestChannelTxOutRef = spendingTxOutRef
                            )

                        val newState =
                            OnChainState(clientPkh, clientPubKey, clientTxOutRef, newChannelState)
                        ownTxInResolvedTxOut match
                            case TxOut(ownInputAddress, ownInputValue, _, _) =>
                                validInitiator.?
                                && balancedSnapshot(
                                  signedSnapshot.snapshotTradingState,
                                  ownInputValue
                                )
                                && validSignedSnapshot(
                                  newSignedSnapshot,
                                  clientTxOutRef,
                                  clientPubKey,
                                  params.exchangePubKey
                                )
                                && expectNewState(
                                  ownOutput,
                                  ownInputAddress,
                                  newState,
                                  ownInputValue
                                )
    }

    inline def handleContestClose(
        params: ExchangeParams,
        tradeContestStart: PosixTime,
        signatories: List[PubKeyHash],
        state: OnChainState,
        contestSnapshot: Snapshot,
        contestSnapshotStart: PosixTime,
        contestInitiator: Party,
        contestChannelTxOutRef: TxOutRef,
        party: Party,
        newSignedSnapshot: SignedSnapshot,
        ownTxInResolvedTxOut: TxOut,
        ownOutput: TxOut
    ): Boolean = contestSnapshot match {
        case Snapshot(snapshotTradingState, snapshotPendingTx, oldVersion) =>
            state match
                case OnChainState(clientPkh, clientPubKey, clientTxOutRef, channelState) =>
                    val validParty = contestInitiator match
                        case Party.Client =>
                            party match
                                case Party.Client   => fail("Invalid party")
                                case Party.Exchange => txSignedBy(signatories, params.exchangePkh)
                        case Party.Exchange =>
                            party match
                                case Party.Client   => txSignedBy(signatories, clientPkh)
                                case Party.Exchange => fail("Invalid party")

                    val latestTradingState =
                        handlePendingTx(
                          contestChannelTxOutRef,
                          snapshotPendingTx,
                          snapshotTradingState
                        )
                    latestTradingState match
                        case TradingState(tsClientBalance, tsExchangeBalance, tsOrders) =>
                            val newChannelState =
                                if List.isEmpty(tsOrders.toList) then
                                    OnChainChannelState.PayoutState(
                                      tsClientBalance,
                                      tsExchangeBalance
                                    )
                                else
                                    OnChainChannelState.TradesContestState(
                                      latestTradingState = latestTradingState,
                                      tradeContestStart = tradeContestStart
                                    )

                            val newState = OnChainState(
                              clientPkh,
                              clientPubKey,
                              clientTxOutRef,
                              newChannelState
                            )
                            val isNewerSnapshot =
                                oldVersion <= newSignedSnapshot.signedSnapshot.snapshotVersion
                            ownTxInResolvedTxOut match
                                case TxOut(ownInputAddress, ownInputValue, _, _) =>
                                    validParty.? && isNewerSnapshot.? && balancedSnapshot(
                                      latestTradingState,
                                      ownInputValue
                                    ) && validSignedSnapshot(
                                      newSignedSnapshot,
                                      clientTxOutRef,
                                      clientPubKey,
                                      params.exchangePubKey
                                    ) && expectNewState(
                                      ownOutput,
                                      ownInputAddress,
                                      newState,
                                      ownInputValue
                                    )
    }

    inline def handleContestTimeout(
        contestSnapshotStart: PosixTime,
        contestationPeriodInMilliseconds: PosixTime,
        txInfoValidRange: (PosixTime, PosixTime),
        contestChannelTxOutRef: TxOutRef,
        contestSnapshot: Snapshot,
        state: OnChainState,
        ownTxInResolvedTxOut: TxOut,
        ownOutput: TxOut
    ): Boolean = {
        val (start, tradeContestStart) = txInfoValidRange
        val timeoutPassed = {
            val timeoutTime = contestSnapshotStart + contestationPeriodInMilliseconds
            timeoutTime < start
        }

        val latestTradingState =
            handlePendingTx(
              contestChannelTxOutRef,
              contestSnapshot.snapshotPendingTx,
              contestSnapshot.snapshotTradingState
            )

        latestTradingState match
            case TradingState(tsClientBalance, tsExchangeBalance, tsOrders) =>
                val newChannelState =
                    if List.isEmpty(tsOrders.toList) then
                        OnChainChannelState.PayoutState(tsClientBalance, tsExchangeBalance)
                    else
                        OnChainChannelState.TradesContestState(
                          latestTradingState,
                          tradeContestStart
                        )

                state match
                    case OnChainState(clientPkh, clientPubKey, clientTxOutRef, channelState) =>
                        val newState =
                            OnChainState(clientPkh, clientPubKey, clientTxOutRef, newChannelState)
                        ownTxInResolvedTxOut match
                            case TxOut(ownInputAddress, ownInputValue, _, _) =>
                                timeoutPassed && expectNewState(
                                  ownOutput,
                                  ownInputAddress,
                                  newState,
                                  ownInputValue
                                )
    }

    inline def handleTradesContestTimeout(
        params: ExchangeParams,
        start: PosixTime,
        tradeContestStart: PosixTime,
        state: OnChainState,
        latestTradingState: TradingState,
        ownTxInResolvedTxOut: TxOut,
        ownOutput: TxOut
    ) = {

        val timeoutPassed =
            val timeoutTime = tradeContestStart + params.contestationPeriodInMilliseconds
            timeoutTime < start
        val newChannelState = latestTradingState match
            case TradingState(tsClientBalance, tsExchangeBalance, _) =>
                OnChainChannelState.PayoutState(
                  clientBalance = tsClientBalance,
                  exchangeBalance = tsExchangeBalance
                )
        state match
            case OnChainState(clientPkh, clientPubKey, clientTxOutRef, channelState) =>
                val newState =
                    OnChainState(clientPkh, clientPubKey, clientTxOutRef, newChannelState)
                ownTxInResolvedTxOut match
                    case TxOut(ownInputAddress, ownInputValue, _, _) =>
                        timeoutPassed && expectNewState(
                          ownOutput,
                          ownInputAddress,
                          newState,
                          ownInputValue
                        )
    }

    inline def handleContestTrades(
        params: ExchangeParams,
        signatories: List[PubKeyHash],
        actionTrades: List[Trade],
        tradeContestStart: PosixTime,
        actionCancelOthers: Boolean,
        latestTradingState: TradingState,
        state: OnChainState,
        ownTxInResolvedTxOut: TxOut,
        ownOutput: TxOut
    ) = {
        val newTradeingState = actionTrades.foldLeft(latestTradingState)(applyTrade)
        val newChannelState = newTradeingState match
            case TradingState(tsClientBalance, tsExchangeBalance, tsOrders) =>
                if actionCancelOthers || List.isEmpty(tsOrders.toList) then
                    OnChainChannelState.PayoutState(tsClientBalance, tsExchangeBalance)
                else OnChainChannelState.TradesContestState(newTradeingState, tradeContestStart)

        state match
            case OnChainState(clientPkh, clientPubKey, clientTxOutRef, channelState) =>
                val newState =
                    OnChainState(clientPkh, clientPubKey, clientTxOutRef, newChannelState)
                ownTxInResolvedTxOut match
                    case TxOut(ownInputAddress, ownInputValue, _, _) =>
                        txSignedBy(signatories, params.exchangePkh) && expectNewState(
                          ownOutput,
                          ownInputAddress,
                          newState,
                          ownInputValue
                        )
    }

    /*  The clientBalance and exchangeBalance can be fulfilled partially, meaning that
      the exchange may owe the client some money.
      This state allows a client to withdraw the money that the exchange owes him
      from other TxOuts where exchangeBalance is available.

      client1: {A: 1000, $: 0} exchange: {A: 0, $: 0} locked: {A: 1000, $: 0}
      client2: {A: 0, $: 1000} exchange: {A: 0, $: 0} locked: {A: 0, $: 1000}
      ==> Trade ADA/DJED SELL A 400 @ 0.3
      client1: {A: 600, $: 120} exchange: {A: 400, $: 0} locked: {A: 1000, $: 0}
      client2: {A: 400, $: 880} exchange: {A: 0, $: 120} locked: {A: 0, $: 1000}
      ==> Payout if the exchange is offline
      client1 can withdraw $120 from client2's locked funds
      client2 can withdraw A400 from client1's locked funds
      even in a single transaction
      client1--|-----|--client1
      client2--| Tx  |--client2
              |-----|
     */
    inline def handlePayoutTransfer(
        params: ExchangeParams,
        state: OnChainState,
        redeemers: SortedMap[ScriptPurpose, Redeemer],
        inputs: List[TxInInfo],
        ownIdx: BigInt,
        transferValue: Value,
        clientBalance: Value,
        exchangeBalance: Value,
        ownTxInResolvedTxOut: TxOut,
        ownOutput: TxOut
    ): Boolean = {
        import Action.*
        import OnChainChannelState.*

        val (locked, cosmexScriptHash) = ownOutput match
            case TxOut(Address(cred, _), txOutValue, _, _) =>
                cred match
                    case Credential.ScriptCredential(sh) => (txOutValue, sh)
                    case Credential.PubKeyCredential(_)  => fail("Invalid output")

        val transferValueIsPositive = transferValue.isPositive

        def cosmexInputTransferAmountToTxOutIdx(txInInfo: TxInInfo): Value = txInInfo match
            case TxInInfo(txOutRef, TxOut(Address(cred, _), _, _, _)) =>
                cred match
                    case Credential.ScriptCredential(sh) =>
                        if sh === cosmexScriptHash then
                            val action =
                                redeemers
                                    .get(ScriptPurpose.Spending(txOutRef))
                                    .getOrFail("No redeemer")
                                    .to[Action]

                            action match
                                case Transfer(targetIdx, amount) =>
                                    if targetIdx === ownIdx then amount
                                    else Value.zero
                                case _ => fail("Invalid action")
                        else Value.zero
                    case Credential.PubKeyCredential(_) => Value.zero

        val transferedToMe = inputs.foldLeft(Value.zero) { (acc, input) =>
            acc + cosmexInputTransferAmountToTxOutIdx(input)
        }
        val diff = transferedToMe - transferValue
        val newOutputValue = locked - diff
        val newExchangeBalance = exchangeBalance - diff
        state match
            case OnChainState(clientPkh, clientPubKey, clientTxOutRef, channelState) =>
                val newChannelState = PayoutState(clientBalance, newExchangeBalance)
                val newState =
                    OnChainState(clientPkh, clientPubKey, clientTxOutRef, newChannelState)
                transferValueIsPositive && expectNewState(
                  ownOutput,
                  ownTxInResolvedTxOut.address,
                  newState,
                  newOutputValue
                )
    }

    inline def handlePayoutPayout(
        params: ExchangeParams,
        state: OnChainState,
        clientBalance: Value,
        exchangeBalance: Value,
        ownTxInResolvedTxOut: TxOut,
        ownOutput: TxOut
    ): Boolean = {
        import OnChainChannelState.*
        ownTxInResolvedTxOut match
            case TxOut(ownInputAddress, ownInputValue, _, _) =>
                val isFilled = clientBalance === ownInputValue && exchangeBalance === Value.zero
                if isFilled then
                    ownOutput match
                        case TxOut(address, txOutValue, _, _) =>
                            address.credential match
                                case Credential.PubKeyCredential(hash) =>
                                    if hash === params.exchangePkh && txOutValue === ownInputValue
                                    then true
                                    else fail("Invalid payout")
                                case Credential.ScriptCredential(hash) =>
                                    fail("Invalid payout")
                else
                    val min = (a: BigInt, b: BigInt) => if a < b then a else b
                    val availableForPayment =
                        Value.zero // FIXME: Value.unionWith(min)(clientBalance, ownInputValue)
                    val newOutputValue = ownInputValue - availableForPayment
                    val newClientBalance = clientBalance - availableForPayment
                    state match
                        case OnChainState(clientPkh, clientPubKey, clientTxOutRef, channelState) =>
                            val newState = OnChainState(
                              clientPkh,
                              clientPubKey,
                              clientTxOutRef,
                              PayoutState(newClientBalance, exchangeBalance)
                            )
                            expectNewState(ownOutput, ownInputAddress, newState, newOutputValue)
    }

    inline def handleOpenState(
        action: Action,
        ownOutput: TxOut,
        ownTxInResolvedTxOut: TxOut,
        params: ExchangeParams,
        spendingTxOutRef: TxOutRef,
        state: OnChainState,
        signatories: List[PubKeyHash],
        range: PosixTimeRange
    ): Boolean =
        import Action.*
        action match
            case Update =>
                handleUpdate(
                  ownTxInResolvedTxOut.address,
                  ownOutput,
                  state,
                  signatories,
                  params.exchangePkh
                )
            case ClientAbort =>
                /*  This should only be called by the client on channel open
        in case the exchange doesn't respond to the initial snapshot
        hence, the currentSnapshot must be version 0, with only the client's balance
        Note: this allows the client to claim all locked funds in the channel,
        hence the exchange MUST contest with a valid snapshot if needed.
        Consider penalizing the client for this. */
                val contestSnapshotStart = validRange(range)._2
                handleClientAbort(
                  ownTxInResolvedTxOut,
                  contestSnapshotStart,
                  signatories,
                  state,
                  spendingTxOutRef,
                  ownOutput
                )
            case Close(party, signedSnapshot) =>
                val contestSnapshotStart = validRange(range)._2
                handleClose(
                  party,
                  ownTxInResolvedTxOut,
                  contestSnapshotStart,
                  signatories,
                  params,
                  state,
                  signedSnapshot,
                  spendingTxOutRef,
                  ownOutput
                )
            case _ => fail("Invalid action")

    inline def handleSnapshotContestState(
        action: Action,
        contestChannelTxOutRef: TxOutRef,
        contestInitiator: Party,
        contestSnapshot: Snapshot,
        contestSnapshotStart: PosixTime,
        ownOutput: TxOut,
        ownTxInResolvedTxOut: TxOut,
        params: ExchangeParams,
        state: OnChainState,
        signatories: List[PubKeyHash],
        range: PosixTimeRange
    ): Boolean =
        import Action.*
        action match
            case Close(party, newSignedSnapshot) =>
                val (_, tradeContestStart) = validRange(range)
                handleContestClose(
                  params,
                  tradeContestStart,
                  signatories,
                  state,
                  contestSnapshot,
                  contestSnapshotStart,
                  contestInitiator,
                  contestChannelTxOutRef,
                  party,
                  newSignedSnapshot,
                  ownTxInResolvedTxOut,
                  ownOutput
                )
            case Timeout =>
                handleContestTimeout(
                  contestSnapshotStart,
                  params.contestationPeriodInMilliseconds,
                  validRange(range),
                  contestChannelTxOutRef,
                  contestSnapshot,
                  state,
                  ownTxInResolvedTxOut,
                  ownOutput
                )
            case _ => fail("Invalid action")

    inline def handleTradesContestState(
        action: Action,
        latestTradingState: TradingState,
        ownOutput: TxOut,
        ownTxInResolvedTxOut: TxOut,
        params: ExchangeParams,
        state: OnChainState,
        tradeContestStart: PosixTime,
        signatories: List[PubKeyHash],
        range: PosixTimeRange
    ): Boolean =
        import Action.*
        action match
            case Timeout =>
                val (start, _) = validRange(range)
                handleTradesContestTimeout(
                  params,
                  start,
                  tradeContestStart,
                  state,
                  latestTradingState,
                  ownTxInResolvedTxOut,
                  ownOutput
                )
            case Trades(actionTrades, actionCancelOthers) =>
                handleContestTrades(
                  params,
                  signatories,
                  actionTrades,
                  tradeContestStart,
                  actionCancelOthers,
                  latestTradingState,
                  state,
                  ownTxInResolvedTxOut,
                  ownOutput
                )
            case _ => fail("Invalid action")

    inline def handlePayoutState(
        action: Action,
        clientBalance: Value,
        exchangeBalance: Value,
        ownOutput: TxOut,
        ownTxInResolvedTxOut: TxOut,
        params: ExchangeParams,
        redeemers: SortedMap[ScriptPurpose, Redeemer],
        inputs: List[TxInInfo],
        state: OnChainState
    ): Boolean =
        import Action.*
        action match
            case Transfer(txOutIndex, value) =>
                handlePayoutTransfer(
                  params,
                  state,
                  redeemers,
                  inputs,
                  txOutIndex,
                  value,
                  clientBalance,
                  exchangeBalance,
                  ownTxInResolvedTxOut,
                  ownOutput
                )
            case Payout =>
                handlePayoutPayout(
                  params,
                  state,
                  clientBalance,
                  exchangeBalance,
                  ownTxInResolvedTxOut,
                  ownOutput
                )
            case _ => fail("Invalid action")

    inline def cosmexSpending(
        params: ExchangeParams,
        state: OnChainState,
        action: Action,
        tx: TxInfo,
        spendingTxOutRef: TxOutRef
    ): Boolean = {
        import OnChainChannelState.*

        def findOwnInputAndIndex(i: BigInt, txIns: List[TxInInfo]): (TxOut, BigInt) = txIns match
            case List.Nil => fail("Own input not found")
            case List.Cons(TxInInfo(txOutRef, resolved), tail) =>
                if txOutRef === spendingTxOutRef then (resolved, i)
                else findOwnInputAndIndex(i + 1, tail)

//        txInfo match
//            case CosmexTxInfo(inputs, outputs, validRange, signatories, redeemers) =>
        findOwnInputAndIndex(0, tx.inputs) match
            case (ownTxInResolvedTxOut, ownIndex) =>
                val ownOutput = tx.outputs !! ownIndex
                state.channelState match
                    case OpenState =>
                        handleOpenState(
                          action,
                          ownOutput,
                          ownTxInResolvedTxOut,
                          params,
                          spendingTxOutRef,
                          state,
                          tx.signatories,
                          tx.validRange
                        )

                    case SnapshotContestState(
                          contestSnapshot,
                          contestSnapshotStart,
                          contestInitiator,
                          contestChannelTxOutRef
                        ) =>
                        handleSnapshotContestState(
                          action,
                          contestChannelTxOutRef,
                          contestInitiator,
                          contestSnapshot,
                          contestSnapshotStart,
                          ownOutput,
                          ownTxInResolvedTxOut,
                          params,
                          state,
                          tx.signatories,
                          tx.validRange
                        )
                    case TradesContestState(latestTradingState, tradeContestStart) =>
                        handleTradesContestState(
                          action,
                          latestTradingState,
                          ownOutput,
                          ownTxInResolvedTxOut,
                          params,
                          state,
                          tradeContestStart,
                          tx.signatories,
                          tx.validRange
                        )

                    case PayoutState(clientBalance, exchangeBalance) =>
                        handlePayoutState(
                          action,
                          clientBalance,
                          exchangeBalance,
                          ownOutput,
                          ownTxInResolvedTxOut,
                          params,
                          tx.redeemers,
                          tx.inputs,
                          state
                        )
    }

    def assetClassValue(assetClass: AssetClass, i: BigInt): Value =
        Value.apply(assetClass._1, assetClass._2, i)

    def handlePendingTx(
        contestChannelTxOutRef: TxOutRef,
        snapshotPendingTx: Option[PendingTx],
        snapshotTradingState: TradingState
    ): TradingState = {
        snapshotPendingTx match
            case Option.None => snapshotTradingState
            case Option.Some(PendingTx(pendingTxValue, pendingTxType, pendingTxSpentTxOutRef)) =>
                snapshotTradingState match
                    case TradingState(tsClientBalance, tsExchangeBalance, tsOrders) =>
                        if pendingTxSpentTxOutRef === contestChannelTxOutRef then
                            pendingTxType match
                                case PendingTxType.PendingIn =>
                                    TradingState(
                                      tsClientBalance + pendingTxValue,
                                      tsExchangeBalance,
                                      tsOrders
                                    )
                                case PendingTxType.PendingOut(_) =>
                                    TradingState(
                                      tsClientBalance - pendingTxValue,
                                      tsExchangeBalance,
                                      tsOrders
                                    )
                                case PendingTxType.PendingTransfer(_) =>
                                    TradingState(
                                      tsClientBalance,
                                      tsExchangeBalance - pendingTxValue,
                                      tsOrders
                                    )
                        else snapshotTradingState
    }

    /*
    Apply a trade to the trading state.
    We represent BUY/SELL orders as positive/negative amounts, respectively.
    This allows us to use the same code for both BUY and SELL orders.

    ADA/USD, Ada is the base currency, USD is the quote currency.

    Order/Trade Amount > 0 => BUY
    Order/Trade Amount < 0 => SELL

    Invariants:
      orderAmount > 0, tradeAmount > 0, orderPrice != 0, tradePrice != 0
      clientBalance >= 0
      tradePrice <= orderPrice (BUY @ 110 of 125 order, SELL @ -130 of -125 order)
      |tradeAmount| <= |orderAmount|
      newOrderAmount = orderAmount - tradeAmount
      newOrderAmount >= 0
      ∀ orders => ∑ (tradeAmount) <= ∑ (orderAmount) <= baseCurrencyAmount clientBalance
      ∀ orders => ∑ |orderAmount * orderPrice| <= quoteCurrency clientBalance
    Conclusions:
     */
    def applyTrade(tradingState: TradingState, trade: Trade): TradingState = {
        trade match
            case Trade(orderId, tradeAmount, tradePrice) =>
                tradingState match
                    case TradingState(tsClientBalance, tsExchangeBalance, tsOrders) =>
                        tsOrders.get(orderId) match {
                            case Option.Some(
                                  LimitOrder(
                                    pair @ (baseAsset, quoteAsset),
                                    orderAmount,
                                    orderPrice
                                  )
                                ) =>
                                if validTrade(orderAmount, orderPrice, tradeAmount, tradePrice) then
                                    val quoteAmount = tradeAmount * tradePrice
                                    val baseAssetValue = assetClassValue(baseAsset, tradeAmount)
                                    val quoteAssetValue = assetClassValue(quoteAsset, quoteAmount)
                                    val clientBalance1 =
                                        tsClientBalance + baseAssetValue - quoteAssetValue
                                    val exchangeBalance1 =
                                        tsExchangeBalance - baseAssetValue + quoteAssetValue
                                    val orderAmountLeft = orderAmount - tradeAmount
                                    val newOrders =
                                        if orderAmountLeft === BigInt(0) then
                                            AssocMap.delete(tsOrders)(orderId)
                                        else
                                            AssocMap.insert(tsOrders)(
                                              orderId,
                                              LimitOrder(
                                                pair,
                                                orderAmount = orderAmountLeft,
                                                orderPrice = orderPrice
                                              )
                                            )
                                    TradingState(clientBalance1, exchangeBalance1, newOrders)
                                else fail("Invalid trade")
                            case Option.None => fail("Invalid order")
                        }
    }

    def abs(x: BigInt): BigInt = if x < 0 then -x else x

    def validTrade(
        orderAmount: BigInt,
        orderPrice: BigInt,
        tradeAmount: BigInt,
        tradePrice: BigInt
    ): Boolean = {
        (0 < orderPrice) && (0 < tradePrice) && (orderAmount != BigInt(
          0
        )) && (tradeAmount != BigInt(0)) &&
        (abs(tradeAmount) <= abs(orderAmount)) &&
        (if 0 < orderAmount then (0 < tradeAmount) && (tradePrice <= orderPrice)
         else (tradeAmount < 0) && (orderPrice <= tradePrice))
    }

    def validRange(interval: Interval): (PosixTime, PosixTime) = {
        interval match
            case Interval(IntervalBound(lower, _), IntervalBound(upper, _)) =>
                lower match
                    case Finite(start) =>
                        upper match
                            case Finite(end) => (start, end)
                            case _           => fail("UBI")
                    case _ => fail("LBI")
    }

    inline override def spend(
        param: Datum,
        datum: Option[Datum],
        redeemer: Datum,
        tx: TxInfo,
        ownRef: TxOutRef
    ): Unit = {
        val result = cosmexSpending(
          param.to[ExchangeParams],
          datum.get.to[OnChainState],
          redeemer.to[Action],
          tx,
          ownRef
        )
        require(result, "Validation failed")
    }
}

object CosmexValidator {
    private given Options = Options(
      targetLoweringBackend = TargetLoweringBackend.SirToUplcV3Lowering
    )
    private val compiledValidator = compile(CosmexContract.validate)

    def mkCosmexValidator(params: ExchangeParams): Program = {
        val program = compiledValidator.toUplcOptimized().plutusV3
        val uplcProgram = program $ params.toData
        uplcProgram
    }
}
