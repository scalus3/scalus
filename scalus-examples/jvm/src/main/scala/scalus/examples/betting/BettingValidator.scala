package scalus.examples.betting

import scalus.compiler.Compile

import scalus.uplc.builtin.ByteString.*
import scalus.uplc.builtin.Data
import scalus.uplc.builtin.Data.FromData
import scalus.uplc.builtin.Data.ToData
import scalus.uplc.builtin.ToData.*
import scalus.cardano.onchain.plutus.v1.Address
import scalus.cardano.onchain.plutus.v3.*
import scalus.cardano.onchain.plutus.prelude.*
import scalus.cardano.onchain.plutus.v3.Validator

// Datum
/** Represents the state of a two-player betting game The bet starts with player1 creating it, then
  * player2 can join The oracle decides the winner and triggers the payout.
  *
  * @param player1
  *   The public key hash of the first player (bet creator)
  * @param player2
  *   The public key hash of the second player (None if no one has joined yet)
  * @param oracle
  *   The public key hash of the trusted oracle who will announce the winner
  * @param expiration
  *   The expiration time of the bet (in seconds since the epoch)
  */
case class Config(
    player1: PubKeyHash,
    player2: PubKeyHash,
    oracle: PubKeyHash,
    expiration: PosixTime
) derives FromData,
      ToData

// Redeemer
/** Actions that can be performed on the betting contract */
enum Action derives FromData, ToData:
    /** Action for player2 to join an existing bet */
    case Join

    /** Action for the oracle to announce the winner and trigger payout
      * @param winner
      *   The winner's public key hash (must be player1 or player2)
      * @param payoutOutputIdx
      *   Index of the payout output in tx.outputs (V005 fix: prevents double satisfaction)
      */
    case AnnounceWinner(winner: PubKeyHash, payoutOutputIdx: BigInt)

    /** Action for a player to reclaim the bet after expiration when the oracle never announced a
      * winner. Without this the funds would lock forever if the oracle goes silent (or nobody
      * joined).
      */
    case Timeout

/** Main betting validator
  * @see
  *   [[https://github.com/cardano-foundation/cardano-template-and-ecosystem-monitoring/blob/main/bet/onchain/aiken/validators/bet.ak Bet]]
  * @note
  *   [[https://github.com/cardano-foundation/cardano-template-and-ecosystem-monitoring/issues/15 known issue]]
  */
@Compile
object BettingValidator extends Validator {

    /** Spending validator: Controls how the bet UTXO can be spent Handles both
      * [[scalus.examples.betting.Action.Join]] and
      * [[scalus.examples.betting.Action.AnnounceWinner]] actions
      */
    inline override def spend(
        @annotation.unused datum: Option[Data],
        redeemer: Data,
        txInfo: TxInfo,
        txOutRef: TxOutRef
    ): Unit =
        val ownInput = txInfo.findInputOrFail(txOutRef, "Bet spent input must be present")
        val TxOut(address, value, currentDatum, _) = ownInput.resolved
        val scriptHash = address.credential.scriptHashOrFail("Bet input must sit at the script")
        val Config(player1, player2, oracle, expiration) =
            currentDatum.inlineOrFail[Config]("Initial bet datum must be inline")

        redeemer.to[Action] match
            case Action.Join =>
                // V016 fix: the continuing output is matched on the full address, staking
                // credential included.
                val continuingOutput = txInfo.findContinuingOutputOrFail(
                  ownInput,
                  "There must be a single continuing spent output with inline new betting config that goes to the script"
                )
                val outputValue = continuingOutput.value
                val Config(newPlayer1, joiningPlayer, newOracle, newExpiration) =
                    continuingOutput.datum.inlineOrFail[Config](
                      "The continuing bet output must carry the new betting config as an inline datum"
                    )
                require(
                  player2.hash.isEmpty,
                  "Current bet must not have a player2 yet"
                )
                require(
                  value.tokens(scriptHash).nonEmpty,
                  "Input must contain the bet token"
                )
                // V002 fix: Verify bet token is preserved in output
                require(
                  outputValue.tokens(scriptHash).nonEmpty,
                  "Output must contain the bet token"
                )
                require(
                  txInfo.isSignedBy(joiningPlayer),
                  "Player2 must sign the transaction"
                )
                require(
                  newOracle === oracle,
                  "Oracle must remain unchanged"
                )
                require(
                  newPlayer1 === player1,
                  "Player1 must remain unchanged"
                )
                require(
                  joiningPlayer !== player1,
                  "Player2 cannot be the same as player1"
                )
                require(
                  joiningPlayer !== oracle,
                  "Player2 cannot be the same as oracle"
                )
                require(
                  outputValue.getLovelace === BigInt(2) * value.getLovelace,
                  "The bet amount must double (player2 matches player1's bet)"
                )
                require(
                  newExpiration === expiration,
                  "The updated betting config must have the same expiration as the current one"
                )
                require(
                  txInfo.validRange.isEntirelyBefore(newExpiration),
                  "Joining must happen before the bet expiration"
                )

            case Action.AnnounceWinner(winner, payoutOutputIdx) =>
                // V005 fix: Use indexed lookup to prevent double satisfaction
                require(
                  payoutOutputIdx >= 0,
                  "Payout output index must be non-negative"
                )
                val payoutOutput = txInfo.outputs.at(payoutOutputIdx)
                val TxOut(payoutAddress, payoutValue, _, _) = payoutOutput
                require(
                  winner === player1 || winner === player2,
                  "Winner must be either player1 or player2"
                )
                require(
                  player2.hash.nonEmpty,
                  "Both players must have joined (player2 is not None)"
                )
                require(
                  payoutAddress === Address.fromPubKeyHash(winner),
                  "Payout goes to the winner's address"
                )
                // V005 fix: Verify payout contains at least this bet's value
                require(
                  payoutValue.getLovelace >= value.getLovelace,
                  "Payout must contain at least the bet amount"
                )
                require(
                  txInfo.isSignedBy(oracle),
                  "Oracle must sign the transaction"
                )
                require(
                  txInfo.validRange.isEntirelyAfter(expiration),
                  "The bet must have been expired (no future bets allowed) before announcing"
                )
                // Burn the bet NFT so the bet is one-shot and cannot be re-locked into a forged bet.
                require(
                  txInfo.mint.hasOnly(scriptHash, betTokenName(value, scriptHash), -1),
                  "The bet token must be burned when announcing the winner"
                )

            case Action.Timeout =>
                // Reclaim is only possible once the bet has expired without a winner announced.
                require(
                  txInfo.validRange.isEntirelyAfter(expiration),
                  "Cannot reclaim before the bet has expired"
                )
                // A player must initiate the reclaim.
                require(
                  txInfo.isSignedBy(player1) || txInfo.isSignedBy(player2),
                  "Reclaim must be signed by one of the players"
                )
                // Exactly one bet input — the per-player refund check below sums outputs by address,
                // so batching two bets in one tx could let one refund satisfy both. One input per
                // reclaim keeps the accounting sound.
                txInfo.inputs.findUniqueOrFail(
                  _.resolved.address.credential === address.credential,
                  "Reclaim must spend exactly one bet input"
                )
                // Burn the bet NFT so a reclaimed bet's token can't be re-locked into a forged bet.
                require(
                  txInfo.mint.hasOnly(scriptHash, betTokenName(value, scriptHash), -1),
                  "The bet token must be burned on timeout"
                )
                if player2.hash.isEmpty then
                    // No opponent joined — refund the whole pot to player1.
                    require(
                      totalPaidTo(txInfo, player1) >= value.getLovelace,
                      "Player1 must be refunded the full bet on timeout"
                    )
                else
                    // Both players staked — return each their half of the doubled pot.
                    val stake = value.getLovelace / BigInt(2)
                    require(
                      totalPaidTo(txInfo, player1) >= stake,
                      "Player1 must be refunded their stake on timeout"
                    )
                    require(
                      totalPaidTo(txInfo, player2) >= stake,
                      "Player2 must be refunded their stake on timeout"
                    )

    /** The bet NFT's token name — the single asset under the bet's own policy in its UTxO value. */
    private inline def betTokenName(value: Value, scriptHash: PolicyId): TokenName =
        value.tokens(scriptHash).singleOrFail("Bet UTxO must hold exactly one bet token")._1

    /** Sum the lovelace paid to a public key's (enterprise) address across all outputs. */
    private inline def totalPaidTo(txInfo: TxInfo, pkh: PubKeyHash): BigInt =
        txInfo.valuePaidTo(Address.fromPubKeyHash(pkh)).getLovelace

    /** Minting policy:
      *
      * Controls the creation of bet tokens This ensures proper initialization of a new bet
      */
    inline override def mint(
        @annotation.unused redeemer: Data,
        policyId: PolicyId,
        tx: TxInfo
    ): Unit =
        // Exactly one token type under this policy, either minted (+1, a new bet) or burned (-1, a
        // bet ending). (V003/V011 fix.)
        val (_, quantity) = tx.mint
            .tokens(policyId)
            .singleOrFail("Must mint or burn exactly one token type under this policy")

        if quantity === BigInt(-1) then
            // Burning the bet NFT at the end of a bet. The token can only ever sit in a bet UTxO at
            // the script address, so consuming it to burn necessarily runs the spending validator
            // (AnnounceWinner / Timeout), which authorizes the end. Allowing the burn here is what
            // makes the token a true one-shot: a finished bet's NFT is destroyed, so it can never be
            // re-locked at the script with a forged config to bypass the initialization checks.
            ()
        else
            require(quantity === BigInt(1), "Must mint exactly one token")
            val scriptAddress = Address.fromScriptHash(policyId)
            val Config(player1, player2, oracle, expiration) = tx.outputs
                .findUniqueOrFail(
                  _.address === scriptAddress,
                  "There must be a single output with inline initial betting config that goes to the script"
                )
                .datum
                .inlineOrFail[Config](
                  "The initial bet output must carry the betting config as an inline datum"
                )
            require(
              tx.isSignedBy(player1),
              "Player1 must sign the transaction (they're creating the bet)"
            )
            require(
              player2.hash.isEmpty,
              "Player2 must be empty (no one has joined yet)"
            )
            require(
              oracle !== player1,
              "Oracle cannot be the same as player1 (conflict of interest)"
            )
            require(
              tx.validRange.isEntirelyBefore(expiration),
              "The bet must have a valid expiration time (after the current time)"
            )
}
