package scalus.examples.betting

import scalus.builtin.ByteString.*
import scalus.builtin.Data
import scalus.builtin.Data.FromData
import scalus.builtin.Data.ToData
import scalus.builtin.ToData.*
import scalus.ledger.api.v1.Address
import scalus.ledger.api.v1.Credential.ScriptCredential
import scalus.ledger.api.v2.OutputDatum.OutputDatum
import scalus.ledger.api.v3.*
import scalus.prelude.*
import scalus.{show as _, *}

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

@Compile
object Config

// Redeemer
/** Actions that can be performed on the betting contract */
enum Action derives FromData, ToData:
    /** Action for player2 to join an existing bet */
    case Join

    /** Action for the oracle to announce the winner and trigger payout
      * @param winner The winner's public key hash (must be player1 or player2)
      * @param payoutOutputIdx Index of the payout output in tx.outputs (V005 fix: prevents double satisfaction)
      */
    case AnnounceWinner(winner: PubKeyHash, payoutOutputIdx: BigInt)

@Compile
object Action

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
        val (scriptHash, address, value, Config(player1, player2, oracle, expiration)) =
            txInfo.findOwnInputOrFail(txOutRef, "Bet spent input must be present") match
                case TxInInfo(
                      _,
                      TxOut(
                        address @ Address(Credential.ScriptCredential(scriptHash), _),
                        value,
                        OutputDatum(currentDatum),
                        _
                      )
                    ) =>
                    (scriptHash, address, value, currentDatum.to[Config])
                case _ => fail("Initial bet datum must be inline")

        // ???: player2 can spend extra token to create a malformed bet, e.g. oracle === player1
        // TODO: minted token should be single
        redeemer.to[Action] match
            case Action.Join =>
                val (
                  outputAddress,
                  outputValue,
                  Config(newPlayer1, joiningPlayer, newOracle, newExpiration)
                ) = txInfo
                    .findOwnScriptOutputs(scriptHash)
                    .match
                        case List.Cons(TxOut(outAddr, outValue, OutputDatum(newDatum), _), List.Nil) =>
                            (outAddr, outValue, newDatum.to[Config])
                        case _ =>
                            fail(
                              "There must be a single continuing spent output with inline new betting config that goes to the script"
                            )
                require(
                  player2.hash.length === BigInt(0),
                  "Current bet must not have a player2 yet"
                )
                require(
                  value.policyIds.contains(scriptHash),
                  "Input must contain the bet token"
                )
                // V002 fix: Verify bet token is preserved in output
                require(
                  outputValue.policyIds.contains(scriptHash),
                  "Output must contain the bet token"
                )
                // V016 fix: Verify full address including staking credential
                require(
                  outputAddress === address,
                  "Output address must match input address (including staking credential)"
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

            // ???: oracle can spend token to create a malformed bet, e.g. oracle === player1
            // TODO: all minted tokens should be burnt
            case Action.AnnounceWinner(winner, payoutOutputIdx) =>
                // V005 fix: Use indexed lookup to prevent double satisfaction
                require(
                  payoutOutputIdx >= BigInt(0),
                  "Payout output index must be non-negative"
                )
                val payoutOutput = txInfo.outputs.at(payoutOutputIdx)
                val TxOut(payoutAddress, payoutValue, _, _) = payoutOutput
                require(
                  winner === player1 || winner === player2,
                  "Winner must be either player1 or player2"
                )
                require(
                  player2.hash.length != BigInt(0),
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

    /** Minting policy:
      *
      * Controls the creation of bet tokens This ensures proper initialization of a new bet
      */
    inline override def mint(
        @annotation.unused redeemer: Data,
        policyId: PolicyId,
        tx: TxInfo
    ): Unit =
        val Config(player1, player2, oracle, expiration) = tx.outputs
            .filter:
                _.address === Address.fromScriptHash(policyId)
            .match
                case List.Cons(TxOut(_, _, OutputDatum(datum), _), List.Nil) => datum.to[Config]
                case _ =>
                    fail(
                      "There must be a single output with inline initial betting config that goes to the script"
                    )
        // Validate exactly one token minted under this policy (V003/V011 fix)
        val mintedTokens = tx.mint.tokens(policyId).toList
        require(
          mintedTokens.length === BigInt(1),
          "Must mint exactly one token type under this policy"
        )
        require(
          mintedTokens.head._2 === BigInt(1),
          "Must mint exactly one token"
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
