package scalus.examples.betting

import scalus.builtin.ByteString.*
import scalus.builtin.Data
import scalus.builtin.Data.FromData
import scalus.builtin.Data.ToData
import scalus.builtin.ToData.*
import scalus.ledger.api.v1.Address
import scalus.ledger.api.v1.Credential.{PubKeyCredential, ScriptCredential}
import scalus.ledger.api.v2.OutputDatum.{NoOutputDatum, OutputDatum}
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

    /** Action for the oracle to announce the winner and trigger payout */
    case AnnounceWinner(winner: PubKeyHash)

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
      * [[scalus.examples.Action.Join]] and [[scalus.examples.Action.AnnounceWinner]] actions
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
                  outputLovelace,
                  Config(newPlayer1, joiningPlayer, newOracle, newExpiration)
                ) = txInfo
                    .findOwnScriptOutputs(scriptHash)
                    .match
                        case List.Cons(TxOut(_, value, OutputDatum(newDatum), _), List.Nil) =>
                            (value.getLovelace, newDatum.to[Config])
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
                  outputLovelace === BigInt(2) * value.getLovelace,
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
            case Action.AnnounceWinner(winner) =>
                val payoutAddress = txInfo.outputs
                    .filter:
                        case TxOut(Address(PubKeyCredential(recipient), _), _, _, _) =>
                            recipient !== oracle
                        case _ => true
                    .match
                        case List.Cons(TxOut(payoutAddress, _, NoOutputDatum, _), List.Nil) =>
                            payoutAddress
                        case _ =>
                            fail(
                              "There's must be a single payout output with no continuing betting config"
                            )
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
