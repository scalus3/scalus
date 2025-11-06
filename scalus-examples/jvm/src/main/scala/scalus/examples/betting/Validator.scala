package scalus.examples.betting

import scalus.Compiler.compileWithOptions
import scalus.builtin.ByteString.*
import scalus.builtin.Data
import scalus.builtin.Data.FromData
import scalus.builtin.Data.ToData
import scalus.cardano.blueprint.Application
import scalus.cardano.blueprint.Blueprint
import scalus.ledger.api.v1.Address
import scalus.ledger.api.v2.OutputDatum.OutputDatum
import scalus.ledger.api.v3.*
import scalus.prelude.*
import scalus.prelude.Option.*
import scalus.{show as _, *}

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
case class BetDatum(
    player1: PubKeyHash,
    player2: PubKeyHash,
    oracle: PubKeyHash,
    expiration: PosixTime
) derives FromData,
      ToData

@Compile
object BetDatum

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
  */
@Compile
object Betting extends Validator:
    /** Spending validator: Controls how the bet UTXO can be spent Handles both
      * [[scalus.examples.Action.Join]] and [[scalus.examples.Action.AnnounceWinner]] actions
      */
    inline override def spend(
        datum: Option[Data],
        redeemer: Data,
        txInfo: TxInfo,
        txOutRef: TxOutRef
    ): Unit =
        val (address, value, BetDatum(player1, player2, oracle, expiration)) =
            txInfo.findOwnInput(txOutRef) match
                case Some(TxInInfo(_, TxOut(address, value, OutputDatum(currentDatum), _))) =>
                    (address, value, currentDatum.to[BetDatum])
                case _ =>
                    fail:
                        "Initial bet spent input must be present,\n" +
                            "current bet datum must be inline"

        redeemer.to[Action] match
            case Action.Join =>
                val outputLovelace = txInfo.outputs.filter(_.address === address) match
                    case List.Cons(TxOut(_, value, _, _), List.Nil) => value.getLovelace
                    case _ => fail("There must be a single continuing output")
                val BetDatum(newPlayer1, joiningPlayer, newOracle, newExpiration) =
                    datum.getOrFail("new datum must be present").to[BetDatum]
                require(
                  player2.hash.length === BigInt(
                    0
                  ),
                  "Current bet must not have a player2 yet"
                )
                require(
                  value.policyIds.map(Address.fromScriptHash).contains(address),
                  "Input must contain the bet token"
                )
                require(
                  txInfo.signatories.contains(
                    joiningPlayer
                  ),
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
                  outputLovelace === BigInt(
                    2
                  ) * value.getLovelace,
                  "The bet amount must double (player2 matches player1's bet)"
                )
                require(
                  newExpiration === expiration,
                  "The updated datum must have the same expiration as the current one"
                )
                require(
                  txInfo.validRange.isEntirelyBefore(
                    newExpiration
                  ),
                  "Joining must happen before the bet expiration"
                )

            case Action.AnnounceWinner(winner) =>
                val payoutAddress = txInfo.outputs match
                    case List.Cons(payoutOutput, List.Nil) => payoutOutput.address
                    case _ => fail("There's must be a single payout output")
                require(
                  winner === player1 || winner === player2,
                  "Winner must be either player1 or player2"
                )
                require(
                  player2.hash.length != BigInt(
                    0
                  ),
                  "Both players must have joined (player2 is not None)"
                )
                require(
                  datum === None,
                  "No continuing datum (bet is being closed)"
                )
                require(
                  payoutAddress === Address.fromPubKeyHash(
                    winner
                  ),
                  "Payout goes to the winner's address"
                )
                require(
                  txInfo.signatories.contains(
                    oracle
                  ),
                  "Oracle must sign the transaction"
                )
                require(
                  txInfo.validRange.isEntirelyAfter(
                    expiration
                  ),
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
        val BetDatum(player1, player2, oracle, expiration) = tx.outputs
            .filter:
                _.address === Address.fromScriptHash(policyId)
            .match
                case List.Cons(TxOut(_, _, OutputDatum(datum), _), List.Nil) => datum.to[BetDatum]
                case _ =>
                    fail(
                      "There's must be a single output that goes to the script (the bet UTXO),\n" +
                          "Bet datum must be inline"
                    )
        require(
          tx.signatories.contains(player1),
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
