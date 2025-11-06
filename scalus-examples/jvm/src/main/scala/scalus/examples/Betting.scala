package scalus.examples

import scalus.Compiler.compile
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
    ): Unit = txInfo.findOwnInput(txOutRef) match
        case Some(TxInInfo(_, TxOut(address, value, OutputDatum(currentDatum), _))) =>
            val BetDatum(player1, player2, oracle, expiration) = currentDatum.to[BetDatum]
            redeemer.to[Action] match
                // Handle player2 joining the bet
                case Action.Join =>
                    // Verify the input contains the bet token (proves it's a valid bet UTXO)
                    val hasBetToken = value.policyIds
                        .map(Address.fromScriptHash)
                        .contains(address)
                    // Find the continuing output (bet UTXO with updated datum)
                    txInfo.outputs.filter(_.address === address) match
                        case List.Cons(TxOut(_, outputValue, _, _), List.Nil) =>
                            datum match
                                // Extract the updated bet state
                                case Some(newDatum) =>
                                    val newBetDatum = newDatum.to[BetDatum]
                                    val joiningPlayer = newBetDatum.player2
                                    // Validation rules for joining:
                                    require(
                                      player2.hash.length === BigInt(
                                        0
                                      ),
                                      "Current bet must not have a player2 yet"
                                    )
                                    require(
                                      hasBetToken,
                                      "Input must contain the bet token"
                                    )
                                    require(
                                      txInfo.signatories.contains(
                                        joiningPlayer
                                      ),
                                      "Player2 must sign the transaction"
                                    )
                                    require(
                                      newBetDatum.oracle === oracle,
                                      "Oracle must remain unchanged"
                                    )
                                    require(
                                      newBetDatum.player1 === player1,
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
                                      outputValue.getLovelace === BigInt(
                                        2
                                      ) * value.getLovelace,
                                      "The bet amount must double (player2 matches player1's bet)"
                                    )
                                    require(
                                      newBetDatum.expiration === expiration,
                                      "The updated datum must have the same expiration as the current one"
                                    )
                                    require(
                                      txInfo.validRange.isEntirelyBefore(
                                        newBetDatum.expiration
                                      ),
                                      "Joining must happen before the bet expiration"
                                    )
                                case _ => fail("New datum must be present")
                        case _ => fail("There must be a single continuing output")
                // Handle oracle announcing the winner
                case Action.AnnounceWinner(winner) =>
                    txInfo.outputs match
                        case List.Cons(payoutOutput, List.Nil) =>
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
                              payoutOutput.address === Address.fromPubKeyHash(
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
                        case _ => fail("There's must be a single payout output")
        case _ =>
            fail:
                "Initial bet spent input must be present,\n" +
                    "current bet datum must be inline"

    /** Minting policy: Controls the creation of bet tokens This ensures proper initialization of a
      * new bet
      */
    inline override def mint(
        @annotation.unused redeemer: Data,
        policyId: PolicyId,
        tx: TxInfo
    ): Unit = tx.outputs.filter(_.address === Address.fromScriptHash(policyId)) match
        case List.Cons(TxOut(_, _, OutputDatum(datum), _), List.Nil) =>
            val BetDatum(player1, player2, oracle, expiration) = datum.to[BetDatum]
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
        case _ =>
            fail:
                "There's must be a single output that goes to the script (the bet UTXO),\n" +
                    "Bet datum must be inline"

object BettingContract:

    inline def compiled(using scalus.Compiler.Options) = compile(Betting.validate)

    def application: Application = Application
        .ofSingleValidator[BetDatum, Action](
          "Betting validator",
          "Decentralized two-player betting system with trustless wagering and oracle-based resolution",
          "1.0.0",
          Betting.validate
        )

    def blueprint: Blueprint = application.blueprint

end BettingContract
