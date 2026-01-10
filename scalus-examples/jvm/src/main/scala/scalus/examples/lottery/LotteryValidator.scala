package scalus.examples.lottery

import scalus.builtin.Builtins.sha2_256
import scalus.builtin.{ByteString, Data, FromData, ToData}
import scalus.ledger.api.v1.{PosixTime, PubKeyHash}
import scalus.ledger.api.v3.{TxInfo, TxOutRef}
import scalus.ledger.api.{v1, v2}
import scalus.prelude.*
import scalus.{prelude, Compile}

type Preimage = ByteString
type Secret = ByteString

case class State(
    playerOneSecret: Secret,
    playerTwoSecret: Secret,
    revealDeadline: PosixTime,
    lotteryState: LotteryState,
) derives FromData,
      ToData

@Compile
object State

enum LotteryState derives FromData, ToData:
    case Empty
    case PlayerOneRevealed(length: BigInt, pubKeyHash: PubKeyHash)
    case PlayerTwoRevealed(length: BigInt, pubKeyHash: PubKeyHash)

@Compile
object LotteryState

enum Action derives ToData, FromData:
    case RevealPlayerOne(preimage: Preimage)
    case RevealPlayerTwo(preimage: Preimage)
    case Lose(preimage: Preimage, winnerOutputIdx: BigInt)
    case Timeout(preimage: Preimage)

@Compile
object Action

/** A lottery between two players, where each of them commits a bet and the winner takes both bets.
  * Since Cardano is deterministic, this lottery uses a commit-reveal-punish scheme to ensure
  * fairness.
  *
  * The scheme works by two players commiting a secret beforehand, and using the preimages of the
  * secrets to determine a winner using a fair function, in this case, a `mod(2)` of the pre-images
  * lengths.
  *
  * The lottery starts off with a multisig transaction that commits both players bets to the
  * contract. Then, the lottery is considered [[LotteryState.Empty]].
  *
  * Then, any of their players can issue a Reveal action, supplying their preimage.
  *
  * After that, any action ends the lottery. The valid actions are:
  *
  * 1) The second player reveals their secret. The revealing player in this case already knows the
  * outcome of the lottery, and by doing their reveal, claims a victory. The player supplies the
  * preimage so that the contract can confirm their victory, and gets their payout. If the supplied
  * preimage does not hash to the initially commited secret, the validator fails, allowing the first
  * player to claim the pot via a [[Action.Timeout]] after a delay.
  *
  * 2) The second player concedes the pot by sending an [[Action.Lose]]. As explained above, the
  * first players reveal has given the second player information to determine their loss.
  *
  * 3) If the second player has failed to reveal the secret, the first player can claim the pot via
  * a [[Action.Timeout]] after a specified delay.
  *
  * @note
  *   It's *recommended* that the players use preimages that are at least 32 bytes long to ensure
  *   security of their secrets. Otherwise, a malicious player could guess their opponents preimage
  *   by using a brute force attack against their secret.
  */
@Compile
object LotteryValidator extends Validator {

    inline def spend(
        datum: prelude.Option[Data],
        redeemer: Data,
        tx: TxInfo,
        ownRef: TxOutRef
    ): Unit = {
        val ownInput = tx.findOwnInputOrFail(ownRef)
        val amount = ownInput.resolved.value.getLovelace

        val action = redeemer.to[Action]
        val state = datum.getOrFail("Datum not found").to[State]
        state.lotteryState match {
            // If the lottery state is empty, i.e. no players have revealed yet, the only possible thing is the revelation
            // by one of the players.
            case LotteryState.Empty =>
                action match {
                    case Action.RevealPlayerOne(preimage) =>
                        // verify player identity
                        val isValid = sha2_256(preimage) === state.playerOneSecret
                        require(isValid, "Fraudulent attempt")

                        val continuationOutputs =
                            tx.outputs.filter(out => out.address === ownInput.resolved.address)
                        require(
                          continuationOutputs.length == BigInt(1),
                          "Must have exactly one continuation output"
                        )

                        val continuationOutput = continuationOutputs.head

                        val newState = continuationOutput.datum match {
                            case v2.OutputDatum.OutputDatum(datum) => datum.to[State]
                            case _ => fail("continuation out must have an inline datum")
                        }

                        newState.lotteryState match {
                            case LotteryState.PlayerOneRevealed(length, pkh) =>
                                require(length === preimage.length, "Length mismatch")
                                require(
                                  tx.signatories.exists(_ === pkh),
                                  "Must be signed by player one"
                                )
                            case _ => fail("Invalid state transition")
                        }

                    case Action.RevealPlayerTwo(preimage) =>
                        // Verify preimage hash matches
                        val isValid = sha2_256(preimage) === state.playerTwoSecret
                        require(isValid, "Fraudulent attempt")

                        // Find the continuation output with updated state
                        val continuationOutputs =
                            tx.outputs.filter(out => out.address === ownInput.resolved.address)
                        require(
                          continuationOutputs.length === BigInt(1),
                          "Must have exactly one continuation output"
                        )

                        val continuationOutput = continuationOutputs.head
                        val newState = continuationOutput.datum match {
                            case v2.OutputDatum.OutputDatum(datum) => datum.to[State]
                            case _ => fail("continuation out must have an inline datum")
                        }

                        newState.lotteryState match {
                            case LotteryState.PlayerTwoRevealed(length, pkh) =>
                                require(length === preimage.length, "Length mismatch")
                                require(
                                  tx.signatories.exists(_ === pkh),
                                  "Must be signed by player two"
                                )
                            case _ => fail("Invalid state transition")
                        }

                    case _ =>
                        fail("Too early to give up or claim a timeout -- need to reveal first")
                }

            // If the first player has revealed, one of the three things is possible:
            // 1) Player two reveals. In this case, since they already know the player 1 preimage they know that they
            //    have won. Otherwise, they can claim a loss
            // 2) Player two claims a loss, since they can see that the other players secret wins.
            // 3) Player one claims the prize after a timeout if the player two has failed to reveal their secret.
            //
            // Any of this actions must be accompanied by the preimage to verify the player's identity. All other actions
            // are impossible.
            case LotteryState.PlayerOneRevealed(playerOnePreimageLen, playerOnePkh) =>
                action match {
                    case Action.RevealPlayerOne(_) =>
                        fail("Player one already revealed")
                    case Action.RevealPlayerTwo(playerTwoPreimage) =>
                        val isReallyPlayerTwo =
                            sha2_256(playerTwoPreimage) === state.playerTwoSecret
                        require(isReallyPlayerTwo, "Fraudulent attempt")
                        val totalLength = playerOnePreimageLen + playerTwoPreimage.length
                        require(totalLength % 2 == BigInt(0), "Unlucky")

                    case Action.Lose(playerTwoPreimage, winnerOutputIdx) =>
                        // Player two concedes, giving pot to player one
                        val isReallyPlayerTwo =
                            sha2_256(playerTwoPreimage) === state.playerTwoSecret
                        require(isReallyPlayerTwo, "Fraudulent attempt")

                        // Verify output to player one contains all the money
                        val supposedWinnerOutput = tx.outputs.at(winnerOutputIdx)
                        supposedWinnerOutput.address.credential match {
                            case v1.Credential.PubKeyCredential(hash) =>
                                require(hash === playerOnePkh, "Wrong winner")
                            case v1.Credential.ScriptCredential(_) => fail("Winner must be pubkey")
                        }
                        require(
                          supposedWinnerOutput.value.getLovelace >= amount,
                          "Insufficient payout"
                        )

                    case Action.Timeout(playerOnePreimage) =>
                        // Player two didn't reveal in time, player one claims pot
                        val isReallyPlayerOne =
                            sha2_256(playerOnePreimage) === state.playerOneSecret
                        require(isReallyPlayerOne, "Fraudulent attempt")
                        // Verify time has passed deadline
                        require(
                          tx.validRange.isEntirelyAfter(state.revealDeadline),
                          "Deadline not reached"
                        )
                }

            // This branch mirrors the one above, but for the player two.    /
            case LotteryState.PlayerTwoRevealed(playerTwoPreimageLen, playerTwoPkh) =>
                action match {
                    case Action.RevealPlayerTwo(_) =>
                        fail("Player two already revealed")
                    case Action.RevealPlayerOne(playerOnePreimage) =>
                        require(
                          sha2_256(playerOnePreimage) === state.playerOneSecret,
                          "Fraudulent attempt"
                        )
                        val totalLength = playerTwoPreimageLen + playerOnePreimage.length
                        require(totalLength % 2 == BigInt(0), "Unlucky")

                    case Action.Lose(playerOnePreimage, winnerOutputIdx) =>
                        // Player one concedes, giving pot to player two
                        require(
                          sha2_256(playerOnePreimage) === state.playerOneSecret,
                          "Fraudulent attempt"
                        )
                        val supposedWinnerOutput = tx.outputs.at(winnerOutputIdx)
                        supposedWinnerOutput.address.credential match {
                            case v1.Credential.PubKeyCredential(hash) =>
                                require(hash === playerTwoPkh, "Wrong winner")
                            case v1.Credential.ScriptCredential(_) => fail("Winner must be pubkey")
                        }
                        require(
                          supposedWinnerOutput.value.getLovelace >= amount,
                          "Insufficient payout"
                        )

                    case Action.Timeout(playerTwoPreimage) =>
                        // Player one didn't reveal in time, player two claims pot
                        require(
                          sha2_256(playerTwoPreimage) === state.playerTwoSecret,
                          "Fraudulent attempt"
                        )
                        // Verify time has passed deadline
                        require(
                          tx.validRange.isEntirelyAfter(state.revealDeadline),
                          "Deadline not reached"
                        )
                }
        }
    }
}
