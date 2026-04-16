package lottery.onchain

import scalus.uplc.builtin.Builtins.sha2_256
import scalus.uplc.builtin.{ByteString, Data, FromData, ToData}
import scalus.cardano.onchain.plutus.v1.{PosixTime, PubKeyHash}
import scalus.cardano.onchain.plutus.v3.{TxInfo, TxOutRef, Validator}
import scalus.cardano.onchain.plutus.{v1, v2}
import scalus.cardano.onchain.plutus.prelude.*
import scalus.Compile

type Preimage = ByteString
type Secret = ByteString

// Datum
case class State(
    playerOneSecret: Secret,
    playerTwoSecret: Secret,
    revealDeadline: PosixTime,
    lotteryState: LotteryState
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

// Redeemer
enum Action derives ToData, FromData:
    case RevealPlayerOne(preimage: Preimage)
    case RevealPlayerTwo(preimage: Preimage)
    case Lose(preimage: Preimage, winnerOutputIdx: BigInt)
    case Timeout(preimage: Preimage)

@Compile
object Action

/** Two-player lottery using a commit-reveal-punish protocol.
  *
  * Two players each commit a secret (SHA-256 hash of a preimage) and place an equal bet. After
  * committing, players reveal their preimages sequentially. The winner is determined by
  * `(len(preimage1) + len(preimage2)) mod 2`: even means the revealing player wins.
  *
  * If a player refuses to reveal, the other player can claim the pot after a deadline via Timeout.
  * If a player knows they lost (odd sum), they concede via Lose.
  *
  * @note
  *   Players should use preimages of at least 32 bytes to prevent brute-force attacks.
  *
  * @see
  *   [[https://github.com/blockchain-unica/rosetta-smart-contracts/tree/main/contracts/lottery Rosetta Specification]]
  */
@Compile
object LotteryValidator extends Validator {

    inline def spend(
        datum: scalus.cardano.onchain.plutus.prelude.Option[Data],
        redeemer: Data,
        tx: TxInfo,
        ownRef: TxOutRef
    ): Unit = {
        val ownInput = tx.findOwnInputOrFail(ownRef)
        val amount = ownInput.resolved.value.getLovelace

        val action = redeemer.to[Action]
        val state = datum.getOrFail("Datum not found").to[State]
        state.lotteryState match {
            case LotteryState.Empty =>
                action match {
                    case Action.RevealPlayerOne(preimage) =>
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

                        require(
                          newState.playerOneSecret === state.playerOneSecret,
                          "Player one secret must not change"
                        )
                        require(
                          newState.playerTwoSecret === state.playerTwoSecret,
                          "Player two secret must not change"
                        )
                        require(
                          newState.revealDeadline === state.revealDeadline,
                          "Reveal deadline must not change"
                        )

                    case Action.RevealPlayerTwo(preimage) =>
                        val isValid = sha2_256(preimage) === state.playerTwoSecret
                        require(isValid, "Fraudulent attempt")

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

                        require(
                          newState.playerOneSecret === state.playerOneSecret,
                          "Player one secret must not change"
                        )
                        require(
                          newState.playerTwoSecret === state.playerTwoSecret,
                          "Player two secret must not change"
                        )
                        require(
                          newState.revealDeadline === state.revealDeadline,
                          "Reveal deadline must not change"
                        )

                    case _ =>
                        fail("Too early to give up or claim a timeout -- need to reveal first")
                }

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
                        val isReallyPlayerTwo =
                            sha2_256(playerTwoPreimage) === state.playerTwoSecret
                        require(isReallyPlayerTwo, "Fraudulent attempt")
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
                        val isReallyPlayerOne =
                            sha2_256(playerOnePreimage) === state.playerOneSecret
                        require(isReallyPlayerOne, "Fraudulent attempt")
                        require(
                          tx.validRange.isEntirelyAfter(state.revealDeadline),
                          "Deadline not reached"
                        )
                }

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
                        require(
                          sha2_256(playerTwoPreimage) === state.playerTwoSecret,
                          "Fraudulent attempt"
                        )
                        require(
                          tx.validRange.isEntirelyAfter(state.revealDeadline),
                          "Deadline not reached"
                        )
                }
        }
    }
}
