package scalus.examples.pricebet

import scalus.Compile
import scalus.builtin.ByteString
import scalus.builtin.Data.{FromData, ToData}
import scalus.builtin.Data
import scalus.ledger.api.v1.{Credential, PosixTime, PubKeyHash}
import scalus.ledger.api.v3.{TxInInfo, TxInfo, TxOutRef}
import scalus.ledger.api.v2
import scalus.prelude.*
import scalus.prelude.Ord.>

// Parameter
case class PricebetConfig(
    oracleScriptHash: ByteString
) derives FromData,
      ToData

/** @param owner
  *   a party that initiates the bet
  * @param player
  *   a player that has accepted the bet. If no player accepts the bet, the owner can redeem the
  *   initial bet using [[Action.Timeout]]
  * @param deadline
  *   a deadline for [[Action.Timeout]] funds redemption
  * @param exchangeRate
  *   the immutable target exchange rate for the [[player]] to win. If the oracle ever returns a
  *   rate greater than this value, the [[player]] wins.
  */
case class PricebetState(
    owner: PubKeyHash,
    player: Option[PubKeyHash],
    deadline: PosixTime,
    exchangeRate: Rational,
) derives FromData,
      ToData

// Redeemer
enum Action derives FromData, ToData:
    case Join
    case Win(oracleOut: BigInt) // oracle input idx
    case Timeout

@Compile
object PricebetValidator extends DataParameterizedValidator {

    inline def spend(
        param: Data,
        datum: Option[BuiltinData],
        redeemer: BuiltinData,
        tx: TxInfo,
        ownRef: TxOutRef
    ): Unit = {
        val state = datum.getOrFail("Datum must be present").to[PricebetState]
        val action = redeemer.to[Action]
        val config = param.to[PricebetConfig]
        val ownInput = tx.findOwnInputOrFail(ownRef)

        action match {
            case Action.Join =>
                // Verify no player has joined yet
                require(state.player.isEmpty, "Player already joined")

                // Find continuation output
                val continuationOutputs =
                    tx.outputs.filter(out => out.address === ownInput.resolved.address)
                require(
                  continuationOutputs.length === BigInt(1),
                  "Must have exactly one continuation output"
                )

                val continuationOutput = continuationOutputs.head
                val initialBetAmount = ownInput.resolved.value.getLovelace

                // Verify continuation output has 2x the bet
                require(
                  continuationOutput.value.getLovelace === initialBetAmount * 2,
                  "Must match bet amount"
                )

                // Verify new datum
                val newState = continuationOutput.datum match {
                    case v2.OutputDatum.OutputDatum(d) => d.to[PricebetState]
                    case _ => fail("Continuation must have inline datum")
                }

                // Find who signed and verify they're the player
                require(newState.player.isDefined, "Player must be set in new datum")
                val playerPkh = newState.player.get
                require(tx.isSignedBy(playerPkh), "Must be signed by player")

                // Verify other fields unchanged
                require(newState.owner === state.owner, "Owner must not change")
                require(newState.deadline === state.deadline, "Deadline must not change")
                require(
                  newState.exchangeRate === state.exchangeRate,
                  "Exchange rate must not change"
                )

            case Action.Win(index) =>
                // Verify player exists and signed
                require(state.player.isDefined, "No player joined yet")
                val playerPkh = state.player.get
                require(tx.isSignedBy(playerPkh), "Must be signed by player")

                // Verify before deadline
                require(!tx.validRange.isEntirelyAfter(state.deadline), "Deadline passed")

                val oracleInput: TxInInfo = tx.referenceInputs.at(index)
                oracleInput.resolved.address.credential match {
                    case Credential.PubKeyCredential(hash) => fail(OracleInputMustBeOracleScript)
                    case Credential.ScriptCredential(hash) =>
                        require(hash == config.oracleScriptHash, OracleInputMustBeOracleScript)
                }

                val oracleState = oracleInput.resolved.datum match {
                    case v2.OutputDatum.OutputDatum(d) => d.to[scalus.examples.pricebet.OracleState]
                    case _                             => fail("Oracle must have inline datum")
                }

                // Verify oracle timestamp is within tx validity window
                val validRange = tx.validRange
                require(
                  validRange.isEntirelyAfter(oracleState.timestamp),
                  "Oracle timestamp must be within transaction validity range"
                )

                val rateToBeat = state.exchangeRate
                require(
                  // by way of cross multiplication
                  oracleState.exchangeRate > rateToBeat,
                  "Oracle rate must exceed bet rate"
                )

            case Action.Timeout =>
                // Verify owner signed
                require(tx.signatories.exists(_ === state.owner), "Must be signed by owner")

                // Verify deadline passed
                require(tx.validRange.isEntirelyAfter(state.deadline), "Deadline not reached")
        }
    }

    private inline val OracleInputMustBeOracleScript =
        "Oracle input must be locked by the oracle script"
}
