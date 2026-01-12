package scalus.examples.pricebet

import scalus.Compile
import scalus.builtin.ByteString
import scalus.builtin.Data.{FromData, ToData}
import scalus.ledger.api.v1.{PosixTime, PubKeyHash}
import scalus.ledger.api.v3.{TxInfo, TxOutRef}
import scalus.ledger.api.v2
import scalus.prelude.*

/** @param owner
  *   a party that initiates the bet
  * @param oracleScriptHash
  *   the script hash of the oracle validator
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
    oracleScriptHash: ByteString,
    player: Option[PubKeyHash],
    deadline: PosixTime,
    exchangeRate: (BigInt, BigInt)
) derives FromData,
      ToData

enum Action derives FromData, ToData:
    case Join
    case Win
    case Timeout

@Compile
object PricebetValidator extends Validator {

    inline def spend(
        datum: Option[BuiltinData],
        redeemer: BuiltinData,
        tx: TxInfo,
        ownRef: TxOutRef
    ): Unit = {
        val state = datum.getOrFail("Datum must be present").to[PricebetState]
        val action = redeemer.to[Action]
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
                val newDatum = continuationOutput.datum match {
                    case v2.OutputDatum.OutputDatum(d) => d.to[PricebetState]
                    case _ => fail("Continuation must have inline datum")
                }

                // Find who signed and verify they're the player
                require(newDatum.player.isDefined, "Player must be set in new datum")
                val playerPkh = newDatum.player.get
                require(tx.signatories.exists(_ === playerPkh), "Must be signed by player")

                // Verify other fields unchanged
                require(newDatum.owner === state.owner, "Owner must not change")
                require(
                  newDatum.oracleScriptHash === state.oracleScriptHash,
                  "Oracle must not change"
                )
                require(newDatum.deadline === state.deadline, "Deadline must not change")
                require(
                  newDatum.exchangeRate === state.exchangeRate,
                  "Exchange rate must not change"
                )

            case Action.Win =>
                // Verify player exists and signed
                require(state.player.isDefined, "No player joined yet")
                val playerPkh = state.player.get
                require(tx.signatories.exists(_ === playerPkh), "Must be signed by player")

                // Verify before deadline
                require(!tx.validRange.isEntirelyAfter(state.deadline), "Deadline passed")

                // Find oracle in reference inputs by script hash
                val oracleInputs = tx.referenceInputs.filter { input =>
                    input.resolved.address.credential match {
                        case scalus.ledger.api.v1.Credential.ScriptCredential(hash) =>
                            hash === state.oracleScriptHash
                        case _ => false
                    }
                }
                require(oracleInputs.length >= BigInt(1), "Oracle reference input required")

                val oracleInput = oracleInputs.head
                val oracleState = oracleInput.resolved.datum match {
                    case v2.OutputDatum.OutputDatum(d) => d.to[scalus.examples.pricebet.OracleState]
                    case _                             => fail("Oracle must have inline datum")
                }

                // Verify oracle timestamp is within tx validity window
                val validRange = tx.validRange
                // TODO: Implement proper timestamp validation with interval bounds

                val (betNom, betDenom) = state.exchangeRate
                val oracleNom = oracleState.exchangeRateNominator
                val oracleDenom = oracleState.exchangeRateDenominator

                require(
                  // by way of cross multiplication
                  oracleNom * betDenom > betNom * oracleDenom,
                  "Oracle rate must exceed bet rate"
                )

            case Action.Timeout =>
                // Verify owner signed
                require(tx.signatories.exists(_ === state.owner), "Must be signed by owner")

                // Verify deadline passed
                require(tx.validRange.isEntirelyAfter(state.deadline), "Deadline not reached")
        }
    }
}
