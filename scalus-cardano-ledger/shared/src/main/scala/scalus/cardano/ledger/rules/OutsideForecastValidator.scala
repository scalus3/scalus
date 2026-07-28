package scalus.cardano.ledger
package rules

/** Alonzo/Babbage `validateOutsideForecast` in cardano-ledger.
  *
  * The Haskell rule rejects a transaction that carries redeemers and a validity-interval upper
  * bound which `epochInfoSlotToUTCTime` cannot translate to wall-clock time. That translation only
  * fails because the consensus layer provides an `EpochInfo` whose horizon is limited to the
  * stability window past the current tip — beyond it, a hard fork could change slot lengths, so the
  * slot-to-time mapping is not guaranteed. The horizon is consensus environment
  * (`Globals.epochInfo` / `stabilityWindow`), not ledger state.
  *
  * Scalus models the chain with a fixed-slot-length [[scalus.cardano.ledger.SlotConfig]] (a single,
  * never-forking era), under which slot-to-time translation is total — so this rule cannot fire and
  * always succeeds. This is the faithful semantics for the emulator and for replaying historical
  * transactions (anything accepted on-chain already passed the node's horizon check); it
  * intentionally does not reproduce the node's forecast-horizon rejection for far-future validity
  * bounds.
  */
object OutsideForecastValidator extends STS.Validator {
    override final type Error = Nothing

    override def validate(context: Context, state: State, event: Event): Result = {
        success
    }
}
