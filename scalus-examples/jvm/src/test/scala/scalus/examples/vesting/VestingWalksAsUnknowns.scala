package scalus.examples.vesting

import scalus.cardano.onchain.plutus.v3.*
import scalus.cardano.onchain.plutus.prelude.*

/** What the walks over lists of [[VestingValidator.spend]] return: the probe of "calls as unknowns"
  * (docs/design/verification-details/multi-transaction.md, "The cost"). Each field is the result of
  * one walk, on the arguments the validator gives it.
  *
  * @param ownInputFound
  *   `txInfo.findInputOrFail(txOutRef)` returns: the spent output is an input
  * @param vestingInputIsUnique
  *   the `findUniqueOrFail` over the inputs at the spent output's credential returns
  * @param locked
  *   the lovelace of the spent output, `ownInput.value.getLovelace`
  * @param signed
  *   `txInfo.isSignedBy(beneficiary)`
  * @param adaInInputs
  *   the lovelace the beneficiary's inputs hold together
  * @param adaInOutputs
  *   the lovelace the beneficiary's outputs hold together
  * @param continuingDatum
  *   the datum of the one output at the spent output's address, where `findContinuingOutputOrFail`
  *   returns
  * @param valueKept
  *   that output's value is the spent output's less the withdrawn lovelace
  * @param continuingLocked
  *   the lovelace of that output, `getLovelace` of its value: a walk of the fourth guarantee, which
  *   the validator does not make
  */
case class Walks(
    ownInputFound: Boolean,
    vestingInputIsUnique: Boolean,
    locked: BigInt,
    signed: Boolean,
    adaInInputs: BigInt,
    adaInOutputs: BigInt,
    continuingDatum: Option[OutputDatum],
    valueKept: Boolean,
    continuingLocked: BigInt
) derives FromData,
      ToData

/** [[VestingValidator]] with the results of its walks over lists given as [[Walks]], and not
  * computed: the same checks in the same order, with no loop left. A statement proved of it for
  * every context and every [[Walks]] holds of the validator, where each walk returns what it is
  * given here, and where the statement needs nothing of a walk but that the validator and the
  * statement make the same one.
  *
  * `validateWithoutDatumCheck` is the same without the check of the datum of the output that keeps
  * the rest, as [[VestingWithoutDatumCheck]] is of the validator.
  */
@Compile
object VestingWalksAsUnknowns {

    inline def validate(scData: Data, walks: Walks): Unit = dispatch(scData, walks, true)

    inline def validateWithoutDatumCheck(scData: Data, walks: Walks): Unit =
        dispatch(scData, walks, false)

    inline def dispatch(scData: Data, walks: Walks, inline checksTheDatum: Boolean): Unit = {
        val sc = scData.to[ScriptContext]
        sc.scriptInfo match
            case ScriptInfo.SpendingScript(_, datum) =>
                spend(datum, sc.redeemer, sc.txInfo, walks, checksTheDatum)
            case _ => fail("Only spending")
    }

    inline def spend(
        datum: Option[Data],
        redeemer: Data,
        txInfo: TxInfo,
        walks: Walks,
        inline checksTheDatum: Boolean
    ): Unit = {
        val vestingDatum = datum.getOrFail(VestingValidator.DatumNotFound)
        val vestingConfig = vestingDatum.to[Config]
        val Action(requestedAmount) = redeemer.to[Action]
        require(requestedAmount > 0, VestingValidator.NonPositiveAmount)
        require(walks.ownInputFound, "Tx input not found")
        require(walks.vestingInputIsUnique, VestingValidator.MultipleVestingInputs)
        val contractAmount = walks.locked
        val txEarliestTime = txInfo.validFromOrFail(VestingValidator.NoValidityLowerBound)
        val released = vestingConfig.initialAmount - contractAmount
        val availableAmount =
            VestingValidator.linearVesting(vestingConfig, txEarliestTime) - released
        require(walks.signed, VestingValidator.NoBeneficiarySignature)
        require(requestedAmount <= availableAmount, VestingValidator.AmountExceedsAvailable)
        val expectedOutput = requestedAmount + walks.adaInInputs - txInfo.fee
        require(
          walks.adaInOutputs === expectedOutput,
          VestingValidator.BeneficiaryOutputMismatch
        )
        if requestedAmount === contractAmount then ()
        else
            val continuingDatum =
                walks.continuingDatum.getOrFail(VestingValidator.NotExactlyOneContractOutput)
            require(walks.valueKept, VestingValidator.ContinuingValueMismatch)
            inline if checksTheDatum then
                require(
                  continuingDatum === OutputDatum.OutputDatum(vestingDatum),
                  VestingValidator.InvalidDatum
                )
    }
}
