package scalus.examples.vault

import scalus.builtin.Data.{FromData, ToData}
import scalus.builtin.{ByteString, Data}
import scalus.examples.vault.Redeemer.{Cancel, Deposit, Finalize, Withdraw}
import scalus.ledger.api
import scalus.ledger.api.v1.Credential.ScriptCredential
import scalus.ledger.api.v1.{Credential, Interval, IntervalBoundType, PosixTime, Value}
import scalus.ledger.api.v2.{OutputDatum, TxOut}
import scalus.ledger.api.v3.{TxInInfo, TxInfo, TxOutRef}
import scalus.ledger.api.{v1, v2}
import scalus.prelude.{===, fail, require, Validator}
import scalus.uplc.Program
import scalus.*

case class Datum(
    owner: ByteString,
    state: State,
    amount: BigInt,
    waitTime: PosixTime,
    finalizationDeadline: PosixTime
) derives FromData,
      ToData

enum Redeemer derives FromData, ToData:
    case Deposit
    case Withdraw
    case Finalize
    case Cancel

enum State derives FromData, ToData:
    case Idle
    case Pending

/** A contract for keeping funds.
  *
  * Allows withdrawal when 2 conditions are met: a withdrawal request had been issued, and the
  * specified amount of time has elapsed since the request.
  *
  * The withdrawals are allowed only to the address specified in the Datum.
  *
  * Additionally, allows to cancel a withdrawal, and add funds to the vault.
  *
  * Withdrawal requires 2 actions: 1) Send a `Withdraw` request that contains the Datum-matching
  * verification key hash. 2) Send a `Finalize` request after a waiting period to confirm the
  * spending of funds.
  */
// todo: allow key-authorized withdrawals
@Compile
object Vault extends Validator {

    override def spend(
        d: prelude.Option[Data],
        redeemer: Data,
        tx: TxInfo,
        ownRef: TxOutRef
    ): Unit = {
        val datum = d.get.to[Datum]
        redeemer.to[Redeemer] match {
            case Deposit  => deposit(tx, ownRef, datum)
            case Withdraw => withdraw(tx, ownRef, datum)
            case Finalize => finalize(tx, ownRef, datum)
            case Cancel   => cancel(tx, ownRef, datum)
        }
    }

    private def deposit(tx: TxInfo, ownRef: TxOutRef, datum: Datum) = {
        val ownInput = tx.findOwnInput(ownRef).getOrFail(OwnInputNotFound)

        val scriptOutputs = tx.outputs.filter(out =>
            out.address.credential === ownInput.resolved.address.credential
        )

        require(scriptOutputs.size == BigInt(1), NotExactlyOneVaultOutput)

        val out = scriptOutputs.head
        requireSameOwner(out, datum)
        requireOutputToOwnAddress(ownInput, out, WrongDepositDestination)

        val value = out.value
        require(value.withoutLovelace.isZero, CannotAddTokens)
        require(value.getLovelace > ownInput.resolved.value.getLovelace, AdaNotConserved)
        requireEntireVaultIsSpent(datum, ownInput.resolved)
        out.datum match {
            case api.v2.OutputDatum.OutputDatum(d) =>
                val newDatum = d.to[Datum]
                require(newDatum.amount == value.getLovelace, VaultAmountChanged)
                require(newDatum.waitTime == datum.waitTime, WaitTimeChanged)
                require(
                  newDatum.finalizationDeadline == datum.finalizationDeadline,
                  FinalizationDeadlineChanged
                )
            case _ =>
                fail(NoDatumProvided)
        }
    }

    private def withdraw(tx: TxInfo, ownRef: TxOutRef, datum: Datum) = {
        require(
          datum.state.isIdle,
          WithdrawalAlreadyPending
        )
        val ownInput = tx.findOwnInput(ownRef).getOrFail(OwnInputNotFound)

        val scriptOutputs = tx.outputs.filter(out =>
            out.address.credential === ownInput.resolved.address.credential
        )

        require(scriptOutputs.size == BigInt(1), NotExactlyOneVaultOutput)

        val out = scriptOutputs.head
        requireSameOwner(out, datum)
        requireOutputToOwnAddress(
          ownInput,
          out,
          NotExactlyOneVaultOutput
        )

        val requestTime = tx.validRange.getEarliestTime
        val finalizationDeadline = requestTime + datum.waitTime

        out.datum match {
            case OutputDatum.OutputDatum(d) =>
                val newDatum = d.to[Datum]
                require(newDatum.state.isPending, MustBePending)
                require(
                  newDatum.finalizationDeadline == finalizationDeadline,
                  IncorrectDatumFinalization
                )
            case _ => fail(MustBePending)
        }
    }

    private def finalize(tx: TxInfo, ownRef: TxOutRef, datum: Datum) = {
        val ownInput = tx.findOwnInput(ownRef).getOrFail(OwnInputNotFound)

        // We can only finalize pending vaults.
        ownInput.resolved.datum match {
            case OutputDatum.OutputDatum(datum) =>
                require(datum.to[Datum].state.isPending, ContractMustBePending)
            case _ => fail(ContractMustBePending)
        }

        require(tx.validRange.isAfter(datum.finalizationDeadline), DeadlineNotPassed)

        val scriptOutputs = tx.outputs.filter(out =>
            out.address.credential === ownInput.resolved.address.credential
        )

        // Finalize should close the vault - no outputs back to the script
        require(scriptOutputs.size == BigInt(0), WithdrawalsMustNotSendBackToVault)

        // Ensure there's at least one output to the owner address
        val ownerOutputs =
            tx.outputs.filter(out => addressEquals(out.address.credential, datum.owner))
        require(ownerOutputs.size > BigInt(0), WrongAddressWithdrawal)
    }

    private def cancel(tx: TxInfo, ownRef: TxOutRef, datum: Datum) = {
        val ownInput = tx.findOwnInput(ownRef).getOrFail("Cannot find own input")

        // Filter outputs to only those going to the script address
        val scriptOutputs = tx.outputs.filter(out =>
            out.address.credential === ownInput.resolved.address.credential
        )

        require(scriptOutputs.size == BigInt(1), NotExactlyOneVaultOutput)

        val out = scriptOutputs.head
        requireSameOwner(out, datum)
        out.datum match {
            case ledger.api.v2.OutputDatum.OutputDatum(d) =>
                val newDatum = d.to[Datum]
                require(newDatum.amount == datum.amount, VaultAmountChanged)
                require(
                  out.value.getLovelace == datum.amount,
                  WrongOutputAmount
                )
                require(newDatum.state.isIdle, StateNotIdle)
                require(newDatum.waitTime == datum.waitTime, WaitTimeChanged)
            case _ => fail(NoDatumProvided)
        }
    }

    private def requireEntireVaultIsSpent(datum: Datum, output: TxOut) = {
        val amountToSpend = datum.amount
        val adaSpent = output.value.getLovelace
        require(amountToSpend == adaSpent, AdaLeftover)
    }

    private def requireOutputToOwnAddress(ownInput: TxInInfo, out: TxOut, message: String) =
        require(out.address.credential === ownInput.resolved.address.credential, message)

    private def requireSameOwner(out: TxOut, datum: Datum) =
        out.datum match {
            case scalus.ledger.api.v2.OutputDatum.OutputDatum(newDatum) =>
                require(
                  newDatum.to[Datum].owner == datum.owner,
                  VaultOwnerChanged
                )
            case _ => fail(NoInlineDatum)
        }

    private val NoDatumExists = "Contract has no datum"

    private val NoDatumProvided = "Vault transactions must have an inline datum"

    private val FinalizationDeadlineChanged =
        "Deposit transactions must not change the finalization deadline"

    private val VaultAmountChanged = "Datum amount must match output lovelace amount"

    private val CannotAddTokens = "Deposits must only contain ADA"

    private val AdaNotConserved = "Deposits must add ADA to the vault"

    private val WrongDepositDestination = "Deposit transactions can only be made to the vault"

    private val NotExactlyOneVaultOutput =
        "Vault transaction must have exactly 1 output to the vault script"

    private val OwnInputNotFound = "Own input not found"

    private val IncorrectDatumFinalization =
        "Finalization deadline must be request time plus wait time"

    private val MustBePending = "Output must have datum with State = Pending"

    private val WithdrawalAlreadyPending = "Cannot withdraw, another withdrawal request is pending"

    private val WrongAddressWithdrawal =
        "Withdrawal finalization must send funds to the vault owner"

    private val WithdrawalsMustNotSendBackToVault =
        "Withdrawal finalization must not send funds back to the vault"

    private val DeadlineNotPassed = "Finalization can only happen after the finalization deadline"

    private val ContractMustBePending = "Contract must be Pending"

    private val WrongOutputAmount = "Cancel transactions must not change the vault amount"

    private val WaitTimeChanged = "Wait time must remain the same"

    private val StateNotIdle = "Idle transactions must change the vault state to Idle"

    private val NoInlineDatum = "Vault transactions must have an inline datum"

    private val VaultOwnerChanged = "Vault transactions cannot change the vault owner"

    private val AdaLeftover = "Must spend entire vault"

    private def addressEquals(left: Credential, right: ByteString) = {
        left match {
            case v1.Credential.PubKeyCredential(hash) => hash.hash === right
            case v1.Credential.ScriptCredential(hash) => hash === right
        }
    }

    extension (s: State) {
        def isPending: Boolean = s match {
            case State.Idle    => false
            case State.Pending => true
        }

        def isIdle: Boolean = s match {
            case State.Idle    => true
            case State.Pending => false
        }
    }

    extension (i: Interval) {
        def getEarliestTime: PosixTime = i.from.boundType match {
            case IntervalBoundType.Finite(t) => t
            case _                           => BigInt(0)
        }

        def isAfter(timePoint: PosixTime): Boolean = i.from.boundType match {
            case IntervalBoundType.Finite(time) => timePoint < time
            case _                              => false
        }
    }
}

object VaultContract:
    inline def compiled(using scalus.Compiler.Options) = scalus.Compiler.compile(Vault.validate)

    inline given scalus.Compiler.Options = scalus.Compiler.Options(
      targetLoweringBackend = scalus.Compiler.TargetLoweringBackend.SirToUplcV3Lowering,
      generateErrorTraces = true,
      optimizeUplc = false
    )

    lazy val script: Program = createScript

    private def createScript: Program = {
        val sir = compiled
        sir.toUplc(
          generateErrorTraces = true,
          backend = scalus.Compiler.TargetLoweringBackend.SirToUplcV3Lowering
        ).plutusV3
    }
