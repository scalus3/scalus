package scalus.cardano.ledger
package utils

import scalus.cardano.ledger.TransactionException.BadInputsUTxOException

import scala.util.boundary
import scala.util.boundary.break

/** The value a transaction consumes and produces.
  *
  * The ledger sums both sides with Haskell's `Integer`, and so do `consumedExact` and
  * `producedExact`, which the value-conservation rule compares. A transaction sets amounts of up to
  * `Long.MaxValue` in its outputs, deposits and withdrawals, so a `Long` sum could wrap around and
  * make two different totals look equal. [[consumed]] and [[produced]] return the same totals as a
  * [[Value]] and throw `ArithmeticException` when one does not fit.
  */
object TxBalance {

    /** The value `tx` consumes, as Haskell `conwayConsumed`.
      *
      * @throws java.lang.ArithmeticException
      *   when an amount does not fit in a `Long`
      */
    def consumed(
        tx: Transaction,
        certState: CertState,
        utxos: Utxos,
        protocolParams: ProtocolParams
    ): Either[BadInputsUTxOException, Value] =
        consumedExact(tx, certState, utxos, protocolParams).map(_.toValue)

    /** [[consumed]] without a bound on the amounts. */
    private[cardano] def consumedExact(
        tx: Transaction,
        certState: CertState,
        utxos: Utxos,
        protocolParams: ProtocolParams
    ): Either[BadInputsUTxOException, ExactValue] = {
        val inputs = consumedInputs(tx, utxos) match
            case Right(value) => value
            case Left(err)    => return Left(err)

        val minted = consumedMint(tx)
        val withdrawals = consumedWithdrawals(tx)

        // Compute the key deregistration refunds in a transaction
        val conwayTotalRefundsTxCerts = this.conwayTotalRefundsTxCerts(
          tx,
          certState,
          protocolParams
        )

        // Total refunds from Certificates in a Transaction)
        val getTotalRefundsTxCerts = conwayTotalRefundsTxCerts
        // Compute the total refunds from the Certificates of a TransactionBody
        val getTotalRefundsTxBody = getTotalRefundsTxCerts
        val refunds = getTotalRefundsTxBody

        // balance (txins tx ◁ u) + wbalance (txwdrls tx) + keyRefunds pp tx
        val consumedValue = inputs + ExactValue.coin(withdrawals + refunds)

        val getConsumedMaryValue = consumedValue + minted
        val conwayConsumed = getConsumedMaryValue
        Right(conwayConsumed)
    }

    /** The value `tx` produces, with a pool deposit for every pool registration, as no pool is
      * registered. Use the overload with a [[CertState]] for a tx that may re-register a pool.
      *
      * @throws java.lang.ArithmeticException
      *   when an amount does not fit in a `Long`
      */
    def produced(tx: Transaction, protocolParams: ProtocolParams): Value =
        produced(tx, CertState.empty, protocolParams)

    /** The value `tx` produces, as Haskell `conwayProducedValue`. A pool registration pays the pool
      * deposit only for a pool not in `certState` and not registered earlier in the tx.
      *
      * @throws java.lang.ArithmeticException
      *   when an amount does not fit in a `Long`
      */
    def produced(tx: Transaction, certState: CertState, protocolParams: ProtocolParams): Value =
        producedExact(tx, certState, protocolParams).toValue

    /** [[produced]] without a bound on the amounts. */
    private[cardano] def producedExact(
        tx: Transaction,
        certState: CertState,
        protocolParams: ProtocolParams
    ): ExactValue = {
        val fee = producedFee(tx)
        val outputs = producedOutputs(tx)
        val burned = producedMint(tx)
        val conwayTotalDepositsTxCerts =
            this.conwayTotalDepositsTxCerts(
              tx,
              certState.pstate.stakePools.contains,
              protocolParams
            )
        val proposalDeposits = producedProposalDeposits(tx)
        val donation = producedDonation(tx)

        val getTotalDepositsTxBody = conwayTotalDepositsTxCerts + proposalDeposits
        val shelleyProducedValue = outputs + ExactValue.coin(fee + getTotalDepositsTxBody)
        val getProducedMaryValue = shelleyProducedValue + burned
        val conwayProducedValue = getProducedMaryValue + ExactValue.coin(donation)
        val getProducedValue = conwayProducedValue
        getProducedValue
    }

    private def consumedInputs(
        tx: Transaction,
        utxo: Utxos
    ): Either[BadInputsUTxOException, ExactValue] = boundary {
        val inputs = tx.body.value.inputs.toSet.view.map { input =>
            utxo.get(input) match {
                case Some(output) => output.value
                // This check allows to be an order independent in the sequence of validation rules
                case None => break(Left(TransactionException.BadInputsUTxOException(tx.id)))
            }
        }

        Right(ExactValue.sum(inputs))
    }

    private def consumedMint(
        tx: Transaction
    ): ExactValue = {
        val mint = tx.body.value.mint.getOrElse(MultiAsset.empty)
        ExactValue(Value(Coin.zero, mint.onlyPositive))
    }

    private def consumedWithdrawals(
        tx: Transaction
    ): BigInt = {
        tx.body.value.withdrawals
            .map { _.withdrawals.values.foldLeft(BigInt(0))(_ + _.value) }
            .getOrElse(BigInt(0))
    }

    // Compute the key deregistration refunds in a transaction
    private def conwayTotalRefundsTxCerts(
        tx: Transaction,
        certState: CertState,
        protocolParams: ProtocolParams
    ): BigInt = {
        val certificates = tx.body.value.certificates.toSeq

        def lookupStakingDeposit(cred: Credential): Option[Coin] = {
            certState.dstate.accounts.get(cred).map(_.deposit)
        }

        def lookupDRepDeposit(cred: Credential): Option[Coin] = {
            certState.vstate.dreps.get(cred).map(_.deposit)
        }

        Certificate.shelleyTotalRefundsTxCertsExact(
          lookupStakingDeposit,
          protocolParams,
          certificates
        ) + Certificate
            .conwayDRepRefundsTxCertsExact(
              lookupDRepDeposit,
              certificates
            )
    }

    private def producedFee(
        tx: Transaction
    ): BigInt = {
        BigInt(tx.body.value.fee.value)
    }

    private def producedOutputs(
        tx: Transaction
    ): ExactValue = {
        ExactValue.sum(tx.body.value.outputs.view.map(_.value.value))
    }

    private def producedMint(
        tx: Transaction
    ): ExactValue = {
        val mint = tx.body.value.mint.getOrElse(MultiAsset.empty)
        // In TxBalance.produced, the burned tokens are added with a positive sign.
        // So we reverse the negative sign that they have in txBody.mint, exactly:
        // negating a burn of 2^63 tokens does not fit in a `Long`.
        ExactValue.zero - ExactValue(Value(Coin.zero, mint.negativeAssets))
    }

    private def conwayTotalDepositsTxCerts(
        tx: Transaction,
        isRegisteredPool: PoolKeyHash => Boolean,
        protocolParams: ProtocolParams
    ): BigInt = {
        // Calculate total deposits for Shelley-era certificates (stake pool and delegation)
        val certificates = tx.body.value.certificates.toSeq

        val shelleyTotalDepositsTxCerts: BigInt = Certificate.shelleyTotalDepositsExact(
          protocolParams,
          isRegisteredPool,
          certificates
        )

        // Calculate total deposits for Conway-era DRep certificates
        val conwayDRepDepositsTxCerts: BigInt = Certificate.conwayDRepDepositsExact(
          protocolParams,
          certificates
        )

        shelleyTotalDepositsTxCerts + conwayDRepDepositsTxCerts
    }

    private def producedDonation(
        tx: Transaction
    ): BigInt = {
        BigInt(tx.body.value.donation.getOrElse(Coin.zero).value)
    }

    private def producedProposalDeposits(
        tx: Transaction
    ): BigInt = {
        tx.body.value.proposalProcedures.toSeq
            .map(_.deposit)
            .foldLeft(BigInt(0))(_ + _.value)
    }
}
