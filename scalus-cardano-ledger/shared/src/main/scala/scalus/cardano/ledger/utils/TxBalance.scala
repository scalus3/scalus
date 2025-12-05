package scalus.cardano.ledger
package utils

import scalus.cardano.ledger.TransactionException.BadInputsUTxOException

import scala.util.boundary
import scala.util.boundary.break

object TxBalance {

    def consumed(
        tx: Transaction,
        certState: CertState,
        utxos: Utxos,
        protocolParams: ProtocolParams
    ): Either[BadInputsUTxOException, Value] = {
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
        val consumedValue = inputs + Value(withdrawals + refunds)

        val getConsumedMaryValue = consumedValue + minted
        val conwayConsumed = getConsumedMaryValue
        Right(conwayConsumed)
    }

    def produced(tx: Transaction, protocolParams: ProtocolParams): Value = {
        val fee = producedFee(tx)
        val outputs = producedOutputs(tx)
        val burned = producedMint(tx)
        val conwayTotalDepositsTxCerts = this.conwayTotalDepositsTxCerts(tx, protocolParams)
        val donation = producedDonation(tx)

        val getTotalDepositsTxBody = conwayTotalDepositsTxCerts
        val shelleyProducedValue = outputs + Value(fee + getTotalDepositsTxBody)
        val getProducedMaryValue = shelleyProducedValue + burned
        val conwayProducedValue = getProducedMaryValue + donation
        val getProducedValue = conwayProducedValue
        getProducedValue
    }

    private def consumedInputs(
        tx: Transaction,
        utxo: Utxos
    ): Either[BadInputsUTxOException, Value] = boundary {
        val inputs = tx.body.value.inputs.toSet.view
            .map { input =>
                utxo.get(input) match {
                    case Some(output) => output.value
                    // This check allows to be an order independent in the sequence of validation rules
                    case None => break(Left(TransactionException.BadInputsUTxOException(tx.id)))
                }
            }
            .foldLeft(Value.zero)(_ + _)

        Right(inputs)
    }

    private def consumedMint(
        tx: Transaction
    ): Value = {
        val mint = tx.body.value.mint.getOrElse(MultiAsset.empty)

        val mintedAssets = MultiAsset(
          mint.assets.flatMap { case (policy, assets) =>
              val mints = assets.filter((_, value) => value > 0)
              if mints.isEmpty then None else Some(policy -> mints)
          }
        )

        Value(Coin.zero, mintedAssets)
    }

    private def consumedWithdrawals(
        tx: Transaction
    ): Coin = {
        tx.body.value.withdrawals
            .map { _.withdrawals.values.foldLeft(Coin.zero)(_ + _) }
            .getOrElse(Coin.zero)
    }

    // Compute the key deregistration refunds in a transaction
    private def conwayTotalRefundsTxCerts(
        tx: Transaction,
        certState: CertState,
        protocolParams: ProtocolParams
    ): Coin = {
        val certificates = tx.body.value.certificates.toSeq

        def lookupStakingDeposit(cred: Credential): Option[Coin] = {
            certState.dstate.deposits.get(cred)
        }

        def lookupDRepDeposit(cred: Credential): Option[Coin] = {
            certState.vstate.dreps.get(cred).map(_.deposit)
        }

        Certificate.shelleyTotalRefundsTxCerts(
          lookupStakingDeposit,
          protocolParams,
          certificates
        ) + Certificate
            .conwayDRepRefundsTxCerts(
              lookupDRepDeposit,
              certificates
            )
    }

    private def producedFee(
        tx: Transaction
    ): Coin = {
        tx.body.value.fee
    }

    private def producedOutputs(
        tx: Transaction
    ): Value = {
        tx.body.value.outputs
            .map(_.value.value)
            .foldLeft(Value.zero)(_ + _)
    }

    private def producedMint(
        tx: Transaction
    ): Value = {
        val mint = tx.body.value.mint.getOrElse(MultiAsset.empty)

        val burnedAssets = MultiAsset(
          mint.assets.flatMap { case (policy, assets) =>
              // In TxBalance.produced, the burned tokens are added with a positive sign.
              // So we reverse the negative sign that they have in txBody.mint.
              val burns = assets.collect { case name -> value if value < 0 => name -> -value }
              if burns.isEmpty then None else Some(policy -> burns)
          }
        )

        Value(Coin.zero, burnedAssets)
    }

    private def conwayTotalDepositsTxCerts(
        tx: Transaction,
        protocolParams: ProtocolParams
    ): Coin = {
        // Calculate total deposits for Shelley-era certificates (stake pool and delegation)
        val certificates = tx.body.value.certificates.toSeq

        val shelleyTotalDepositsTxCerts: Coin = Certificate.shelleyTotalDeposits(
          protocolParams,
          certificates
        )

        // Calculate total deposits for Conway-era DRep certificates
        val conwayDRepDepositsTxCerts: Coin = Certificate.conwayDRepDeposits(
          protocolParams,
          certificates
        )

        shelleyTotalDepositsTxCerts + conwayDRepDepositsTxCerts
    }

    private def producedDonation(
        tx: Transaction
    ): Value = {
        Value(tx.body.value.donation.getOrElse(Coin.zero))
    }
}
