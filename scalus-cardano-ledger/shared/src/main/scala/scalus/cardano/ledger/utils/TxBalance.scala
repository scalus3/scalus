package scalus.cardano.ledger.utils
import scalus.cardano.ledger.*
import scalus.cardano.ledger.TransactionException.BadInputsUTxOException

import scala.util.boundary
import scala.util.boundary.break

object TxBalance {

    def consumed(
        tx: Transaction,
        certState: CertState,
        utxo: Utxos,
        protocolParams: ProtocolParams
    ): Either[BadInputsUTxOException, Value] = boundary {
        val txBody = tx.body.value
        val mint = txBody.mint.getOrElse(MultiAsset.empty)
        val inputs = txBody.inputs.toSet.view
            .map { input =>
                utxo.get(input) match {
                    case Some(output) => output.value
                    case None => break(Left(TransactionException.BadInputsUTxOException(tx.id)))
                }
            }
            .foldLeft(Value.zero)(_ + _)
        val withdrawals =
            txBody.withdrawals
                .map { withdrawals =>
                    withdrawals.withdrawals.values.foldLeft(Coin.zero)(_ + _)
                }
                .getOrElse(Coin.zero)

        def lookupStakingDeposit(cred: Credential): Option[Coin] = {
            certState.dstate.deposits.get(cred)
        }

        def lookupDRepDeposit(cred: Credential): Option[Coin] = {
            certState.vstate.dreps.get(cred).map(_.deposit)
        }

        // Compute the key deregistration refunds in a transaction
        val conwayTotalRefundsTxCerts =
            Certificate.shelleyTotalRefundsTxCerts(
              lookupStakingDeposit,
              protocolParams,
              txBody.certificates.toSeq
            ) + Certificate
                .conwayDRepRefundsTxCerts(
                  lookupDRepDeposit,
                  txBody.certificates.toSeq
                )
        val getTotalRefundsTxCerts = conwayTotalRefundsTxCerts
        // Compute the total refunds from the Certificates of a TransactionBody
        val getTotalRefundsTxBody = getTotalRefundsTxCerts
        val refunds = getTotalRefundsTxBody

        // balance (txins tx ◁ u) + wbalance (txwdrls tx) + keyRefunds pp tx
        val consumedValue = inputs + Value(withdrawals + refunds)
        val minted = Value(
          Coin.zero,
          MultiAsset(mint.assets.flatMap { case (policy, assets) =>
              val mints = assets.filter((_, value) => value > 0)
              if mints.isEmpty then None else Some(policy -> mints)
          })
        )
        val getConsumedMaryValue = consumedValue + minted
        val conwayConsumed = getConsumedMaryValue
        Right(conwayConsumed)
    }

    def produced(tx: Transaction): Value = {
        val txBody = tx.body.value
        val mint = txBody.mint.getOrElse(MultiAsset.empty)
        val burned = Value(
          Coin.zero,
          MultiAsset(mint.assets.flatMap { case (policy, assets) =>
              // In TxBalance.produced, the burned tokens are added with a positive sign.
              // So we reverse the negative sign that they have in txBody.mint.
              val burns = assets.collect { case name -> value if value < 0 => name -> -value }
              if burns.isEmpty then None else Some(policy -> burns)
          })
        )
        val outputs = txBody.outputs
            .map(_.value.value)
            .foldLeft(Value.zero)(_ + _)
        val shelleyTotalDepositsTxCerts: Coin = Coin.zero // FIXME: implement
        val conwayDRepDepositsTxCerts: Coin = Coin.zero // FIXME: implement
        val conwayTotalDepositsTxCerts = shelleyTotalDepositsTxCerts + conwayDRepDepositsTxCerts
        val getTotalDepositsTxBody = conwayTotalDepositsTxCerts
        val shelleyProducedValue = outputs + Value(txBody.fee + getTotalDepositsTxBody)
        val getProducedMaryValue = shelleyProducedValue + burned
        val conwayProducedValue =
            getProducedMaryValue + Value(txBody.donation.getOrElse(Coin.zero))
        val getProducedValue = conwayProducedValue
        getProducedValue
    }

}
