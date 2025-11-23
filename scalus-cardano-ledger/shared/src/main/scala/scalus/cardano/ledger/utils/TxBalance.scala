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
        consumedFromFields(
          txBody.inputs.toSet,
          txBody.mint,
          txBody.withdrawals,
          txBody.certificates.toSeq,
          utxo,
          certState,
          protocolParams,
          Some(tx.id)
        )
    }

    def produced(tx: Transaction): Value = {
        val txBody = tx.body.value
        producedFromFields(
          txBody.outputs.map(_.value),
          txBody.mint,
          txBody.fee,
          txBody.certificates.toSeq,
          txBody.donation
        )
    }

    /** Calculate consumed value from transaction fields. This is useful when you don't have a full
      * Transaction yet but need to calculate what would be consumed.
      *
      * @param inputs
      *   Set of transaction inputs
      * @param mint
      *   Minting policy (positive amounts are consumed)
      * @param withdrawals
      *   Stake reward withdrawals
      * @param certificates
      *   Certificates (for deregistration refunds)
      * @param utxo
      *   UTXO map to lookup input values
      * @param certState
      *   Certificate state for looking up deposits
      * @param protocolParams
      *   Protocol parameters
      * @param txIdOpt
      *   Optional transaction ID for error reporting
      * @return
      *   Either error or consumed value
      */
    def consumedFromFields(
        inputs: Set[TransactionInput],
        mint: Option[MultiAsset],
        withdrawals: Option[Withdrawals],
        certificates: Seq[Certificate],
        utxo: Utxos,
        certState: CertState,
        protocolParams: ProtocolParams,
        txIdOpt: Option[TransactionHash] = None
    ): Either[BadInputsUTxOException, Value] = boundary {
        val mintAssets = mint.getOrElse(MultiAsset.empty)
        val inputValues = inputs.view
            .map { input =>
                utxo.get(input) match {
                    case Some(output) => output.value
                    case None =>
                        break(
                          Left(
                            TransactionException.BadInputsUTxOException(
                              txIdOpt.getOrElse(
                                TransactionHash.fromByteString(
                                  scalus.builtin.ByteString.empty
                                )
                              )
                            )
                          )
                        )
                }
            }
            .foldLeft(Value.zero)(_ + _)
        val withdrawalCoins =
            withdrawals
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
              certificates
            ) + Certificate
                .conwayDRepRefundsTxCerts(
                  lookupDRepDeposit,
                  certificates
                )
        val getTotalRefundsTxCerts = conwayTotalRefundsTxCerts
        // Compute the total refunds from the Certificates of a TransactionBody
        val getTotalRefundsTxBody = getTotalRefundsTxCerts
        val refunds = getTotalRefundsTxBody

        // balance (txins tx ◁ u) + wbalance (txwdrls tx) + keyRefunds pp tx
        val consumedValue = inputValues + Value(withdrawalCoins + refunds)
        val minted = Value(
          Coin.zero,
          MultiAsset(mintAssets.assets.flatMap { case (policy, assets) =>
              val mints = assets.filter((_, value) => value > 0)
              if mints.isEmpty then None else Some(policy -> mints)
          })
        )
        val getConsumedMaryValue = consumedValue + minted
        val conwayConsumed = getConsumedMaryValue
        Right(conwayConsumed)
    }

    /** Calculate produced value from transaction fields. This is useful when you don't have a full
      * Transaction yet but need to calculate what would be produced.
      *
      * @param outputs
      *   Transaction outputs
      * @param mint
      *   Minting policy (negative amounts are produced as burns)
      * @param fee
      *   Transaction fee
      * @param certificates
      *   Certificates (for deposits)
      * @param donation
      *   Optional donation amount
      * @return
      *   Total produced value
      */
    def producedFromFields(
        outputs: Seq[TransactionOutput],
        mint: Option[MultiAsset],
        fee: Coin,
        certificates: Seq[Certificate],
        donation: Option[Coin]
    ): Value = {
        val mintAssets = mint.getOrElse(MultiAsset.empty)
        val burned = Value(
          Coin.zero,
          MultiAsset(mintAssets.assets.flatMap { case (policy, assets) =>
              // In TxBalance.produced, the burned tokens are added with a positive sign.
              // So we reverse the negative sign that they have in mint.
              val burns = assets.collect { case name -> value if value < 0 => name -> -value }
              if burns.isEmpty then None else Some(policy -> burns)
          })
        )
        val outputValues = outputs
            .map(_.value)
            .foldLeft(Value.zero)(_ + _)
        val shelleyTotalDepositsTxCerts: Coin = Coin.zero // FIXME: implement
        val conwayDRepDepositsTxCerts: Coin = Coin.zero // FIXME: implement
        val conwayTotalDepositsTxCerts = shelleyTotalDepositsTxCerts + conwayDRepDepositsTxCerts
        val getTotalDepositsTxBody = conwayTotalDepositsTxCerts
        val shelleyProducedValue = outputValues + Value(fee + getTotalDepositsTxBody)
        val getProducedMaryValue = shelleyProducedValue + burned
        val conwayProducedValue =
            getProducedMaryValue + Value(donation.getOrElse(Coin.zero))
        val getProducedValue = conwayProducedValue
        getProducedValue
    }

}
