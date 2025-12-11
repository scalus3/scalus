package scalus.cardano.ledger
package rules

import scalus.cardano.ledger.utils.{CollateralSufficient, MinTransactionFee}
import scala.util.boundary
import scala.util.boundary.break

// It's Babbage.FeesOK in cardano-ledger
//feesOK is a predicate with several parts. Some parts only apply in special circumstances.
//--   1) The fee paid is >= the minimum fee
//--   2) If the total ExUnits are 0 in both Memory and Steps, no further part needs to be checked.
//--   3) The collateral consists only of VKey addresses
//--   4) The collateral inputs do not contain any non-ADA part
//--   5) The collateral is sufficient to cover the appropriate percentage of the
//--      fee marked in the transaction
//--   6) The collateral is equivalent to total collateral asserted by the transaction
//--   7) There is at least one collateral input
object FeesOkValidator extends STS.Validator {
    override final type Error = TransactionException.BadInputsUTxOException |
        TransactionException.BadCollateralInputsUTxOException |
        TransactionException.BadReferenceInputsUTxOException | TransactionException.FeesOkException

    override def validate(context: Context, state: State, event: Event): Result = {
        val transactionId = event.id
        val body = event.body.value
        val utxos = state.utxos
        val transactionFee = body.fee
        val protocolParams = context.env.params
        val collateralPercentage = protocolParams.collateralPercentage

        val collateralInputs = body.collateralInputs.toSet
        val expectedTotalSumOfCollateralCoins = body.totalCollateral
        val collateralReturnOutput = body.collateralReturnOutput.map(_.value)
        val Value(collateralReturnOutputCoin, collateralReturnOutputAssets) =
            collateralReturnOutput.map(_.value).getOrElse(Value.zero)

        val collateralUtxos = extractCollateralUtxos(transactionId, collateralInputs, utxos) match
            case Right(utxos) => utxos
            case Left(error)  => return failure(error)

        val Value(totalSumOfCollateralCoins, totalSumOfCollateralAssets) =
            calculateTotalSumOfCollateralValues(collateralUtxos)

        val actualTotalSumOfCollateralCoins =
            totalSumOfCollateralCoins - collateralReturnOutputCoin

        val minRequiredFee = MinTransactionFee.computeMinFee(event, utxos, protocolParams) match
            case Right(fee)  => fee
            case Left(error) => return failure(error)

        // 1) The fee paid is >= the minimum fee
        val isTransactionFeeGreaterOrEqualThanMinRequiredFee = transactionFee >= minRequiredFee

        // 2) If the total ExUnits are 0 in both Memory and Steps, no further part needs to be checked.
        // The total ExUnits are zero if there are no redeemers in the witness set
        // or if all redeemers have ExUnits.zero.
        // This is a simplified check, as in original haskell cardano code and specification.
        val areTotalExUnitsZero = event.witnessSet.redeemers.isEmpty
        if areTotalExUnitsZero then
            return if isTransactionFeeGreaterOrEqualThanMinRequiredFee then success
            else
                failure(
                  TransactionException.FeesOkException(
                    transactionId,
                    transactionFee,
                    minRequiredFee,
                    collateralPercentage,
                    areTotalExUnitsZero,
                    collateralReturnOutput,
                    actualTotalSumOfCollateralCoins,
                    expectedTotalSumOfCollateralCoins
                  )
                )

        // 3) The collateral consists only of VKey addresses
        val collateralsConsistNotVKeyAddress =
            findCollateralsConsistNotVKeyAddress(collateralUtxos)

        // 4) The collateral inputs do not contain any non-ADA part
        // Collateral inputs can contain multi-assets, as long all of them are returned to the
        // `collateralReturnTxBodyL`. This design decision was also intentional, in order to
        // simplify utxos selection for collateral.
        val remainingAssets = totalSumOfCollateralAssets - collateralReturnOutputAssets
        val collateralsContainNotOnlyADA =
            findCollateralsContainNotOnlyADA(collateralUtxos)

        // 5) The collateral is sufficient to cover the appropriate percentage of the fee marked in the transaction
        val isCollateralSufficient = CollateralSufficient.check(
          actualTotalSumOfCollateralCoins,
          transactionFee,
          collateralPercentage
        )

        // 6) The collateral is equivalent to total collateral asserted by the transaction
        val isCollateralEqualToExpected =
            expectedTotalSumOfCollateralCoins.forall {
                actualTotalSumOfCollateralCoins == _
            } // True when expectedTotalSumOfCollateralCoins is None

        // 7) There is at least one collateral input
        val hasCollateralInputs = collateralInputs.nonEmpty

        if !isTransactionFeeGreaterOrEqualThanMinRequiredFee ||
            collateralsConsistNotVKeyAddress.nonEmpty ||
            remainingAssets.nonEmpty ||
            !isCollateralSufficient ||
            !isCollateralEqualToExpected ||
            !hasCollateralInputs
        then
            failure(
              TransactionException.FeesOkException(
                transactionId,
                transactionFee,
                minRequiredFee,
                collateralPercentage,
                areTotalExUnitsZero,
                collateralReturnOutput,
                actualTotalSumOfCollateralCoins,
                expectedTotalSumOfCollateralCoins,
                collateralsConsistNotVKeyAddress,
                collateralsContainNotOnlyADA,
                remainingAssets,
                hasCollateralInputs
              )
            )
        else success
    }

    private def extractCollateralUtxos(
        transactionId: TransactionHash,
        collateralInputs: Set[TransactionInput],
        utxos: Utxos
    ): Either[TransactionException.BadCollateralInputsUTxOException, Utxos] = boundary {
        val collateralUtxos = collateralInputs.view.map { input =>
            utxos.get(input) match {
                case Some(output) => input -> output
                // This check allows to be an order independent in the sequence of validation rules
                case None =>
                    break(
                      Left(TransactionException.BadCollateralInputsUTxOException(transactionId))
                    )
            }
        }.toMap

        Right(collateralUtxos)
    }

    private def calculateTotalSumOfCollateralValues(
        collateralUtxos: Utxos
    ): scalus.cardano.ledger.Value = {
        collateralUtxos.foldLeft(Value.zero) { case (acc, (_, collateralOutput)) =>
            acc + collateralOutput.value
        }
    }

    private def findCollateralsConsistNotVKeyAddress(
        collateralUtxos: Utxos
    ): Utxos = collateralUtxos.filter { _._2.address.keyHashOption.isEmpty }

    private def findCollateralsContainNotOnlyADA(
        collateralUtxos: Utxos
    ): Utxos = collateralUtxos.filter { _._2.value.nonEmptyAssets }
}
