package scalus.cardano.ledger
package rules

import scalus.cardano.ledger.utils.MinTransactionFee
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
        TransactionException.BadReferenceInputsUTxOException |
        TransactionException.FeeTooSmallException |
        TransactionException.CollateralsConsistNotOnlyVKeyAddressException |
        TransactionException.CollateralsContainNotOnlyADAException |
        TransactionException.InsufficientTotalSumOfCollateralCoinsException |
        TransactionException.IncorrectTotalCollateralException |
        TransactionException.NoCollateralInputsException

    override def validate(context: Context, state: State, event: Event): Result = boundary {
        val transactionId = event.id
        val collateralInputs = event.body.value.collateralInputs
        val collateralReturnOutput = event.body.value.collateralReturnOutput.map(_.value)
        val utxo = state.utxos

        for
            _ <- feePaidIsGreeterOrEqualThanMinimumFee(context, state, event)
            _ <-
                if totalExUnitsAreZero(event) then success
                else
                    val (
                      notVKeyAddressCollaterals,
                      notOnlyADACollaterals,
                      totalSumOfCollaterals
                    ) = collateralInputs.toSortedSet.view
                        .map { collateralInput =>
                            utxo.get(collateralInput) match
                                case Some(collateralOutput) => collateralInput -> collateralOutput
                                // This check allows to be an order independent in the sequence of validation rules
                                case None =>
                                    break(
                                      failure(
                                        TransactionException.BadCollateralInputsUTxOException(
                                          transactionId
                                        )
                                      )
                                    )
                        }
                        .foldLeft(
                          (
                            Set.empty[(TransactionInput, TransactionOutput)],
                            Set.empty[(TransactionInput, TransactionOutput)],
                            Value.zero
                          )
                        ) { (acc, collateralData) =>
                            val (_, collateralOutput) = collateralData

                            val (
                              notVKeyAddressCollaterals,
                              notOnlyADACollaterals,
                              totalSumOfCollaterals
                            ) = acc

                            val newNotVKeyAddressCollaterals =
                                if isCollateralConsistsOnlyVKeyAddress(collateralOutput) then
                                    notVKeyAddressCollaterals
                                else notVKeyAddressCollaterals + collateralData

                            val newNotOnlyADACollaterals =
                                if isCollateralContainsOnlyADA(collateralOutput) then
                                    notOnlyADACollaterals
                                else notOnlyADACollaterals + collateralData

                            val newTotalSumOfCollaterals =
                                totalSumOfCollaterals + collateralOutput.value

                            (
                              newNotVKeyAddressCollaterals,
                              newNotOnlyADACollaterals,
                              newTotalSumOfCollaterals
                            )
                        }

                    if notVKeyAddressCollaterals.nonEmpty then
                        break(
                          failure(
                            TransactionException.CollateralsConsistNotOnlyVKeyAddressException(
                              transactionId,
                              notVKeyAddressCollaterals
                            )
                          )
                        )

                    // Collateral inputs can contain multi-assets, as long all of them are returned to the
                    // `collateralReturnTxBodyL`. This design decision was also intentional, in order to
                    // simplify utxo selection for collateral.
                    val collateralReturnOutputValue = collateralReturnOutput
                        .map(_.value)
                        .getOrElse(Value.zero)
                    if !(
                          totalSumOfCollaterals.assets.isEmpty && collateralReturnOutputValue.assets.isEmpty ||
                              (totalSumOfCollaterals.assets - collateralReturnOutputValue.assets).isEmpty
                        )
                    then
                        break(
                          failure(
                            TransactionException.CollateralsContainNotOnlyADAException(
                              transactionId,
                              notOnlyADACollaterals,
                              collateralReturnOutput
                            )
                          )
                        )

                    for
                        _ <- totalSumOfCollateralCoinsIsSufficient(
                          context,
                          event,
                          totalSumOfCollaterals.coin
                        )
                        _ <- totalSumOfCollateralCoinsIsEquivalentToTotalCollateral(
                          event,
                          totalSumOfCollaterals.coin
                        )
                        _ <- isAtLeastOneCollateralInput(event)
                    yield ()
        yield ()
    }

    //
    private def feePaidIsGreeterOrEqualThanMinimumFee(
        context: Context,
        state: State,
        event: Event
    ): Result = {
        val transactionId = event.id
        val transactionFee = event.body.value.fee
        val utxo = state.utxos
        val protocolParams = context.env.params

        for
            minTransactionFee <- MinTransactionFee(event, utxo, protocolParams)
            _ <-
                if transactionFee < minTransactionFee then
                    failure(
                      TransactionException.FeeTooSmallException(
                        transactionId,
                        transactionFee,
                        minTransactionFee
                      )
                    )
                else success
        yield ()
    }

    private def totalExUnitsAreZero(event: Event): Boolean = {
        // The total ExUnits are zero if there are no redeemers in the witness set
        // or if all redeemers have ExUnits.zero.
        // This is a simplified check, as in original haskell cardano code and specification.
//        calculateTotalExUnits(event: Event) == ExUnits.zero
        event.witnessSet.redeemers.isEmpty
    }

    private def isCollateralConsistsOnlyVKeyAddress(
        collateralOutput: TransactionOutput
    ): Boolean = collateralOutput.address.keyHashOption.nonEmpty

    private def isCollateralContainsOnlyADA(
        collateralOutput: TransactionOutput
    ): Boolean = collateralOutput.value.assets.isEmpty

    private def totalSumOfCollateralCoinsIsSufficient(
        context: Context,
        event: Event,
        totalSumOfCollateralCoins: Coin
    ): Result = {
        val transactionId = event.id
        val transactionFee = event.body.value.fee
        val collateralReturnOutput = event.body.value.collateralReturnOutput
        val collateralPercentage = context.env.params.collateralPercentage

        val deltaCoins = collateralReturnOutput match
            case Some(collateralReturnOutput) =>
                totalSumOfCollateralCoins - collateralReturnOutput.value.value.coin
            case None => totalSumOfCollateralCoins

        if (deltaCoins.value * 100) < transactionFee.value * collateralPercentage then
            failure(
              TransactionException.InsufficientTotalSumOfCollateralCoinsException(
                transactionId,
                totalSumOfCollateralCoins,
                collateralReturnOutput.map(_.value),
                transactionFee,
                collateralPercentage
              )
            )
        else success
    }

    private def totalSumOfCollateralCoinsIsEquivalentToTotalCollateral(
        event: Event,
        totalSumOfCollateralCoins: Coin
    ): Result = {
        val transactionId = event.id
        val totalCollateral = event.body.value.totalCollateral

        totalCollateral match
            case None => success
            case Some(collateral) =>
                if collateral != totalSumOfCollateralCoins then
                    failure(
                      TransactionException.IncorrectTotalCollateralException(
                        transactionId,
                        totalSumOfCollateralCoins,
                        totalCollateral
                      )
                    )
                else success
    }

    private def isAtLeastOneCollateralInput(
        event: Event
    ): Result = {
        if event.body.value.collateralInputs.toSortedSet.isEmpty then
            failure(
              TransactionException.NoCollateralInputsException(event.id)
            )
        else success
    }
}
