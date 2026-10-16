package scalus.cardano.ledger
package utils

object CollateralSufficient {

    /** Checks if the actual collateral amount is sufficient according to the protocol's collateral
      * percentage requirement.
      *
      * The collateral is considered sufficient if: actualCollateral * 100 >= fee *
      * collateralPercentage
      *
      * @param actualTotalSumOfCollateralCoins
      *   the actual sum of all collateral input coins
      * @param transactionFee
      *   the transaction fee
      * @param collateralPercentage
      *   the collateral percentage from protocol parameters (e.g., 150 means 150%)
      * @return
      *   true if the collateral is sufficient, false otherwise
      */
    def check(
        actualTotalSumOfCollateralCoins: Coin,
        transactionFee: Coin,
        collateralPercentage: Long,
    ): Boolean =
        check(BigInt(actualTotalSumOfCollateralCoins.value), transactionFee, collateralPercentage)

    /** [[check]] for an exactly summed collateral. Both products are taken without a bound, as the
      * ledger's `Integer` takes them: a `Long` product wraps for a large fee or collateral.
      */
    private[ledger] def check(
        actualTotalSumOfCollateralCoins: BigInt,
        transactionFee: Coin,
        collateralPercentage: Long
    ): Boolean =
        actualTotalSumOfCollateralCoins * 100 >= BigInt(transactionFee.value) * collateralPercentage

    /** Calculates the required collateral amount based on the transaction fee and collateral
      * percentage from protocol parameters.
      *
      * @param fee
      *   the transaction fee
      * @param collateralPercentage
      *   the collateral percentage from protocol parameters
      * @return
      *   the required collateral amount
      */
    def calculateRequiredCollateral(fee: Coin, collateralPercentage: Long): Coin = {
        val feeValue = BigDecimal(fee.value)
        val percentage = BigDecimal(collateralPercentage)
        val result = (feeValue * percentage / 100).setScale(0, BigDecimal.RoundingMode.UP)
        // `toLong` would wrap an amount past `Long.MaxValue`
        Coin.exact(result.toBigInt)
    }
}
