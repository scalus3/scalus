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
    ): Boolean = {
        (actualTotalSumOfCollateralCoins.value * 100) >= transactionFee.value * collateralPercentage
    }

    /** Calculates the required collateral amount based on the transaction fee and collateral
      * percentage from protocol parameters.
      *
      * The required collateral is calculated as: (fee * collateralPercentage) / 100
      *
      * @param fee
      *   the transaction fee
      * @param collateralPercentage
      *   the collateral percentage from protocol parameters
      * @return
      *   the required collateral amount
      */
    def calculateRequiredCollateral(fee: Coin, collateralPercentage: Long): Coin = {
        // TODO: precision handling
        Coin((fee.value * collateralPercentage) / 100)
    }
}
