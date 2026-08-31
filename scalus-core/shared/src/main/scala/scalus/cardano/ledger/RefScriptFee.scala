package scalus.cardano.ledger

import scala.annotation.tailrec

/** Conway tiered reference-script fee (`tierRefScriptFee` in cardano-ledger).
  *
  * The first [[sizeIncrement]] bytes cost `costPerByte` lovelace each; each subsequent stride costs
  * [[multiplier]] times the previous stride's per-byte price. The floor of the exact rational total
  * is charged. On mainnet `costPerByte` is `ProtocolParams.minFeeRefScriptCostPerByte` (15 as of
  * epoch 645).
  *
  * Callable from Java/JS: `RefScriptFee.fee(1200, 15L)`.
  */
object RefScriptFee {

    /** Per-stride price growth factor, 1.2. */
    val multiplier: NonNegativeInterval = NonNegativeInterval(12, 10)

    /** Tier stride in bytes, 25,600. */
    val sizeIncrement: Int = 25600

    /** Fee for `sizeInBytes` total reference-script bytes at base price `costPerByte`. */
    def fee(sizeInBytes: Int, costPerByte: Long): Coin = {
        require(sizeInBytes >= 0, s"negative reference-script size: $sizeInBytes")
        @tailrec
        def go(acc: NonNegativeInterval, tierPrice: NonNegativeInterval, n: Int): Coin =
            if n < sizeIncrement then Coin((acc + tierPrice * n).floor)
            else go(acc + tierPrice * sizeIncrement, multiplier * tierPrice, n - sizeIncrement)
        go(NonNegativeInterval.zero, NonNegativeInterval(costPerByte), sizeInBytes)
    }

    /** Fee at the protocol parameters' `minFeeRefScriptCostPerByte`. */
    def fee(sizeInBytes: Int, params: ProtocolParams): Coin =
        fee(sizeInBytes, params.minFeeRefScriptCostPerByte)
}
