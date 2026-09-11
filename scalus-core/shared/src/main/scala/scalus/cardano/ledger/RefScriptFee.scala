package scalus.cardano.ledger

import scala.annotation.{static, tailrec}

/** Reference-script fees at the supplied protocol parameters' base price. */
final class RefScriptFee(params: ProtocolParams) {

    /** Use the protocol parameters from the current Cardano environment. */
    def this()(using info: CardanoInfo) = this(info.protocolParams)

    // A distinct name lets the companion expose fee as a JVM static method.
    def calculate(sizeInBytes: Int): Coin =
        RefScriptFee.fee(sizeInBytes, params.minFeeRefScriptCostPerByte)
}

/** Conway tiered reference-script fee (`tierRefScriptFee` in cardano-ledger).
  *
  * The first 25,600 bytes cost `costPerByte` lovelace each; each subsequent stride costs 1.2 times
  * the previous stride's per-byte price. The floor of the exact rational total is charged.
  */
object RefScriptFee {

    /** Per-stride price growth factor, 1.2. */
    private val multiplier: NonNegativeInterval = NonNegativeInterval(12, 10)

    /** Tier stride in bytes, 25,600. */
    private val sizeIncrement: Int = 25600

    /** Fee for `sizeInBytes` total reference-script bytes at base price `costPerByte`. */
    @static
    def fee(sizeInBytes: Int, costPerByte: Long): Coin = {
        require(sizeInBytes >= 0, s"negative reference-script size: $sizeInBytes")
        @tailrec
        def go(acc: NonNegativeInterval, tierPrice: NonNegativeInterval, n: Int): Coin =
            if n < sizeIncrement then Coin((acc + tierPrice * n).floor)
            else go(acc + tierPrice * sizeIncrement, multiplier * tierPrice, n - sizeIncrement)
        go(NonNegativeInterval.zero, NonNegativeInterval(costPerByte), sizeInBytes)
    }
}
