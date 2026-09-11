package scalus.cardano.ledger

import scala.annotation.static

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

    /** Tier stride in bytes, 25,600. */
    private val sizeIncrement: Int = 25600

    /** Fee for `sizeInBytes` total reference-script bytes at base price `costPerByte`.
      *
      * @throws ArithmeticException
      *   if the final fee exceeds the range of Coin.
      */
    @static
    def fee(sizeInBytes: Int, costPerByte: Long): Coin = {
        require(sizeInBytes >= 0, s"negative reference-script size: $sizeInBytes")
        require(costPerByte >= 0, s"negative reference-script base price: $costPerByte")
        if sizeInBytes == 0 || costPerByte == 0 then return Coin(0)

        val tiers = sizeInBytes / sizeIncrement
        val remainder = sizeInBytes % sizeIncrement
        val numerator = BigInt(6).pow(tiers)
        val denominator = BigInt(5).pow(tiers)
        // Sum the full tiers as a geometric series: sum((6/5)^i) = 5 * ((6/5)^tiers - 1).
        // Keep the partial tier over the same denominator and floor only the final total.
        val total = BigInt(costPerByte) * (
          BigInt(sizeIncrement) * 5 * (numerator - denominator) + remainder * numerator
        ) / denominator
        if !total.isValidLong then
            throw new ArithmeticException("reference-script fee exceeds Coin range")
        Coin(total.toLong)
    }
}
