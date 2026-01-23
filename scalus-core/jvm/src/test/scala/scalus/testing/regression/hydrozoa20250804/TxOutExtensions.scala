package scalus.testing.regression.hydrozoa20250804

import scalus.*
import scalus.uplc.builtin.{Data, FromData}
import scalus.cardano.onchain.plutus.v2.OutputDatum.OutputDatum
import scalus.cardano.onchain.plutus.v3.TxOut

@Compile
object TxOutExtensions {
    extension (self: TxOut)
        /** Returns inline datum of type T of fails.
          *
          * @param x$1
          * @tparam T
          * @return
          */
        def inlineDatumOfType[T](using FromData[T]): T =
            val OutputDatum(inlineDatum) = self.datum: @unchecked
            inlineDatum.to[T]

}
