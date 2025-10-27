package scalus.regression.match_tv_20251021

import scalus.*
import scalus.builtin.{Data, FromData}

// Simplified TxOut type
case class SimpleTxOut(datum: Data)

@Compile
object TxOutHelper {
    extension (self: SimpleTxOut)
        /** Returns inline datum of type T
          */
        def inlineDatumOfType[T](using FromData[T]): T =
            self.datum.to[T]
}
