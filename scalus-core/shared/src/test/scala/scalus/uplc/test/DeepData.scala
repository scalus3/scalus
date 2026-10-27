package scalus.uplc.test

import scalus.cardano.onchain.plutus.prelude.List as PList
import scalus.uplc.builtin.Data

/** Deeply nested [[Data]], built without recursion, for code that must not recurse over the nesting
  * (#398).
  */
object DeepData {

    /** `depth` one-element lists around `I(1)`: its CBOR is `9f` `depth` times, `01`, then `ff`
      * `depth` times.
      */
    def nestedLists(depth: Int): Data =
        (1 to depth).foldLeft(Data.I(1): Data)((d, _) => Data.List(PList.singleton(d)))
}
