package scalus.examples

import scalus.uplc.builtin.Builtins.{constrData, mkCons, mkNilData}
import scalus.uplc.builtin.Data

/** Script contexts built from the `Data` of their fields, for the statements of the HTLC and the
  * payment splitter suites that quantify over the fields as `Data`. The vesting suite quantifies
  * over real contexts instead (`forAllData[ScriptContext]`), and so will these.
  *
  * They are `inline`, so that each is part of the expression it is used in, which the Scalus plugin
  * compiles.
  */
object ScriptContextData {

    /** A script context, as the ledger encodes it, from the encodings of its three fields. */
    inline def scriptContext(
        inline txInfo: Data,
        inline redeemer: Data,
        inline scriptInfo: Data
    ): Data =
        constrData(0, mkCons(txInfo, mkCons(redeemer, mkCons(scriptInfo, mkNilData()))))

    /** The script info of spending `ref`, whose datum is the encoded `Option[Data]`. */
    inline def spendingInfo(inline ref: Data, inline datum: Data): Data =
        constrData(1, mkCons(ref, mkCons(datum, mkNilData())))

    inline def noDatum: Data = constrData(1, mkNilData())

    inline def someDatum(inline datum: Data): Data =
        constrData(0, mkCons(datum, mkNilData()))
}
