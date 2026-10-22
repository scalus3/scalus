package scalus.compiler.plugin

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.report
import dotty.tools.dotc.util.SrcPos
import scalus.compiler.sir.SIR
import scalus.uplc.Constant

/** Compile-time checks of `@UplcRepr` arguments, shared by the two places that encode them. */
object UplcReprChecks {

    /** Rejects `SumBuiltinList(UplcConstr)`. A builtin list holds constants only, and Plutus fails
      * `mkCons` of a `constr` value, so such a list cannot be built on chain. `SumBuiltinList`
      * takes a `Data`, primitive or pair element; a native `constr` list is `@UplcRepr(UplcConstr)`
      * on the list itself.
      */
    def checkReprConstr(constrName: String, encodedArgs: List[SIR], pos: SrcPos)(using
        Context
    ): Unit =
        if constrName.endsWith("SumBuiltinList") then
            encodedArgs match
                case SIR.Const(Constant.String("UplcConstr"), _, _) :: Nil =>
                    report.error(
                      "@UplcRepr(SumBuiltinList(UplcConstr)) is not supported: a builtin list " +
                          "holds constants only, so Plutus fails to add a constr value to it. " +
                          "Use @UplcRepr(UplcConstr) on the list for a list of constr values.",
                      pos
                    )
                case _ =>
}
