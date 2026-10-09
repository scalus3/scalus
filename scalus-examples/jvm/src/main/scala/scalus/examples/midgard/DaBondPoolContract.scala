package scalus.examples.midgard

import scalus.compiler.Options
import scalus.uplc.builtin.Data.toData
import scalus.uplc.{PlutusV3, Program}

/** Compiled Midgard DA bond pool. Apply the four parameters with [[applyParams]] to get a pool
  * instance.
  */
object DaBondPoolContract {
    private given Options = Options.release

    lazy val compiled = PlutusV3.compile(DaBondPoolValidator.validate)

    /** Applies the four validator parameters as `Data`, in Aiken's order. Works for the Aiken
      * blueprint's program too.
      */
    def applyParams(program: Program, p: DaBondPoolParams): Program =
        program $ p.initRef.toData $ p.hubOraclePolicyId.toData $ p.daParamsPolicyId.toData $
            p.parameters.toData
}
