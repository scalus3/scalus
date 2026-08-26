package scalus.examples.cape.fibonacci

import scalus.compiler.Options
import scalus.uplc.PlutusV3
import scalus.uplc.Program
import scalus.uplc.transform.CaseConstrApply

object FibonacciContract {
    private given Options = Options.releaseUntagged

    lazy val baseCompiled = PlutusV3.compile(FibonacciBase.fibonacci)
    lazy val baseProgram: Program = baseCompiled.program

    lazy val openProgram: Program = CaseConstrApply(FibonacciOpen.term).plutusV3
}
