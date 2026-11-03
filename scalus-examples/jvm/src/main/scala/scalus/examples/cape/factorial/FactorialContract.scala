package scalus.examples.cape.factorial

import scalus.compiler.Options
import scalus.uplc.PlutusV3
import scalus.uplc.Program

object FactorialContract {
    private given Options = Options.releaseUntagged

    lazy val baseCompiled = PlutusV3.compile(FactorialBase.factorial)
    lazy val baseProgram: Program = baseCompiled.program

    lazy val openCompiled = PlutusV3.compile(FactorialOpen.factorial)
    lazy val openProgram: Program = openCompiled.program
}
