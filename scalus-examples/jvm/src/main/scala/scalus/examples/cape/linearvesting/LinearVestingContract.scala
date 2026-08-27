package scalus.examples.cape.linearvesting

import scalus.compiler.Options
import scalus.uplc.{PlutusV3, Program}

object LinearVestingContract {

    private given Options = Options.releaseUntagged
    lazy val compiled = PlutusV3.compile(LinearVestingValidator.validate)
    lazy val program: Program = compiled.program
}
