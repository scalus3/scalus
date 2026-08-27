package scalus.examples.cape.htlc

import scalus.compiler.Options
import scalus.uplc.{PlutusV3, Program}

object HtlcContract {
    private given Options = Options.releaseUntagged
    lazy val compiled = PlutusV3.compile(HtlcValidator.validate)
    lazy val program: Program = compiled.program
}
