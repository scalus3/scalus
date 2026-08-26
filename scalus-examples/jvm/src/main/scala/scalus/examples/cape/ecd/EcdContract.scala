package scalus.examples.cape.ecd

import scalus.compiler.Options
import scalus.uplc.{PlutusV3, Program}

object EcdContract {
    private given Options = Options.releaseUntagged
    lazy val program: Program = PlutusV3.compile(EcdBase.ecd).program
}
