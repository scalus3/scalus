package scalus.examples.lottery

import scalus.cardano.blueprint.{Blueprint, Contract}
import scalus.compiler.Options
import scalus.uplc.PlutusV3

object LotteryContract extends Contract {
    private given Options = Options.release
    lazy val compiled = PlutusV3.compile(LotteryValidator.validate)
    lazy val blueprint = Blueprint.plutusV3[State, Action](
      title = "Two-player lottery contract",
      description =
          "Two-player commit-reveal betting contract where winner is determined by combined preimage length modulo 2.",
      version = "1.0.0",
      compiled = compiled,
      license = None
    )
}
