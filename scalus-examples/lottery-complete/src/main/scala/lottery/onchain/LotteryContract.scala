package lottery.onchain

import scalus.cardano.blueprint.Blueprint
import scalus.compiler.Options
import scalus.uplc.PlutusV3

object LotteryContract {
    private given Options = Options.release
    lazy val compiled = PlutusV3.compile(LotteryValidator.validate)
    lazy val blueprint = Blueprint.plutusV3[State, Action](
      title = "Two-player lottery contract",
      description =
          "Two-player commit-reveal betting contract where winner is determined by combined preimage length modulo 2.",
      version = "1.0.0",
      compiled = compiled,
      license = Some("Apache License Version 2.0")
    )
}
