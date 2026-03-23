package scalus.examples.betting

import scalus.cardano.blueprint.{Blueprint, Contract}
import scalus.compiler.Options
import scalus.uplc.PlutusV3

object BettingContract extends Contract {
    private given Options = Options.release
    lazy val compiled = PlutusV3.compile(BettingValidator.validate)
    lazy val blueprint = Blueprint.plutusV3[Config, Action](
      title = "Betting validator",
      description =
          "Decentralized two-player betting system with trustless wagering and oracle-based resolution",
      version = "1.0.0",
      license = None,
      compiled = compiled
    )
}
