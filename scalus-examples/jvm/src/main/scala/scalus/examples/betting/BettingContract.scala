package scalus.examples.betting

import scalus.cardano.blueprint.Blueprint
import scalus.compiler.Options
import scalus.uplc.PlutusV3

private given Options = Options.release
lazy val BettingContract = PlutusV3.compile(BettingValidator.validate)
lazy val BettingBlueprint = Blueprint.plutusV3[Config, Action](
  title = "Betting validator",
  description =
      "Decentralized two-player betting system with trustless wagering and oracle-based resolution",
  version = "1.0.0",
  license = None,
  compiled = BettingContract
)
