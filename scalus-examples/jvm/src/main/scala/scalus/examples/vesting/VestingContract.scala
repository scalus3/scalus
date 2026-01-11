package scalus.examples.vesting

import scalus.cardano.blueprint.Blueprint
import scalus.compiler.Options
import scalus.uplc.PlutusV3

private given Options = Options.release
lazy val VestingContract = PlutusV3.compile(VestingValidator.validate)
lazy val VestingBlueprint = Blueprint.plutusV3[Config, Action](
  title = "Vesting validator",
  description = "Time-locked token distribution with linear vesting schedule",
  version = "1.0.0",
  license = None,
  compiled = VestingContract
)
