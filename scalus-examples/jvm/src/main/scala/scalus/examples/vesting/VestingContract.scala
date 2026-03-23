package scalus.examples.vesting

import scalus.cardano.blueprint.{Blueprint, Contract}
import scalus.compiler.Options
import scalus.uplc.PlutusV3

object VestingContract extends Contract {
    private given Options = Options.release
    lazy val compiled = PlutusV3.compile(VestingValidator.validate)
    lazy val blueprint = Blueprint.plutusV3[Config, Action](
      title = "Vesting validator",
      description = "Time-locked token distribution with linear vesting schedule",
      version = "1.0.0",
      license = None,
      compiled = compiled
    )
}
