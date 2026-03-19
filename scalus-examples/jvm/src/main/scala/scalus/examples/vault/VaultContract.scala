package scalus.examples.vault

import scalus.cardano.blueprint.{Blueprint, Contract}
import scalus.compiler.Options
import scalus.uplc.PlutusV3

object VaultContract extends Contract {
    private given Options = Options.release
    lazy val compiled = PlutusV3.compile(VaultValidator.validate)
    lazy val blueprint = Blueprint.plutusV3[State, Action](
      title = "Vault",
      description =
          "Keeps the funds safe by requiring a 2-stage withdrawal with a mandatory confirmation period.",
      version = "1.0.0",
      license = None,
      compiled = compiled
    )
}
