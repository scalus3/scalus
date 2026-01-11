package scalus.examples.vault

import scalus.cardano.blueprint.Blueprint
import scalus.compiler.Options
import scalus.uplc.PlutusV3

private given Options = Options.release
lazy val VaultContract = PlutusV3.compile(VaultValidator.validate)
lazy val VaultBlueprint = Blueprint.plutusV3[State, Action](
  title = "Vault",
  description =
      "Keeps the funds safe by requiring a 2-stage withdrawal with a mandatory confirmation period.",
  version = "1.0.0",
  license = None,
  compiled = VaultContract
)
