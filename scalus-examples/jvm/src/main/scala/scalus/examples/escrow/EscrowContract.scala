package scalus.examples.escrow

import scalus.cardano.blueprint.{Blueprint, Contract}
import scalus.compiler.Options
import scalus.uplc.PlutusV3

object EscrowContract extends Contract {
    private given Options = Options.release
    lazy val compiled = PlutusV3.compile(EscrowValidator.validate)
    lazy val blueprint = Blueprint.plutusV3[Config, Action](
      title = "Three-party escrow smart contract",
      description =
          "Contract that enforces a simple escrow with three actions: Deposit, Pay, Refund",
      version = "1.0.0",
      license = None,
      compiled = compiled
    )
}
