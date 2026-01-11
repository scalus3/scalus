package scalus.examples.escrow

import scalus.cardano.blueprint.Blueprint
import scalus.compiler.Options
import scalus.uplc.PlutusV3

private given Options = Options.release
lazy val EscrowContract = PlutusV3.compile(EscrowValidator.validate)
lazy val EscrowBlueprint = Blueprint.plutusV3[Config, Action](
  title = "Three-party escrow smart contract",
  description = "Contract that enforces a simple escrow with three actions: Deposit, Pay, Refund",
  version = "1.0.0",
  license = None,
  compiled = EscrowContract
)
