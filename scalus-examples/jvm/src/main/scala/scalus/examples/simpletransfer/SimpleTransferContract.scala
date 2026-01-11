package scalus.examples.simpletransfer

import scalus.cardano.blueprint.Blueprint
import scalus.compiler.Options
import scalus.uplc.PlutusV3

private given Options = Options.release
lazy val SimpleTransferContract = PlutusV3.compile(SimpleTransferValidator.validate)
lazy val SimpleTransferBlueprint = Blueprint.plutusV3[Parties, Action](
  title = "Simple Transfer contract",
  description =
      "The contract allows a user (the owner) to deposit native cryptocurrency, and another user (the recipient) to withdraw arbitrary fractions of the contract balance",
  version = "1.0.0",
  license = None,
  compiled = SimpleTransferContract
)
