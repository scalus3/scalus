package scalus.examples.simpletransfer

import scalus.cardano.blueprint.{CompilerInfo, Contract, Preamble}
import scalus.cardano.ledger.Language

lazy val SimpleTransferContract = Contract.PlutusV3Contract[Parties, Action](
  Preamble(
    title = "Simple Transfer contract",
    description = Some(
      "The contract allows a user (the owner) to deposit native cryptocurrency, and another user (the recipient) to withdraw arbitrary fractions of the contract balance"
    ),
    version = Some("1.0.0"),
    compiler = Some(CompilerInfo.currentScalus),
    plutusVersion = Some(Language.PlutusV3),
    license = None
  ),
  SimpleTransferValidator.validate
)
