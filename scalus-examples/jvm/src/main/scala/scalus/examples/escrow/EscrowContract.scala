package scalus.examples.escrow

import scalus.cardano.blueprint.{CompilerInfo, Contract, Preamble}
import scalus.cardano.ledger.Language

lazy val EscrowContract = Contract.PlutusV3Contract[EscrowDatum, EscrowAction](
  Preamble(
    title = "Three-party escrow smart contract",
    description = Some(
      "Contract that enforces a simple escrow with three actions: Deposit, Pay, Refund"
    ),
    version = Some("1.0.0"),
    compiler = Some(CompilerInfo.currentScalus),
    plutusVersion = Some(Language.PlutusV3),
    license = None
  ),
  EscrowValidator.validate
)
