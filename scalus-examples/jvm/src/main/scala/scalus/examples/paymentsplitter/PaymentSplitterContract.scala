package scalus.examples.paymentsplitter

import scalus.cardano.blueprint.{CompilerInfo, Contract, Preamble}
import scalus.cardano.ledger.Language

lazy val PaymentSplitterContract = Contract.PlutusV3Contract[Unit, Unit](
  Preamble(
    title = "Payment splitter",
    description = Some("Allows to split payouts equally among a list of specified payees"),
    version = Some("1.0.0"),
    compiler = Some(CompilerInfo.currentScalus),
    plutusVersion = Some(Language.PlutusV3),
    license = None
  ),
  PaymentSplitterValidator.validate
)
