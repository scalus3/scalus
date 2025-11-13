package scalus.examples.htlc

import scalus.cardano.blueprint.{CompilerInfo, Contract, Preamble}
import scalus.cardano.ledger.Language

lazy val HtlcContract = Contract.PlutusV3Contract[Config, Action](
  Preamble(
    title = "Hashed timelocked contract",
    description = Some(
      "Releases funds when recipient reveals hash preimage before deadline, otherwise refunds to sender."
    ),
    version = Some("1.0.0"),
    compiler = Some(CompilerInfo.currentScalus),
    plutusVersion = Some(Language.PlutusV3),
    license = None
  ),
  HtlcValidator.validate
)
