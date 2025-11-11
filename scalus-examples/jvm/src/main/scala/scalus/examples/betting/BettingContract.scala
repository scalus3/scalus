package scalus.examples.betting

import scalus.cardano.blueprint.{CompilerInfo, Contract, Preamble}
import scalus.cardano.ledger.Language

lazy val BettingContract = Contract.PlutusV3Contract[BettingConfig, Action](
  Preamble(
    title = "Betting validator",
    description = Some(
      "Decentralized two-player betting system with trustless wagering and oracle-based resolution"
    ),
    version = Some("1.0.0"),
    compiler = Some(CompilerInfo.currentScalus),
    plutusVersion = Some(Language.PlutusV3),
    license = None
  ),
  BettingValidator.validate
)
