package scalus.examples.vesting

import scalus.cardano.blueprint.{CompilerInfo, Contract, Preamble}
import scalus.cardano.ledger.Language

lazy val VestingContract = Contract.PlutusV3Contract[Config, Action](
  Preamble(
    title = "Vesting validator",
    description = Some(
      "Time-locked token distribution with linear vesting schedule"
    ),
    version = Some("1.0.0"),
    compiler = Some(CompilerInfo.currentScalus),
    plutusVersion = Some(Language.PlutusV3),
    license = None
  ),
  VestingValidator.validate
)
