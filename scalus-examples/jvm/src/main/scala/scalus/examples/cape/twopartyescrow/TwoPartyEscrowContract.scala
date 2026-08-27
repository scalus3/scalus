package scalus.examples.cape.twopartyescrow

import scalus.cardano.blueprint.{Blueprint, Contract}
import scalus.compiler.Options
import scalus.uplc.PlutusV3

object TwoPartyEscrowContract extends Contract {
    private given Options = Options.releaseUntagged
    lazy val compiled = PlutusV3.compile(TwoPartyEscrowValidator.validate)

    lazy val blueprint = Blueprint.plutusV3[EscrowDatum, BigInt](
      title = "Two-party escrow (CAPE)",
      description = "CAPE two-party escrow: the seller deposits, then the buyer accepts (releasing funds to " +
          "the seller) or the seller refunds after the deposit window. The redeemer selects the " +
          "action (0 = deposit, 1 = accept, 2 = refund).",
      version = "1.0.0",
      license = Some("Apache-2.0"),
      compiled = compiled
    )
}
