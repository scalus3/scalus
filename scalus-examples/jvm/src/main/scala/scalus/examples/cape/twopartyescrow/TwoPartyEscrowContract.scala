package scalus.examples.cape.twopartyescrow

import scalus.cardano.blueprint.{Blueprint, Contract}
import scalus.compiler.Options
import scalus.uplc.PlutusV3

object TwoPartyEscrowContract extends Contract {
    private given Options = Options.releaseUntagged
    lazy val compiled = PlutusV3.compile(TwoPartyEscrowValidator.validate)

    lazy val blueprint = Blueprint.plutusV3[EscrowDatum, BigInt](
      title = "Two-party escrow (CAPE)",
      description = "CAPE two-party escrow, revision 2: one script that mints a thread token at the " +
          "deposit (minting, redeemer 0), burns it at the end (minting, 1 or 2), and guards the " +
          "funds when the seller accepts (spending, 1) or the buyer refunds after the deadline " +
          "(spending, 2).",
      version = "2.0.0",
      license = Some("Apache-2.0"),
      compiled = compiled
    )
}
