package scalus.examples.decentralizedidentity

import scalus.cardano.blueprint.{Blueprint, Contract}
import scalus.compiler.Options
import scalus.uplc.PlutusV3
import scalus.uplc.builtin.Data
import scalus.cardano.onchain.plutus.v3.TxOutRef

/** Blueprint and compiled script for the decentralized identity contract. */
object DecentralizedIdentityContract extends Contract {
    private given Options = Options.release
    lazy val compiled = PlutusV3.compile(DecentralizedIdentityValidator.validate)
    lazy val blueprint = Blueprint.plutusV3[TxOutRef, IdentityDatum, SpendAction](
      title = "Decentralized identity",
      description =
          "Self-sovereign identity: a one-shot UTxO mints a unique identity NFT, after which the " +
              "owner can issue delegations and attribute credentials. Datum/redeemer shown for the " +
              "primary identity-spend path (delegation and attribute UTxOs carry their own datums).",
      version = "1.0.0",
      license = Some("Apache-2.0"),
      // DataParameterizedValidator applies the one-shot TxOutRef parameter as Data on the UPLC
      // level; the cast only re-labels the phantom type for schema derivation.
      compiled = compiled.asInstanceOf[PlutusV3[TxOutRef => Data => Unit]]
    )
}
