package scalus.examples.editablenft

import scalus.cardano.blueprint.{Blueprint, Contract}
import scalus.compiler.Options
import scalus.uplc.PlutusV3
import scalus.uplc.builtin.Data
import scalus.cardano.onchain.plutus.v3.TxOutRef

/** Blueprint and compiled script for the editable NFT (CIP-68 style) contract. */
object EditableNftContract extends Contract {
    private given Options = Options.release
    lazy val compiled = PlutusV3.compile(EditableNftValidator.validate)
    lazy val blueprint = Blueprint.plutusV3[TxOutRef, ReferenceNftDatum, SpendRedeemer](
      title = "Editable NFT",
      description =
          "Reference-NFT metadata contract: a one-shot UTxO mints the NFT, then the holder may " +
              "edit the on-chain datum until it is sealed, after which the metadata is immutable.",
      version = "1.0.0",
      license = Some("Apache-2.0"),
      // DataParameterizedValidator applies the one-shot TxOutRef parameter as Data on the UPLC
      // level; the cast only re-labels the phantom type for schema derivation.
      compiled = compiled.asInstanceOf[PlutusV3[TxOutRef => Data => Unit]]
    )
}
