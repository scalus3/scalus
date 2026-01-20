package scalus.examples.editablenft

import scalus.builtin.Data
import scalus.cardano.blueprint.Blueprint
import scalus.compiler.Options
import scalus.uplc.PlutusV3

private given Options = Options.release

lazy val EditableNftContract = PlutusV3.compile(EditableNftValidator.validate)

lazy val EditableNftBlueprint = Blueprint.plutusV3[ReferenceNftDatum, Unit](
  title = "Editable NFT Contract",
  description =
      "An NFT with mutable metadata following CIP-68 standard. Creates a reference NFT at script address and a user NFT for ownership. Owner can edit metadata until sealed.",
  version = "1.0.0",
  compiled = EditableNftContract,
  license = None
)
