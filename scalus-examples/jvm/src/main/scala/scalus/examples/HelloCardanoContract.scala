package scalus.examples

import scalus.cardano.blueprint.Blueprint
import scalus.compiler.Options
import scalus.ledger.api.v3.PubKeyHash
import scalus.uplc.PlutusV3

private object HelloCardanoCompiler:
    given Options = Options.release
    lazy val contract = PlutusV3.compile(HelloCardano.validate)

lazy val HelloCardanoContract = HelloCardanoCompiler.contract
lazy val HelloCardanoBlueprint = Blueprint.plutusV3[PubKeyHash, String](
  title = "Hello Cardano",
  description =
      "A simple validator that checks if the redeemer is \"Hello, World!\" and if the transaction is signed by the owner.",
  version = "1.0.0",
  license = Some("Apache License Version 2.0"),
  compiled = HelloCardanoContract
)
