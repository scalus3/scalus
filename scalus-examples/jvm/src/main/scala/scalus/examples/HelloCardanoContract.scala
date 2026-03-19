package scalus.examples

import scalus.cardano.blueprint.{Blueprint, Contract}
import scalus.cardano.onchain.plutus.v3.PubKeyHash
import scalus.compiler.Options
import scalus.uplc.PlutusV3

object HelloCardanoContract extends Contract {
    private given Options = Options.release
    lazy val compiled = PlutusV3.compile(HelloCardano.validate)
    lazy val blueprint = Blueprint.plutusV3[PubKeyHash, String](
      title = "Hello Cardano",
      description =
          "A simple validator that checks if the redeemer is \"Hello, World!\" and if the transaction is signed by the owner.",
      version = "1.0.0",
      license = Some("Apache License Version 2.0"),
      compiled = compiled
    )
}
