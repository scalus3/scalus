package scalus.examples.htlc

import scalus.cardano.blueprint.Blueprint
import scalus.compiler.Options
import scalus.uplc.PlutusV3

object HtlcContract {
    private given Options = Options.release
    lazy val compiled = PlutusV3.compile(HtlcValidator.validate)
    lazy val blueprint = Blueprint.plutusV3[Config, Action](
      title = "Hashed timelocked contract",
      description =
          "Releases funds when recipient reveals hash preimage before deadline, otherwise refunds to sender.",
      version = "1.0.0",
      license = Some("Apache License Version 2.0"),
      compiled = compiled
    )

    @main
    def main(): Unit = {
        println(blueprint.toJson())
    }
}
