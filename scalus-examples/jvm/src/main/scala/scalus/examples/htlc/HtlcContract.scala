package scalus.examples.htlc

import scalus.Compiler
import scalus.cardano.blueprint.Blueprint
import scalus.uplc.PlutusV3

private given Compiler.Options = Compiler.Options.release
lazy val HtlcContract = PlutusV3.compile(HtlcValidator.validate)
lazy val HtlcBlueprint = Blueprint.plutusV3[Config, Action](
  title = "Hashed timelocked contract",
  description =
      "Releases funds when recipient reveals hash preimage before deadline, otherwise refunds to sender.",
  version = "1.0.0",
  license = Some("Apache License Version 2.0"),
  compiled = HtlcContract
)

@main
def main(): Unit = {
    println(HtlcBlueprint.toJson())
}
