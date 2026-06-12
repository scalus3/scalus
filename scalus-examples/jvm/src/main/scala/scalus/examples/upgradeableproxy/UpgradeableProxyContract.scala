package scalus.examples.upgradeableproxy

import scalus.cardano.blueprint.{Blueprint, Contract}
import scalus.compiler.Options
import scalus.uplc.PlutusV3

object UpgradeableProxyContract extends Contract {
    private given Options = Options.release
    lazy val compiled = PlutusV3.compile(ProxyValidator.validate)
    lazy val blueprint = Blueprint.plutusV3[ProxyDatum, ProxyRedeemer](
      title = "Upgradeable proxy validator",
      description =
          "Delegates validation to an upgradeable logic stake validator; the owner can repoint it to new logic.",
      version = "1.0.0",
      license = Some("Apache License Version 2.0"),
      compiled = compiled
    )
}
