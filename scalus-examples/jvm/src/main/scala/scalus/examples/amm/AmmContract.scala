package scalus.examples.amm

import scalus.cardano.blueprint.{Blueprint, Contract}
import scalus.compiler.Options
import scalus.uplc.PlutusV3
import scalus.uplc.builtin.Data

/** Blueprint and compiled script for the constant-product AMM. */
object AmmContract extends Contract {
    private given Options = Options.release
    lazy val compiled = PlutusV3.compile(AmmValidator.validate)
    lazy val blueprint = Blueprint.plutusV3[AmmParams, AmmDatum, AmmRedeemer](
      title = "Constant-product AMM",
      description =
          "Constant-product (x*y=k) automated market maker parameterized by the token pair and " +
              "fee rate. Supports deposit, redeem and swap while binding the datum reserves to the " +
              "tokens actually held by the continuing pool output.",
      version = "1.0.0",
      license = Some("Apache-2.0"),
      // AmmValidator is a DataParameterizedValidator: the AmmParams parameter is applied as Data on
      // the UPLC level, so `compiled` is typed `Data => Data => Unit`. The cast only re-labels the
      // phantom type so the parameter schema is derived as AmmParams; the program is unchanged.
      compiled = compiled.asInstanceOf[PlutusV3[AmmParams => Data => Unit]]
    )
}
