package scalus.examples.pricebet

import scalus.cardano.blueprint.{Blueprint, Contract}
import scalus.compiler.Options
import scalus.uplc.PlutusV3
import scalus.uplc.builtin.Data

/** Blueprint and compiled script for the price-bet contract (parameterized, unapplied). */
object PricebetContract extends Contract {
    private given Options = Options.release
    lazy val compiled = PlutusV3.compile(PricebetValidator.validate)
    lazy val blueprint = Blueprint.plutusV3[PricebetConfig, PricebetState, Action](
      title = "Price bet",
      description =
          "Two-party bet on a future oracle price, parameterized by a PricebetConfig. The winner " +
              "is determined by comparing the oracle's reported price against the agreed strike.",
      version = "1.0.0",
      license = Some("Apache-2.0"),
      // DataParameterizedValidator applies the PricebetConfig parameter as Data on the UPLC level;
      // the cast only re-labels the phantom type for schema derivation.
      compiled = compiled.asInstanceOf[PlutusV3[PricebetConfig => Data => Unit]]
    )
}

/** Blueprint and compiled script for the price oracle contract (parameterized, unapplied). */
object PricebetOracleContract extends Contract {
    private given Options = Options.release
    lazy val compiled = PlutusV3.compile(OracleValidator.validate)
    lazy val blueprint = Blueprint.plutusV3[OracleConfig, OracleState, SpendOracleRedeemer](
      title = "Price oracle",
      description =
          "Oracle that publishes a signed price feed consumed by the price-bet contract, " +
              "parameterized by an OracleConfig. Datum/redeemer shown for the oracle-spend path.",
      version = "1.0.0",
      license = Some("Apache-2.0"),
      // DataParameterizedValidator applies the OracleConfig parameter as Data on the UPLC level;
      // the cast only re-labels the phantom type for schema derivation.
      compiled = compiled.asInstanceOf[PlutusV3[OracleConfig => Data => Unit]]
    )
}
