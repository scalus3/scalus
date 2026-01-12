package scalus.examples.pricebet

import scalus.compiler.Options
import scalus.uplc.PlutusV3

private given Options = Options.release

lazy val PriceBetContract = PlutusV3.compile(PricebetValidator.validate)
lazy val OracleContract = PlutusV3.compile(OracleValidator.validate)
