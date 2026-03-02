package scalus.examples.amm

import scalus.compiler.Options
import scalus.uplc.PlutusV3

private given Options = Options.release

lazy val AmmContract = PlutusV3.compile(AmmValidator.validate)
