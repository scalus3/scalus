package scalus.examples.pricebet

import scalus.compiler.Options
import scalus.uplc.PlutusV3
import scalus.builtin.Data.toData
import scalus.uplc.*

private given Options = Options.release.copy(generateErrorTraces = true)

def PriceBetContract(config: PricebetConfig) =
    PlutusV3.compile(PricebetValidator.validate).apply(config.toData)
def OracleContract(config: OracleConfig) =
    PlutusV3.compile(OracleValidator.validate).apply(config.toData)
