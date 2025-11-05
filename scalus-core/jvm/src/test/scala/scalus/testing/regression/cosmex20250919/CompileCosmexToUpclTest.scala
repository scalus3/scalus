package scalus.testing.regression.cosmex20250919

import org.scalatest.funsuite.AnyFunSuite
import scalus.builtin.ByteString

class CompileCosmexToUpclTest extends AnyFunSuite {

    import scalus.ledger.api.v3.*

    test("compile cosmex to uplc") {
        // import scalus.Compiler.compile

        pending

        val key = ByteString
            .fromHex("aabbccddeeff00112233445566778899aabbccddeeff00112233445566778899")

        val exchangeParams = ExchangeParams(
          exchangePkh = PubKeyHash(key),
          contestationPeriodInMilliseconds = 5000,
          exchangePubKey = key
        )

        val uplc = CosmexValidator.mkCosmexValidator(exchangeParams)

        // println(uplc.pretty.render(100))
        // println(uplc.plutusV3.cborByteString)
    }

}
