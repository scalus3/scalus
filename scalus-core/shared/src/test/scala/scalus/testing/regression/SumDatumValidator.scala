package scalus.testing.regression

import org.scalatest.funsuite.AnyFunSuite

import scalus.*
import scalus.builtin.{ByteString, Data, FromData, ToData}
import scalus.ledger.api.v3.*
import scalus.prelude.*

@Compile
object SumDatumValidator extends Validator:

    enum TreasuryDatum:
        case Unresolved(unresolvedDatum: ByteString)
        case Resolved(resolvedDatum: ByteString)

    given FromData[TreasuryDatum] = FromData.derived
    given ToData[TreasuryDatum] = ToData.derived

    inline override def spend(
        datum: Option[Data],
        redeemer: Data,
        tx: TxInfo,
        ownRef: TxOutRef
    ): Unit =
        //  val _: TreasuryDatum = datum match  // works
        //  val x = datum match // works
        val _ = datum match
            case Option.Some(d) => d.to[TreasuryDatum]
            case Option.None    => fail()

end SumDatumValidator

object SumDatumScript {
    val sir = Compiler.compile(SumDatumValidator.validate)
    def uplc = sir.toUplc(true)
}

class SumDatumValidatorTest extends AnyFunSuite:

    test("SumDatumValidator should compile and transform to UPLC") {
        pending
        val script = SumDatumScript.uplc
        // val result = script.evaluateDebug
        // assert(result.isSuccess)
    }

end SumDatumValidatorTest
