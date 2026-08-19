package scalus.cardano.onchain.plutus.v2

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.onchain.OnchainError
import scalus.cardano.onchain.plutus.prelude.*
import scalus.testing.kit.EvalTestKit
import scalus.uplc.builtin.ByteString.hex
import scalus.uplc.builtin.Data.toData

class OutputDatumTest extends AnyFunSuite with EvalTestKit {

    test("inlineOf decodes an inline datum") {
        assert(OutputDatum.OutputDatum(BigInt(42).toData).inlineOf[BigInt] == BigInt(42))

        assertEvalEq(OutputDatum.OutputDatum(BigInt(42).toData).inlineOf[BigInt], BigInt(42))
    }

    test("inlineOf fails on NoOutputDatum and OutputDatumHash") {
        assertThrows[OnchainError](OutputDatum.NoOutputDatum.inlineOf[BigInt])
        assertThrows[OnchainError](OutputDatum.OutputDatumHash(hex"deadbeef").inlineOf[BigInt])

        assertEvalFails[OnchainError](OutputDatum.NoOutputDatum.inlineOf[BigInt])
        assertEvalFails[OnchainError](
          OutputDatum.OutputDatumHash(hex"deadbeef").inlineOf[BigInt]
        )
    }

    test("inlineOf with a custom message") {
        assert(
          OutputDatum.OutputDatum(BigInt(7).toData).inlineOf[BigInt]("Campaign datum") == BigInt(7)
        )

        val thrown = intercept[OnchainError](
          OutputDatum.NoOutputDatum.inlineOf[BigInt]("Campaign datum")
        )
        assert(thrown.getMessage == "Campaign datum")

        assertEvalEq(
          OutputDatum.OutputDatum(BigInt(7).toData).inlineOf[BigInt]("Campaign datum"),
          BigInt(7)
        )
        assertEvalFails[OnchainError](
          OutputDatum.NoOutputDatum.inlineOf[BigInt]("Campaign datum")
        )
    }
}
