package scalus.cardano.onchain.plutus.v2

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.{Coin, ExUnits}
import scalus.cardano.onchain.OnchainError
import scalus.cardano.onchain.plutus.prelude.*
import scalus.testing.kit.EvalTestKit
import scalus.uplc.builtin.ByteString.hex
import scalus.uplc.builtin.Data.toData

class OutputDatumTest extends AnyFunSuite with EvalTestKit {

    test("inlineOrFail decodes an inline datum") {
        assert(OutputDatum.OutputDatum(BigInt(42).toData).inlineOrFail[BigInt] == BigInt(42))

        assertEvalEq(OutputDatum.OutputDatum(BigInt(42).toData).inlineOrFail[BigInt], BigInt(42))
    }

    test("inlineOrFail fails on NoOutputDatum and OutputDatumHash") {
        // Receivers are widened to the enum type: on a receiver statically known to be a
        // non-inline case, inlineOrFail is a compile error, not a runtime failure.
        val hashDatum: OutputDatum = OutputDatum.OutputDatumHash(hex"deadbeef")
        assertThrows[OnchainError](OutputDatum.NoOutputDatum.inlineOrFail[BigInt])
        assertThrows[OnchainError](hashDatum.inlineOrFail[BigInt])

        assertEvalFails[OnchainError](OutputDatum.NoOutputDatum.inlineOrFail[BigInt])
        assertEvalFails[OnchainError](
          (OutputDatum.OutputDatumHash(hex"deadbeef"): OutputDatum).inlineOrFail[BigInt]
        )
    }

    test("TxOut.hasInlineDatum") {
        import scalus.cardano.onchain.plutus.v1.{Address, Credential, PubKeyHash, Value}
        val addr = Address(Credential.PubKeyCredential(PubKeyHash(hex"aa")), Option.None)
        val out = TxOut(addr, Value.lovelace(1), OutputDatum.OutputDatum(BigInt(42).toData))
        assert(out.hasInlineDatum(BigInt(42)))
        assert(!out.hasInlineDatum(BigInt(43)))
        assert(!TxOut(addr, Value.lovelace(1)).hasInlineDatum(BigInt(42)))
        assert(
          !TxOut(addr, Value.lovelace(1), OutputDatum.OutputDatumHash(hex"bb"))
              .hasInlineDatum(BigInt(42))
        )

        assertEval(
          TxOut(
            Address(Credential.PubKeyCredential(PubKeyHash(hex"aa")), Option.None),
            Value.lovelace(1),
            OutputDatum.OutputDatum(BigInt(42).toData)
          ).hasInlineDatum(BigInt(42))
        )
        assertEval(
          !TxOut(
            Address(Credential.PubKeyCredential(PubKeyHash(hex"aa")), Option.None),
            Value.lovelace(1),
            OutputDatum.OutputDatum(BigInt(42).toData)
          ).hasInlineDatum(BigInt(43))
        )
        assertEval(
          !TxOut(
            Address(Credential.PubKeyCredential(PubKeyHash(hex"aa")), Option.None),
            Value.lovelace(1),
            OutputDatum.OutputDatumHash(hex"bb")
          ).hasInlineDatum(BigInt(42))
        )
    }

    test("budget: TxOut.hasInlineDatum") {
        import scalus.cardano.onchain.plutus.v1.{Address, Credential, PubKeyHash, Value}
        assertEvalWithBudgetAndFee(
          (out: TxOut) => out.hasInlineDatum(BigInt(42)),
          TxOut(
            Address(Credential.PubKeyCredential(PubKeyHash(hex"aa")), Option.None),
            Value.lovelace(1),
            OutputDatum.OutputDatum(BigInt(42).toData)
          ),
          true,
          ExUnits(memory = 2401, steps = 1_866114),
          Coin(274)
        )
    }

    test("inlineOrFail with a custom message") {
        assert(
          OutputDatum
              .OutputDatum(BigInt(7).toData)
              .inlineOrFail[BigInt]("Campaign datum") == BigInt(7)
        )

        val thrown = intercept[OnchainError](
          OutputDatum.NoOutputDatum.inlineOrFail[BigInt]("Campaign datum")
        )
        assert(thrown.getMessage == "Campaign datum")

        assertEvalEq(
          OutputDatum.OutputDatum(BigInt(7).toData).inlineOrFail[BigInt]("Campaign datum"),
          BigInt(7)
        )
        assertEvalFails[OnchainError](
          OutputDatum.NoOutputDatum.inlineOrFail[BigInt]("Campaign datum")
        )
    }
}
