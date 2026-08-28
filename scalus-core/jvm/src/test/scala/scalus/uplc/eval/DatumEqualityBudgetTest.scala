package scalus.uplc.eval

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.cardano.ledger.CardanoInfo
import scalus.cardano.onchain.plutus.prelude.{===, Eq, Option}
import scalus.cardano.onchain.plutus.v1.{Address, Credential, PubKeyHash, Value}
import scalus.cardano.onchain.plutus.v2.{OutputDatum, TxOut}
import scalus.compiler.Options
import scalus.uplc.PlutusV3
import scalus.uplc.builtin.ByteString.hex
import scalus.uplc.builtin.Data.toData
import scalus.uplc.builtin.{Data, FromData, ToData}

case class DatumEqualityBudgetTestDatum(owner: PubKeyHash, count: BigInt, tag: BigInt)
    derives FromData,
      ToData,
      Eq

/** Pins the claim in `TxOut.hasInlineDatum`'s scaladoc: comparing a continuing output's datum by
  * wrapping the expected value (`datum === OutputDatum(x.toData)`) is cheaper than decoding the
  * datum and comparing the decoded value (`datum.inlineOrFail[A] === x`), on memory, cpu and fee.
  * The decode form takes the `OutputDatum` apart and then rewraps the decoded value with
  * `constrData(0, ...)` before `equalsData`; the wrap form is one `constrData` and one
  * `equalsData`.
  */
class DatumEqualityBudgetTest extends AnyFunSuite {
    private given PlutusVM = PlutusVM.makePlutusV3VM()
    private given Options = Options.release.copy(noWarn = true)
    private val prices = CardanoInfo.mainnet.protocolParams.executionUnitPrices

    private val wrap = PlutusV3.compile { (outData: Data, cur: Data) =>
        val out = outData.to[TxOut]
        val c = cur.to[DatumEqualityBudgetTestDatum]
        out.hasInlineDatum(c.copy(count = c.count + 1))
    }
    private val decode = PlutusV3.compile { (outData: Data, cur: Data) =>
        val out = outData.to[TxOut]
        val c = cur.to[DatumEqualityBudgetTestDatum]
        out.datum.inlineOrFail[DatumEqualityBudgetTestDatum]("no inline") === c.copy(count =
            c.count + 1
        )
    }

    test("hasInlineDatum is cheaper than inlineOrFail + === on mem, cpu and fee") {
        val cur = DatumEqualityBudgetTestDatum(PubKeyHash(hex"deadbeef"), 41, 7)
        val out = TxOut(
          Address(
            Credential.PubKeyCredential(PubKeyHash(hex"aa")),
            Option.None
          ),
          Value.lovelace(1),
          OutputDatum.OutputDatum(cur.copy(count = 42).toData)
        )
        val wrapBudget = (wrap.program $ out.toData $ cur.toData).evaluateDebug.budget
        val decodeBudget = (decode.program $ out.toData $ cur.toData).evaluateDebug.budget
        info(
          s"wrap:   $wrapBudget fee=${wrapBudget.fee(prices)} size=${wrap.program.cborEncoded.length}"
        )
        info(
          s"decode: $decodeBudget fee=${decodeBudget.fee(prices)} size=${decode.program.cborEncoded.length}"
        )
        assert(wrapBudget.memory < decodeBudget.memory)
        assert(wrapBudget.steps < decodeBudget.steps)
        assert(wrapBudget.fee(prices) < decodeBudget.fee(prices))
        assert(wrap.program.cborEncoded.length < decode.program.cborEncoded.length)
    }
}
