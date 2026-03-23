package scalus.uplc.eval

import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.*
import scalus.cardano.ledger.{CardanoInfo, Coin, ExUnits, NonNegativeInterval}
import scalus.cardano.onchain.plutus
import scalus.cardano.onchain.plutus.prelude.===
import scalus.cardano.onchain.plutus.v1.{Address, Credential, PubKeyHash, Value}
import scalus.cardano.onchain.plutus.v2.OutputDatum
import scalus.cardano.onchain.plutus.v3.{TxId, TxInInfo, TxOutRef}
import scalus.compiler.Options
import scalus.uplc.PlutusV3
import scalus.uplc.builtin.Builtins.*
import scalus.uplc.builtin.ByteString.hex
import scalus.uplc.builtin.Data.toData
import scalus.uplc.builtin.{ByteString, Data}

import scala.annotation.tailrec

class EqualsDataVsTypedComparisonTest extends AnyFunSuite with ScalaCheckPropertyChecks {
    private given PlutusVM = PlutusVM.makePlutusV3VM()
    private given Options = Options.release
    private val params = CardanoInfo.mainnet.protocolParams
    private val prices = params.executionUnitPrices

    /** Reference script fee using tiered pricing (1.2x multiplier per 25,600 byte tier) */
    private def refScriptFee(sizeBytes: Int): Coin = {
        val basePricePerByte = NonNegativeInterval(params.minFeeRefScriptCostPerByte)
        val multiplier = NonNegativeInterval(12, 10)
        val stride = 25600

        @tailrec
        def go(acc: NonNegativeInterval, curTierPrice: NonNegativeInterval, n: Int): Coin = {
            if n < stride then Coin((acc + curTierPrice * n).floor)
            else go(acc + curTierPrice * stride, multiplier * curTierPrice, n - stride)
        }

        go(NonNegativeInterval.zero, basePricePerByte, sizeBytes)
    }

    /** Total tx fee contribution = exUnits fee + reference script fee */
    private def totalFee(budget: ExUnits, scriptSizeBytes: Int): Coin =
        Coin(budget.fee(prices).value + refScriptFee(scriptSizeBytes).value)

    /** Print a comparison line with all metrics */
    private def formatLine(
        label: String,
        budget: ExUnits,
        scriptSizeBytes: Int
    ): String = {
        val exFee = budget.fee(prices)
        val rsFee = refScriptFee(scriptSizeBytes)
        val total = Coin(exFee.value + rsFee.value)
        f"$label%-18s mem=${budget.memory}%6d  cpu=${budget.steps}%9d  exFee=${exFee.value}%4d  script=${scriptSizeBytes}%4dB  refScriptFee=${rsFee.value}%5d  txFee=${total.value}%5d"
    }

    test("equalsData vs equalsInteger — one side constant") {
        val eqData = PlutusV3.compile { (d: Data) => equalsData(d, iData(1)) }
        val eqInteger = PlutusV3.compile { (d: Data) => equalsInteger(unIData(d), 1) }
        val d = iData(1)
        val eqDataBudget = (eqData.program $ d).evaluateDebug.budget
        val eqIntegerBudget = (eqInteger.program $ d).evaluateDebug.budget
        val eqDataSize = eqData.program.cborEncoded.length
        val eqIntegerSize = eqInteger.program.cborEncoded.length
        info(formatLine("equalsData", eqDataBudget, eqDataSize))
        info(formatLine("equalsInteger", eqIntegerBudget, eqIntegerSize))
        assert(eqIntegerBudget.steps < eqDataBudget.steps)
        assert(eqIntegerBudget.memory > eqDataBudget.memory)
        assert(eqIntegerBudget.fee(prices) < eqDataBudget.fee(prices))
    }

    test("equalsData vs equalsInteger — both sides Data") {
        val eqData = PlutusV3.compile { (d1: Data, d2: Data) => equalsData(d1, d2) }
        val eqInteger = PlutusV3.compile { (d1: Data, d2: Data) =>
            equalsInteger(unIData(d1), unIData(d2))
        }
        val d1 = iData(42)
        val d2 = iData(42)
        val eqDataBudget = (eqData.program $ d1 $ d2).evaluateDebug.budget
        val eqIntegerBudget =
            (eqInteger.program $ d1 $ d2).evaluateDebug.budget
        val eqDataSize = eqData.program.cborEncoded.length
        val eqIntegerSize = eqInteger.program.cborEncoded.length
        info(formatLine("equalsData", eqDataBudget, eqDataSize))
        info(formatLine("equalsInteger", eqIntegerBudget, eqIntegerSize))
        // equalsInteger uses fewer steps but more memory; with optimized UPLC equalsData wins on exFee
        assert(eqIntegerBudget.steps < eqDataBudget.steps)
        assert(eqIntegerBudget.memory > eqDataBudget.memory)
        assert(eqIntegerBudget.fee(prices) > eqDataBudget.fee(prices))
    }

    test("equalsData vs equalsByteString for 32-byte ByteString — one side constant") {
        val bs32 = hex"0102030405060708091011121314151617181920212223242526272829303132"
        val eqData = PlutusV3.compile { (d: Data) =>
            equalsData(
              d,
              bData(hex"0102030405060708091011121314151617181920212223242526272829303132")
            )
        }
        val eqBs = PlutusV3.compile { (d: Data) =>
            equalsByteString(
              unBData(d),
              hex"0102030405060708091011121314151617181920212223242526272829303132"
            )
        }
        val d = bData(bs32)
        val eqDataBudget = (eqData.program $ d).evaluateDebug.budget
        val eqBsBudget = (eqBs.program $ d).evaluateDebug.budget
        val eqDataSize = eqData.program.cborEncoded.length
        val eqBsSize = eqBs.program.cborEncoded.length
        info(formatLine("equalsData", eqDataBudget, eqDataSize))
        info(formatLine("equalsByteString", eqBsBudget, eqBsSize))
        assert(eqBsBudget.steps < eqDataBudget.steps)
        assert(eqBsBudget.memory > eqDataBudget.memory)
        assert(eqBsBudget.fee(prices) < eqDataBudget.fee(prices))
    }

    test("equalsData vs equalsByteString for 32-byte ByteString — both sides Data") {
        val bs32 = hex"0102030405060708091011121314151617181920212223242526272829303132"
        val eqData = PlutusV3.compile { (d1: Data, d2: Data) => equalsData(d1, d2) }
        val eqBs = PlutusV3.compile { (d1: Data, d2: Data) =>
            equalsByteString(unBData(d1), unBData(d2))
        }
        val d1 = bData(bs32)
        val d2 = bData(bs32)
        val eqDataBudget = (eqData.program $ d1 $ d2).evaluateDebug.budget
        val eqBsBudget = (eqBs.program $ d1 $ d2).evaluateDebug.budget
        val eqDataSize = eqData.program.cborEncoded.length
        val eqBsSize = eqBs.program.cborEncoded.length
        info(formatLine("equalsData", eqDataBudget, eqDataSize))
        info(formatLine("equalsByteString", eqBsBudget, eqBsSize))
        // equalsByteString uses fewer steps but more memory; with optimized UPLC the fee margin is thin
        assert(eqBsBudget.steps < eqDataBudget.steps)
        assert(eqBsBudget.memory > eqDataBudget.memory)
        assert(eqBsBudget.fee(prices) < eqDataBudget.fee(prices))
    }

    test("equalsData vs === for TxOutRef") {
        val eqData = PlutusV3.compile { (d1: Data, d2: Data) => equalsData(d1, d2) }
        val eqFields = PlutusV3.compile { (a: TxOutRef, b: TxOutRef) => a === b }
        val ref = TxOutRef(
          TxId(hex"0102030405060708091011121314151617181920212223242526272829303132"),
          BigInt(0)
        )

        val eqDataBudget = (eqData.program $ ref.toData $ ref.toData).evaluateDebug.budget
        val eqFieldsBudget = (eqFields.program $ ref.toData $ ref.toData).evaluateDebug.budget
        val eqDataSize = eqData.program.cborEncoded.length
        val eqFieldsSize = eqFields.program.cborEncoded.length
        info(formatLine("equalsData", eqDataBudget, eqDataSize))
        info(formatLine("===", eqFieldsBudget, eqFieldsSize))
        assert(eqFieldsBudget.steps < eqDataBudget.steps)
        assert(eqFieldsBudget.memory > eqDataBudget.memory)
        assert(eqFieldsBudget.fee(prices) > eqDataBudget.fee(prices))
    }

    test("equalsData vs === for Address") {
        val eqData = PlutusV3.compile { (d1: Data, d2: Data) => equalsData(d1, d2) }
        val eqFields = PlutusV3.compile { (a: Address, b: Address) => a === b }
        val addr = Address(
          Credential.PubKeyCredential(
            PubKeyHash(hex"01020304050607080910111213141516171819202122232425262728")
          ),
          scalus.cardano.onchain.plutus.prelude.Option.None
        )
        val eqDataBudget = (eqData.program $ addr.toData $ addr.toData).evaluateDebug.budget
        val eqFieldsBudget = (eqFields.program $ addr.toData $ addr.toData).evaluateDebug.budget
        val eqDataSize = eqData.program.cborEncoded.length
        val eqFieldsSize = eqFields.program.cborEncoded.length
        info(formatLine("equalsData", eqDataBudget, eqDataSize))
        info(formatLine("===", eqFieldsBudget, eqFieldsSize))
        assert(eqFieldsBudget.steps < eqDataBudget.steps)
        assert(eqFieldsBudget.memory > eqDataBudget.memory)
        assert(eqFieldsBudget.fee(prices) > eqDataBudget.fee(prices))
    }

    test("equalsData vs === for OutputDatum") {
        val eqData = PlutusV3.compile { (d1: Data, d2: Data) => equalsData(d1, d2) }
        val eqFields = PlutusV3.compile { (a: OutputDatum, b: OutputDatum) => a === b }
        val datum = OutputDatum.NoOutputDatum
        val eqDataBudget = (eqData.program $ datum.toData $ datum.toData).evaluateDebug.budget
        val eqFieldsBudget =
            (eqFields.program $ datum.toData $ datum.toData).evaluateDebug.budget
        val eqDataSize = eqData.program.cborEncoded.length
        val eqFieldsSize = eqFields.program.cborEncoded.length
        info(formatLine("equalsData", eqDataBudget, eqDataSize))
        info(formatLine("===", eqFieldsBudget, eqFieldsSize))
        // With V3 lowering, equalsData is cheaper for enum types
        assert(eqDataBudget.steps < eqFieldsBudget.steps)
        assert(eqDataBudget.memory < eqFieldsBudget.memory)
        assert(eqDataBudget.fee(prices) < eqFieldsBudget.fee(prices))
    }

    test("equalsData vs === for Value") {
        val eqData = PlutusV3.compile { (d1: Data, d2: Data) => equalsData(d1, d2) }
        val eqFields = PlutusV3.compile { (a: Value, b: Value) => a === b }
        val value = Value.lovelace(BigInt(2_000_000))
        val eqDataBudget = (eqData.program $ value.toData $ value.toData).evaluateDebug.budget
        val eqFieldsBudget =
            (eqFields.program $ value.toData $ value.toData).evaluateDebug.budget
        val eqDataSize = eqData.program.cborEncoded.length
        val eqFieldsSize = eqFields.program.cborEncoded.length
        info(formatLine("equalsData", eqDataBudget, eqDataSize))
        info(formatLine("===", eqFieldsBudget, eqFieldsSize))
        // With V3 lowering, equalsData is cheaper for recursive map types
        assert(eqDataBudget.steps < eqFieldsBudget.steps)
    }

    test("equalsData vs === for v1.TxOut (3 fields)") {
        val eqData = PlutusV3.compile { (d1: Data, d2: Data) => equalsData(d1, d2) }
        val eqFields =
            PlutusV3.compile { (a: plutus.v1.TxOut, b: plutus.v1.TxOut) => a === b }
        val txOut = plutus.v1.TxOut(
          Address(
            Credential.PubKeyCredential(
              PubKeyHash(hex"01020304050607080910111213141516171819202122232425262728")
            ),
            scalus.cardano.onchain.plutus.prelude.Option.None
          ),
          Value.lovelace(BigInt(2_000_000)),
          scalus.cardano.onchain.plutus.prelude.Option.None
        )
        val eqDataBudget = (eqData.program $ txOut.toData $ txOut.toData).evaluateDebug.budget
        val eqFieldsBudget =
            (eqFields.program $ txOut.toData $ txOut.toData).evaluateDebug.budget
        val eqDataSize = eqData.program.cborEncoded.length
        val eqFieldsSize = eqFields.program.cborEncoded.length
        info(formatLine("equalsData", eqDataBudget, eqDataSize))
        info(formatLine("===", eqFieldsBudget, eqFieldsSize))
        assert(eqFieldsBudget.steps < eqDataBudget.steps)
        assert(eqFieldsBudget.memory > eqDataBudget.memory)
        assert(eqFieldsBudget.fee(prices) > eqDataBudget.fee(prices))
    }

    test("equalsData vs === for v2.TxOut (4 fields)") {
        val eqData = PlutusV3.compile { (d1: Data, d2: Data) => equalsData(d1, d2) }
        val eqFields =
            PlutusV3.compile { (a: plutus.v2.TxOut, b: plutus.v2.TxOut) => a === b }
        val txOut = plutus.v2.TxOut(
          Address(
            Credential.PubKeyCredential(
              PubKeyHash(hex"01020304050607080910111213141516171819202122232425262728")
            ),
            scalus.cardano.onchain.plutus.prelude.Option.None
          ),
          Value.lovelace(BigInt(2_000_000)),
          OutputDatum.NoOutputDatum,
          scalus.cardano.onchain.plutus.prelude.Option.None
        )
        val eqDataBudget = (eqData.program $ txOut.toData $ txOut.toData).evaluateDebug.budget
        val eqFieldsBudget =
            (eqFields.program $ txOut.toData $ txOut.toData).evaluateDebug.budget
        val eqDataSize = eqData.program.cborEncoded.length
        val eqFieldsSize = eqFields.program.cborEncoded.length
        info(formatLine("equalsData", eqDataBudget, eqDataSize))
        info(formatLine("===", eqFieldsBudget, eqFieldsSize))
        assert(eqFieldsBudget.steps < eqDataBudget.steps)
        assert(eqFieldsBudget.memory > eqDataBudget.memory)
        assert(eqFieldsBudget.fee(prices) > eqDataBudget.fee(prices))
    }

    test("equalsData vs === for TxInInfo (6 leaf fields: TxOutRef + v2.TxOut)") {
        val eqData = PlutusV3.compile { (d1: Data, d2: Data) => equalsData(d1, d2) }
        val eqFields = PlutusV3.compile { (a: TxInInfo, b: TxInInfo) => a === b }
        val txInInfo = TxInInfo(
          TxOutRef(
            TxId(hex"0102030405060708091011121314151617181920212223242526272829303132"),
            BigInt(0)
          ),
          plutus.v2.TxOut(
            Address(
              Credential.PubKeyCredential(
                PubKeyHash(hex"01020304050607080910111213141516171819202122232425262728")
              ),
              scalus.cardano.onchain.plutus.prelude.Option.None
            ),
            Value.lovelace(BigInt(2_000_000)),
            OutputDatum.NoOutputDatum,
            scalus.cardano.onchain.plutus.prelude.Option.None
          )
        )
        val eqDataBudget =
            (eqData.program $ txInInfo.toData $ txInInfo.toData).evaluateDebug.budget
        val eqFieldsBudget =
            (eqFields.program $ txInInfo.toData $ txInInfo.toData).evaluateDebug.budget
        val eqDataSize = eqData.program.cborEncoded.length
        val eqFieldsSize = eqFields.program.cborEncoded.length
        info(formatLine("equalsData", eqDataBudget, eqDataSize))
        info(formatLine("===", eqFieldsBudget, eqFieldsSize))
        assert(eqFieldsBudget.steps < eqDataBudget.steps)
        assert(eqFieldsBudget.memory > eqDataBudget.memory)
        assert(eqFieldsBudget.fee(prices) > eqDataBudget.fee(prices))
    }

    test("forAll (TxOutRef, Address): === vs equalsData") {
        import scalus.cardano.onchain.plutus.v3.ArbitraryInstances.given
        val eqData = PlutusV3.compile { (d1: Data, d2: Data) => equalsData(d1, d2) }
        val eqFields =
            PlutusV3.compile { (a: (TxOutRef, Address), b: (TxOutRef, Address)) => a === b }
        info(
          "===.steps < equalsData.steps, ===.memory > equalsData.memory, equalsData.fee < ===.fee"
        )
        forAll { (a: (TxOutRef, Address), b: (TxOutRef, Address)) =>
            val eqDataBudget =
                (eqData.program $ a.toData $ b.toData).evaluateDebug.budget
            val eqFieldsBudget =
                (eqFields.program $ a.toData $ b.toData).evaluateDebug.budget
            assert(eqFieldsBudget.steps < eqDataBudget.steps)
            assert(eqFieldsBudget.memory > eqDataBudget.memory)
            assert(eqFieldsBudget.fee(prices) > eqDataBudget.fee(prices))
        }
    }

    test("forAll (TxOutRef, (Address, Credential)): === vs equalsData") {
        import scalus.cardano.onchain.plutus.v3.ArbitraryInstances.given
        val eqData = PlutusV3.compile { (d1: Data, d2: Data) => equalsData(d1, d2) }
        val eqFields = PlutusV3.compile {
            (a: (TxOutRef, (Address, Credential)), b: (TxOutRef, (Address, Credential))) =>
                a === b
        }
        info(
          "===.steps < equalsData.steps, ===.memory > equalsData.memory, equalsData.fee < ===.fee"
        )
        forAll { (a: (TxOutRef, (Address, Credential)), b: (TxOutRef, (Address, Credential))) =>
            val eqDataBudget =
                (eqData.program $ a.toData $ b.toData).evaluateDebug.budget
            val eqFieldsBudget =
                (eqFields.program $ a.toData $ b.toData).evaluateDebug.budget
            assert(eqFieldsBudget.steps < eqDataBudget.steps)
            assert(eqFieldsBudget.memory > eqDataBudget.memory)
            assert(eqFieldsBudget.fee(prices) > eqDataBudget.fee(prices))
        }
    }

    test("forAll BigInt: equalsInteger vs equalsData") {
        val eqData = PlutusV3.compile { (d1: Data, d2: Data) => equalsData(d1, d2) }
        val eqInteger = PlutusV3.compile { (d1: Data, d2: Data) =>
            equalsInteger(unIData(d1), unIData(d2))
        }
        info("equalsInteger.steps < equalsData.steps, equalsInteger.memory > equalsData.memory")
        forAll(Gen.choose(-1000000L, 1000000L), Gen.choose(-1000000L, 1000000L)) {
            (a: Long, b: Long) =>
                val eqDataBudget =
                    (eqData.program $ iData(a) $ iData(b)).evaluateDebug.budget
                val eqIntegerBudget =
                    (eqInteger.program $ iData(a) $ iData(b)).evaluateDebug.budget
                assert(eqIntegerBudget.steps < eqDataBudget.steps)
                assert(eqIntegerBudget.memory > eqDataBudget.memory)
        }
    }

    test("forAll ByteString(32): equalsByteString vs equalsData") {
        val eqData = PlutusV3.compile { (d1: Data, d2: Data) => equalsData(d1, d2) }
        val eqBs = PlutusV3.compile { (d1: Data, d2: Data) =>
            equalsByteString(unBData(d1), unBData(d2))
        }
        val gen32Bytes = Gen
            .containerOfN[Array, Byte](32, Gen.choose(Byte.MinValue, Byte.MaxValue))
            .map(a => ByteString.unsafeFromArray(a))
        info(
          "equalsByteString.steps < equalsData.steps, equalsByteString.memory > equalsData.memory, equalsByteString.fee < equalsData.fee"
        )
        forAll(gen32Bytes, gen32Bytes) { (a: ByteString, b: ByteString) =>
            val eqDataBudget =
                (eqData.program $ bData(a) $ bData(b)).evaluateDebug.budget
            val eqBsBudget =
                (eqBs.program $ bData(a) $ bData(b)).evaluateDebug.budget
            assert(eqBsBudget.steps < eqDataBudget.steps)
            assert(eqBsBudget.memory > eqDataBudget.memory)
            assert(eqBsBudget.fee(prices) < eqDataBudget.fee(prices))
        }
    }

    test("forAll TxOutRef: === vs equalsData") {
        import scalus.cardano.onchain.plutus.v3.ArbitraryInstances.given
        val eqData = PlutusV3.compile { (d1: Data, d2: Data) => equalsData(d1, d2) }
        val eqFields = PlutusV3.compile { (a: TxOutRef, b: TxOutRef) => a === b }
        info(
          "===.steps < equalsData.steps, ===.memory > equalsData.memory, equalsData.fee < ===.fee"
        )
        forAll { (a: TxOutRef, b: TxOutRef) =>
            val eqDataBudget =
                (eqData.program $ a.toData $ b.toData).evaluateDebug.budget
            val eqFieldsBudget =
                (eqFields.program $ a.toData $ b.toData).evaluateDebug.budget
            assert(eqFieldsBudget.steps < eqDataBudget.steps)
            assert(eqFieldsBudget.memory > eqDataBudget.memory)
            assert(eqFieldsBudget.fee(prices) > eqDataBudget.fee(prices))
        }
    }

    test("forAll Address: === vs equalsData") {
        import scalus.cardano.onchain.plutus.v3.ArbitraryInstances.given
        val eqData = PlutusV3.compile { (d1: Data, d2: Data) => equalsData(d1, d2) }
        val eqFields = PlutusV3.compile { (a: Address, b: Address) => a === b }
        info(
          "===.steps < equalsData.steps, ===.memory > equalsData.memory, equalsData.fee < ===.fee"
        )
        forAll { (a: Address, b: Address) =>
            val eqDataBudget =
                (eqData.program $ a.toData $ b.toData).evaluateDebug.budget
            val eqFieldsBudget =
                (eqFields.program $ a.toData $ b.toData).evaluateDebug.budget
            assert(eqFieldsBudget.steps < eqDataBudget.steps)
            assert(eqFieldsBudget.memory > eqDataBudget.memory)
            assert(eqFieldsBudget.fee(prices) > eqDataBudget.fee(prices))
        }
    }

    test("forAll OutputDatum: === vs equalsData") {
        import scalus.cardano.onchain.plutus.v3.ArbitraryInstances.given
        val eqData = PlutusV3.compile { (d1: Data, d2: Data) => equalsData(d1, d2) }
        val eqFields = PlutusV3.compile { (a: OutputDatum, b: OutputDatum) => a === b }
        info(
          "equalsData.steps < ===.steps, equalsData.memory < ===.memory, equalsData.fee < ===.fee (V3 lowering)"
        )
        forAll { (a: OutputDatum, b: OutputDatum) =>
            val eqDataBudget =
                (eqData.program $ a.toData $ b.toData).evaluateDebug.budget
            val eqFieldsBudget =
                (eqFields.program $ a.toData $ b.toData).evaluateDebug.budget
            assert(eqDataBudget.steps < eqFieldsBudget.steps)
            assert(eqDataBudget.memory < eqFieldsBudget.memory)
            assert(eqDataBudget.fee(prices) < eqFieldsBudget.fee(prices))
        }
    }
}
