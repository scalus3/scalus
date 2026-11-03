package scalus.cardano.ledger
package rules

import org.scalacheck.Arbitrary
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.ShelleyAddress
import scalus.serialization.cbor.Cbor
import scalus.uplc.builtin.ByteString

import scala.collection.immutable.SortedMap

class OutputsHaveTooBigValueStorageSizeValidatorTest extends AnyFunSuite, ValidatorRulesTestKit {
    test("OutputsHaveTooBigValueStorageSizeValidator TransactionOutputs success") {
        val context = Context()

        val output = Arbitrary.arbitrary[TransactionOutput].sample.get

        val transaction = {
            val tx = randomTransactionWithIsValidField
            tx.copy(
              body = KeepRaw(
                tx.body.value.copy(
                  outputs = IndexedSeq(Sized(output)),
                  collateralReturnOutput = None
                )
              )
            )
        }

        val state = State()

        val result =
            OutputsHaveTooBigValueStorageSizeValidator.validate(context, state, transaction)
        assert(result.isRight)
    }

    test("OutputsHaveTooBigValueStorageSizeValidator TransactionOutputs failure") {
        val context = Context()

        val output = Output(
          Arbitrary.arbitrary[ShelleyAddress].sample.get,
          Value(
            Coin(1L),
            genMultiAsset(
              minPolicies = 10,
              maxPolicies = 10,
              minAssets = 100,
              maxAssets = 100
            ).sample.get
          )
        )

        val transaction = {
            val tx = randomTransactionWithIsValidField
            tx.copy(
              body = KeepRaw(
                tx.body.value.copy(
                  outputs = IndexedSeq(Sized(output)),
                  collateralReturnOutput = None
                )
              )
            )
        }

        val state = State()

        val result =
            OutputsHaveTooBigValueStorageSizeValidator.validate(context, state, transaction)
        assert(result.isLeft)
    }

    test("OutputsHaveTooBigValueStorageSizeValidator CollateralReturnOutput success") {
        val context = Context()

        val collateralReturnOutput = Arbitrary.arbitrary[TransactionOutput].sample.get

        val transaction = {
            val tx = randomTransactionWithIsValidField
            tx.copy(
              body = KeepRaw(
                tx.body.value.copy(
                  outputs = IndexedSeq.empty,
                  collateralReturnOutput = Some(Sized(collateralReturnOutput))
                )
              )
            )
        }

        val state = State()

        val result =
            OutputsHaveTooBigValueStorageSizeValidator.validate(context, state, transaction)
        assert(result.isRight)
    }

    test("OutputsHaveTooBigValueStorageSizeValidator CollateralReturnOutput failure") {
        val context = Context()

        val collateralReturnOutput = Output(
          Arbitrary.arbitrary[ShelleyAddress].sample.get,
          Value(
            Coin(1L),
            genMultiAsset(
              minPolicies = 10,
              maxPolicies = 10,
              minAssets = 100,
              maxAssets = 100
            ).sample.get
          )
        )

        val transaction = {
            val tx = randomTransactionWithIsValidField
            tx.copy(
              body = KeepRaw(
                tx.body.value.copy(
                  outputs = IndexedSeq.empty,
                  collateralReturnOutput = Some(Sized(collateralReturnOutput))
                )
              )
            )
        }

        val state = State()

        val result =
            OutputsHaveTooBigValueStorageSizeValidator.validate(context, state, transaction)
        assert(result.isLeft)
    }

    // spec [D115]: the ledger measures `serialize (pvMajor protVer) v`
    // (eras/alonzo/impl/src/Cardano/Ledger/Alonzo/Rules/Utxo.hs:444-445), and its Map encoder
    // writes a map of more than 23 entries with an indefinite length, a 2-byte head
    // (libs/cardano-ledger-binary/src/Cardano/Ledger/Binary/Encoding/Encoder.hs:395-425)
    private def valueWithAssets(count: Int): Value = Value(
      Coin(2_000_000L),
      MultiAsset.fromPolicy(
        Arbitrary.arbitrary[PolicyId].sample.get,
        (0 until count).map(i =>
            AssetName(ByteString.fromArray(Array(i.toByte, (i >> 8).toByte))) -> 1L
        )
      )
    )

    private def validateAt(value: Value, maxValueSize: Long, asCollateral: Boolean) = {
        val context = Context.testMainnet()
        val params = context.env.params.copy(maxValueSize = maxValueSize)
        val output = Sized(
          Output(Arbitrary.arbitrary[ShelleyAddress].sample.get, value): TransactionOutput
        )
        val tx = randomTransactionWithIsValidField
        val body = tx.body.value.copy(
          outputs = if asCollateral then IndexedSeq.empty else IndexedSeq(output),
          collateralReturnOutput = if asCollateral then Some(output) else None
        )
        OutputsHaveTooBigValueStorageSizeValidator.validate(
          context.copy(env = context.env.copy(params = params)),
          State(),
          tx.copy(body = KeepRaw(body))
        )
    }

    private def reportedSize(
        result: Either[TransactionException, Unit],
        asCollateral: Boolean
    ): Int = result match
        case Left(e: TransactionException.OutputsHaveTooBigValueStorageSizeException) =>
            if asCollateral then e.invalidCollateralOutput.get._2 else e.invalidOutputs.head._2
        case other => fail(s"expected OutputsHaveTooBigValueStorageSizeException, got $other")

    private def assertBoundary(value: Value, ledgerSize: Int): Unit =
        for asCollateral <- Seq(false, true) do
            assert(validateAt(value, ledgerSize, asCollateral).isRight, s"collateral $asCollateral")
            val result = validateAt(value, ledgerSize - 1, asCollateral)
            assert(reportedSize(result, asCollateral) == ledgerSize, s"collateral $asCollateral")

    test(
      "a pure-ADA value is measured as the bare coin, as the ledger's MaryValue encoder writes it"
    ) {
        // spec [D101]: eras/mary/impl/src/Cardano/Ledger/Mary/Value.hs:334-342
        val value = Value(Coin(2_000_000L))
        assert(Cbor.encode(value).toSeq == Cbor.encode(Coin(2_000_000L)).toSeq)
        assertBoundary(value, 5)
    }

    test("a value of 255 assets under one policy fits exactly at the ledger's serialized size") {
        // spec [D115]: 24 to 255 entries take a 2-byte head, definite or indefinite
        val value = valueWithAssets(255)
        assertBoundary(value, Cbor.encode(value).length)
    }

    test("a policy of 256 assets is measured with the ledger's indefinite-length map head") {
        // spec [D115]: the definite head of 256 entries is 3 bytes, the ledger's indefinite one 2
        // ledger: 1 + 5 + 1 + 30 + 2 + 256 * 4 = 1063 bytes (section 5.3 example)
        val value = valueWithAssets(256)
        assert(Cbor.encode(value).length == 1064)
        assertBoundary(value, 1063)
    }

    test("a policy of 65 536 assets is measured with the ledger's indefinite-length map head") {
        // spec [D115]: the definite head of 65 536 entries is 5 bytes, the ledger's indefinite one
        // 2; no tx within maxTxSize holds such an output, so only this direct check reaches it
        val value = valueWithAssets(65536)
        assertBoundary(value, Cbor.encode(value).length - 3)
    }

    test("256 policies are measured with the ledger's indefinite-length map head") {
        // spec [D115]: the outer map follows the same rule
        val value = Value(
          Coin(2_000_000L),
          MultiAsset(
            SortedMap.from(
              (0 until 256).map(_ =>
                  Arbitrary.arbitrary[PolicyId].sample.get -> SortedMap(AssetName.empty -> 1L)
              )
            )
          )
        )
        assert(value.assets.assets.size == 256)
        assertBoundary(value, Cbor.encode(value).length - 1)
    }
}
