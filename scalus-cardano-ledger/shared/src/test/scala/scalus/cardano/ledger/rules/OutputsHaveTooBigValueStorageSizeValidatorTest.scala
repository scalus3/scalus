package scalus.cardano.ledger
package rules

import org.scalatest.funsuite.AnyFunSuite
import scalus.serialization.cbor.Cbor
import scalus.uplc.builtin.ByteString

import scala.collection.immutable.SortedMap

class OutputsHaveTooBigValueStorageSizeValidatorTest extends AnyFunSuite, ValidatorRulesTestKit {

    /** `policies` policies of `assetsPerPolicy` assets each. Each asset name has 32 bytes and each
      * amount 5 bytes, so 10 policies of 100 assets serialize to about 39 000 bytes, far over the
      * `maxValueSize` of `Context()`.
      */
    private def multiAsset(policies: Int, assetsPerPolicy: Int): MultiAsset =
        MultiAsset.from(
          for
              p <- 1 to policies
              a <- 1 to assetsPerPolicy
          yield (scriptHash(p), AssetName(ByteString.fromHex(f"$a%064x")), 1_000_000L)
        )

    // The rule reads only the outputs and the collateral return output.
    private def tx(
        outputs: IndexedSeq[TransactionOutput],
        collateralReturnOutput: Option[TransactionOutput]
    ): Transaction =
        txSpending(input(0))(TransactionWitnessSet.empty).withBody(
          _.copy(
            outputs = outputs.map(Sized(_)),
            collateralReturnOutput = collateralReturnOutput.map(Sized(_))
          )
        )

    private val fitting: TransactionOutput =
        Output(keyAddress(0), Value(Coin.ada(2), multiAsset(2, 2)))

    private val tooBig: TransactionOutput =
        Output(keyAddress(0), Value(Coin(1L), multiAsset(10, 100)))

    test("OutputsHaveTooBigValueStorageSizeValidator TransactionOutputs success") {
        val transaction = tx(IndexedSeq(fitting), None)

        val result = validate(OutputsHaveTooBigValueStorageSizeValidator, Map.empty, transaction)
        assert(result.isRight)
    }

    test("OutputsHaveTooBigValueStorageSizeValidator TransactionOutputs failure") {
        val transaction = tx(IndexedSeq(tooBig), None)

        val result = validate(OutputsHaveTooBigValueStorageSizeValidator, Map.empty, transaction)
        assert(result.isLeft)
    }

    test("OutputsHaveTooBigValueStorageSizeValidator CollateralReturnOutput success") {
        val transaction = tx(IndexedSeq.empty, Some(fitting))

        val result = validate(OutputsHaveTooBigValueStorageSizeValidator, Map.empty, transaction)
        assert(result.isRight)
    }

    test("OutputsHaveTooBigValueStorageSizeValidator CollateralReturnOutput failure") {
        val transaction = tx(IndexedSeq.empty, Some(tooBig))

        val result = validate(OutputsHaveTooBigValueStorageSizeValidator, Map.empty, transaction)
        assert(result.isLeft)
    }

    // spec [D115]: the ledger measures `serialize (pvMajor protVer) v`
    // (eras/alonzo/impl/src/Cardano/Ledger/Alonzo/Rules/Utxo.hs:444-445), and its Map encoder
    // writes a map of more than 23 entries with an indefinite length, a 2-byte head
    // (libs/cardano-ledger-binary/src/Cardano/Ledger/Binary/Encoding/Encoder.hs:395-425)
    private def valueWithAssets(count: Int): Value = Value(
      Coin(2_000_000L),
      MultiAsset.fromPolicy(
        scriptHash(1),
        (0 until count).map(i =>
            AssetName(ByteString.fromArray(Array(i.toByte, (i >> 8).toByte))) -> 1L
        )
      )
    )

    private def validateAt(value: Value, maxValueSize: Long, asCollateral: Boolean) = {
        val context = Context.testMainnet()
        val params = context.env.params.copy(maxValueSize = maxValueSize)
        val output: TransactionOutput = Output(keyAddress(0), value)
        val transaction =
            if asCollateral then tx(IndexedSeq.empty, Some(output))
            else tx(IndexedSeq(output), None)
        OutputsHaveTooBigValueStorageSizeValidator.validate(
          context.copy(env = context.env.copy(params = params)),
          State(),
          transaction
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
              (0 until 256).map(i => scriptHash(i) -> SortedMap(AssetName.empty -> 1L))
            )
          )
        )
        assert(value.assets.assets.size == 256)
        assertBoundary(value, Cbor.encode(value).length - 1)
    }
}
