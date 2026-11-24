package scalus.cardano.ledger
package rules

import org.scalatest.funsuite.AnyFunSuite

class OutputsHaveNotEnoughCoinsValidatorTest extends AnyFunSuite, ValidatorRulesTestKit {

    /** 2 policies of 2 assets each: a multi-asset part raises the minimum ada of an output. */
    private val assets = MultiAsset.from(
      (scriptHash(1), AssetName.fromString("a"), 1L),
      (scriptHash(1), AssetName.fromString("b"), 2L),
      (scriptHash(2), AssetName.fromString("a"), 3L),
      (scriptHash(2), AssetName.fromString("b"), 4L)
    )

    private val negativeAsset = MultiAsset.asset(scriptHash(1), AssetName.fromString("a"), -1L)

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

    test("OutputsHaveNotEnoughCoinsValidator TransactionOutputs success") {
        val output = Output(keyAddress(0), Value(Coin(1000000000L), assets))

        val transaction = tx(IndexedSeq(output), None)

        val result = validate(OutputsHaveNotEnoughCoinsValidator, Map.empty, transaction)
        assert(result.isRight)
    }

    test("OutputsHaveNotEnoughCoinsValidator TransactionOutputs failure for minimum ada") {
        val output = Output(keyAddress(0), Value(Coin(1L)))

        val transaction = tx(IndexedSeq(output), None)

        val result = validate(OutputsHaveNotEnoughCoinsValidator, Map.empty, transaction)
        assert(result.isLeft)
    }

    test("OutputsHaveNotEnoughCoinsValidator TransactionOutputs failure for negative assets") {
        val output = Output(keyAddress(0), Value(Coin(1000000000L), negativeAsset))

        val transaction = tx(IndexedSeq(output), None)

        val result = validate(OutputsHaveNotEnoughCoinsValidator, Map.empty, transaction)
        assert(result.isLeft)
    }

    test("OutputsHaveNotEnoughCoinsValidator CollateralReturnOutput success") {
        val collateralReturnOutput = Output(keyAddress(0), Value(Coin(1000000000L), assets))

        val transaction = tx(IndexedSeq.empty, Some(collateralReturnOutput))

        val result = validate(OutputsHaveNotEnoughCoinsValidator, Map.empty, transaction)
        assert(result.isRight)
    }

    test("OutputsHaveNotEnoughCoinsValidator CollateralReturnOutput failure for minimum ada") {
        val collateralReturnOutput = Output(keyAddress(0), Value(Coin(1L)))

        val transaction = tx(IndexedSeq.empty, Some(collateralReturnOutput))

        val result = validate(OutputsHaveNotEnoughCoinsValidator, Map.empty, transaction)
        assert(result.isLeft)
    }

    test("OutputsHaveNotEnoughCoinsValidator CollateralReturnOutput failure for negative assets") {
        val collateralReturnOutput = Output(keyAddress(0), Value(Coin(1000000000L), negativeAsset))

        val transaction = tx(IndexedSeq.empty, Some(collateralReturnOutput))

        val result = validate(OutputsHaveNotEnoughCoinsValidator, Map.empty, transaction)
        assert(result.isLeft)
    }
}
