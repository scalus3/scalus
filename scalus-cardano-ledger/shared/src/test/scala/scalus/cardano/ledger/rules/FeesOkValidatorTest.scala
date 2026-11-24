package scalus.cardano.ledger
package rules

import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.builtin.Data

class FeesOkValidatorTest extends AnyFunSuite, ValidatorRulesTestKit {

    // A non-empty token, for the tests where the collateral holds or returns more than ada.
    private val token = MultiAsset.asset(scriptHash(1), AssetName.fromString("token"), 100)

    // Within maxTxExecutionUnits, so the script part of the min fee stays far below the 10 ada fee.
    private val redeemer =
        Redeemer(RedeemerTag.Spend, 0, Data.unit, ExUnits(1_000_000, 1_000_000_000))

    /** A transaction with only the fields that FeesOkValidator reads. It spends no input: the min
      * fee resolves the inputs against the UTxO, and an unresolved input fails the rule for another
      * reason. The rule computes the min fee from this transaction.
      */
    private def collateralTx(
        collateralInputs: Seq[TransactionInput],
        collateralReturn: Option[TransactionOutput],
        totalCollateral: Option[Coin],
        fee: Coin = Coin.ada(10),
        redeemers: Seq[Redeemer] = Seq(redeemer)
    ): Transaction =
        Transaction(
          emptyBody.copy(
            fee = fee,
            collateralInputs = TaggedSortedSet.from(collateralInputs),
            collateralReturnOutput = collateralReturn.map(output => Sized(output)),
            totalCollateral = totalCollateral
          ),
          TransactionWitnessSet.empty.copy(redeemers =
              Option.when(redeemers.nonEmpty)(KeepRaw(Redeemers.Array(redeemers.toIndexedSeq)))
          )
        )

    /** The UTxOs of the collateral inputs `input(1)` and `input(2)`, at key addresses. */
    private def collateralUtxos(
        first: Value = Value.ada(30),
        second: Value = Value.ada(30)
    ): Utxos =
        Map(input(1) -> Output(keyAddress(1), first), input(2) -> Output(keyAddress(2), second))

    test("FeesOkValidator rule success") {
        val transaction = collateralTx(
          Seq(input(1), input(2)),
          Some(Output(keyAddress(3), Value(Coin.ada(20), token))),
          // totalCollateral = net collateral (inputs - return)
          // 30000000 + 30000000 - 20000000 = 40000000
          Some(Coin(40000000L))
        )
        // The return takes the token back, so only ada stays in the collateral.
        val utxos = collateralUtxos(second = Value(Coin.ada(30), token))

        val result = validate(FeesOkValidator, utxos, transaction)
        assert(result.isRight)
    }

    test(
      "FeesOkValidator If the total ExUnits are 0 in both Memory and Steps, no further part needs to be checked"
    ) {
        // No collateral input: the rule would fail at part 7 if it checked further.
        val transaction = collateralTx(Seq.empty, None, None, redeemers = Seq.empty)

        val result = validate(FeesOkValidator, Map.empty, transaction)
        assert(result.isRight)
    }

    test("FeesOkValidator feePaidIsGreeterOrEqualThanMinimumFee rule failure") {
        val transaction = collateralTx(
          Seq(input(1), input(2)),
          Some(Output(keyAddress(3), Value(Coin.ada(20)))),
          Some(Coin.ada(40)),
          fee = Coin(1L)
        )
        val utxos = collateralUtxos()

        val result = validate(FeesOkValidator, utxos, transaction)
        assert(result.isLeft)
    }

    test("FeesOkValidator collateralConsistsOnlyOfVKeyAddress rule failure") {
        val transaction = collateralTx(Seq(input(1), input(2)), None, None)
        val utxos = Map(
          input(1) -> Output(keyAddress(1), Value(Coin.ada(20))),
          input(2) -> Output(scriptAddress(scriptHash(2)), Value(Coin.ada(20)))
        )

        val result = validate(FeesOkValidator, utxos, transaction)
        assert(result.isLeft)
    }

    test(
      "FeesOkValidator collateralDoesNotContainAnyNonADA rule failure when collateralReturnOutput has more tokens than collaterals"
    ) {
        val transaction = collateralTx(
          Seq(input(1), input(2)),
          Some(Output(keyAddress(3), Value(Coin.ada(20), token))),
          Some(Coin.ada(40))
        )
        val utxos = collateralUtxos()

        val result = validate(FeesOkValidator, utxos, transaction)
        assert(result.isLeft)
    }

    test(
      "FeesOkValidator collateralDoesNotContainAnyNonADA rule failure when collateralReturnOutput has less tokens than collaterals"
    ) {
        val transaction = collateralTx(
          Seq(input(1), input(2)),
          Some(Output(keyAddress(3), Value(Coin.ada(20)))),
          Some(Coin.ada(40))
        )
        val utxos = collateralUtxos(second = Value(Coin.ada(30), token))

        val result = validate(FeesOkValidator, utxos, transaction)
        assert(result.isLeft)
    }

    test("FeesOkValidator totalSumOfCollateralCoinsIsSufficient rule failure") {
        // The return takes all 60 ada back, so the net collateral is 0.
        val transaction = collateralTx(
          Seq(input(1), input(2)),
          Some(Output(keyAddress(3), Value(Coin.ada(60)))),
          Some(Coin.zero)
        )
        val utxos = collateralUtxos()

        val result = validate(FeesOkValidator, utxos, transaction)
        assert(result.isLeft)
    }

    test("FeesOkValidator totalSumOfCollateralCoinsIsEquivalentToTotalCollateral rule failure") {
        // The net collateral is 40 ada.
        val transaction = collateralTx(
          Seq(input(1), input(2)),
          Some(Output(keyAddress(3), Value(Coin.ada(20)))),
          Some(Coin.ada(50))
        )
        val utxos = collateralUtxos()

        val result = validate(FeesOkValidator, utxos, transaction)
        assert(result.isLeft)
    }

    test("FeesOkValidator isAtLeastOneCollateralInput rule failure") {
        val transaction = collateralTx(Seq.empty, None, None)

        val result = validate(FeesOkValidator, Map.empty, transaction)
        assert(result.isLeft)
    }

    test("FeesOkValidator sums collateral tokens past Long.MaxValue exactly") {
        // Two collateral inputs hold Long.MaxValue of a token each, and the return takes one of
        // them back, so Long.MaxValue of the token stays in the collateral. The sum of the inputs
        // does not fit in a Long.
        val maxToken =
            MultiAsset.asset(scriptHash(1), AssetName.fromString("token"), Long.MaxValue)
        val transaction = collateralTx(
          Seq(input(1), input(2)),
          Some(Output(keyAddress(1), Value(Coin.ada(20), maxToken))),
          None,
          redeemers = Seq(Redeemer(RedeemerTag.Spend, 0, Data.unit, ExUnits(1000, 1000)))
        )
        val utxos = Map(
          input(1) -> Output(keyAddress(1), Value(Coin.ada(30), maxToken)),
          input(2) -> Output(keyAddress(1), Value(Coin.ada(30), maxToken))
        )

        validate(FeesOkValidator, utxos, transaction) match
            case Left(error: TransactionException.FeesOkException) =>
                assert(error.remainingAssets == maxToken)
                assert(error.actualTotalSumOfCollateralCoins == Coin.ada(40))
            case other => fail(s"expected FeesOkException, got $other")
    }

    /** The exact script fee of `count` redeemers at `ExUnits.enormous`, as cardano-ledger
      * `txscriptfee` computes it: one ceiling over the unbounded sum.
      */
    private def exactEnormousScriptFee(count: Int): BigInt = {
        val prices = Context().env.params.executionUnitPrices
        val units = BigInt(Long.MaxValue) * count
        val pm = prices.priceMemory
        val ps = prices.priceSteps
        val numerator =
            units * pm.numerator * ps.denominator + units * ps.numerator * pm.denominator
        val denominator = BigInt(pm.denominator) * ps.denominator
        (numerator + denominator - 1) / denominator
    }

    /** FeesOkValidator on a transaction with `count` redeemers at `ExUnits.enormous`. */
    private def validateEnormousRedeemers(count: Int): FeesOkValidator.Result = {
        val redeemers = IndexedSeq.tabulate(count) { index =>
            Redeemer(RedeemerTag.Spend, index, Data.unit, ExUnits.enormous)
        }
        val transaction = collateralTx(Seq(input(1)), None, None, redeemers = redeemers)
        val utxos = Map(input(1) -> Output(keyAddress(1), Value.ada(30)))
        validate(FeesOkValidator, utxos, transaction)
    }

    test("FeesOkValidator reports the exact min fee for one redeemer at Long.MaxValue units") {
        val scriptFee = exactEnormousScriptFee(1)
        validateEnormousRedeemers(1) match
            case Left(error: TransactionException.FeesOkException) =>
                val sizeFee = BigInt(error.minRequiredFee.value) - scriptFee
                assert(sizeFee > 0 && sizeFee < Coin.ada(1).value, s"size fee $sizeFee")
            case other => fail(s"expected FeesOkException, got $other")
    }

    test("FeesOkValidator sums redeemer units without saturation") {
        // Two redeemers at Long.MaxValue: the ledger sums them as Natural, so the script fee
        // is twice that of one redeemer, and it still fits in a Long.
        val scriptFee = exactEnormousScriptFee(2)
        assert(scriptFee.isValidLong)
        validateEnormousRedeemers(2) match
            case Left(error: TransactionException.FeesOkException) =>
                val sizeFee = BigInt(error.minRequiredFee.value) - scriptFee
                assert(sizeFee > 0 && sizeFee < Coin.ada(1).value, s"size fee $sizeFee")
            case other => fail(s"expected FeesOkException, got $other")
    }

    test("FeesOkValidator rejects a min fee past Long.MaxValue without an exception") {
        val scriptFee = exactEnormousScriptFee(20)
        assert(!scriptFee.isValidLong)
        validateEnormousRedeemers(20) match
            case Left(error: TransactionException.FeesOkException) =>
                // The report clamps the min fee to the Long range.
                assert(error.minRequiredFee == Coin(Long.MaxValue))
                assert(error.transactionFee == Coin.ada(10))
            case other => fail(s"expected FeesOkException, got $other")
    }
}
