package scalus.cardano.txbuilder

import monocle.Focus
import monocle.Focus.refocus
import org.scalacheck.{Gen, Shrink}
import org.scalatest.compatible.Assertion
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.builtin.ByteString.utf8
import scalus.builtin.{platform, ByteString}
import scalus.cardano.address.Address
import scalus.cardano.address.Address.addr
import scalus.cardano.ledger.*
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.rules.{Context, State, ValueNotConservedUTxOValidator}
import scalus.cardano.ledger.utils.{MinCoinSizedTransactionOutput, MinTransactionFee}
import scalus.cardano.txbuilder.ChangeOutputDiffHandler
import scalus.cardano.txbuilder.TxBalancingError.InsufficientFunds
import scalus.|>

import scala.collection.immutable.SortedSet
import scala.math.pow

class ChangeOutputDiffHandlerTest extends AnyFunSuite with ScalaCheckPropertyChecks {

    private val params: ProtocolParams = CardanoInfo.mainnet.protocolParams
    private val evaluator = PlutusScriptEvaluator(
      CardanoInfo.mainnet,
      EvaluatorMode.EvaluateAndComputeCost
    )

    enum Expected {
        case Success(output: Value, fee: Long)
        case Failure(error: TxBalancingError)
    }

    object Expected {
        def success(outputLovelace: Long, fee: Long) =
            Expected.Success(Value.lovelace(outputLovelace), fee)
    }

    test("should fail when insufficient funds would require change output below minimum ADA") {
        check(
          in = 1_000_000,
          output = 800_000,
          fee = 200_000,
          expected = Expected.Failure(InsufficientFunds(Value.zero, 178370))
        )
    }

    test("should add change to output when excess funds are available") {
        check(
          in = 1_500_000,
          output = 1_000_000,
          fee = 200_000,
          expected = Expected.success(outputLovelace = 1_300_000, fee = 200_000)
        )
    }

    test("should handle exact balance with no change needed") {
        check(
          in = 2_000_000,
          output = 1_800_000,
          fee = 200_000,
          expected = Expected.success(outputLovelace = 1_800_000, fee = 200_000)
        )
    }

    test("should remove from change output for balancing") {
        check(
          in = 5_000_000,
          output = 5_000_000,
          fee = 200_000,
          expected = Expected.success(outputLovelace = 4_800_000, fee = 200_000)
        )
    }

    test("MinCoinSizedTransactionOutput should ceil the output up to an actual min coin value") {
        val zeroOut = Babbage(address = testAddress, value = Value.zero)
        val zeroOutAfterMinCoin =
            MinCoinSizedTransactionOutput.ensureMinAda(Sized(zeroOut), protocolParams = params)

        val minOut = Babbage(address = testAddress, value = Value(zeroOutAfterMinCoin))
        val minOutValueCoin =
            MinCoinSizedTransactionOutput.ensureMinAda(Sized(minOut), protocolParams = params)

        assert(
          zeroOutAfterMinCoin == minOutValueCoin,
          s"Initial minAda ${zeroOutAfterMinCoin} and new minAda ${minOutValueCoin} are not equal."
        )
    }

    test("MinTransactionFee should return the same after setting fee") {
        val emptyTx = Transaction(
          body = TransactionBody(
            inputs = TaggedSortedSet.empty,
            outputs = IndexedSeq.empty,
            fee = Coin(0L)
          ),
          witnessSet = TransactionWitnessSet.empty
        )

        val res = for {
            initialFee <- MinTransactionFee.ensureMinFee(emptyTx, Map.empty, params)
            newTx = emptyTx |>
                Focus[Transaction](_.body)
                    .andThen(KeepRaw.lens[TransactionBody]())
                    .refocus(_.fee)
                    .replace(initialFee)
            newFee <- MinTransactionFee.ensureMinFee(newTx, Map.empty, params)
        } yield (initialFee, newFee)

        res match {
            case Left(e) => fail(s"fee calculation failed: ${e.toString}")
            case Right(initialFee, newFee) =>
                assert(
                  initialFee == newFee,
                  s"initial fee $initialFee and new fee $newFee are not equal "
                )
        }
    }

    test("should fail when output would become below minimum ADA") {
        val insufficientFunds: InsufficientFunds =
            InsufficientFunds(Value.lovelace(-160705), 139075)
        check(
          in = 1_000_000,
          output = 1_000_000,
          fee = 0,
          expected = Expected.Failure(insufficientFunds)
        )

        check(
          in = 1_000_000 + insufficientFunds.minRequired,
          output = 1_000_000,
          fee = 0,
          expected = Expected.success(
            outputLovelace = 978_370,
            fee = -insufficientFunds.valueDiff.coin.value
          )
        )
    }

    test("should handle single multi asset change") {
        val initialAda = Coin.ada(2)
        val expectedFee = Coin(162377L)
        check(
          in = Value.asset(policyId, co2, 5, initialAda),
          output = Value.zero,
          fee = 0L,
          expected = Expected.Success(
            output = Value.asset(policyId, co2, 5, initialAda - expectedFee),
            fee = expectedFee.value
          )
        )
    }

    test("should handle multiple token types in change") {
        val initialAda = Coin.ada(3)
        val assets = MultiAsset.asset(policyId, co2, 100) + MultiAsset.asset(policyId, h2so4, 50)
        val expectedFee = Coin(162773L)

        check(
          in = Value(initialAda, assets),
          output = Value.zero,
          fee = 0L,
          expected = Expected.Success(
            output = Value(initialAda - expectedFee, assets),
            fee = expectedFee.value
          )
        )
    }

    test("should handle tokens with initial output value") {
        val initialAda = Coin.ada(5)
        val inputTokens = MultiAsset.asset(policyId, co2, 1000)
        val outputAda = Coin.ada(2)
        val expectedFee = Coin(162465L)

        check(
          in = Value(initialAda, inputTokens),
          output = Value(outputAda),
          fee = 0L,
          expected = Expected.Success(
            output = Value(initialAda - expectedFee, inputTokens),
            fee = expectedFee.value
          )
        )
    }

    test("should silently remove when input doesn't have them") {
        val expectedFee = Coin(160705L)
        check(
          in = Value.ada(3),
          output = Value.asset(policyId, co2, 100, Coin.ada(1)),
          fee = 0L,
          expected = Expected.Success(
            output = Value(Coin.ada(3) - expectedFee), // just ADA, no tokens
            fee = expectedFee.value
          )
        )
    }

    test("should handle zero fee scenario with sufficient funds") {
        check(
          in = 3_000_000,
          output = 2_000_000,
          fee = 0,
          expected = Expected.success(
            outputLovelace = 2_839295,
            fee = 160705
          )
        )
    }

    test("should handle maximum possible coin values") {
        val maxPossible = 45_000_000_000L
        check(
          in = maxPossible,
          output = maxPossible - 1_000_000,
          fee = 500_000,
          expected = Expected.success(outputLovelace = 44_999_500_000L, fee = 500_000)
        )
    }

    test("should fail on invalid change output index") {
        val (utxo, tx) = mkTx(Coin(2_000_000), Coin(1_000_000), Coin(200_000))
        val handler = ChangeOutputDiffHandler(params, 5) // Invalid index > outputs.size

        try
            TransactionBuilder.balanceFeeAndChangeWithTokens(
              tx,
              handler.changeOutputDiffHandler,
              params,
              utxo,
              evaluator
            )
        catch case err => assert(err.getMessage.contains("requirement failed"))
    }

    // ===================================
    // FeesOkValidator tests
    // ===================================

    // The case being tested here is as follows:
    // - The input UTxO has a massive amount of ADA
    // - Pre-balancing, the output utxo has 0 and the fee is 0.
    // - The unbalanced transaction thus has space for the TransactionInput (fixed size) and (morally)
    //   0 space allocated for the fee or output value.
    // - After the fee is set, we need to balance the output. This can change the size of the tx, and thus
    //   invalidate the fee.
    // - Or, if the implementation balances first, setting the fee might change the tx size, thus invalidating the
    //   balance.
    // This is only apparent when either the pre-and-post balanced-and-fee'd tx sizes are different, which requires
    // either a massive balance diff or a massive fee.

    // General case, should pass easily.
    test("balanced transaction should pass FeesOkValidator when starting with zero fee") {
        checkFeeOk(inAda = 3_000_000, outAda = 2_000_000, feeAda = 0)
    }

    test(
      "balanced transaction with massive input should pass FeesOkValidator when starting with zero fee"
    )(checkFeeOk(inAda = pow(2, 56).toLong, outAda = 0, feeAda = 0))

    implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
        PropertyCheckConfiguration(minSuccessful = 100_000)
    test("fees okay property") {
        val gen0 = {
            // Input is generated large enough to cover the fee and output
            for {
                fee <- Gen.choose(0L, pow(2, 55).toLong)
                out <- Gen.choose(0L, pow(2, 55).toLong)
                in <- Gen.choose(fee + out, pow(2, 57).toLong)

            } yield (in, out, fee)
        }

        // Input generated big enough to cover fee
        val gen1 =
            for {
                in <- Gen.choose(3_000_000L, pow(2, 56).toLong)
            } yield (in, 0L, 0L)

        val gen2 =
            for {
                in <- Gen.choose(3_000_000L, pow(2, 56).toLong)
                out <- Gen.choose(0L, in - 2_000_000L)
            } yield (in, out, 0L)

        val gen3 =
            for {
                in <- Gen.choose(3_000_000L, pow(2, 56).toLong)
                fee <- Gen.choose(0L, in)
            } yield (in, 0L, fee)

        // Avoid shrinking; shrunk values will change the error to a different cause.
        implicit def noShrink[T]: Shrink[T] = Shrink.shrinkAny

        forAll(Gen.oneOf(gen0, gen1, gen2, gen3))(t => {
            checkFeeOk.tupled(t)
        })
    }

    private val testAddress: Address =
        addr"addr1qxwg0u9fpl8dac9rkramkcgzerjsfdlqgkw0q8hy5vwk8tzk5pgcmdpe5jeh92guy4mke4zdmagv228nucldzxv95clqe35r3m"

    private lazy val policyId: PolicyId = Hash(ByteString.fromString("a".repeat(28)))

    private lazy val co2 = AssetName.fromString("co2")
    private lazy val h2so4 = AssetName.fromString("h2so4")

    private def mkTx(in: Value, output: Value, fee: Coin): (Utxos, Transaction) = {
        val input = TransactionInput(Hash(platform.blake2b_256(utf8"asdf")), 0)
        val utxo = Map(
          input -> TransactionOutput(
            address = testAddress,
            value = in,
            None
          )
        )
        val tx = Transaction(
          TransactionBody(
            inputs = TaggedSortedSet(SortedSet(input)),
            outputs = Vector(
              Sized(
                TransactionOutput(
                  address = testAddress,
                  value = output,
                  None
                )
              )
            ),
            fee = fee,
          ),
          witnessSet = TransactionWitnessSet.empty
        )
        (utxo, tx)
    }

    private def mkTx(in: Coin, output: Coin, fee: Coin): (Utxos, Transaction) =
        mkTx(Value(in), Value(output), fee)

    private def check(
        in: Long,
        output: Long,
        fee: Long,
        expected: Expected
    ): Assertion = check(Value.lovelace(in), Value.lovelace(output), fee, expected)

    private def check(
        in: Value,
        output: Value,
        fee: Long,
        expected: Expected
    ): Assertion = {
        val (utxo, tx) = mkTx(in, output, Coin(fee))

        val handler = ChangeOutputDiffHandler(params, 0)
        val r = TransactionBuilder.balanceFeeAndChangeWithTokens(
          tx,
          handler.changeOutputDiffHandler,
          params,
          utxo,
          evaluator
        )
        (r, expected) match
            case (Right(value), Expected.Success(expectedValue, expectedFee)) =>
                assert(
                  ValueNotConservedUTxOValidator.validate(Context(), State(utxo), value).isRight
                )
                val body = value.body.value
                assert(body.fee.value == expectedFee, "unexpected fee")
                assert(
                  body.outputs(0).value.value == expectedValue,
                  "unexpected output value"
                )
            case (Left(err), Expected.Failure(expectedError)) =>
                assert(err == expectedError)
            case _ =>
                fail(s"Unexpected result: $r")

    }

    private def checkFeeOk(in: Value, out: Value, feeAda: Long): Unit = {
        val (utxo, tx) = mkTx(in, out, Coin(feeAda))
        val handler = ChangeOutputDiffHandler(params, 0)
        val result = TransactionBuilder.balanceFeeAndChangeWithTokens(
          tx,
          handler.changeOutputDiffHandler,
          params,
          utxo,
          evaluator
        )

        result match
            case Right(balancedTx) =>
                val context = Context()
                val state = State(utxo)
                val validationResult = rules.FeesOkValidator.validate(context, state, balancedTx)
                validationResult match
                    case Left(err) =>
                        fail(
                          s"FeesOkValidator failed with: $err. " +
                              s"Balanced fee: ${balancedTx.body.value.fee.value}"
                        )
                    case Right(_) => // success
            case Left(err) =>
                fail(s"balanceFeeAndChange failed with: $err")
    }

    private def checkFeeOk(inAda: Long, outAda: Long, feeAda: Long): Unit =
        checkFeeOk(Value.lovelace(inAda), Value.lovelace(outAda), feeAda)
}
