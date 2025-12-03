package scalus.cardano.txbuilder

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.cardano.address.Address
import scalus.builtin.ByteString.utf8
import scalus.cardano.ledger.*
import scalus.cardano.ledger.ArbitraryInstances.given_Arbitrary_Hash
import scalus.cardano.txbuilder.TxBalancingError.InsufficientFunds

class ChangeTest extends AnyFunSuite with ScalaCheckPropertyChecks {

    val testEnv: CardanoInfo = CardanoInfo.mainnet

    // Run each property test with 1000 successful cases
    implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
        PropertyCheckConfiguration(minSuccessful = 1000)

    test("does not kill the multiassets") {
        val asset = MultiAsset.asset(
          Arbitrary.arbitrary[ScriptHash].sample.get,
          AssetName(utf8"co2"),
          1
        )

        val inputAda = Coin.ada(100_000L)
        val outputAda = Coin.ada(80_000L)
        // ensure change = 0
        val generousFee = inputAda - outputAda

        val output = TransactionOutput(genPubkeyAddr().sample.get, Value(outputAda))
        val changeOutput = TransactionOutput(genPubkeyAddr().sample.get, Value(Coin.zero, asset))

        val input = genTransactionInput.sample.get
        val tx = makeSimpleTx(
          // A single random input is enough
          ins = Seq(input),
          outs = Seq(output, changeOutput),
          fee = generousFee
        )

        Change.handleChange(
          diff = Value.zero,
          tx,
          changeOutput.address,
          testEnv.protocolParams
        ) match {
            case Right(value) =>
                val outs = value.body.value.outputs.map(_.value)
                assert(outs.size == 2)
                val o = outs.find(_.address == changeOutput.address).get
                assert(o.value.assets == asset)
            case Left(value) => fail("Expected transaction to balance successfully")
        }
    }

    test(
      "should fail when trying to remove more tokens than exist in change output"
    ) {
        val policyId = Arbitrary.arbitrary[ScriptHash].sample.get
        val assetName = AssetName(utf8"token")

        val changeAssets = MultiAsset.asset(policyId, assetName, 50)
        val changeAddr = genPubkeyAddr().sample.get
        val changeOutput = TransactionOutput(changeAddr, Value(Coin.ada(2), changeAssets))

        val otherOutput = TransactionOutput(genPubkeyAddr().sample.get, Value(Coin.ada(3)))

        val input = genTransactionInput.sample.get
        val tx = makeSimpleTx(
          ins = Seq(input),
          outs = Seq(otherOutput, changeOutput),
          fee = Coin.ada(1)
        )

        val diff = Value(
          Coin.zero,
          MultiAsset.asset(policyId, assetName, -100)
        )

        val result = Change.handleChange(
          diff,
          tx,
          changeAddr,
          testEnv.protocolParams
        )

        result match {
            case Right(updatedTx) =>
                val changeOut = updatedTx.body.value.outputs
                    .find(_.value.address == changeAddr)
                    .get
                    .value
                    .value

                val finalTokenAmount = changeOut.assets.assets(policyId)(assetName)

                fail(
                  s"Expected InsufficientFunds error but got success with $finalTokenAmount tokens"
                )

            case Left(InsufficientFunds(_, _)) => succeed
        }
    }

    test("property: should fail when outputs exceed inputs (no change output)") {
        forAll(genInsufficientBalanceScenario) { case (tx, changeAddr, negativeDiff) =>
            val result = Change.handleChange(negativeDiff, tx, changeAddr, testEnv.protocolParams)

            result match {
                case Left(InsufficientFunds(_, _)) => succeed
                case Left(err) =>
                    fail(s"Expected InsufficientFunds but got different error: $err")
                case Right(_) =>
                    fail(
                      s"Expected InsufficientFunds but handleChange succeeded with negative diff ${negativeDiff.coin.value}"
                    )
            }
        }
    }

    test("property: all outputs meet minAda requirement") {
        forAll(genHandleChangeScenario) { case (tx, changeAddr, diff) =>
            val result = Change.handleChange(diff, tx, changeAddr, testEnv.protocolParams)

            result match {
                case Right(updatedTx) =>
                    updatedTx.body.value.outputs.foreach { output =>
                        val minAda =
                            scalus.cardano.ledger.utils.MinCoinSizedTransactionOutput(
                              output,
                              testEnv.protocolParams
                            )
                        assert(
                          output.value.value.coin.value >= minAda.value,
                          s"Output ${output.value.address} has ${output.value.value.coin.value} lovelace, " +
                              s"requires at least ${minAda.value}"
                        )
                    }
                case Left(e) => // invariant: _if_ the change is handled, minAda is maintained.
            }
        }
    }

    test("property: non-change outputs are never modified") {
        forAll(genHandleChangeScenario) { case (tx, changeAddr, diff) =>
            val result = Change.handleChange(diff, tx, changeAddr, testEnv.protocolParams)

            result match {
                case Right(updatedTx) =>
                    val originalNonChange = tx.body.value.outputs
                        .filter(_.value.address != changeAddr)
                    val updatedNonChange = updatedTx.body.value.outputs
                        .filter(_.value.address != changeAddr)

                    assert(
                      originalNonChange == updatedNonChange,
                      "Non-change outputs were modified"
                    )
                case Left(_) =>
                // invariant: _if_ the change is handled, non-change outputs are preserved
            }
        }
    }

    test("property: change address appears at most once") {
        forAll(genHandleChangeScenario) { case (tx, changeAddr, diff) =>
            val result = Change.handleChange(diff, tx, changeAddr, testEnv.protocolParams)

            result match {
                case Right(updatedTx) =>
                    val changeCount = updatedTx.body.value.outputs
                        .count(_.value.address == changeAddr)
                    assert(
                      changeCount <= 1,
                      s"Found $changeCount change outputs, expected at most 1"
                    )
                case Left(_) =>
                // invariant: _if_ the change is handled, one change output exists
            }
        }
    }

    test("property: positive diff with sufficient ADA succeeds") {
        forAll(genPositiveDiffScenario) { case (tx, changeAddr, positiveDiff) =>
            val result = Change.handleChange(positiveDiff, tx, changeAddr, testEnv.protocolParams)

            assert(
              result.isRight,
              s"Expected success with positive diff ${positiveDiff.coin.value} but got: $result"
            )
        }
    }

    test("property: token conservation") {
        forAll(genTokenConservationScenario) { case (tx, changeAddr, diff) =>
            val result = Change.handleChange(diff, tx, changeAddr, testEnv.protocolParams)

            result match {
                case Right(updatedTx) =>
                    // Get original change output tokens (if exists)
                    val originalChangeTokens = tx.body.value.outputs
                        .find(_.value.address == changeAddr)
                        .map(_.value.value.assets)
                        .getOrElse(MultiAsset.empty)

                    // Get updated change output tokens (if exists)
                    val updatedChangeTokens = updatedTx.body.value.outputs
                        .find(_.value.address == changeAddr)
                        .map(_.value.value.assets)
                        .getOrElse(MultiAsset.empty)

                    // Token delta should equal diff.assets
                    val actualDelta = updatedChangeTokens - originalChangeTokens
                    assert(
                      actualDelta == diff.assets,
                      s"Expected token delta ${diff.assets}, got $actualDelta"
                    )
                case Left(error) => // invariant _if_ the change is handled, tokens are preserved
            }
        }
    }

    test("property: witness set and fee are immutable") {
        forAll(genHandleChangeScenario) { case (tx, changeAddr, diff) =>
            val result = Change.handleChange(diff, tx, changeAddr, testEnv.protocolParams)

            result match {
                case Right(updatedTx) =>
                    assert(
                      updatedTx.witnessSet == tx.witnessSet,
                      "Witness set was modified"
                    )
                    assert(
                      updatedTx.body.value.fee == tx.body.value.fee,
                      "Fee was modified"
                    )
                case Left(e) =>
                // invariant _if_ the change is handled, ws is never modified
            }
        }
    }

    /** Base generator for transaction scenarios */
    private def genBaseTxScenario(
        inputAdaRange: (Long, Long) = (5_000_000L, 100_000_000L),
        outputAdaRange: Option[(Long, Long)] = None,
        inputAmountRange: (Int, Int) = (1, 5),
        outputAmountRange: (Int, Int) = (1, 5)
    ): Gen[(Transaction, Long, Long)] = {
        for {
            inputAmount <- Gen.choose(inputAmountRange._1, inputAmountRange._2)
            outputAmount <- Gen.choose(outputAmountRange._1, outputAmountRange._2)
            inputAda <- Gen.choose(inputAdaRange._1, inputAdaRange._2)

            // Output range can depend on input if not specified
            outputAda <- outputAdaRange match {
                case Some((min, max)) => Gen.choose(min, max)
                case None             => Gen.choose(2_000_000L, inputAda)
            }

            (_, tx) <- txGen(
              inputAmount = inputAmount,
              inputSum = Value.lovelace(inputAda),
              outputAmount = outputAmount,
              outputSum = Value.lovelace(outputAda)
            )
        } yield (tx, inputAda, outputAda)
    }

    /** Add a change output to a transaction */
    private def addChangeOutput(
        tx: Transaction,
        changeAddr: Address,
        value: Value
    ): Transaction = {
        val changeOutput = Sized(TransactionOutput(changeAddr, value))
        tx.copy(body = KeepRaw(tx.body.value.copy(outputs = tx.body.value.outputs :+ changeOutput)))
    }

    /** General scenario generator for handleChange testing */
    private def genHandleChangeScenario: Gen[(Transaction, Address, Value)] = {
        for {
            (tx, inputAda, outputAda) <- genBaseTxScenario()

            // Generate diff (can be positive, negative, or zero)
            diffAda <- Gen.choose(-outputAda / 2, inputAda)

            // Optionally include a change output
            hasChangeOutput <- Gen.oneOf(true, false)
            changeAddr <- genPubkeyAddr()

            txWithChange =
                if hasChangeOutput then addChangeOutput(tx, changeAddr, Value.lovelace(2_000_000))
                else tx

            diff = Value.lovelace(diffAda)
        } yield (txWithChange, changeAddr, diff)
    }

    /** Generate scenarios with positive diff and sufficient ADA for minAda */
    private def genPositiveDiffScenario: Gen[(Transaction, Address, Value)] = {
        for {
            (tx, _, _) <- genBaseTxScenario(
              inputAdaRange = (10_000_000L, 100_000_000L),
              outputAdaRange = Some((5_000_000L, 50_000_000L))
            )

            // Positive diff with enough for minAda
            positiveDiffAda <- Gen.choose(2_000_000L, 10_000_000L)
            changeAddr <- genPubkeyAddr()
            diff = Value.lovelace(positiveDiffAda)
        } yield (tx, changeAddr, diff)
    }

    /** Generate scenarios for testing token conservation */
    private def genTokenConservationScenario: Gen[(Transaction, Address, Value)] = {
        for {
            (tx, _, _) <- genBaseTxScenario(
              inputAdaRange = (10_000_000L, 50_000_000L),
              inputAmountRange = (1, 3),
              outputAmountRange = (1, 3)
            )

            // Generate tokens
            policyId <- genPolicyId
            assetName <- Gen.const(AssetName.fromString("token"))
            tokenAmount <- Gen.choose(1L, 1000L)
            tokenDiff <- Gen.choose(-tokenAmount, tokenAmount)

            changeAddr <- genPubkeyAddr()

            // Add change output with tokens
            changeTokens = MultiAsset.asset(policyId, assetName, tokenAmount)
            txWithChange = addChangeOutput(
              tx,
              changeAddr,
              Value(Coin(5_000_000L), changeTokens)
            )

            diff = Value(Coin.zero, MultiAsset.asset(policyId, assetName, tokenDiff))
        } yield (txWithChange, changeAddr, diff)
    }

    private def genInsufficientBalanceScenario: Gen[(Transaction, Address, Value)] = {
        for {
            // Generate inputs smaller than outputs (insufficient funds)
            inputAda <- Gen.choose(1_000_000L, 50_000_000L)
            deficitAda <- Gen.choose(500_000L, 50_000_000L)
            outputAda = inputAda + deficitAda

            (tx, _, _) <- genBaseTxScenario(
              inputAdaRange = (inputAda, inputAda), // Fixed input amount
              outputAdaRange = Some((outputAda, outputAda)) // Fixed output amount (exceeds input)
            )

            negativeDiff = Value.lovelace(-deficitAda)

            // Ensure change address is NOT in the transaction (no change output to remove from)
            changeAddr <- genPubkeyAddr().suchThat(addr =>
                !tx.body.value.outputs.exists(_.value.address == addr)
            )
        } yield (tx, changeAddr, negativeDiff)
    }

    private def txGen(
        inputAmount: Int = 1,
        inputSum: Value,
        outputAmount: Int = 1,
        outputSum: Value
    ): Gen[(Utxos, Transaction)] = {
        require(inputAmount > 0, "Must have at least one input")
        require(outputAmount > 0, "Must have at least one output")

        for {
            inputs <- Gen.listOfN(inputAmount, genTransactionInput)
            inputValues = distributeValue(inputSum, inputAmount)
            inputAddresses <- Gen.listOfN(inputAmount, genPubkeyAddr())
            outputAddresses <- Gen.listOfN(outputAmount, genPubkeyAddr())
            outputValues = distributeValue(outputSum, outputAmount)

        } yield {
            val utxos: Utxos = inputs
                .zip(inputAddresses)
                .zip(inputValues)
                .map { case ((txIn, addr), value) =>
                    txIn -> TransactionOutput(addr, value)
                }
                .toMap

            val outputs = outputAddresses
                .zip(outputValues)
                .map { case (addr, value) =>
                    Sized(TransactionOutput(addr, value))
                }

            val tx = Transaction(
              body = TransactionBody(
                inputs = TaggedSortedSet(inputs*),
                outputs = outputs.toIndexedSeq,
                fee = Coin.zero
              ),
              witnessSet = TransactionWitnessSet.empty
            )

            (utxos, tx)
        }
    }

    /** Calculate the actual minAda for a simple output with no tokens */
    private lazy val minAdaForSimpleOutput: Long = {
        val dummyAddr = genPubkeyAddr().sample.get
        val simpleOutput = TransactionOutput.Shelley(
          address = dummyAddr,
          value = Value.zero
        )
        scalus.cardano.ledger.utils
            .MinCoinSizedTransactionOutput(Sized(simpleOutput), testEnv.protocolParams)
            .value
    }

    private def distributeValue(total: Value, parts: Int): List[Value] = {
        require(parts > 0, "Must have at least one part")

        if parts == 1 then List(total)
        else {
            // Ensure each part gets at least minAda for simple outputs
            val adaPerPart = math.max(minAdaForSimpleOutput, total.coin.value / parts)
            val totalDistributed = adaPerPart * (parts - 1)
            val remainder = total.coin.value - totalDistributed

            val adaDistribution = List.tabulate(parts) { i =>
                if i < parts - 1 then adaPerPart
                else
                    math.max(minAdaForSimpleOutput, remainder) // Ensure last part also meets minAda
            }

            val tokenDistribution = List.tabulate(parts) { i =>
                if i == 0 then total.assets
                else MultiAsset.empty
            }

            adaDistribution
                .zip(tokenDistribution)
                .map { case (ada, assets) =>
                    Value(Coin(ada), assets)
                }
        }
    }

    private def makeSimpleTx(
        ins: Seq[TransactionInput],
        outs: Seq[TransactionOutput],
        fee: Coin
    ) = {
        val body = TransactionBody(
          TaggedSortedSet(ins*),
          outs.toIndexedSeq.map(Sized.apply),
          fee
        )
        Transaction(
          body,
          TransactionWitnessSet.empty
        )
    }
}
