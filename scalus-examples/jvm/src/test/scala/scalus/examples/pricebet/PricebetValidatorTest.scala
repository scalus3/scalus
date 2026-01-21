package scalus.examples.pricebet

import org.scalatest.funsuite.AnyFunSuite
import scalus.builtin.{ByteString, Data}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.rules.Context
import scalus.cardano.node.Emulator
import scalus.cardano.txbuilder.RedeemerPurpose
import scalus.testing.kit.Party.{Alice, Bob, Oracle}
import scalus.testing.kit.TestUtil.getScriptContextV3
import scalus.testing.kit.{ScalusTest, TestUtil}
import scalus.uplc.PlutusV3
import scalus.utils.await

import java.time.Instant
import scala.concurrent.ExecutionContext.Implicits.global

class PricebetValidatorTest extends AnyFunSuite, ScalusTest {
    import PricebetValidatorTest.{*, given}

    test(s"Pricebet validator size is ${PriceBetContract.script.script.size} bytes") {
        info(s"Validator size: ${PriceBetContract.script.script.size} bytes")
    }

    test(s"Oracle validator size is ${OracleContract.script.script.size} bytes") {
        info(s"Validator size: ${OracleContract.script.script.size} bytes")
    }

    test("Owner initiates bet successfully") {
        val provider = createProvider()
        val utxos = provider.findUtxos(Alice.address).await().toOption.get

        val initTx = txCreator.initiatePricebet(
          ownerUtxos = utxos,
          betAmount = betAmount,
          ownerPkh = Alice.addrKeyHash,
          oracleScriptHash = oracleScriptAddress.scriptHashOption.get,
          deadline = deadline.toEpochMilli,
          exchangeRate = betExchangeRate,
          changeAddress = Alice.address,
          signer = Alice.signer
        )

        val result = provider.submit(initTx).await()
        assert(result.isRight)
    }

    test("Player joins successfully") {
        val provider = createProvider()
        val (_, pricebetUtxo) = createAndSubmitInitiateTx(provider)
        val utxos = provider.findUtxos(Bob.address).await().toOption.get

        val joinTx = txCreator.join(
          utxos = utxos,
          pricebetUtxo = pricebetUtxo,
          playerPkh = Bob.addrKeyHash,
          sponsor = Bob.address,
          signer = Bob.signer
        )

        assertSuccess(provider, joinTx, pricebetUtxo._1)
    }

    test("Doesn't allow double join") {
        val provider = createProvider()
        val (_, pricebetUtxo) = createAndSubmitInitiateTx(provider)

        // First join succeeds
        val utxos1 = provider.findUtxos(Bob.address).await().toOption.get
        val joinTx1 = txCreator.join(
          utxos = utxos1,
          pricebetUtxo = pricebetUtxo,
          playerPkh = Bob.addrKeyHash,
          sponsor = Bob.address,
          signer = Bob.signer
        )
        assertSuccess(provider, joinTx1, pricebetUtxo._1)

        // Find the new pricebet UTXO after join
        val pricebetUtxos = provider.findUtxos(pricebetScriptAddress).await().toOption.get
        val newPricebetUtxo = Utxo(pricebetUtxos.head)

        // Second join should fail
        val utxos2 = provider.findUtxos(Alice.address).await().toOption.get
        val joinTx2 = txCreator.join(
          utxos = utxos2,
          pricebetUtxo = newPricebetUtxo,
          playerPkh = Alice.addrKeyHash,
          sponsor = Alice.address,
          signer = Alice.signer
        )

        assertFailure(provider, joinTx2, newPricebetUtxo._1, "Player already joined")
    }

    test("Player wins with oracle rate above threshold") {
        val provider = createProvider()
        val oracleUtxo = createAndSubmitOracleUtxo(provider, winningRate)
        val (_, pricebetUtxo) = createAndSubmitInitiateTx(provider)

        // Player joins
        val utxos1 = provider.findUtxos(Bob.address).await().toOption.get
        val joinTx = txCreator.join(
          utxos = utxos1,
          pricebetUtxo = pricebetUtxo,
          playerPkh = Bob.addrKeyHash,
          sponsor = Bob.address,
          signer = Bob.signer
        )
        assertSuccess(provider, joinTx, pricebetUtxo._1)

        // Find the pricebet UTXO after join
        val pricebetUtxos = provider.findUtxos(pricebetScriptAddress).await().toOption.get
        val joinedPricebetUtxo = Utxo(pricebetUtxos.head)

        // Player wins
        val utxos2 = provider.findUtxos(Bob.address).await().toOption.get
        val winTx = txCreator.win(
          utxos = utxos2,
          pricebetUtxo = joinedPricebetUtxo,
          oracleUtxo = oracleUtxo,
          playerAddress = Bob.address,
          sponsor = Bob.address,
          validTo = deadline,
          signer = Bob.signer
        )

        provider.setSlot(beforeSlot)
        assertSuccess(provider, winTx, joinedPricebetUtxo._1)
    }

    test("Fails to win with a low rate") {
        val provider = createProvider()
        val oracleUtxo = createAndSubmitOracleUtxo(provider, losingRate)
        val (_, pricebetUtxo) = createAndSubmitInitiateTx(provider)

        // Player joins
        val utxos1 = provider.findUtxos(Bob.address).await().toOption.get
        val joinTx = txCreator.join(
          utxos = utxos1,
          pricebetUtxo = pricebetUtxo,
          playerPkh = Bob.addrKeyHash,
          sponsor = Bob.address,
          signer = Bob.signer
        )
        assertSuccess(provider, joinTx, pricebetUtxo._1)

        // Find the pricebet UTXO after join
        val pricebetUtxos = provider.findUtxos(pricebetScriptAddress).await().toOption.get
        val joinedPricebetUtxo = Utxo(pricebetUtxos.head)

        // Player tries to win but should fail
        val utxos2 = provider.findUtxos(Bob.address).await().toOption.get
        val winTx = txCreator.win(
          utxos = utxos2,
          pricebetUtxo = joinedPricebetUtxo,
          oracleUtxo = oracleUtxo,
          playerAddress = Bob.address,
          sponsor = Bob.address,
          validTo = deadline,
          signer = Bob.signer
        )

        provider.setSlot(beforeSlot)
        assertFailure(provider, winTx, joinedPricebetUtxo._1, "Oracle rate must exceed bet rate")
    }

    test("Owner times out after deadline") {
        val provider = createProvider()
        val (_, pricebetUtxo) = createAndSubmitInitiateTx(provider)

        val utxos = provider.findUtxos(Alice.address).await().toOption.get
        val timeoutTx = txCreator.timeout(
          utxos = utxos,
          pricebetUtxo = pricebetUtxo,
          ownerAddress = Alice.address,
          sponsor = Alice.address,
          validFrom = afterDeadline,
          signer = Alice.signer
        )

        provider.setSlot(afterDeadlineSlot)
        assertSuccess(provider, timeoutTx, pricebetUtxo._1)
    }

    test("Cannot timeout before deadline") {
        val provider = createProvider()
        val (_, pricebetUtxo) = createAndSubmitInitiateTx(provider)

        val utxos = provider.findUtxos(Alice.address).await().toOption.get
        val timeoutTx = txCreator.timeout(
          utxos = utxos,
          pricebetUtxo = pricebetUtxo,
          ownerAddress = Alice.address,
          sponsor = Alice.address,
          validFrom = beforeDeadline,
          signer = Alice.signer
        )

        provider.setSlot(beforeSlot)
        assertFailure(provider, timeoutTx, pricebetUtxo._1, "Deadline not reached")
    }

    test("Oracle updates successfully") {
        val provider = createProvider()
        val oracleUtxo = createAndSubmitOracleUtxo(provider, initialRate)

        val utxos = provider.findUtxos(Alice.address).await().toOption.get
        val updateTx = txCreator.updateOracle(
          utxos = utxos,
          oracleUtxo = oracleUtxo,
          newTimestamp = updateTimestamp.toEpochMilli,
          newRateNominator = winningRate._1,
          newRateDenominator = winningRate._2,
          authorizedSigner = Oracle.addrKeyHash,
          sponsor = Alice.address,
          validFrom = updateValidFrom,
          validTo = updateValidTo,
          oracleSigner = Oracle.signer,
          sponsorSigner = Alice.signer
        )

        provider.setSlot(updateSlot)
        assertSuccess(provider, updateTx, oracleUtxo._1)
    }

    test("Oracle forbids unauthorized price updates") {
        val provider = createProvider()
        val oracleUtxo = createAndSubmitOracleUtxo(provider, initialRate)

        // Alice (not authorized) tries to update oracle
        val utxos = provider.findUtxos(Bob.address).await().toOption.get
        val updateTx = txCreator.updateOracle(
          utxos = utxos,
          oracleUtxo = oracleUtxo,
          newTimestamp = updateTimestamp.toEpochMilli,
          newRateNominator = winningRate._1,
          newRateDenominator = winningRate._2,
          authorizedSigner = Alice.addrKeyHash,
          sponsor = Bob.address,
          validFrom = updateValidFrom,
          validTo = updateValidTo,
          oracleSigner = Alice.signer,
          sponsorSigner = Bob.signer
        )

        provider.setSlot(updateSlot)
        assertFailure(provider, updateTx, oracleUtxo._1, "Must be signed by authorized signer")
    }

    test("Oracle discovery via beacon token") {
        val provider = createProvider()

        val oracleUtxos = provider.findUtxos(Oracle.address).await().toOption.get
        val seedUtxo = Utxo(oracleUtxos.head)

        val createTx = txCreator.mintBeaconAndCreateOracle(
          utxos = oracleUtxos,
          seedUtxo = seedUtxo,
          beaconTokenName = beaconTokenName,
          authorizedSigner = Oracle.addrKeyHash,
          initialTimestamp = beforeDeadline.toEpochMilli,
          initialRateNominator = winningRate._1,
          initialRateDenominator = winningRate._2,
          sponsor = Oracle.address,
          signer = Oracle.signer
        )

        val result = provider.submit(createTx).await()
        assert(result.isRight, s"Failed to submit oracle creation: ${result.left.getOrElse(null)}")

        // Step 2: Discover oracle by searching for beacon token (real-world flow)
        val beaconPolicyId = oracleScriptAddress.scriptHashOption.get

        // Search all UTXOs at oracle script address
        val allOracleUtxos = provider.findUtxos(oracleScriptAddress).await().toOption.get

        // Filter for the one with our beacon token
        val discoveredOracleUtxo = allOracleUtxos.find { case (input, output) =>
            output.value.assets.assets.exists { case (assetId, assets) =>
                assetId == beaconPolicyId && assets.size == 1 && assets.head._1 == AssetName(
                  beaconTokenName
                )
            }
        }

        assert(discoveredOracleUtxo.isDefined, "Should be able to find oracle by beacon token")

        // Step 3: Verify we can read the oracle state from discovered UTXO
        val (_, discoveredOutput) = discoveredOracleUtxo.get
        val oracleState = discoveredOutput.datumOption.get.dataOption.get.to[OracleState]

        assert(
          oracleState.beaconPolicyId == beaconPolicyId,
          "Oracle state should contain beacon policy ID"
        )
        assert(
          oracleState.beaconTokenName == beaconTokenName,
          "Oracle state should contain beacon token name"
        )
        assert(
          oracleState.exchangeRateNominator == winningRate._1,
          "Oracle should have correct rate numerator"
        )
        assert(
          oracleState.exchangeRateDenominator == winningRate._2,
          "Oracle should have correct rate denominator"
        )
    }

    test("Happy path") {
        val provider = createProvider()

        // create oracle with rate = 1/2
        val lowRate = (BigInt(1), BigInt(2))
        val oracleUtxo = createAndSubmitOracleUtxo(provider, lowRate)

        // owner initiates bet with rate = 3/4
        val betRate = (BigInt(3), BigInt(4))
        val (_, pricebetUtxo) = {
            val utxos = provider.findUtxos(Alice.address).await().toOption.get
            val initTx = txCreator.initiatePricebet(
              ownerUtxos = utxos,
              betAmount = betAmount,
              ownerPkh = Alice.addrKeyHash,
              oracleScriptHash = oracleScriptAddress.scriptHashOption.get,
              deadline = deadline.toEpochMilli,
              exchangeRate = betRate,
              changeAddress = Alice.address,
              signer = Alice.signer
            )
            val result = provider.submit(initTx).await()
            assert(result.isRight, s"Failed to submit: ${result.left.getOrElse(null)}")

            val pricebetUtxos = provider.findUtxos(pricebetScriptAddress).await().toOption.get
            (initTx.body.value.inputs.toSeq.head, Utxo(pricebetUtxos.head))
        }

        // Bob joins and tries to win with the rate 1/2 (must fail since the pricebet has it at 3/4)
        val pricebetUtxoAfterJoin = {
            val utxos = provider.findUtxos(Bob.address).await().toOption.get
            val joinTx = txCreator.join(
              utxos = utxos,
              pricebetUtxo = pricebetUtxo,
              playerPkh = Bob.addrKeyHash,
              sponsor = Bob.address,
              signer = Bob.signer
            )
            val result = provider.submit(joinTx).await()
            assert(result.isRight, s"Failed to submit join: ${result.left.getOrElse(null)}")

            val pricebetUtxos = provider.findUtxos(pricebetScriptAddress).await().toOption.get
            Utxo(pricebetUtxos.head)
        }

        val bobUtxosBeforeWin = provider.findUtxos(Bob.address).await().toOption.get
        val winTxLowRate = txCreator.win(
          utxos = bobUtxosBeforeWin,
          pricebetUtxo = pricebetUtxoAfterJoin,
          oracleUtxo = oracleUtxo,
          playerAddress = Bob.address,
          sponsor = Bob.address,
          validTo = deadline,
          signer = Bob.signer
        )

        provider.setSlot(beforeSlot)
        assertFailure(
          provider,
          winTxLowRate,
          pricebetUtxoAfterJoin._1,
          "Oracle rate must exceed bet rate"
        )

        // Update oracle to high rate 7/8 > 3/4
        val highRate = (BigInt(7), BigInt(8))
        val updatedOracleUtxo = {
            provider.setSlot(updateSlot) // Set slot for oracle update
            val aliceUtxos = provider.findUtxos(Alice.address).await().toOption.get
            val updateTx = txCreator.updateOracle(
              utxos = aliceUtxos,
              oracleUtxo = oracleUtxo,
              newTimestamp = updateTimestamp.toEpochMilli,
              newRateNominator = highRate._1,
              newRateDenominator = highRate._2,
              authorizedSigner = Oracle.addrKeyHash,
              sponsor = Alice.address,
              validFrom = updateValidFrom,
              validTo = updateValidTo,
              oracleSigner = Oracle.signer,
              sponsorSigner = Alice.signer
            )
            val result = provider.submit(updateTx).await()
            assert(result.isRight, s"Failed to update oracle: ${result.left.getOrElse(null)}")

            val oracleUtxos = provider.findUtxos(oracleScriptAddress).await().toOption.get
            Utxo(oracleUtxos.head)
        }

        // Bob wins
        val bobUtxosAfterUpdate = provider.findUtxos(Bob.address).await().toOption.get
        val winTxHighRate = txCreator.win(
          utxos = bobUtxosAfterUpdate,
          pricebetUtxo = pricebetUtxoAfterJoin,
          oracleUtxo = updatedOracleUtxo,
          playerAddress = Bob.address,
          sponsor = Bob.address,
          validTo = deadline,
          signer = Bob.signer
        )

        provider.setSlot(beforeSlot)
        assertSuccess(provider, winTxHighRate, pricebetUtxoAfterJoin._1)
    }
}

object PricebetValidatorTest extends ScalusTest {
    given env: CardanoInfo = TestUtil.testEnvironment
    val pricebetContract: PlutusV3[Data => Unit] = PriceBetContract.withErrorTraces
    val oracleContract: PlutusV3[Data => Unit] = OracleContract.withErrorTraces
    val pricebetScriptAddress = pricebetContract.address(env.network)
    val oracleScriptAddress = oracleContract.address(env.network)

    val evaluator = PlutusScriptEvaluator.constMaxBudget(env)

    val txCreator = PricebetTransactions(
      env = env,
      evaluator = evaluator,
      pricebetContract = pricebetContract,
      oracleContract = oracleContract
    )

    // Beacon token name for oracle (policy ID is derived from oracle script hash)
    val beaconTokenName = ByteString.fromString("ORACLE")

    // Test parameters
    val betAmount = Coin.ada(10)
    val deadline = Instant.parse("2025-01-15T00:00:00Z")
    val beforeDeadline = Instant.parse("2025-01-14T23:59:59Z")
    val afterDeadline = Instant.parse("2025-01-15T00:00:01Z")
    val updateTimestamp = Instant.parse("2025-01-14T12:00:00Z")
    val updateValidFrom = Instant.parse("2025-01-14T11:00:00Z")
    val updateValidTo = Instant.parse("2025-01-14T13:00:00Z")

    val beforeSlot = env.slotConfig.timeToSlot(beforeDeadline.toEpochMilli).toLong
    val deadlineSlot = env.slotConfig.timeToSlot(deadline.toEpochMilli).toLong
    val afterDeadlineSlot = env.slotConfig.timeToSlot(afterDeadline.toEpochMilli).toLong
    val updateSlot = env.slotConfig.timeToSlot(updateTimestamp.toEpochMilli).toLong

    // Exchange rates: (numerator, denominator)
    // Bet rate: 3/2 = 1.5
    val betExchangeRate = (BigInt(3), BigInt(2))
    // Winning rate: 8/5 = 1.6 > 1.5
    val winningRate = (BigInt(8), BigInt(5))
    // Losing rate: 7/5 = 1.4 < 1.5
    val losingRate = (BigInt(7), BigInt(5))
    // Initial rate: 1/1 = 1.0 < 1.5
    val initialRate = (BigInt(1), BigInt(1))

    def createProvider(): Emulator = {
        val genesisHash = TestUtil.genesisHash

        Emulator(
          initialUtxos = Map(
            Input(genesisHash, 0) ->
                Output(
                  address = Alice.address,
                  value = Value.ada(100)
                ),
            Input(genesisHash, 1) ->
                Output(
                  address = Bob.address,
                  value = Value.ada(100)
                ),
            Input(genesisHash, 2) ->
                Output(
                  address = Oracle.address,
                  value = Value.ada(100)
                )
          ),
          initialContext = Context.testMainnet(),
          mutators = Set(scalus.cardano.ledger.rules.PlutusScriptsTransactionMutator)
        )
    }

    def createAndSubmitInitiateTx(provider: Emulator): (TransactionInput, Utxo) = {
        val utxos = provider.findUtxos(Alice.address).await().toOption.get
        val initTx = txCreator.initiatePricebet(
          ownerUtxos = utxos,
          betAmount = betAmount,
          ownerPkh = Alice.addrKeyHash,
          oracleScriptHash = oracleScriptAddress.scriptHashOption.get,
          deadline = deadline.toEpochMilli,
          exchangeRate = betExchangeRate,
          changeAddress = Alice.address,
          signer = Alice.signer
        )

        val result = provider.submit(initTx).await()
        assert(result.isRight, s"Failed to submit initiate tx")

        val pricebetUtxos = provider.findUtxos(pricebetScriptAddress).await().toOption.get
        val pricebetUtxo = Utxo(pricebetUtxos.head)
        (pricebetUtxo._1, pricebetUtxo)
    }

    def createAndSubmitOracleUtxo(
        provider: Emulator,
        rate: (BigInt, BigInt)
    ): Utxo = {
        val utxos = provider.findUtxos(Oracle.address).await().toOption.get
        val seedUtxo = Utxo(utxos.head)

        val createTx = txCreator.mintBeaconAndCreateOracle(
          utxos = utxos,
          seedUtxo = seedUtxo,
          beaconTokenName = beaconTokenName,
          authorizedSigner = Oracle.addrKeyHash,
          initialTimestamp = beforeDeadline.toEpochMilli,
          initialRateNominator = rate._1,
          initialRateDenominator = rate._2,
          sponsor = Oracle.address,
          signer = Oracle.signer
        )

        val result = provider.submit(createTx).await()
        assert(result.isRight, s"Failed to submit oracle creation: ${result.left.getOrElse(null)}")

        // Find the oracle UTXO
        val oracleUtxos = provider.findUtxos(oracleScriptAddress).await().toOption.get
        Utxo(oracleUtxos.head)
    }

    def assertSuccess(
        provider: Emulator,
        tx: Transaction,
        scriptInput: TransactionInput
    ): Unit = {
        val result = runValidator(provider, tx, scriptInput)
        assert(result.isSuccess, s"Direct validation failed: $result")

        val submitResult = provider.submit(tx).await()
        submitResult.left.foreach(err => fail(s"Emulator submission failed: $err"))
    }

    def assertFailure(
        provider: Emulator,
        tx: Transaction,
        scriptInput: TransactionInput,
        expectedError: String
    ): Unit = {
        // Test direct validation
        val result = runValidator(provider, tx, scriptInput)
        assert(result.isFailure, "Expected validation to fail but it succeeded")

        // Test via emulator
        val submitResult = provider.submit(tx).await()
        assert(submitResult.isLeft, s"Expected submission to fail but it succeeded")
    }

    def runValidator(
        provider: Emulator,
        tx: Transaction,
        scriptInput: TransactionInput
    ): scalus.uplc.eval.Result = {
        val utxos = {
            val body = tx.body.value
            val allInputs =
                (body.inputs.toSet.view ++ body.collateralInputs.toSet.view ++ body.referenceInputs.toSet.view).toSet
            provider.findUtxos(allInputs).await().toOption.get
        }

        val scriptContext = tx.getScriptContextV3(utxos, RedeemerPurpose.ForSpend(scriptInput))

        val allResolvedPlutusScriptsMap =
            scalus.cardano.ledger.utils.AllResolvedScripts
                .allResolvedPlutusScriptsMap(tx, utxos)
                .toOption
                .get

        // Determine which script address we're testing
        val scriptHash = utxos(scriptInput).address.scriptHashOption.get
        val plutusScript = allResolvedPlutusScriptsMap(scriptHash)
        val program = scalus.uplc.Program.fromCborByteString(plutusScript.script)

        program.runWithDebug(scriptContext)
    }
}
