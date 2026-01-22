package scalus.examples.pricebet

import org.scalatest.funsuite.AnyFunSuite
import scalus.builtin.{ByteString, Data}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.EvaluatorMode.EvaluateAndComputeCost
import scalus.cardano.ledger.rules.Context
import scalus.cardano.node.Emulator
import scalus.cardano.txbuilder.RedeemerPurpose
import scalus.ledger.api.v3.TxOutRef
import scalus.prelude.Rational
import scalus.testing.kit.Party.{Alice, Bob, Oracle}
import scalus.testing.kit.TestUtil.getScriptContextV3
import scalus.testing.kit.{ScalusTest, TestUtil}
import scalus.utils.await

import java.time.Instant
import scala.concurrent.ExecutionContext.Implicits.global

class PricebetValidatorTest extends AnyFunSuite, ScalusTest {
    import PricebetValidatorTest.{*, given}

    test("Owner initiates bet successfully") {
        val provider = createProvider()
        val txCreator = createTxCreator(provider)
        val utxos = provider.findUtxos(Alice.address).await().toOption.get

        val initTx = txCreator.initiatePricebet(
          ownerUtxos = utxos,
          betAmount = betAmount,
          ownerPkh = Alice.addrKeyHash,
          deadline = deadline.toEpochMilli,
          exchangeRate = betExchangeRate,
          changeAddress = Alice.address,
          signer = Alice.signer
        )

        val result = provider.submit(initTx).await()
        assert(result.isRight, s"Failed to submit initiate tx: ${result.left}")
    }

    test("Player joins successfully") {
        val provider = createProvider()
        val txCreator = createTxCreator(provider)
        val (_, pricebetUtxo) = createAndSubmitInitiateTx(provider, txCreator)
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
        val txCreator = createTxCreator(provider)
        val (_, pricebetUtxo) = createAndSubmitInitiateTx(provider, txCreator)

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
        val pricebetUtxos =
            provider.findUtxos(txCreator.pricebetScriptAddress).await().toOption.get
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
        val txCreator = createTxCreator(provider)
        val oracleUtxo = createAndSubmitOracleUtxo(provider, txCreator, winningRate)
        val (_, pricebetUtxo) = createAndSubmitInitiateTx(provider, txCreator)

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
        val pricebetUtxos =
            provider.findUtxos(txCreator.pricebetScriptAddress).await().toOption.get
        val joinedPricebetUtxo = Utxo(pricebetUtxos.head)

        // Player wins
        val utxos2 = provider.findUtxos(Bob.address).await().toOption.get
        val winTx = txCreator.win(
          utxos = utxos2,
          pricebetUtxo = joinedPricebetUtxo,
          oracleUtxo = oracleUtxo,
          playerAddress = Bob.address,
          sponsor = Bob.address,
          validFrom = beforeDeadline,
          validTo = deadline,
          signer = Bob.signer
        )

        provider.setSlot(beforeSlot)
        assertSuccess(provider, winTx, joinedPricebetUtxo._1)
    }

    test("Fails to win with a low rate") {
        val provider = createProvider()
        val txCreator = createTxCreator(provider)
        val oracleUtxo = createAndSubmitOracleUtxo(provider, txCreator, losingRate)
        val (_, pricebetUtxo) = createAndSubmitInitiateTx(provider, txCreator)

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
        val pricebetUtxos =
            provider.findUtxos(txCreator.pricebetScriptAddress).await().toOption.get
        val joinedPricebetUtxo = Utxo(pricebetUtxos.head)

        // Player tries to win but should fail
        val utxos2 = provider.findUtxos(Bob.address).await().toOption.get
        val winTx = txCreator.win(
          utxos = utxos2,
          pricebetUtxo = joinedPricebetUtxo,
          oracleUtxo = oracleUtxo,
          playerAddress = Bob.address,
          sponsor = Bob.address,
          validFrom = beforeDeadline,
          validTo = deadline,
          signer = Bob.signer
        )

        provider.setSlot(beforeSlot)
        assertFailure(provider, winTx, joinedPricebetUtxo._1, "Oracle rate must exceed bet rate")
    }

    test("Owner times out after deadline") {
        val provider = createProvider()
        val txCreator = createTxCreator(provider)
        val (_, pricebetUtxo) = createAndSubmitInitiateTx(provider, txCreator)

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
        val txCreator = createTxCreator(provider)
        val (_, pricebetUtxo) = createAndSubmitInitiateTx(provider, txCreator)

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
        val txCreator = createTxCreator(provider)
        val oracleUtxo = createAndSubmitOracleUtxo(provider, txCreator, initialRate)

        val utxos = provider.findUtxos(Alice.address).await().toOption.get
        val updateTx = txCreator.updateOracle(
          utxos = utxos,
          oracleUtxo = oracleUtxo,
          newTimestamp = updateTimestamp.toEpochMilli,
          newExchangeRate = winningRate,
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
        val txCreator = createTxCreator(provider)
        val oracleUtxo = createAndSubmitOracleUtxo(provider, txCreator, initialRate)

        // Build a transaction manually with Alice as required signer instead of Oracle
        // The script bakes in Oracle as authorizedSigner, so using Alice should fail
        val newState = OracleState(
          timestamp = updateTimestamp.toEpochMilli,
          exchangeRate = winningRate
        )
        val redeemer = SpendOracleRedeemer.Update(oracleUtxoIndex = BigInt(0))
        val utxos = provider.findUtxos(Bob.address).await().toOption.get

        import scalus.cardano.txbuilder.TxBuilder
        val updateTx = TxBuilder(env, evaluator)
            .spend(
              oracleUtxo,
              redeemer,
              txCreator.oracleScript,
              Set(Alice.addrKeyHash) // Wrong signer - Alice instead of Oracle
            )
            .payTo(txCreator.oracleScriptAddress, oracleUtxo.output.value, newState)
            .validFrom(updateValidFrom)
            .validTo(updateValidTo)
            .complete(availableUtxos = utxos, Bob.address)
            .sign(Alice.signer) // Alice signs but script requires Oracle
            .sign(Bob.signer)
            .transaction

        provider.setSlot(updateSlot)
        assertFailure(provider, updateTx, oracleUtxo._1, "Must be signed by the authorized signer")
    }

    test("Oracle discovery via beacon token") {
        val provider = createProvider()
        val txCreator = createTxCreator(provider)

        val oracleUtxos = provider.findUtxos(Oracle.address).await().toOption.get

        val createTx = txCreator.mintBeaconAndCreateOracle(
          utxos = oracleUtxos,
          initialTimestamp = oracleTimestamp.toEpochMilli,
          initialExchangeRate = winningRate,
          sponsor = Oracle.address,
          signer = Oracle.signer
        )

        val result = provider.submit(createTx).await()
        assert(result.isRight, s"Failed to submit oracle creation: ${result.left}")

        // Step 2: Discover oracle by searching for beacon token (real-world flow)
        val beaconPolicyId = txCreator.beaconPolicyId

        // Search all UTXOs at oracle script address
        val allOracleUtxos = provider.findUtxos(txCreator.oracleScriptAddress).await().toOption.get

        // Filter for the one with our beacon token
        val discoveredOracleUtxo = allOracleUtxos.find { case (input, output) =>
            output.value.assets.assets.exists { case (assetId, assets) =>
                assetId == ScriptHash.fromByteString(
                  beaconPolicyId
                ) && assets.size == 1 && assets.head._1 == AssetName(
                  beaconTokenName
                )
            }
        }

        assert(discoveredOracleUtxo.isDefined, "Should be able to find oracle by beacon token")

        // Step 3: Verify we can read the oracle state from discovered UTXO
        val (_, discoveredOutput) = discoveredOracleUtxo.get
        val oracleState = discoveredOutput.datumOption.get.dataOption.get.to[OracleState]

        assert(
          oracleState.exchangeRate == winningRate,
          "Oracle should have correct exchange rate"
        )
    }

    test("Burn beacon token successfully") {
        val provider = createProvider()
        val txCreator =
            createTxCreator(provider, PlutusScriptEvaluator(env, EvaluateAndComputeCost))

        // First create the oracle with beacon
        val oracleUtxo = createAndSubmitOracleUtxo(provider, txCreator, initialRate)

        val oracleUtxos = provider.findUtxos(Oracle.address).await().toOption.get

        // Now burn the beacon token by spending the oracle UTXO
        // Oracle has UTXOs for fees and is the authorized signer
        val burnTx = txCreator.burnBeacon(
          utxos = oracleUtxos,
          oracleUtxo = oracleUtxo,
          sponsor = Oracle.address,
          signer = Oracle.signer
        )

        val result = provider.submit(burnTx).await()
        assert(result.isRight, s"Failed to burn beacon: ${result.left}")
    }

    test("Burn beacon fails without authorized signer") {
        val provider = createProvider()
        val txCreator =
            createTxCreator(provider, PlutusScriptEvaluator(env, EvaluateAndComputeCost))

        // First create the oracle with beacon
        val oracleUtxo = createAndSubmitOracleUtxo(provider, txCreator, initialRate)

        // Try to burn with wrong signer (Alice instead of Oracle)
        import scalus.cardano.txbuilder.{TxBuilder, TwoArgumentPlutusScriptWitness}
        val aliceUtxos = provider.findUtxos(Alice.address).await().toOption.get
        val collateralUtxo = Utxo(aliceUtxos.head)

        val burnTx = TxBuilder(env, evaluator)
            .spend(
              oracleUtxo,
              SpendOracleRedeemer.Burn,
              txCreator.oracleScript,
              Set(Alice.addrKeyHash) // Wrong signer
            )
            .collaterals(collateralUtxo)
            .mint(
              ScriptHash.fromByteString(txCreator.beaconPolicyId),
              Map(AssetName(beaconTokenName) -> -1L),
              TwoArgumentPlutusScriptWitness.attached(
                txCreator.oracleScript,
                MintOracleRedeemer.Burn,
                Set(Alice.addrKeyHash) // Wrong signer
              )
            )
            .complete(availableUtxos = aliceUtxos, Alice.address)
            .sign(Alice.signer)
            .transaction

        val result = provider.submit(burnTx).await()
        assert(result.isLeft, s"Expected burn to fail but it succeeded")
    }

    test("Happy path") {
        val provider = createProvider()
        val txCreator = createTxCreator(provider)

        // create oracle with rate = 1/2
        val lowRate = Rational(BigInt(1), BigInt(2))
        val oracleUtxo = createAndSubmitOracleUtxo(provider, txCreator, lowRate)

        // owner initiates bet with rate = 3/4
        val betRate = Rational(BigInt(3), BigInt(4))
        val (_, pricebetUtxo) = {
            val utxos = provider.findUtxos(Alice.address).await().toOption.get
            val initTx = txCreator.initiatePricebet(
              ownerUtxos = utxos,
              betAmount = betAmount,
              ownerPkh = Alice.addrKeyHash,
              deadline = deadline.toEpochMilli,
              exchangeRate = betRate,
              changeAddress = Alice.address,
              signer = Alice.signer
            )
            val result = provider.submit(initTx).await()
            assert(result.isRight, s"Failed to submit: ${result.left}")

            val pricebetUtxos =
                provider.findUtxos(txCreator.pricebetScriptAddress).await().toOption.get
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
            assert(result.isRight, s"Failed to submit join: ${result.left}")

            val pricebetUtxos =
                provider.findUtxos(txCreator.pricebetScriptAddress).await().toOption.get
            Utxo(pricebetUtxos.head)
        }

        val bobUtxosBeforeWin = provider.findUtxos(Bob.address).await().toOption.get
        val winTxLowRate = txCreator.win(
          utxos = bobUtxosBeforeWin,
          pricebetUtxo = pricebetUtxoAfterJoin,
          oracleUtxo = oracleUtxo,
          playerAddress = Bob.address,
          sponsor = Bob.address,
          validFrom = beforeDeadline,
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
        val highRate = Rational(BigInt(7), BigInt(8))
        val updatedOracleUtxo = {
            provider.setSlot(updateSlot) // Set slot for oracle update
            val aliceUtxos = provider.findUtxos(Alice.address).await().toOption.get
            val updateTx = txCreator.updateOracle(
              utxos = aliceUtxos,
              oracleUtxo = oracleUtxo,
              newTimestamp = updateTimestamp.toEpochMilli,
              newExchangeRate = highRate,
              sponsor = Alice.address,
              validFrom = updateValidFrom,
              validTo = updateValidTo,
              oracleSigner = Oracle.signer,
              sponsorSigner = Alice.signer
            )
            val result = provider.submit(updateTx).await()
            assert(result.isRight, s"Failed to update oracle: ${result.left}")

            val oracleUtxos =
                provider.findUtxos(txCreator.oracleScriptAddress).await().toOption.get
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
          signer = Bob.signer,
          validFrom = updateValidFrom
        )

        provider.setSlot(beforeSlot)
        assertSuccess(provider, winTxHighRate, pricebetUtxoAfterJoin._1)
    }
}

object PricebetValidatorTest extends ScalusTest {
    given env: CardanoInfo = TestUtil.testEnvironment
    private val evaluator = PlutusScriptEvaluator.constMaxBudget(env)

    // Beacon token name for oracle (policy ID is derived from oracle script hash)
    private val beaconTokenName = ByteString.fromString("ORACLE")

    // Test parameters
    private val betAmount = Coin.ada(10)
    private val deadline = Instant.parse("2025-01-15T00:00:00Z")
    private val beforeDeadline =
        Instant.parse("2025-01-14T12:00:00Z") // Middle of day, well before deadline
    private val afterDeadline = Instant.parse("2025-01-15T00:00:01Z")
    // Oracle timestamp must be BEFORE the validity interval
    private val oracleTimestamp = Instant.parse("2025-01-14T10:00:00Z")
    // Update timestamps for oracle update tests
    private val updateTimestamp = Instant.parse("2025-01-14T11:00:00Z")
    private val updateValidFrom = Instant.parse("2025-01-14T12:00:00Z")
    private val updateValidTo = Instant.parse("2025-01-14T14:00:00Z")

    private val beforeSlot = env.slotConfig.timeToSlot(beforeDeadline.toEpochMilli).toLong
    private val deadlineSlot = env.slotConfig.timeToSlot(deadline.toEpochMilli).toLong
    private val afterDeadlineSlot = env.slotConfig.timeToSlot(afterDeadline.toEpochMilli).toLong
    // Use validFrom time for the slot since that's when the tx is valid
    private val updateSlot = env.slotConfig.timeToSlot(updateValidFrom.toEpochMilli).toLong

    // Exchange rates as Rational
    // Bet rate: 3/2 = 1.5
    private val betExchangeRate = Rational(BigInt(3), BigInt(2))
    // Winning rate: 8/5 = 1.6 > 1.5
    private val winningRate = Rational(BigInt(8), BigInt(5))
    // Losing rate: 7/5 = 1.4 < 1.5
    private val losingRate = Rational(BigInt(7), BigInt(5))
    // Initial rate: 1/1 = 1.0 < 1.5
    private val initialRate = Rational(BigInt(1), BigInt(1))

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
                  address = Alice.address,
                  value = Value.ada(100)
                ),
            Input(genesisHash, 2) ->
                Output(
                  address = Bob.address,
                  value = Value.ada(100)
                ),
            Input(genesisHash, 3) ->
                Output(
                  address = Oracle.address,
                  value = Value.ada(100)
                ),
            Input(genesisHash, 4) ->
                Output(
                  address = Oracle.address,
                  value = Value.ada(100)
                ),
          ),
          initialContext = Context.testMainnet(),
          mutators = Set(scalus.cardano.ledger.rules.PlutusScriptsTransactionMutator)
        )
    }

    /** Creates a txCreator with configs derived from the first Oracle UTXO as seed. */
    def createTxCreator(
        provider: Emulator,
        eval: PlutusScriptEvaluator = evaluator
    ): PricebetTransactions = {
        val oracleUtxos = provider.findUtxos(Oracle.address).await().toOption.get
        val seedUtxo = oracleUtxos.head
        val seedTxOutRef = TxOutRef(
          scalus.ledger.api.v3.TxId(seedUtxo._1.transactionId),
          seedUtxo._1.index.toLong
        )

        val oracleConfig = OracleConfig(
          seedUtxo = seedTxOutRef,
          beaconPolicyId = ByteString.empty, // Will be set from script hash
          beaconName = beaconTokenName,
          authorizedSigner = scalus.ledger.api.v1.PubKeyHash(Oracle.addrKeyHash)
        )

        val oracleContract = OracleContract(oracleConfig)
        val actualBeaconPolicyId = oracleContract.address(env.network).scriptHashOption.get

        val pricebetConfig = PricebetConfig(oracleScriptHash = actualBeaconPolicyId)

        PricebetTransactions(
          env = env,
          evaluator = eval,
          oracleConfig = oracleConfig,
          pricebetConfig = pricebetConfig
        )
    }

    def createAndSubmitInitiateTx(
        provider: Emulator,
        txCreator: PricebetTransactions
    ): (TransactionInput, Utxo) = {
        val utxos = provider.findUtxos(Alice.address).await().toOption.get
        val initTx = txCreator.initiatePricebet(
          ownerUtxos = utxos,
          betAmount = betAmount,
          ownerPkh = Alice.addrKeyHash,
          deadline = deadline.toEpochMilli,
          exchangeRate = betExchangeRate,
          changeAddress = Alice.address,
          signer = Alice.signer
        )

        val result = provider.submit(initTx).await()
        assert(result.isRight, s"Failed to submit initiate tx: ${result.left}")

        val pricebetUtxos =
            provider.findUtxos(txCreator.pricebetScriptAddress).await().toOption.get
        val pricebetUtxo = Utxo(pricebetUtxos.head)
        (pricebetUtxo._1, pricebetUtxo)
    }

    def createAndSubmitOracleUtxo(
        provider: Emulator,
        txCreator: PricebetTransactions,
        rate: Rational
    ): Utxo = {
        val utxos = provider.findUtxos(Oracle.address).await().toOption.get

        val createTx = txCreator.mintBeaconAndCreateOracle(
          utxos = utxos,
          initialTimestamp = oracleTimestamp.toEpochMilli,
          initialExchangeRate = rate,
          sponsor = Oracle.address,
          signer = Oracle.signer
        )

        val result = provider.submit(createTx).await()
        assert(result.isRight, s"Failed to submit oracle creation: ${result.left}")

        // Find the oracle UTXO
        val oracleUtxos = provider.findUtxos(txCreator.oracleScriptAddress).await().toOption.get
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
        assert(
          submitResult.isRight,
          s"Emulator submission failed: ${submitResult.left}"
        )
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
