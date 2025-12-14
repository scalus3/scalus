package scalus.examples.betting

import org.scalatest.funsuite.AnyFunSuite
import scalus.builtin.ByteString.hex
import scalus.builtin.Data.toData
import scalus.builtin.{platform, ByteString}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.rules.*
import scalus.cardano.ledger.utils.AllResolvedScripts
import scalus.cardano.node.Emulator
import scalus.cardano.txbuilder.TransactionSigner
import scalus.ledger.api.v1.{PosixTime, PubKeyHash}
import scalus.testing.kit.{ScalusTest, TestUtil}
import scalus.uplc.Program
import scalus.uplc.eval.Result

class BettingTransactionTest extends AnyFunSuite, ScalusTest {
    private val env = TestUtil.testEnvironment
    private val compiledContract = BettingContract.debugCompiledContract
    private val scriptAddress = compiledContract.address(env.network)

    // Generate real key pairs for all participants
    private val player1KeyPair @ (player1PrivateKey, player1PublicKey) = generateKeyPair()
    private val player2KeyPair @ (player2PrivateKey, player2PublicKey) = generateKeyPair()
    private val oracleKeyPair @ (oraclePrivateKey, oraclePublicKey) = generateKeyPair()
    private val deploymentKeyPair @ (deploymentPrivateKey, deploymentPublicKey) = generateKeyPair()

    // Create signers
    private val player1Signer = TransactionSigner(Set(player1KeyPair))
    private val player2Signer = TransactionSigner(Set(player2KeyPair))
    private val oracleSigner = TransactionSigner(Set(oracleKeyPair))
    private val deploymentSigner = TransactionSigner(Set(deploymentKeyPair))

    // Derive public key hashes and addresses
    private val player1Pkh = AddrKeyHash(platform.blake2b_224(player1PublicKey))
    private val player2Pkh = AddrKeyHash(platform.blake2b_224(player2PublicKey))
    private val oraclePkh = AddrKeyHash(platform.blake2b_224(oraclePublicKey))
    private val deploymentPkh = AddrKeyHash(platform.blake2b_224(deploymentPublicKey))

    private val player1Address = TestUtil.createTestAddress(player1Pkh)
    private val player2Address = TestUtil.createTestAddress(player2Pkh)
    private val oracleAddress = TestUtil.createTestAddress(oraclePkh)
    private val deploymentAddress = TestUtil.createTestAddress(deploymentPkh)
    private val changeAddress = TestUtil.createTestAddress("a" * 56)

    // Transaction creator factories
    private def transactionCreatorFor(signer: TransactionSigner) = BettingTransactionCreator(
      env = env,
      evaluator = PlutusScriptEvaluator(env, EvaluatorMode.EvaluateAndComputeCost),
      signer = signer,
      compiledContract = compiledContract
    )

    private def transactionCreatorWithConstEvaluatorFor(signer: TransactionSigner) =
        BettingTransactionCreator(
          env = env,
          evaluator = PlutusScriptEvaluator.constMaxBudget(env),
          signer = signer,
          compiledContract = compiledContract
        )

    // Test parameters
    private val betAmount = Coin(10_000_000L)
    private val commissionAmount = Coin(2_000_000L)

    private val slot: SlotNo = 3L
    private val beforeSlot: SlotNo = 2L
    private val afterSlot: SlotNo = 4L
    private val expiration: PosixTime = BigInt(env.slotConfig.slotToTime(slot))
    private val beforeTime: Long = env.slotConfig.slotToTime(beforeSlot)
    private val afterTime: Long = env.slotConfig.slotToTime(afterSlot)

    // Provider factory
    private def createProvider(): Emulator = {
        val genesisHash = TransactionHash.fromByteString(ByteString.fromHex("0" * 64))

        Emulator(
          initialUtxos = Map(
            TransactionInput(genesisHash, 0) ->
                TransactionOutput.Babbage(
                  address = player1Address,
                  value = Value.ada(1_000_000L)
                ),
            TransactionInput(genesisHash, 1) ->
                TransactionOutput.Babbage(
                  address = player2Address,
                  value = Value.ada(1_000_000L)
                ),
            TransactionInput(genesisHash, 2) ->
                TransactionOutput.Babbage(
                  address = oracleAddress,
                  value = Value.ada(1_000_000_000L)
                ),
            TransactionInput(genesisHash, 3) ->
                TransactionOutput.Babbage(
                  address = deploymentAddress,
                  value = Value.ada(1_000_000_000L)
                )
          ),
          initialContext = Context.testMainnet(),
          mutators = Set(PlutusScriptsTransactionMutator)
        )
    }

    private def runValidator(
        provider: Emulator,
        tx: Transaction,
        scriptInput: TransactionInput
    ): Result = {
        val utxos = {
            val body = tx.body.value
            val allInputs =
                (body.inputs.toSet.view ++ body.collateralInputs.toSet.view ++ body.referenceInputs.toSet.view).toSet
            provider.findUtxos(allInputs).toOption.get
        }

        val scriptContext =
            TestUtil.getScriptContextV3(tx, utxos, scriptInput, RedeemerTag.Spend, env)

        val allResolvedPlutusScriptsMap =
            AllResolvedScripts.allResolvedPlutusScriptsMap(tx, utxos).toOption.get
        val plutusScript =
            scriptAddress.scriptHashOption.flatMap(allResolvedPlutusScriptsMap.get).get
        val program = Program.fromCborByteString(plutusScript.script)

        program.runWithDebug(scriptContext)
    }

    test("deploy script") {
        val provider = createProvider()

        val deployTx = {
            val utxos = provider
                .findUtxos(
                  address = deploymentAddress,
                  minRequiredTotalAmount = Some(commissionAmount)
                )
                .toOption
                .get

            transactionCreatorFor(deploymentSigner)
                .deploy(utxos, deploymentAddress, deploymentAddress)
        }

        assert(provider.submit(deployTx).isRight)

        val scriptUtxo = provider
            .findUtxo(
              address = deploymentAddress,
              transactionId = Some(deployTx.id),
              datum = None,
              minAmount = None
            )
            .toOption
            .get

        assert(scriptUtxo._2.scriptRef.isDefined)
    }

    test("init bet") {
        val provider = createProvider()

        val deployTx = {
            val utxos = provider
                .findUtxos(
                  address = deploymentAddress,
                  minRequiredTotalAmount = Some(commissionAmount)
                )
                .toOption
                .get

            transactionCreatorFor(deploymentSigner)
                .deploy(utxos, deploymentAddress, deploymentAddress)
        }

        assert(provider.submit(deployTx).isRight)

        val scriptUtxo = provider
            .findUtxo(
              address = deploymentAddress,
              transactionId = Some(deployTx.id)
            )
            .toOption
            .get

        val initTx = {
            val utxos = provider
                .findUtxos(
                  address = player1Address,
                  minRequiredTotalAmount = Some(betAmount + commissionAmount)
                )
                .toOption
                .get

            transactionCreatorFor(player1Signer)
                .init(
                  utxos,
                  Utxo(utxos.head),
                  scriptUtxo,
                  betAmount,
                  PubKeyHash(player1Pkh),
                  PubKeyHash(oraclePkh),
                  expiration,
                  player1Address,
                  beforeTime
                )
        }

        assert(provider.submit(initTx).isRight)

        val initConfig = Config(
          PubKeyHash(player1Pkh),
          PubKeyHash(hex""),
          PubKeyHash(oraclePkh),
          expiration
        )

        val betUtxo = provider
            .findUtxo(
              address = scriptAddress,
              transactionId = Some(initTx.id),
              datum = Some(DatumOption.Inline(initConfig.toData)),
              minAmount = Some(betAmount)
            )
            .toOption
            .get

        assert(betUtxo._2.value.coin == betAmount)
    }

    test("player2 joins bet before expiration") {
        val provider = createProvider()

        val deployTx = {
            val utxos = provider
                .findUtxos(
                  address = deploymentAddress,
                  minRequiredTotalAmount = Some(commissionAmount)
                )
                .toOption
                .get

            transactionCreatorFor(deploymentSigner)
                .deploy(utxos, deploymentAddress, deploymentAddress)
        }

        assert(provider.submit(deployTx).isRight)

        val scriptUtxo = provider
            .findUtxo(address = deploymentAddress, transactionId = Some(deployTx.id))
            .toOption
            .get

        val initTx = {
            val utxos = provider
                .findUtxos(
                  address = player1Address,
                  minRequiredTotalAmount = Some(betAmount + commissionAmount)
                )
                .toOption
                .get

            transactionCreatorFor(player1Signer)
                .init(
                  utxos,
                  Utxo(utxos.head),
                  scriptUtxo,
                  betAmount,
                  PubKeyHash(player1Pkh),
                  PubKeyHash(oraclePkh),
                  expiration,
                  player1Address,
                  beforeTime
                )
        }

        assert(provider.submit(initTx).isRight)

        val initConfig = Config(
          PubKeyHash(player1Pkh),
          PubKeyHash(hex""),
          PubKeyHash(oraclePkh),
          expiration
        )

        val betUtxo = provider
            .findUtxo(
              address = scriptAddress,
              transactionId = Some(initTx.id),
              datum = Some(DatumOption.Inline(initConfig.toData)),
              minAmount = Some(betAmount)
            )
            .toOption
            .get

        val joinTx = {
            val utxos = provider
                .findUtxos(
                  address = player2Address,
                  minRequiredTotalAmount = Some(betAmount + commissionAmount)
                )
                .toOption
                .get

            transactionCreatorFor(player2Signer)
                .join(
                  utxos,
                  Utxo(utxos.head),
                  scriptUtxo,
                  betUtxo,
                  betAmount,
                  PubKeyHash(player1Pkh),
                  PubKeyHash(player2Pkh),
                  player2Pkh,
                  PubKeyHash(oraclePkh),
                  expiration,
                  player2Address,
                  beforeTime
                )
        }

        val result = runValidator(provider, joinTx, betUtxo._1)
        assert(result.isSuccess)

        provider.setSlot(beforeSlot - 1)
        assert(provider.submit(joinTx).isRight)

        val joinConfig = Config(
          PubKeyHash(player1Pkh),
          PubKeyHash(player2Pkh),
          PubKeyHash(oraclePkh),
          expiration
        )

        val joinedBetUtxo = provider
            .findUtxo(
              address = scriptAddress,
              transactionId = Some(joinTx.id),
              datum = Some(DatumOption.Inline(joinConfig.toData)),
              minAmount = Some(betAmount + betAmount)
            )
            .toOption
            .get

        assert(joinedBetUtxo._2.value.coin == betAmount + betAmount)
    }

    test("player2 joining fails after expiration") {
        val provider = createProvider()

        val deployTx = {
            val utxos = provider
                .findUtxos(
                  address = deploymentAddress,
                  minRequiredTotalAmount = Some(commissionAmount)
                )
                .toOption
                .get

            transactionCreatorFor(deploymentSigner)
                .deploy(utxos, deploymentAddress, deploymentAddress)
        }

        assert(provider.submit(deployTx).isRight)

        val scriptUtxo = provider
            .findUtxo(address = deploymentAddress, transactionId = Some(deployTx.id))
            .toOption
            .get

        val initTx = {
            val utxos = provider
                .findUtxos(
                  address = player1Address,
                  minRequiredTotalAmount = Some(betAmount + commissionAmount)
                )
                .toOption
                .get

            transactionCreatorFor(player1Signer)
                .init(
                  utxos,
                  Utxo(utxos.head),
                  scriptUtxo,
                  betAmount,
                  PubKeyHash(player1Pkh),
                  PubKeyHash(oraclePkh),
                  expiration,
                  player1Address,
                  beforeTime
                )
        }

        assert(provider.submit(initTx).isRight)

        val initConfig = Config(
          PubKeyHash(player1Pkh),
          PubKeyHash(hex""),
          PubKeyHash(oraclePkh),
          expiration
        )

        val betUtxo = provider
            .findUtxo(
              address = scriptAddress,
              transactionId = Some(initTx.id),
              datum = Some(DatumOption.Inline(initConfig.toData)),
              minAmount = Some(betAmount)
            )
            .toOption
            .get

        provider.setSlot(env.slotConfig.timeToSlot(expiration.toLong))

        val joinTx = {
            val utxos = provider
                .findUtxos(
                  address = player2Address,
                  minRequiredTotalAmount = Some(betAmount + commissionAmount)
                )
                .toOption
                .get

            transactionCreatorWithConstEvaluatorFor(player2Signer)
                .join(
                  utxos,
                  Utxo(utxos.head),
                  scriptUtxo,
                  betUtxo,
                  betAmount,
                  PubKeyHash(player1Pkh),
                  PubKeyHash(player2Pkh),
                  player2Pkh,
                  PubKeyHash(oraclePkh),
                  expiration,
                  player2Address,
                  beforeTime
                )
        }

        provider.submit(joinTx) match
            case Left(err) => succeed // Expected to fail
            case Right(_)  => fail("Transaction should have failed after expiration")
    }

    test("oracle announces winner after expiration") {
        val provider = createProvider()

        val deployTx = {
            val utxos = provider
                .findUtxos(
                  address = deploymentAddress,
                  minRequiredTotalAmount = Some(commissionAmount)
                )
                .toOption
                .get

            transactionCreatorFor(deploymentSigner)
                .deploy(utxos, deploymentAddress, deploymentAddress)
        }

        assert(provider.submit(deployTx).isRight)

        val scriptUtxo = provider
            .findUtxo(address = deploymentAddress, transactionId = Some(deployTx.id))
            .toOption
            .get

        val initTx = {
            val utxos = provider
                .findUtxos(
                  address = player1Address,
                  minRequiredTotalAmount = Some(betAmount + commissionAmount)
                )
                .toOption
                .get

            transactionCreatorFor(player1Signer)
                .init(
                  utxos,
                  Utxo(utxos.head),
                  scriptUtxo,
                  betAmount,
                  PubKeyHash(player1Pkh),
                  PubKeyHash(oraclePkh),
                  expiration,
                  player1Address,
                  beforeTime
                )
        }

        assert(provider.submit(initTx).isRight)

        val betUtxo = provider
            .findUtxo(address = scriptAddress, transactionId = Some(initTx.id))
            .toOption
            .get

        val joinTx = {
            val utxos = provider
                .findUtxos(
                  address = player2Address,
                  minRequiredTotalAmount = Some(betAmount + commissionAmount)
                )
                .toOption
                .get

            transactionCreatorFor(player2Signer)
                .join(
                  utxos,
                  Utxo(utxos.head),
                  scriptUtxo,
                  betUtxo,
                  betAmount,
                  PubKeyHash(player1Pkh),
                  PubKeyHash(player2Pkh),
                  player2Pkh,
                  PubKeyHash(oraclePkh),
                  expiration,
                  player2Address,
                  beforeTime
                )
        }

        provider.setSlot(beforeSlot - 1)
        assert(provider.submit(joinTx).isRight)

        val joinedBetUtxo = provider
            .findUtxo(address = scriptAddress, transactionId = Some(joinTx.id))
            .toOption
            .get

        val winTx = {
            val utxos = provider
                .findUtxos(address = oracleAddress, minRequiredTotalAmount = Some(commissionAmount))
                .toOption
                .get

            transactionCreatorFor(oracleSigner)
                .win(
                  utxos,
                  Utxo(utxos.head),
                  scriptUtxo,
                  joinedBetUtxo,
                  isJoinWin = true,
                  PubKeyHash(player1Pkh),
                  PubKeyHash(player2Pkh),
                  PubKeyHash(oraclePkh),
                  oraclePkh,
                  oracleAddress,
                  afterTime
                )
        }

        val result = runValidator(provider, winTx, joinedBetUtxo._1)
        assert(result.isSuccess)

        provider.setSlot(env.slotConfig.timeToSlot(afterTime))

        assert(provider.submit(winTx).isRight)

        val winnerUtxo = provider
            .findUtxo(
              address = player2Address,
              transactionId = Some(winTx.id),
              minAmount = Some(betAmount + betAmount)
            )
            .toOption
            .get

        assert(winnerUtxo._2.value.coin == betAmount + betAmount)
    }

    test("oracle announcing winner fails before expiration") {
        val provider = createProvider()

        val deployTx = {
            val utxos = provider
                .findUtxos(
                  address = deploymentAddress,
                  minRequiredTotalAmount = Some(commissionAmount)
                )
                .toOption
                .get

            transactionCreatorFor(deploymentSigner)
                .deploy(utxos, deploymentAddress, deploymentAddress)
        }

        assert(provider.submit(deployTx).isRight)

        val scriptUtxo = provider
            .findUtxo(address = deploymentAddress, transactionId = Some(deployTx.id))
            .toOption
            .get

        val initTx = {
            val utxos = provider
                .findUtxos(
                  address = player1Address,
                  minRequiredTotalAmount = Some(betAmount + commissionAmount)
                )
                .toOption
                .get

            transactionCreatorFor(player1Signer)
                .init(
                  utxos,
                  Utxo(utxos.head),
                  scriptUtxo,
                  betAmount,
                  PubKeyHash(player1Pkh),
                  PubKeyHash(oraclePkh),
                  expiration,
                  player1Address,
                  beforeTime
                )
        }

        assert(provider.submit(initTx).isRight)

        val betUtxo = provider
            .findUtxo(address = scriptAddress, transactionId = Some(initTx.id))
            .toOption
            .get

        val joinTx = {
            val utxos = provider
                .findUtxos(
                  address = player2Address,
                  minRequiredTotalAmount = Some(betAmount + commissionAmount)
                )
                .toOption
                .get

            transactionCreatorFor(player2Signer)
                .join(
                  utxos,
                  Utxo(utxos.head),
                  scriptUtxo,
                  betUtxo,
                  betAmount,
                  PubKeyHash(player1Pkh),
                  PubKeyHash(player2Pkh),
                  player2Pkh,
                  PubKeyHash(oraclePkh),
                  expiration,
                  player2Address,
                  beforeTime
                )
        }

        provider.setSlot(beforeSlot - 1)
        assert(provider.submit(joinTx).isRight)

        val joinedBetUtxo = provider
            .findUtxo(address = scriptAddress, transactionId = Some(joinTx.id))
            .toOption
            .get

        val winTx = {
            val utxos = provider
                .findUtxos(address = oracleAddress, minRequiredTotalAmount = Some(commissionAmount))
                .toOption
                .get

            transactionCreatorWithConstEvaluatorFor(oracleSigner)
                .win(
                  utxos,
                  Utxo(utxos.head),
                  scriptUtxo,
                  joinedBetUtxo,
                  isJoinWin = true,
                  PubKeyHash(player1Pkh),
                  PubKeyHash(player2Pkh),
                  PubKeyHash(oraclePkh),
                  oraclePkh,
                  oracleAddress,
                  beforeTime // Try to announce winner before expiration
                )
        }

        val result = runValidator(provider, winTx, joinedBetUtxo._1)
        assert(result.isFailure)

        provider.submit(winTx) match
            case Left(err) => succeed // Expected to fail
            case Right(_)  => fail("Transaction should have failed before expiration")
    }
}
