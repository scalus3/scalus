package scalus.examples.betting

import org.scalatest.funsuite.AnyFunSuite
import scalus.builtin.ByteString.hex
import scalus.builtin.Data.toData
import scalus.builtin.platform
import scalus.cardano.ledger.*
import scalus.cardano.ledger.rules.*
import scalus.cardano.ledger.utils.AllResolvedScripts
import scalus.cardano.node.Emulator
import scalus.cardano.txbuilder.{RedeemerPurpose, TransactionSigner}
import scalus.ledger.api.v1.{PosixTime, PubKeyHash}
import scalus.testing.kit.TestUtil.{genesisHash, getScriptContextV3}
import scalus.testing.kit.{ScalusTest, TestUtil}
import scalus.uplc.eval.Result
import scalus.utils.await

import scala.concurrent.ExecutionContext.Implicits.global

class BettingTransactionTest extends AnyFunSuite, ScalusTest {
    private given env: CardanoInfo = TestUtil.testEnvironment
    private val contract = BettingContract.withErrorTraces
    private val scriptAddress = contract.address(env.network)

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

    // Transaction creator factories
    private def transactionCreatorFor(signer: TransactionSigner) = BettingTransactionCreator(
      env = env,
      evaluator = PlutusScriptEvaluator(env, EvaluatorMode.EvaluateAndComputeCost),
      signer = signer,
      contract = contract
    )

    private def transactionCreatorWithConstEvaluatorFor(signer: TransactionSigner) =
        BettingTransactionCreator(
          env = env,
          evaluator = PlutusScriptEvaluator.constMaxBudget(env),
          signer = signer,
          contract = contract
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
        Emulator(
          initialUtxos = Map(
            Input(genesisHash, 0) ->
                Output(
                  address = player1Address,
                  value = Value.ada(1_000_000L)
                ),
            Input(genesisHash, 1) ->
                Output(
                  address = player2Address,
                  value = Value.ada(1_000_000L)
                ),
            Input(genesisHash, 2) ->
                Output(
                  address = oracleAddress,
                  value = Value.ada(1_000_000_000L)
                ),
            Input(genesisHash, 3) ->
                Output(
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
            provider.findUtxos(allInputs).await().toOption.get
        }

        val scriptContext = tx.getScriptContextV3(utxos, RedeemerPurpose.ForSpend(scriptInput))

        val allResolvedPlutusScriptsMap =
            AllResolvedScripts.allResolvedPlutusScriptsMap(tx, utxos).toOption.get
        val plutusScript =
            scriptAddress.scriptHashOption.flatMap(allResolvedPlutusScriptsMap.get).get
        val program = plutusScript.deBruijnedProgram.toProgram

        program.runWithDebug(scriptContext)
    }

    private def deployScript(provider: Emulator) = {
        val deployTx = {
            val utxos = provider
                .findUtxos(
                  address = deploymentAddress,
                  minRequiredTotalAmount = Some(commissionAmount)
                )
                .await()
                .toOption
                .get

            transactionCreatorFor(deploymentSigner)
                .deploy(utxos, deploymentAddress, deploymentAddress)
        }

        assert(provider.submit(deployTx).await().isRight)

        val scriptUtxos = provider
            .findUtxos(
              address = deploymentAddress,
              transactionId = Some(deployTx.id),
            )
            .await()
            .getOrElse(fail("No UTXOs found at deployment address"))
        Utxo(scriptUtxos.find((in, out) => out.scriptRef.isDefined).get)
    }

    test("deploy script") {
        val provider = createProvider()
        val scriptUtxo = deployScript(provider)
        assert(scriptUtxo.output.scriptRef.isDefined)
    }

    test("init bet") {
        val provider = createProvider()
        val scriptUtxo = deployScript(provider)

        val initTx = {
            val utxos = provider
                .findUtxos(
                  address = player1Address,
                  minRequiredTotalAmount = Some(betAmount + commissionAmount)
                )
                .await()
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

        assert(provider.submit(initTx).await().isRight)

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
            .await()
            .toOption
            .get

        assert(betUtxo._2.value.coin == betAmount)
    }

    test("player2 joins bet before expiration") {
        val provider = createProvider()
        val scriptUtxo = deployScript(provider)

        val initTx = {
            val utxos = provider
                .findUtxos(
                  address = player1Address,
                  minRequiredTotalAmount = Some(betAmount + commissionAmount)
                )
                .await()
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

        assert(provider.submit(initTx).await().isRight)

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
            .await()
            .toOption
            .get

        val joinTx = {
            val utxos = provider
                .findUtxos(
                  address = player2Address,
                  minRequiredTotalAmount = Some(betAmount + commissionAmount)
                )
                .await()
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
        assert(provider.submit(joinTx).await().isRight)

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
            .await()
            .toOption
            .get

        assert(joinedBetUtxo._2.value.coin == betAmount + betAmount)
    }

    test("player2 joining fails after expiration") {
        val provider = createProvider()
        val scriptUtxo = deployScript(provider)

        val initTx = {
            val utxos = provider
                .findUtxos(
                  address = player1Address,
                  minRequiredTotalAmount = Some(betAmount + commissionAmount)
                )
                .await()
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

        assert(provider.submit(initTx).await().isRight)

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
            .await()
            .toOption
            .get

        provider.setSlot(env.slotConfig.timeToSlot(expiration.toLong))

        val joinTx = {
            val utxos = provider
                .findUtxos(
                  address = player2Address,
                  minRequiredTotalAmount = Some(betAmount + commissionAmount)
                )
                .await()
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

        provider.submit(joinTx).await() match
            case Left(err) => succeed // Expected to fail
            case Right(_)  => fail("Transaction should have failed after expiration")
    }

    test("oracle announces winner after expiration") {
        val provider = createProvider()
        val scriptUtxo = deployScript(provider)

        val initTx = {
            val utxos = provider
                .findUtxos(
                  address = player1Address,
                  minRequiredTotalAmount = Some(betAmount + commissionAmount)
                )
                .await()
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

        assert(provider.submit(initTx).await().isRight)

        val betUtxo = provider
            .findUtxo(address = scriptAddress, transactionId = Some(initTx.id))
            .await()
            .toOption
            .get

        val joinTx = {
            val utxos = provider
                .findUtxos(
                  address = player2Address,
                  minRequiredTotalAmount = Some(betAmount + commissionAmount)
                )
                .await()
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
        assert(provider.submit(joinTx).await().isRight)

        val joinedBetUtxo = provider
            .findUtxo(address = scriptAddress, transactionId = Some(joinTx.id))
            .await()
            .toOption
            .get

        val winTx = {
            val utxos = provider
                .findUtxos(address = oracleAddress, minRequiredTotalAmount = Some(commissionAmount))
                .await()
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

        assert(provider.submit(winTx).await().isRight)

        val winnerUtxo = provider
            .findUtxo(
              address = player2Address,
              transactionId = Some(winTx.id),
              minAmount = Some(betAmount + betAmount)
            )
            .await()
            .toOption
            .get

        assert(winnerUtxo._2.value.coin == betAmount + betAmount)
    }

    test("oracle announcing winner fails before expiration") {
        val provider = createProvider()
        val scriptUtxo = deployScript(provider)

        val initTx = {
            val utxos = provider
                .findUtxos(
                  address = player1Address,
                  minRequiredTotalAmount = Some(betAmount + commissionAmount)
                )
                .await()
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

        assert(provider.submit(initTx).await().isRight)

        val betUtxo = provider
            .findUtxo(address = scriptAddress, transactionId = Some(initTx.id))
            .await()
            .toOption
            .get

        val joinTx = {
            val utxos = provider
                .findUtxos(
                  address = player2Address,
                  minRequiredTotalAmount = Some(betAmount + commissionAmount)
                )
                .await()
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
        assert(provider.submit(joinTx).await().isRight)

        val joinedBetUtxo = provider
            .findUtxo(address = scriptAddress, transactionId = Some(joinTx.id))
            .await()
            .toOption
            .get

        val winTx = {
            val utxos = provider
                .findUtxos(address = oracleAddress, minRequiredTotalAmount = Some(commissionAmount))
                .await()
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

        provider.submit(winTx).await() match
            case Left(err) => succeed // Expected to fail
            case Right(_)  => fail("Transaction should have failed before expiration")
    }
}
