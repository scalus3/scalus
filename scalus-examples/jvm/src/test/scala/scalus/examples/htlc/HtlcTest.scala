package scalus.examples.htlc

import org.scalatest.funsuite.AnyFunSuite
import scalus.builtin.Builtins.sha3_256
import scalus.builtin.Data.toData
import scalus.builtin.{platform, ByteString}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.rules.*
import scalus.cardano.ledger.utils.AllResolvedScripts
import scalus.cardano.node.{Emulator, SubmitError}
import scalus.cardano.txbuilder.TransactionSigner
import scalus.ledger.api.v1.PubKeyHash
import scalus.testing.kit.{ScalusTest, TestUtil}
import scalus.uplc.Program
import scalus.uplc.eval.Result
import scalus.utils.await

import scala.concurrent.ExecutionContext.Implicits.global

class HtlcTest extends AnyFunSuite, ScalusTest {
    private val env = TestUtil.testEnvironment
    private val compiledContract = HtlcContract.withErrorTraces
    private val scriptAddress = compiledContract.address(env.network)

    private val committerKeyPair @ (committerPrivateKey, committerPublicKey) = generateKeyPair()
    private val receiverKeyPair @ (receiverPrivateKey, receiverPublicKey) = generateKeyPair()
    private val wrongCommitterKeyPair @ (wrongCommitterPrivateKey, wrongCommitterPublicKey) =
        generateKeyPair()
    private val wrongReceiverKeyPair @ (wrongReceiverPrivateKey, wrongReceiverPublicKey) =
        generateKeyPair()

    private val committerSigner = TransactionSigner(Set(committerKeyPair))
    private val receiverSigner = TransactionSigner(Set(receiverKeyPair))
    private val wrongCommitterSigner = TransactionSigner(Set(wrongCommitterKeyPair))
    private val wrongReceiverSigner = TransactionSigner(Set(wrongReceiverKeyPair))

    private val txCreator = HtlcTransactionCreator(
      env = env,
      evaluator = PlutusScriptEvaluator(env, EvaluatorMode.EvaluateAndComputeCost),
      contract = compiledContract
    )

    private val txCreatorWithConstEvaluator = HtlcTransactionCreator(
      env = env,
      evaluator = PlutusScriptEvaluator.constMaxBudget(env),
      contract = compiledContract
    )

    private val committerPkh = AddrKeyHash(platform.blake2b_224(committerPublicKey))
    private val receiverPkh = AddrKeyHash(platform.blake2b_224(receiverPublicKey))
    private val wrongCommitterPkh = AddrKeyHash(platform.blake2b_224(wrongCommitterPublicKey))
    private val wrongReceiverPkh = AddrKeyHash(platform.blake2b_224(wrongReceiverPublicKey))

    private val committerAddress = TestUtil.createTestAddress(committerPkh)
    private val receiverAddress = TestUtil.createTestAddress(receiverPkh)
    private val changeAddress = TestUtil.createTestAddress("a" * 56)

    private val lockAmount = Coin(10_000_000L)
    private val commissionAmount = Coin(2_000_000L)

    private val slot: SlotNo = 10
    private val beforeSlot: SlotNo = slot - 1
    private val afterSlot: SlotNo = slot + 1
    private val timeout: Long = env.slotConfig.slotToTime(slot)
    private val beforeTimeout: Long = env.slotConfig.slotToTime(beforeSlot)
    private val afterTimeout: Long = env.slotConfig.slotToTime(afterSlot)

    private val validPreimage: Preimage = genByteStringOfN(32).sample.get
    private val wrongPreimage: Preimage = genByteStringOfN(12).sample.get
    private val image: Image = sha3_256(validPreimage)

    private val datum = Config(
      PubKeyHash(committerPkh),
      PubKeyHash(receiverPkh),
      image,
      timeout
    ).toData

    private def createProvider(): Emulator = {
        val genesisHash = TransactionHash.fromByteString(ByteString.fromHex("0" * 64))

        Emulator(
          initialUtxos = Map(
            Input(genesisHash, 0) ->
                TransactionOutput.Babbage(
                  address = committerAddress,
                  value = Value.lovelace(100_000_000L)
                ),
            Input(genesisHash, 1) ->
                TransactionOutput.Babbage(
                  address = receiverAddress,
                  value = Value.lovelace(100_000_000L)
                )
          ),
          initialContext = Context.testMainnet(),
          mutators = Set(PlutusScriptsTransactionMutator)
        )
    }

    private def runValidator(
        provider: Emulator,
        tx: Transaction,
        lockedInput: TransactionInput
    ): Result = {
        val utxos = {
            val body = tx.body.value
            val allInputs =
                (body.inputs.toSet.view ++ body.collateralInputs.toSet.view ++ body.referenceInputs.toSet.view).toSet
            provider.findUtxos(allInputs).await().toOption.get
        }

        val scriptContext =
            TestUtil.getScriptContextV3(tx, utxos, lockedInput, RedeemerTag.Spend, env)

        val allResolvedPlutusScriptsMap =
            AllResolvedScripts.allResolvedPlutusScriptsMap(tx, utxos).toOption.get
        val plutusScript =
            scriptAddress.scriptHashOption.flatMap(allResolvedPlutusScriptsMap.get).get
        val program = Program.fromCborByteString(plutusScript.script)

        program.runWithDebug(scriptContext)
    }

    private def createAndSubmitLockTx(provider: Emulator): (Transaction, Utxo) = {
        val utxos = provider
            .findUtxos(
              address = committerAddress,
              minRequiredTotalAmount = Some(lockAmount + commissionAmount)
            )
            .await()
            .toOption
            .get

        val lockTx = txCreator.lock(
          utxos = utxos,
          value = Value(lockAmount),
          changeAddress = committerAddress,
          committer = committerPkh,
          receiver = receiverPkh,
          image = image,
          timeout = timeout,
          signer = committerSigner
        )

        assert(provider.submit(lockTx).await().isRight)

        val lockedUtxo = provider
            .findUtxo(
              address = scriptAddress,
              transactionId = Some(lockTx.id),
              datum = Some(DatumOption.Inline(datum)),
              minAmount = Some(lockAmount)
            )
            .await()
            .toOption
            .get

        assert(lockedUtxo._2.value.coin == lockAmount)

        (lockTx, lockedUtxo)
    }

    test("receiver reveals preimage before timeout") {
        val provider = createProvider()
        val (lockTx, lockedUtxo) = createAndSubmitLockTx(provider)

        val revealTx = {
            val utxos = provider
                .findUtxos(
                  address = receiverAddress,
                  minRequiredTotalAmount = Some(commissionAmount)
                )
                .await()
                .toOption
                .get

            txCreator.reveal(
              utxos = utxos,
              collateralUtxos = utxos,
              lockedUtxo = lockedUtxo,
              payeeAddress = receiverAddress,
              changeAddress = changeAddress,
              preimage = validPreimage,
              receiverPkh = receiverPkh,
              time = timeout,
              signer = receiverSigner
            )
        }

        val result = runValidator(provider, revealTx, lockedUtxo._1)
        assert(result.isSuccess)

        provider.setSlot(env.slotConfig.timeToSlot(beforeTimeout))

        assert(provider.submit(revealTx).await().isRight)

        val unlockedUtxo = provider
            .findUtxo(
              address = receiverAddress,
              transactionId = Some(revealTx.id),
              minAmount = Some(lockAmount)
            )
            .await()
            .toOption
            .get

        assert(unlockedUtxo._2.value.coin == lockAmount)
    }

    test("receiver fails with wrong preimage") {
        val provider = createProvider()
        val (lockTx, lockedUtxo) = createAndSubmitLockTx(provider)

        val revealTx = {
            val utxos = provider
                .findUtxos(
                  address = receiverAddress,
                  minRequiredTotalAmount = Some(commissionAmount)
                )
                .await()
                .toOption
                .get

            txCreatorWithConstEvaluator.reveal(
              utxos = utxos,
              collateralUtxos = utxos,
              lockedUtxo = lockedUtxo,
              payeeAddress = receiverAddress,
              changeAddress = changeAddress,
              preimage = wrongPreimage,
              receiverPkh = receiverPkh,
              time = timeout,
              signer = receiverSigner
            )
        }

        val result = runValidator(provider, revealTx, lockedUtxo._1)
        assert(result.isFailure)
        assert(result.logs.last.contains(HtlcValidator.InvalidReceiverPreimage))

        provider.setSlot(env.slotConfig.timeToSlot(beforeTimeout))

        provider.submit(revealTx).await() match
            case Left(nodeError: SubmitError.NodeError) =>
                assert(nodeError.message.endsWith(HtlcValidator.InvalidReceiverPreimage))
            case _ =>
                fail(
                  "Transaction should have failed with a node error indicating the violated ledger rule"
                )
    }

    test("receiver fails with wrong receiver pubkey hash") {
        val provider = createProvider()
        val (lockTx, lockedUtxo) = createAndSubmitLockTx(provider)

        val revealTx = {
            val utxos = provider
                .findUtxos(
                  address = receiverAddress,
                  minRequiredTotalAmount = Some(commissionAmount)
                )
                .await()
                .toOption
                .get

            txCreatorWithConstEvaluator.reveal(
              utxos = utxos,
              collateralUtxos = utxos,
              lockedUtxo = lockedUtxo,
              payeeAddress = receiverAddress,
              changeAddress = changeAddress,
              preimage = validPreimage,
              receiverPkh = wrongReceiverPkh,
              time = timeout,
              signer = TransactionSigner(Set(receiverKeyPair, wrongReceiverKeyPair))
            )
        }

        val result = runValidator(provider, revealTx, lockedUtxo._1)
        assert(result.isFailure)
        assert(result.logs.last.contains(HtlcValidator.UnsignedReceiverTransaction))

        provider.setSlot(env.slotConfig.timeToSlot(beforeTimeout))

        provider.submit(revealTx).await() match
            case Left(nodeError: SubmitError.NodeError) =>
                assert(nodeError.message.endsWith(HtlcValidator.UnsignedReceiverTransaction))
            case _ =>
                fail(
                  "Transaction should have failed with a node error indicating the violated ledger rule"
                )
    }

    test("receiver fails after timeout") {
        val provider = createProvider()
        val (lockTx, lockedUtxo) = createAndSubmitLockTx(provider)

        val revealTx = {
            val utxos = provider
                .findUtxos(
                  address = receiverAddress,
                  minRequiredTotalAmount = Some(commissionAmount)
                )
                .await()
                .toOption
                .get

            txCreatorWithConstEvaluator.reveal(
              utxos = utxos,
              collateralUtxos = utxos,
              lockedUtxo = lockedUtxo,
              payeeAddress = receiverAddress,
              changeAddress = changeAddress,
              preimage = validPreimage,
              receiverPkh = receiverPkh,
              time = afterTimeout,
              signer = receiverSigner
            )
        }

        val result = runValidator(provider, revealTx, lockedUtxo._1)
        assert(result.isFailure)
        assert(result.logs.last.contains(HtlcValidator.InvalidReceiverTimePoint))

        provider.setSlot(env.slotConfig.timeToSlot(timeout))

        provider.submit(revealTx).await() match
            case Left(nodeError: SubmitError.NodeError) =>
                assert(nodeError.message.endsWith(HtlcValidator.InvalidReceiverTimePoint))
            case _ =>
                fail(
                  "Transaction should have failed with a node error indicating the violated ledger rule"
                )
    }

    test("committer reclaims after timeout") {
        val provider = createProvider()
        val (lockTx, lockedUtxo) = createAndSubmitLockTx(provider)

        val timeoutTx = {
            val utxos = provider
                .findUtxos(
                  address = committerAddress,
                  minRequiredTotalAmount = Some(commissionAmount)
                )
                .await()
                .toOption
                .get

            txCreator.timeout(
              utxos = utxos,
              collateralUtxos = utxos,
              lockedUtxo = lockedUtxo,
              payeeAddress = committerAddress,
              changeAddress = changeAddress,
              committerPkh = committerPkh,
              time = afterTimeout,
              signer = committerSigner
            )
        }

        val result = runValidator(provider, timeoutTx, lockedUtxo._1)
        assert(result.isSuccess)

        provider.setSlot(env.slotConfig.timeToSlot(afterTimeout))

        assert(provider.submit(timeoutTx).await().isRight)

        val unlockedUtxo = provider
            .findUtxo(
              address = committerAddress,
              transactionId = Some(timeoutTx.id),
              minAmount = Some(lockAmount)
            )
            .await()
            .toOption
            .get

        assert(unlockedUtxo._2.value.coin == lockAmount)
    }

    test("committer fails with wrong committer pubkey hash") {
        val provider = createProvider()
        val (lockTx, lockedUtxo) = createAndSubmitLockTx(provider)

        val timeoutTx = {
            val utxos = provider
                .findUtxos(
                  address = committerAddress,
                  minRequiredTotalAmount = Some(commissionAmount)
                )
                .await()
                .toOption
                .get

            txCreatorWithConstEvaluator.timeout(
              utxos = utxos,
              collateralUtxos = utxos,
              lockedUtxo = lockedUtxo,
              payeeAddress = committerAddress,
              changeAddress = changeAddress,
              committerPkh = wrongCommitterPkh,
              time = afterTimeout,
              signer = TransactionSigner(Set(committerKeyPair, wrongCommitterKeyPair))
            )
        }

        val result = runValidator(provider, timeoutTx, lockedUtxo._1)
        assert(result.isFailure)
        assert(result.logs.last.contains(HtlcValidator.UnsignedCommitterTransaction))

        provider.setSlot(env.slotConfig.timeToSlot(afterTimeout))

        provider.submit(timeoutTx).await() match
            case Left(nodeError: SubmitError.NodeError) =>
                assert(nodeError.message.endsWith(HtlcValidator.UnsignedCommitterTransaction))
            case _ =>
                fail(
                  "Transaction should have failed with a node error indicating the violated ledger rule"
                )
    }

    test("committer fails before timeout") {
        val provider = createProvider()
        val (lockTx, lockedUtxo) = createAndSubmitLockTx(provider)

        val timeoutTx = {
            val utxos = provider
                .findUtxos(
                  address = committerAddress,
                  minRequiredTotalAmount = Some(commissionAmount)
                )
                .await()
                .toOption
                .get

            txCreatorWithConstEvaluator.timeout(
              utxos = utxos,
              collateralUtxos = utxos,
              lockedUtxo = lockedUtxo,
              payeeAddress = committerAddress,
              changeAddress = changeAddress,
              committerPkh = committerPkh,
              time = beforeTimeout,
              signer = committerSigner
            )
        }

        val result = runValidator(provider, timeoutTx, lockedUtxo._1)
        assert(result.isFailure)
        assert(result.logs.last.contains(HtlcValidator.InvalidCommitterTimePoint))

        provider.setSlot(env.slotConfig.timeToSlot(beforeTimeout))

        provider.submit(timeoutTx).await() match
            case Left(nodeError: SubmitError.NodeError) =>
                assert(nodeError.message.endsWith(HtlcValidator.InvalidCommitterTimePoint))
            case _ =>
                fail(
                  "Transaction should have failed with a node error indicating the violated ledger rule"
                )
    }
}
