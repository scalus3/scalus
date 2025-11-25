package scalus.examples.htlc

import org.scalatest.funsuite.AnyFunSuite
import scalus.builtin.Builtins.sha3_256
import scalus.builtin.Data.toData
import scalus.builtin.{platform, ByteString}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.rules.*
import scalus.cardano.ledger.utils.AllResolvedScripts
import scalus.cardano.txbuilder.TransactionSigner
import scalus.examples.TestUtil
import scalus.ledger.api.v1.PubKeyHash
import scalus.testing.kit.{MockLedgerApi, ScalusTest}
import scalus.uplc.Program
import scalus.uplc.eval.Result

class HtlcTest extends AnyFunSuite, ScalusTest {
    private val env = TestUtil.testEnvironment
    private val compiledContract = HtlcContract.debugCompiledContract
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

    private def transactionCreatorFor(signer: TransactionSigner) = HtlcTransactionCreator(
      env = env,
      evaluator = PlutusScriptEvaluator(env, EvaluatorMode.EvaluateAndComputeCost),
      signer = signer,
      compiledContract = compiledContract
    )

    private def transactionCreatorWithConstEvaluatorFor(signer: TransactionSigner) =
        HtlcTransactionCreator(
          env = env,
          evaluator = PlutusScriptEvaluator.constMaxBudget(env),
          signer = signer,
          compiledContract = compiledContract
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

    private def createProvider(): MockLedgerApi = {
        val genesisHash = TransactionHash.fromByteString(ByteString.fromHex("0" * 64))

        MockLedgerApi(
          initialUtxos = Map(
            TransactionInput(genesisHash, 0) ->
                TransactionOutput.Babbage(
                  address = committerAddress,
                  value = Value.lovelace(100_000_000L)
                ),
            TransactionInput(genesisHash, 1) ->
                TransactionOutput.Babbage(
                  address = receiverAddress,
                  value = Value.lovelace(100_000_000L)
                )
          ),
          context = Context.testMainnet(),
          mutators = Set(PlutusScriptsTransactionMutator)
        )
    }

    private def runValidator(
        provider: MockLedgerApi,
        tx: Transaction,
        lockedInput: TransactionInput
    ): Result = {
        val utxos = {
            val body = tx.body.value
            val allInputs =
                (body.inputs.toSet.view ++ body.collateralInputs.toSet.view ++ body.referenceInputs.toSet.view).toSet
            provider.findUtxos(allInputs).toOption.get
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

    test("receiver reveals preimage before timeout") {
        val provider = createProvider()

        val lockTx = {
            val utxos = provider
                .findUtxos(
                  address = committerAddress,
                  minRequiredTotalAmount = Some(lockAmount + commissionAmount)
                )
                .toOption
                .get

            transactionCreatorFor(committerSigner)
                .lock(
                  utxos = utxos,
                  value = Value(lockAmount),
                  changeAddress = committerAddress,
                  committer = committerPkh,
                  receiver = receiverPkh,
                  image = image,
                  timeout = timeout
                )
        }

        assert(provider.submit(lockTx).isRight)

        val lockedUtxo = provider
            .findUtxo(
              address = scriptAddress,
              transactionId = Some(lockTx.id),
              datum = Some(DatumOption.Inline(datum)),
              minAmount = Some(lockAmount)
            )
            .toOption
            .get

        assert(lockedUtxo._2.value.coin == lockAmount)

        val revealTx = {
            val utxos = provider
                .findUtxos(
                  address = receiverAddress,
                  minRequiredTotalAmount = Some(commissionAmount)
                )
                .toOption
                .get

            transactionCreatorFor(receiverSigner)
                .reveal(
                  utxos = utxos,
                  collateralUtxos = utxos,
                  lockedUtxo = lockedUtxo,
                  payeeAddress = receiverAddress,
                  changeAddress = changeAddress,
                  preimage = validPreimage,
                  receiverPkh = receiverPkh,
                  time = beforeTimeout
                )
        }

        val result = runValidator(provider, revealTx, lockedUtxo._1)
        assert(result.isSuccess)

        provider.setSlot(env.slotConfig.timeToSlot(beforeTimeout))

        assert(provider.submit(revealTx).isRight)

        val unlockedUtxo = provider
            .findUtxo(
              address = receiverAddress,
              transactionId = Some(revealTx.id),
              minAmount = Some(lockAmount)
            )
            .toOption
            .get

        assert(unlockedUtxo._2.value.coin == lockAmount)
    }

    test("receiver fails with wrong preimage") {
        val provider = createProvider()

        val lockTx = {
            val utxos = provider
                .findUtxos(
                  address = committerAddress,
                  minRequiredTotalAmount = Some(lockAmount + commissionAmount)
                )
                .toOption
                .get

            transactionCreatorFor(committerSigner)
                .lock(
                  utxos = utxos,
                  value = Value(lockAmount),
                  changeAddress = committerAddress,
                  committer = committerPkh,
                  receiver = receiverPkh,
                  image = image,
                  timeout = timeout
                )
        }

        assert(provider.submit(lockTx).isRight)

        val lockedUtxo = provider
            .findUtxo(
              address = scriptAddress,
              transactionId = Some(lockTx.id),
              datum = Some(DatumOption.Inline(datum)),
              minAmount = Some(lockAmount)
            )
            .toOption
            .get

        assert(lockedUtxo._2.value.coin == lockAmount)

        val revealTx = {
            val utxos = provider
                .findUtxos(
                  address = receiverAddress,
                  minRequiredTotalAmount = Some(commissionAmount)
                )
                .toOption
                .get

            transactionCreatorWithConstEvaluatorFor(receiverSigner)
                .reveal(
                  utxos = utxos,
                  collateralUtxos = utxos,
                  lockedUtxo = lockedUtxo,
                  payeeAddress = receiverAddress,
                  changeAddress = changeAddress,
                  preimage = wrongPreimage,
                  receiverPkh = receiverPkh,
                  time = beforeTimeout
                )
        }

        val result = runValidator(provider, revealTx, lockedUtxo._1)
        assert(result.isFailure)
        assert(result.logs.last.contains(HtlcValidator.InvalidReceiverPreimage))

        provider.setSlot(env.slotConfig.timeToSlot(beforeTimeout))

        provider.submit(revealTx) match
            case Left(err) => assert(err.getMessage.endsWith(HtlcValidator.InvalidReceiverPreimage))
            case Right(_)  => fail("Transaction should have failed")
    }

    test("receiver fails with wrong receiver pubkey hash") {
        val provider = createProvider()

        val lockTx = {
            val utxos = provider
                .findUtxos(
                  address = committerAddress,
                  minRequiredTotalAmount = Some(lockAmount + commissionAmount)
                )
                .toOption
                .get

            transactionCreatorFor(committerSigner)
                .lock(
                  utxos = utxos,
                  value = Value(lockAmount),
                  changeAddress = committerAddress,
                  committer = committerPkh,
                  receiver = receiverPkh,
                  image = image,
                  timeout = timeout
                )
        }

        assert(provider.submit(lockTx).isRight)

        val lockedUtxo = provider
            .findUtxo(
              address = scriptAddress,
              transactionId = Some(lockTx.id),
              datum = Some(DatumOption.Inline(datum)),
              minAmount = Some(lockAmount)
            )
            .toOption
            .get

        assert(lockedUtxo._2.value.coin == lockAmount)

        val revealTx = {
            val utxos = provider
                .findUtxos(
                  address = receiverAddress,
                  minRequiredTotalAmount = Some(commissionAmount)
                )
                .toOption
                .get

            transactionCreatorWithConstEvaluatorFor(
              TransactionSigner(Set(receiverKeyPair, wrongReceiverKeyPair))
            )
                .reveal(
                  utxos = utxos,
                  collateralUtxos = utxos,
                  lockedUtxo = lockedUtxo,
                  payeeAddress = receiverAddress,
                  changeAddress = changeAddress,
                  preimage = validPreimage,
                  receiverPkh = wrongReceiverPkh,
                  time = beforeTimeout
                )
        }

        val result = runValidator(provider, revealTx, lockedUtxo._1)
        assert(result.isFailure)
        assert(result.logs.last.contains(HtlcValidator.UnsignedReceiverTransaction))

        provider.setSlot(env.slotConfig.timeToSlot(beforeTimeout))

        provider.submit(revealTx) match
            case Left(err) =>
                assert(err.getMessage.endsWith(HtlcValidator.UnsignedReceiverTransaction))
            case Right(_) => fail("Transaction should have failed")
    }

    test("receiver fails after timeout") {
        val provider = createProvider()

        val lockTx = {
            val utxos = provider
                .findUtxos(
                  address = committerAddress,
                  minRequiredTotalAmount = Some(lockAmount + commissionAmount)
                )
                .toOption
                .get

            transactionCreatorFor(committerSigner)
                .lock(
                  utxos = utxos,
                  value = Value(lockAmount),
                  changeAddress = committerAddress,
                  committer = committerPkh,
                  receiver = receiverPkh,
                  image = image,
                  timeout = timeout
                )
        }

        assert(provider.submit(lockTx).isRight)

        val lockedUtxo = provider
            .findUtxo(
              address = scriptAddress,
              transactionId = Some(lockTx.id),
              datum = Some(DatumOption.Inline(datum)),
              minAmount = Some(lockAmount)
            )
            .toOption
            .get

        assert(lockedUtxo._2.value.coin == lockAmount)

        val revealTx = {
            val utxos = provider
                .findUtxos(
                  address = receiverAddress,
                  minRequiredTotalAmount = Some(commissionAmount)
                )
                .toOption
                .get

            transactionCreatorWithConstEvaluatorFor(receiverSigner)
                .reveal(
                  utxos = utxos,
                  collateralUtxos = utxos,
                  lockedUtxo = lockedUtxo,
                  payeeAddress = receiverAddress,
                  changeAddress = changeAddress,
                  preimage = validPreimage,
                  receiverPkh = receiverPkh,
                  time = afterTimeout
                )
        }

        val result = runValidator(provider, revealTx, lockedUtxo._1)
        assert(result.isFailure)
        assert(result.logs.last.contains(HtlcValidator.InvalidReceiverTimePoint))

        provider.setSlot(env.slotConfig.timeToSlot(afterTimeout))

        provider.submit(revealTx) match
            case Left(err) =>
                assert(err.getMessage.endsWith(HtlcValidator.InvalidReceiverTimePoint))
            case Right(_) => fail("Transaction should have failed")
    }

    test("committer reclaims after timeout") {
        val provider = createProvider()

        val lockTx = {
            val utxos = provider
                .findUtxos(
                  address = committerAddress,
                  minRequiredTotalAmount = Some(lockAmount + commissionAmount)
                )
                .toOption
                .get

            transactionCreatorFor(committerSigner)
                .lock(
                  utxos = utxos,
                  value = Value(lockAmount),
                  changeAddress = committerAddress,
                  committer = committerPkh,
                  receiver = receiverPkh,
                  image = image,
                  timeout = timeout
                )
        }

        assert(provider.submit(lockTx).isRight)

        val lockedUtxo = provider
            .findUtxo(
              address = scriptAddress,
              transactionId = Some(lockTx.id),
              datum = Some(DatumOption.Inline(datum)),
              minAmount = Some(lockAmount)
            )
            .toOption
            .get

        assert(lockedUtxo._2.value.coin == lockAmount)

        val timeoutTx = {
            val utxos = provider
                .findUtxos(
                  address = committerAddress,
                  minRequiredTotalAmount = Some(commissionAmount)
                )
                .toOption
                .get

            transactionCreatorFor(committerSigner)
                .timeout(
                  utxos = utxos,
                  collateralUtxos = utxos,
                  lockedUtxo = lockedUtxo,
                  payeeAddress = committerAddress,
                  changeAddress = changeAddress,
                  committerPkh = committerPkh,
                  time = afterTimeout
                )
        }

        val result = runValidator(provider, timeoutTx, lockedUtxo._1)
        assert(result.isSuccess)

        provider.setSlot(env.slotConfig.timeToSlot(afterTimeout))

        assert(provider.submit(timeoutTx).isRight)

        val unlockedUtxo = provider
            .findUtxo(
              address = committerAddress,
              transactionId = Some(timeoutTx.id),
              minAmount = Some(lockAmount)
            )
            .toOption
            .get

        assert(unlockedUtxo._2.value.coin == lockAmount)
    }

    test("committer fails with wrong committer pubkey hash") {
        val provider = createProvider()

        val lockTx = {
            val utxos = provider
                .findUtxos(
                  address = committerAddress,
                  minRequiredTotalAmount = Some(lockAmount + commissionAmount)
                )
                .toOption
                .get

            transactionCreatorFor(committerSigner)
                .lock(
                  utxos = utxos,
                  value = Value(lockAmount),
                  changeAddress = committerAddress,
                  committer = committerPkh,
                  receiver = receiverPkh,
                  image = image,
                  timeout = timeout
                )
        }

        assert(provider.submit(lockTx).isRight)

        val lockedUtxo = provider
            .findUtxo(
              address = scriptAddress,
              transactionId = Some(lockTx.id),
              datum = Some(DatumOption.Inline(datum)),
              minAmount = Some(lockAmount)
            )
            .toOption
            .get

        assert(lockedUtxo._2.value.coin == lockAmount)

        val timeoutTx = {
            val utxos = provider
                .findUtxos(
                  address = committerAddress,
                  minRequiredTotalAmount = Some(commissionAmount)
                )
                .toOption
                .get

            transactionCreatorWithConstEvaluatorFor(
              TransactionSigner(Set(committerKeyPair, wrongCommitterKeyPair))
            )
                .timeout(
                  utxos = utxos,
                  collateralUtxos = utxos,
                  lockedUtxo = lockedUtxo,
                  payeeAddress = committerAddress,
                  changeAddress = changeAddress,
                  committerPkh = wrongCommitterPkh,
                  time = afterTimeout
                )
        }

        val result = runValidator(provider, timeoutTx, lockedUtxo._1)
        assert(result.isFailure)
        assert(result.logs.last.contains(HtlcValidator.UnsignedCommitterTransaction))

        provider.setSlot(env.slotConfig.timeToSlot(afterTimeout))

        provider.submit(timeoutTx) match
            case Left(err) =>
                assert(err.getMessage.endsWith(HtlcValidator.UnsignedCommitterTransaction))
            case Right(_) => fail("Transaction should have failed")
    }

    test("committer fails before timeout") {
        val provider = createProvider()

        val lockTx = {
            val utxos = provider
                .findUtxos(
                  address = committerAddress,
                  minRequiredTotalAmount = Some(lockAmount + commissionAmount)
                )
                .toOption
                .get

            transactionCreatorFor(committerSigner)
                .lock(
                  utxos = utxos,
                  value = Value(lockAmount),
                  changeAddress = committerAddress,
                  committer = committerPkh,
                  receiver = receiverPkh,
                  image = image,
                  timeout = timeout
                )
        }

        assert(provider.submit(lockTx).isRight)

        val lockedUtxo = provider
            .findUtxo(
              address = scriptAddress,
              transactionId = Some(lockTx.id),
              datum = Some(DatumOption.Inline(datum)),
              minAmount = Some(lockAmount)
            )
            .toOption
            .get

        assert(lockedUtxo._2.value.coin == lockAmount)

        val timeoutTx = {
            val utxos = provider
                .findUtxos(
                  address = committerAddress,
                  minRequiredTotalAmount = Some(commissionAmount)
                )
                .toOption
                .get

            transactionCreatorWithConstEvaluatorFor(committerSigner)
                .timeout(
                  utxos = utxos,
                  collateralUtxos = utxos,
                  lockedUtxo = lockedUtxo,
                  payeeAddress = committerAddress,
                  changeAddress = changeAddress,
                  committerPkh = committerPkh,
                  time = beforeTimeout
                )
        }

        val result = runValidator(provider, timeoutTx, lockedUtxo._1)
        assert(result.isFailure)
        assert(result.logs.last.contains(HtlcValidator.InvalidCommitterTimePoint))

        provider.setSlot(env.slotConfig.timeToSlot(beforeTimeout))

        provider.submit(timeoutTx) match
            case Left(err) =>
                assert(err.getMessage.endsWith(HtlcValidator.InvalidCommitterTimePoint))
            case Right(_) => fail("Transaction should have failed")
    }
}
