package scalus.examples.htlc

import org.scalatest.funsuite.AnyFunSuite
import scalus.builtin.Builtins.sha3_256
import scalus.builtin.ByteString
import scalus.builtin.Data.toData
import scalus.cardano.ledger.*
import scalus.cardano.ledger.rules.*
import scalus.cardano.node.{Emulator, SubmitError}
import scalus.ledger.api.v3.ScriptContext
import scalus.testing.kit.Party.{Alice, Bob, Eve}
import scalus.testing.kit.{ScalusTest, TestUtil}
import scalus.utils.await

import java.time.Instant
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Try

class HtlcTest extends AnyFunSuite, ScalusTest {
    import HtlcTest.{given, *}

    test(s"HTLC validator size is ${HtlcContract.script.script.size} bytes") {
        assert(HtlcContract.script.script.size == 569)
    }

    test("receiver reveals preimage before timeout") {
        val provider = createProvider()
        val (_, lockedUtxo) = createAndSubmitLockTx(provider)
        val utxos = provider.findUtxos(Bob.address).await().toOption.get

        val revealTx = txCreator.reveal(
          utxos = utxos,
          lockedUtxo = lockedUtxo,
          payeeAddress = Bob.address,
          sponsor = Bob.address,
          preimage = validPreimage,
          receiverPkh = Bob.addrKeyHash,
          validTo = timeout,
          signer = Bob.signer
        )

        provider.setSlot(beforeSlot)
        assertSuccess(provider, revealTx, lockedUtxo._1)
    }

    test("receiver fails with wrong preimage") {
        val provider = createProvider()
        val (_, lockedUtxo) = createAndSubmitLockTx(provider)
        val utxos = provider.findUtxos(Bob.address).await().toOption.get

        val revealTx = txCreator.reveal(
          utxos = utxos,
          lockedUtxo = lockedUtxo,
          payeeAddress = Bob.address,
          sponsor = Bob.address,
          preimage = wrongPreimage,
          receiverPkh = Bob.addrKeyHash,
          validTo = timeout,
          signer = Bob.signer
        )

        provider.setSlot(beforeSlot)
        assertFailure(provider, revealTx, lockedUtxo._1, HtlcValidator.InvalidReceiverPreimage)
    }

    test("receiver fails with wrong receiver pubkey hash") {
        val provider = createProvider()
        val (_, lockedUtxo) = createAndSubmitLockTx(provider)
        val utxos = provider.findUtxos(Eve.address).await().toOption.get

        val revealTx = txCreator.reveal(
          utxos = utxos,
          lockedUtxo = lockedUtxo,
          payeeAddress = Eve.address,
          sponsor = Eve.address,
          preimage = validPreimage,
          receiverPkh = Eve.addrKeyHash, // Wrong receiver PKH (should be Bob)
          validTo = timeout,
          signer = Eve.signer
        )

        provider.setSlot(beforeSlot)
        assertFailure(provider, revealTx, lockedUtxo._1, HtlcValidator.UnsignedReceiverTransaction)
    }

    test("receiver fails after timeout") {
        val provider = createProvider()
        val (_, lockedUtxo) = createAndSubmitLockTx(provider)
        val utxos = provider.findUtxos(Bob.address).await().toOption.get

        val revealTx = txCreator.reveal(
          utxos = utxos,
          lockedUtxo = lockedUtxo,
          payeeAddress = Bob.address,
          sponsor = Bob.address,
          preimage = validPreimage,
          receiverPkh = Bob.addrKeyHash,
          validTo = afterTimeout,
          signer = Bob.signer
        )

        // Submit at timeout slot (not after) because ledger validity interval check happens first
        provider.setSlot(slot)
        assertFailure(provider, revealTx, lockedUtxo._1, HtlcValidator.InvalidReceiverTimePoint)
    }

    test("committer reclaims after timeout") {
        val provider = createProvider()
        val (_, lockedUtxo) = createAndSubmitLockTx(provider)
        val utxos = provider.findUtxos(Alice.address).await().toOption.get

        val timeoutTx = txCreator.timeout(
          utxos = utxos,
          lockedUtxo = lockedUtxo,
          payeeAddress = Alice.address,
          sponsor = Alice.address,
          committerPkh = Alice.addrKeyHash,
          validFrom = afterTimeout,
          signer = Alice.signer
        )

        provider.setSlot(afterSlot)
        assertSuccess(provider, timeoutTx, lockedUtxo._1)
    }

    test("committer fails with wrong committer pubkey hash") {
        val provider = createProvider()
        val (_, lockedUtxo) = createAndSubmitLockTx(provider)
        val utxos = provider.findUtxos(Eve.address).await().toOption.get

        val timeoutTx = txCreator.timeout(
          utxos = utxos,
          lockedUtxo = lockedUtxo,
          payeeAddress = Eve.address,
          sponsor = Eve.address,
          committerPkh = Eve.addrKeyHash, // Wrong committer PKH (should be Alice)
          validFrom = afterTimeout,
          signer = Eve.signer
        )

        provider.setSlot(afterSlot)
        assertFailure(
          provider,
          timeoutTx,
          lockedUtxo._1,
          HtlcValidator.UnsignedCommitterTransaction
        )
    }

    test("committer fails before timeout") {
        val provider = createProvider()
        val (_, lockedUtxo) = createAndSubmitLockTx(provider)
        val utxos = provider.findUtxos(Alice.address).await().toOption.get

        val timeoutTx = txCreator.timeout(
          utxos = utxos,
          lockedUtxo = lockedUtxo,
          payeeAddress = Alice.address,
          sponsor = Alice.address,
          committerPkh = Alice.addrKeyHash,
          validFrom = beforeTimeout,
          signer = Alice.signer
        )

        provider.setSlot(beforeSlot)
        assertFailure(provider, timeoutTx, lockedUtxo._1, HtlcValidator.InvalidCommitterTimePoint)
    }
}

object HtlcTest extends ScalusTest {
    private given env: CardanoInfo = TestUtil.testEnvironment
    private val compiledContract = HtlcContract.withErrorTraces
    private val scriptAddress = compiledContract.address(env.network)

    private val txCreator = HtlcTransactionCreator(
      env = env,
      evaluator = PlutusScriptEvaluator.constMaxBudget(env),
      contract = compiledContract
    )

    private val lockAmount = Coin(10_000_000L)

    private val slot: SlotNo = 10
    private val beforeSlot: SlotNo = slot - 1
    private val afterSlot: SlotNo = slot + 1
    private val timeout: Instant = env.slotConfig.slotToInstant(slot)
    private val beforeTimeout: Instant = env.slotConfig.slotToInstant(beforeSlot)
    private val afterTimeout: Instant = env.slotConfig.slotToInstant(afterSlot)

    val validPreimage: Preimage = genByteStringOfN(32).sample.get
    val wrongPreimage: Preimage = genByteStringOfN(12).sample.get
    private val image: Image = sha3_256(validPreimage)

    private def createProvider(): Emulator = {
        val genesisHash = TransactionHash.fromByteString(ByteString.fromHex("0" * 64))

        Emulator(
          initialUtxos = Map(
            Input(genesisHash, 0) ->
                TransactionOutput.Babbage(
                  address = Alice.address,
                  value = Value.lovelace(100_000_000L)
                ),
            Input(genesisHash, 1) ->
                TransactionOutput.Babbage(
                  address = Bob.address,
                  value = Value.lovelace(100_000_000L)
                ),
            Input(genesisHash, 2) ->
                TransactionOutput.Babbage(
                  address = Eve.address,
                  value = Value.lovelace(100_000_000L)
                )
          ),
          initialContext = Context.testMainnet(),
          mutators = Set(PlutusScriptsTransactionMutator)
        )
    }

    private def getScriptContext(
        provider: Emulator,
        tx: Transaction,
        lockedInput: TransactionInput
    ): ScriptContext = {
        val utxos = {
            val body = tx.body.value
            val allInputs =
                (body.inputs.toSet.view ++ body.collateralInputs.toSet.view ++ body.referenceInputs.toSet.view).toSet
            provider.findUtxos(allInputs).await().toOption.get
        }
        TestUtil.getScriptContextV3(tx, utxos, lockedInput, RedeemerTag.Spend, env)
    }

    private def createAndSubmitLockTx(provider: Emulator): (Transaction, Utxo) = {
        val utxos = provider.findUtxos(address = Alice.address).await().toOption.get

        val lockTx = txCreator.lock(
          utxos = utxos,
          value = Value(lockAmount),
          sponsor = Alice.address,
          committer = Alice.addrKeyHash,
          receiver = Bob.addrKeyHash,
          image = image,
          timeout = timeout,
          signer = Alice.signer
        )
        assert(provider.submit(lockTx).await().isRight)
        val lockedUtxo = lockTx.utxos.find { case (_, txOut) => txOut.address == scriptAddress }.get
        (lockTx, Utxo(lockedUtxo))
    }

    private def assertSuccess(
        provider: Emulator,
        tx: Transaction,
        lockedInput: TransactionInput
    ): Unit = {
        val scriptContext = getScriptContext(provider, tx, lockedInput)
        val directResult = Try(HtlcContract.code(scriptContext.toData))
        val submissionResult = provider.submit(tx).await()

        assert(directResult.isSuccess, s"Direct validator call failed: ${directResult.failed.get}")
        assert(submissionResult.isRight, s"Emulator submission failed: $submissionResult")
    }

    private def assertFailure(
        provider: Emulator,
        tx: Transaction,
        lockedInput: TransactionInput,
        expectedError: String
    ): Unit = {
        val scriptContext = getScriptContext(provider, tx, lockedInput)
        val directResult = Try(HtlcContract.code(scriptContext.toData))
        val submissionResult = provider.submit(tx).await()

        assert(directResult.isFailure, "Direct validator call should have failed but succeeded")
        submissionResult match {
            case Left(nodeError: SubmitError.NodeError) =>
                assert(
                  nodeError.message.endsWith(expectedError),
                  s"Expected error '$expectedError' but got '${nodeError.message}'"
                )
            case Left(other) =>
                throw AssertionError(s"Expected NodeError but got: $other")
            case Right(_) =>
                throw AssertionError("Emulator submission should have failed but succeeded")
        }
    }
}
