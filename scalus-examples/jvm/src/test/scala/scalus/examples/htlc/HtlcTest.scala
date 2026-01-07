package scalus.examples.htlc

import org.scalatest.funsuite.AnyFunSuite
import scalus.builtin.Builtins.sha3_256
import scalus.builtin.Data.toData
import scalus.builtin.{platform, ByteString}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.rules.*
import scalus.cardano.node.{Emulator, SubmitError}
import scalus.cardano.txbuilder.TransactionSigner
import scalus.ledger.api.v1.PubKeyHash
import scalus.testing.kit.{ScalusTest, TestUtil}
import scalus.utils.await

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Try

class HtlcTest extends AnyFunSuite, ScalusTest {
    import HtlcTest.*

    test(s"HTLC validator size is ${HtlcContract.script.script.size} bytes") {
        assert(HtlcContract.script.script.size == 755)
    }

    test("receiver reveals preimage before timeout") {
        TestCase(
          action = Action.Reveal(validPreimage),
          person = Person.Receiver,
          time = Time.BeforeTimeout,
          expected = Expected.Success
        ).run()
    }

    test("receiver fails with wrong preimage") {
        TestCase(
          action = Action.Reveal(wrongPreimage),
          person = Person.Receiver,
          time = Time.BeforeTimeout,
          expected = Expected.Failure(HtlcValidator.InvalidReceiverPreimage)
        ).run()
    }

    test("receiver fails with wrong receiver pubkey hash") {
        TestCase(
          action = Action.Reveal(validPreimage),
          person = Person.WrongReceiver,
          time = Time.BeforeTimeout,
          expected = Expected.Failure(HtlcValidator.UnsignedReceiverTransaction)
        ).run()
    }

    test("receiver fails after timeout") {
        TestCase(
          action = Action.Reveal(validPreimage),
          person = Person.Receiver,
          time = Time.AfterTimeout,
          expected = Expected.Failure(HtlcValidator.InvalidReceiverTimePoint)
        ).run()
    }

    test("committer reclaims after timeout") {
        TestCase(
          action = Action.Timeout,
          person = Person.Committer,
          time = Time.AfterTimeout,
          expected = Expected.Success
        ).run()
    }

    test("committer fails with wrong committer pubkey hash") {
        TestCase(
          action = Action.Timeout,
          person = Person.WrongCommitter,
          time = Time.AfterTimeout,
          expected = Expected.Failure(HtlcValidator.UnsignedCommitterTransaction)
        ).run()
    }

    test("committer fails before timeout") {
        TestCase(
          action = Action.Timeout,
          person = Person.Committer,
          time = Time.BeforeTimeout,
          expected = Expected.Failure(HtlcValidator.InvalidCommitterTimePoint)
        ).run()
    }
}

object HtlcTest extends ScalusTest {
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
    private val wrongCommitterSigner =
        TransactionSigner(Set(committerKeyPair, wrongCommitterKeyPair))
    private val wrongReceiverSigner = TransactionSigner(Set(receiverKeyPair, wrongReceiverKeyPair))

    private val txCreator = HtlcTransactionCreator(
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

    val validPreimage: Preimage = genByteStringOfN(32).sample.get
    val wrongPreimage: Preimage = genByteStringOfN(12).sample.get
    private val image: Image = sha3_256(validPreimage)

    private val datum = Config(
      PubKeyHash(committerPkh),
      PubKeyHash(receiverPkh),
      image,
      timeout
    ).toData

    enum Person:
        case Committer, Receiver, WrongCommitter, WrongReceiver

    enum Time:
        case BeforeTimeout, AfterTimeout

    enum Expected:
        case Success
        case Failure(errorMsg: String)

    case class TestCase(
        action: Action,
        person: Person,
        time: Time,
        expected: Expected
    ):
        def run(): Unit = {
            val provider = createProvider()
            val (lockTx, lockedUtxo) = createAndSubmitLockTx(provider)

            val (tx, lockedInput) = action match
                case Action.Reveal(preimage) =>
                    val utxos = provider
                        .findUtxos(
                          address = receiverAddress,
                          minRequiredTotalAmount = Some(commissionAmount)
                        )
                        .await()
                        .toOption
                        .get

                    val (pkh, signer) = person match
                        case Person.Receiver      => (receiverPkh, receiverSigner)
                        case Person.WrongReceiver => (wrongReceiverPkh, wrongReceiverSigner)
                        case _ =>
                            throw IllegalArgumentException(s"Invalid person for Reveal: $person")

                    val txTime = time match
                        case Time.BeforeTimeout => timeout
                        case Time.AfterTimeout  => afterTimeout

                    val revealTx = txCreator.reveal(
                      utxos = utxos,
                      lockedUtxo = lockedUtxo,
                      payeeAddress = receiverAddress,
                      changeAddress = changeAddress,
                      preimage = preimage,
                      receiverPkh = pkh,
                      time = txTime,
                      signer = signer
                    )
                    (revealTx, lockedUtxo._1)

                case Action.Timeout =>
                    val utxos = provider
                        .findUtxos(
                          address = committerAddress,
                          minRequiredTotalAmount = Some(commissionAmount)
                        )
                        .await()
                        .toOption
                        .get

                    val (pkh, signer) = person match
                        case Person.Committer => (committerPkh, committerSigner)
                        case Person.WrongCommitter =>
                            (wrongCommitterPkh, wrongCommitterSigner)
                        case _ =>
                            throw IllegalArgumentException(s"Invalid person for Timeout: $person")

                    val txTime = time match
                        case Time.BeforeTimeout => beforeTimeout
                        case Time.AfterTimeout  => afterTimeout

                    val timeoutTx = txCreator.timeout(
                      utxos = utxos,
                      lockedUtxo = lockedUtxo,
                      payeeAddress = committerAddress,
                      changeAddress = changeAddress,
                      committerPkh = pkh,
                      time = txTime,
                      signer = signer
                    )
                    (timeoutTx, lockedUtxo._1)

            // Direct validator call for JVM debugging
            val scriptContext = getScriptContext(provider, tx, lockedInput)
            val directResult = Try(HtlcContract.code(scriptContext.toData))

            // Set slot and submit to Emulator
            // For reveal actions after timeout, submit at timeout slot (not after)
            // because the ledger validity interval check happens before the validator
            val submissionSlot = (action, time) match
                case (Action.Reveal(_), Time.AfterTimeout) => slot // at timeout, not after
                case (_, Time.BeforeTimeout)               => beforeSlot
                case (_, Time.AfterTimeout)                => afterSlot
            provider.setSlot(submissionSlot)
            val submissionResult = provider.submit(tx).await()

            // Verify results
            expected match
                case Expected.Success =>
                    assert(
                      directResult.isSuccess,
                      s"Direct validator call failed: ${directResult.failed.get}"
                    )
                    assert(
                      submissionResult.isRight,
                      s"Emulator submission failed: $submissionResult"
                    )

                case Expected.Failure(errorMsg) =>
                    assert(
                      directResult.isFailure,
                      s"Direct validator call should have failed but succeeded"
                    )
                    submissionResult match
                        case Left(nodeError: SubmitError.NodeError) =>
                            assert(
                              nodeError.message.endsWith(errorMsg),
                              s"Expected error '$errorMsg' but got '${nodeError.message}'"
                            )
                        case Left(other) =>
                            throw AssertionError(s"Expected NodeError but got: $other")
                        case Right(_) =>
                            throw AssertionError(
                              "Emulator submission should have failed but succeeded"
                            )
        }

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

    private def getScriptContext(
        provider: Emulator,
        tx: Transaction,
        lockedInput: TransactionInput
    ): scalus.ledger.api.v3.ScriptContext = {
        val utxos = {
            val body = tx.body.value
            val allInputs =
                (body.inputs.toSet.view ++ body.collateralInputs.toSet.view ++ body.referenceInputs.toSet.view).toSet
            provider.findUtxos(allInputs).await().toOption.get
        }
        TestUtil.getScriptContextV3(tx, utxos, lockedInput, RedeemerTag.Spend, env)
    }

    private def createAndSubmitLockTx(provider: Emulator): (Transaction, Utxo) = {
        val utxos = provider.findUtxos(address = committerAddress).await().toOption.get

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
        val lockedUtxo = lockTx.utxos.find { case (_, txOut) => txOut.address == scriptAddress }.get
        (lockTx, Utxo(lockedUtxo))
    }
}
