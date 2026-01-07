package scalus.examples.htlc

import org.scalatest.funsuite.AnyFunSuite
import scalus.builtin.Builtins.sha3_256
import scalus.builtin.ByteString
import scalus.builtin.Data.toData
import scalus.cardano.ledger.*
import scalus.cardano.ledger.rules.*
import scalus.cardano.node.{Emulator, SubmitError}
import scalus.cardano.txbuilder.TransactionSigner
import scalus.cardano.wallet.BloxbeanAccount
import scalus.ledger.api.v1.PubKeyHash
import scalus.testing.kit.Party.{Alice, Bob, Eve, Mallory}
import scalus.testing.kit.{ScalusTest, TestUtil}
import scalus.utils.await

import java.time.Instant
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Try

class HtlcTest extends AnyFunSuite, ScalusTest {
    import HtlcTest.*

    test(s"HTLC validator size is ${HtlcContract.script.script.size} bytes") {
//        println(HtlcContract.sir.showHighlighted)
//        println(HtlcContract.program.showHighlighted)
        assert(HtlcContract.script.script.size == 569)
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
    private given env: CardanoInfo = TestUtil.testEnvironment
    private val compiledContract = HtlcContract.withErrorTraces
    private val scriptAddress = compiledContract.address(env.network)

    // Party to role mapping
    private val wrongCommitter = Mallory
    private val wrongReceiver = Eve

    private val committerPkh = Alice.addrKeyHash
    private val receiverPkh = Bob.addrKeyHash
    private val wrongCommitterPkh = wrongCommitter.addrKeyHash
    private val wrongReceiverPkh = wrongReceiver.addrKeyHash

    private val committerAddress = Alice.address
    private val receiverAddress = Bob.address
    private val changeAddress = TestUtil.createTestAddress("a" * 56)

    private val txCreator = HtlcTransactionCreator(
      env = env,
      evaluator = PlutusScriptEvaluator.constMaxBudget(env),
      contract = compiledContract
    )

    private val lockAmount = Coin(10_000_000L)
    private val commissionAmount = Coin(2_000_000L)

    private val slot: SlotNo = 10
    private val beforeSlot: SlotNo = slot - 1
    private val afterSlot: SlotNo = slot + 1
    private val timeout: Instant = env.slotConfig.slotToInstant(slot)
    private val beforeTimeout: Instant = env.slotConfig.slotToInstant(beforeSlot)
    private val afterTimeout: Instant = env.slotConfig.slotToInstant(afterSlot)

    val validPreimage: Preimage = genByteStringOfN(32).sample.get
    val wrongPreimage: Preimage = genByteStringOfN(12).sample.get
    private val image: Image = sha3_256(validPreimage)

    private val datum = Config(
      PubKeyHash(committerPkh),
      PubKeyHash(receiverPkh),
      image,
      timeout.toEpochMilli
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
                        .findUtxos(address = receiverAddress)
                        .await()
                        .toOption
                        .get

                    val (pkhVal, signer) = person match
                        case Person.Receiver      => (receiverPkh, Bob.signer)
                        case Person.WrongReceiver =>
                            // Use wrong PKH but sign with both keys to pass ledger checks
                            val combinedSigner = new TransactionSigner(
                              Set(
                                new BloxbeanAccount(Bob.account).paymentKeyPair,
                                new BloxbeanAccount(wrongReceiver.account).paymentKeyPair
                              )
                            )
                            (wrongReceiverPkh, combinedSigner)
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
                      receiverPkh = pkhVal,
                      validTo = txTime,
                      signer = signer
                    )
                    (revealTx, lockedUtxo._1)

                case Action.Timeout =>
                    val utxos = provider
                        .findUtxos(address = committerAddress)
                        .await()
                        .toOption
                        .get

                    val (pkhVal, signer) = person match
                        case Person.Committer      => (committerPkh, Alice.signer)
                        case Person.WrongCommitter =>
                            // Use wrong PKH but sign with both keys to pass ledger checks
                            val combinedSigner = new TransactionSigner(
                              Set(
                                new BloxbeanAccount(Alice.account).paymentKeyPair,
                                new BloxbeanAccount(wrongCommitter.account).paymentKeyPair
                              )
                            )
                            (wrongCommitterPkh, combinedSigner)
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
                      committerPkh = pkhVal,
                      validFrom = txTime,
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
          signer = Alice.signer
        )
        assert(provider.submit(lockTx).await().isRight)
        val lockedUtxo = lockTx.utxos.find { case (_, txOut) => txOut.address == scriptAddress }.get
        (lockTx, Utxo(lockedUtxo))
    }
}
