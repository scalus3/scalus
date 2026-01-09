package scalus.examples.htlc

import org.scalatest.funsuite.AnyFunSuite
import scalus.builtin.Builtins.sha3_256
import scalus.cardano.ledger.*
import scalus.cardano.ledger.utils.MinTransactionFee
import scalus.cardano.node.Emulator
import scalus.cardano.txbuilder.TxBuilderException
import scalus.testing.kit.Party.{Alice, Bob, Eve}
import scalus.testing.kit.{ScalusTest, TestUtil}
import scalus.utils.await

import java.time.Instant
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Try

class HtlcTest extends AnyFunSuite, ScalusTest {
    private given env: CardanoInfo = TestUtil.testEnvironment
    private val compiledContract = HtlcContract.withErrorTraces

    /** Transaction creator with real script evaluation */
    private val txCreator = HtlcTransactionCreator(
      env = env,
      evaluator = PlutusScriptEvaluator(env, EvaluatorMode.EvaluateAndComputeCost),
      contract = compiledContract
    )

    private val slot: SlotNo = 10
    private val beforeSlot: SlotNo = slot - 1
    private val afterSlot: SlotNo = slot + 1
    private val timeout: Instant = env.slotConfig.slotToInstant(slot)
    private val beforeTimeout: Instant = env.slotConfig.slotToInstant(beforeSlot)
    private val afterTimeout: Instant = env.slotConfig.slotToInstant(afterSlot)

    val validPreimage: Preimage = genByteStringOfN(32).sample.get
    val wrongPreimage: Preimage = genByteStringOfN(12).sample.get
    private val image: Image = sha3_256(validPreimage)

    private def createProvider(): Emulator =
        Emulator.withAddresses(Seq(Alice.address, Bob.address, Eve.address))

    private def lock(provider: Emulator): Utxo = {
        val utxos = provider.findUtxos(address = Alice.address).await().toOption.get

        val lockTx = txCreator.lock(
          utxos = utxos,
          value = Value.ada(10),
          sponsor = Alice.address,
          committer = Alice.addrKeyHash,
          receiver = Bob.addrKeyHash,
          image = image,
          timeout = timeout,
          signer = Alice.signer
        )
        assert(provider.submit(lockTx).await().isRight)
        val lockedUtxo = lockTx.utxos.find { case (_, txOut) =>
            txOut.address == compiledContract.address(env.network)
        }.get
        Utxo(lockedUtxo)
    }

    /** Verifies successful transaction with real script evaluation. Checks:
      *   - Execution units are within expected bounds
      *   - Execution fee is positive
      *   - Transaction fee covers minimum required fee
      *   - Emulator submission succeeds
      */
    private def assertSuccess(provider: Emulator, tx: Transaction): Unit = {
        // Verify execution units are reasonable (not max budget)
        val totalExUnits = tx.witnessSet.redeemers
            .map(_.value.toSeq.foldLeft(ExUnits.zero)(_ + _.exUnits))
            .getOrElse(ExUnits.zero)
        assert(totalExUnits.memory > 0, "Execution units memory should be positive")
        assert(totalExUnits.steps > 0, "Execution units steps should be positive")
        assert(
          totalExUnits.memory < env.protocolParams.maxTxExecutionUnits.memory,
          s"ExUnits memory ${totalExUnits.memory} should be less than max ${env.protocolParams.maxTxExecutionUnits.memory}"
        )
        assert(
          totalExUnits.steps < env.protocolParams.maxTxExecutionUnits.steps,
          s"ExUnits steps ${totalExUnits.steps} should be less than max ${env.protocolParams.maxTxExecutionUnits.steps}"
        )

        // Verify execution fee
        val executionFee = totalExUnits.fee(env.protocolParams.executionUnitPrices)
        assert(executionFee.value > 0, "Execution fee should be positive")

        // Verify transaction fee covers minimum
        val txFee = tx.body.value.fee
        val allInputs = tx.body.value.inputs.toSet ++ tx.body.value.referenceInputs.toSet
        val utxos = provider.findUtxos(allInputs).await().toOption.get
        val minFee = MinTransactionFee.computeMinFee(tx, utxos, env.protocolParams).toOption.get
        assert(
          txFee >= minFee,
          s"Transaction fee $txFee should be >= minimum fee $minFee"
        )

        // Verify emulator submission succeeds
        val submissionResult = provider.submit(tx).await()
        assert(submissionResult.isRight, s"Emulator submission failed: $submissionResult")
    }

    /** Verifies that transaction building fails with the expected error. */
    private def assertBuildFailure(expectedError: String)(buildTx: => Transaction): Unit = {
        val result = Try(buildTx)
        assert(result.isFailure, "Transaction building should have failed but succeeded")
        result.failed.get match {
            case e: TxBuilderException.BalancingException =>
                val logs = e.scriptLogs.getOrElse(Seq.empty)
                assert(
                  logs.exists(_.contains(expectedError)),
                  s"Expected error containing '$expectedError' but got logs: ${logs.mkString("\n")}"
                )
            case e =>
                assert(
                  e.getMessage.contains(expectedError),
                  s"Expected error containing '$expectedError' but got: ${e.getMessage}"
                )
        }
    }

    test(s"HTLC validator size is ${HtlcContract.script.script.size} bytes") {
        assert(HtlcContract.script.script.size == 569)
    }

    test("receiver reveals preimage before timeout") {
        val provider = createProvider()
        val lockedUtxo = lock(provider)
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
        assertSuccess(provider, revealTx)
    }

    test("receiver fails with wrong preimage") {
        val provider = createProvider()
        val lockedUtxo = lock(provider)
        val utxos = provider.findUtxos(Bob.address).await().toOption.get

        assertBuildFailure(HtlcValidator.InvalidReceiverPreimage) {
            txCreator.reveal(
              utxos = utxos,
              lockedUtxo = lockedUtxo,
              payeeAddress = Bob.address,
              sponsor = Bob.address,
              preimage = wrongPreimage,
              receiverPkh = Bob.addrKeyHash,
              validTo = timeout,
              signer = Bob.signer
            )
        }
    }

    test("receiver fails with wrong receiver pubkey hash") {
        val provider = createProvider()
        val lockedUtxo = lock(provider)
        val utxos = provider.findUtxos(Eve.address).await().toOption.get

        assertBuildFailure(HtlcValidator.UnsignedReceiverTransaction) {
            txCreator.reveal(
              utxos = utxos,
              lockedUtxo = lockedUtxo,
              payeeAddress = Eve.address,
              sponsor = Eve.address,
              preimage = validPreimage,
              receiverPkh = Eve.addrKeyHash, // Wrong receiver PKH (should be Bob)
              validTo = timeout,
              signer = Eve.signer
            )
        }
    }

    test("receiver fails after timeout") {
        val provider = createProvider()
        val lockedUtxo = lock(provider)
        val utxos = provider.findUtxos(Bob.address).await().toOption.get

        assertBuildFailure(HtlcValidator.InvalidReceiverTimePoint) {
            txCreator.reveal(
              utxos = utxos,
              lockedUtxo = lockedUtxo,
              payeeAddress = Bob.address,
              sponsor = Bob.address,
              preimage = validPreimage,
              receiverPkh = Bob.addrKeyHash,
              validTo = afterTimeout,
              signer = Bob.signer
            )
        }
    }

    test("committer reclaims after timeout") {
        val provider = createProvider()
        val lockedUtxo = lock(provider)
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
        assertSuccess(provider, timeoutTx)
    }

    test("committer fails with wrong committer pubkey hash") {
        val provider = createProvider()
        val lockedUtxo = lock(provider)
        val utxos = provider.findUtxos(Eve.address).await().toOption.get

        assertBuildFailure(HtlcValidator.UnsignedCommitterTransaction) {
            txCreator.timeout(
              utxos = utxos,
              lockedUtxo = lockedUtxo,
              payeeAddress = Eve.address,
              sponsor = Eve.address,
              committerPkh = Eve.addrKeyHash, // Wrong committer PKH (should be Alice)
              validFrom = afterTimeout,
              signer = Eve.signer
            )
        }
    }

    test("committer fails before timeout") {
        val provider = createProvider()
        val lockedUtxo = lock(provider)
        val utxos = provider.findUtxos(Alice.address).await().toOption.get

        assertBuildFailure(HtlcValidator.InvalidCommitterTimePoint) {
            txCreator.timeout(
              utxos = utxos,
              lockedUtxo = lockedUtxo,
              payeeAddress = Alice.address,
              sponsor = Alice.address,
              committerPkh = Alice.addrKeyHash,
              validFrom = beforeTimeout,
              signer = Alice.signer
            )
        }
    }
}
