package scalus.examples.htlc

import org.scalatest.funsuite.AnyFunSuite
import scalus.builtin.Builtins.sha3_256
import scalus.builtin.Data.toData
import scalus.cardano.ledger.*
import scalus.cardano.node.Emulator
import scalus.cardano.txbuilder.RedeemerPurpose.ForSpend
import scalus.cardano.txbuilder.txBuilder
import scalus.testing.kit.Party.{Alice, Bob, Eve}
import scalus.testing.kit.TestUtil.getScriptContextV3
import scalus.testing.kit.{ScalusTest, TestUtil, TxTestKit}
import scalus.utils.await

import java.time.Instant
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Try

class HtlcTest extends AnyFunSuite, ScalusTest, TxTestKit {

    private given env: CardanoInfo = TestUtil.testEnvironment
    private val contract = HtlcContract.compiled.withErrorTraces

    /** Transaction creator with real script evaluation */
    private val txCreator = HtlcTransactions(
      env = env,
      contract = contract
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

    private def createProvider: Emulator =
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
            txOut.address == contract.address(env.network)
        }.get
        Utxo(lockedUtxo)
    }

    // Transaction assertions provided by TxTestKit

    test(s"HTLC validator size is ${HtlcContract.compiled.script.script.size} bytes") {
        assert(HtlcContract.compiled.script.script.size == 569)
    }

    test("VALIDATOR: receiver reveals preimage before timeout") {
        val provider = createProvider
        val lockedUtxo = lock(provider)
        val utxos = provider.utxos

        val scriptCtx = txBuilder
            .spend(
              lockedUtxo,
              redeemer = Action.Reveal(validPreimage),
              script = contract.script,
              requiredSigners = Set(Bob.addrKeyHash)
            )
            .payTo(Bob.address, Value.ada(10))
            .validTo(timeout)
            .draft
            .getScriptContextV3(utxos, ForSpend(lockedUtxo.input))

        assert(Try(contract(scriptCtx.toData).code).isSuccess)
        val result = contract(scriptCtx.toData).program.evaluateDebug
        assert(result.isSuccess)
        assert(result.budget == ExUnits(46134, 16953083))
        assert(result.budget.fee == Coin(3885))
    }

    test("receiver reveals preimage before timeout") {
        val provider = createProvider
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

        assertResult(ExUnits(46134, 16953083)):
            revealTx.witnessSet.redeemers.get.value.totalExUnits

        provider.setSlot(beforeSlot)

        val result = provider.submit(revealTx).await()
        assert(result.isRight, s"Emulator submission failed: $result")
    }

    test("receiver fails with wrong preimage") {
        val provider = createProvider
        val lockedUtxo = lock(provider)
        val utxos = provider.findUtxos(Bob.address).await().toOption.get

        assertScriptFail(HtlcValidator.InvalidReceiverPreimage) {
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
        val provider = createProvider
        val lockedUtxo = lock(provider)
        val utxos = provider.findUtxos(Eve.address).await().toOption.get

        assertScriptFail(HtlcValidator.UnsignedReceiverTransaction) {
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
        val provider = createProvider
        val lockedUtxo = lock(provider)
        val utxos = provider.findUtxos(Bob.address).await().toOption.get

        assertScriptFail(HtlcValidator.InvalidReceiverTimePoint) {
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
        val provider = createProvider
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
        val submissionResult = provider.submit(timeoutTx).await()
        assert(submissionResult.isRight, s"Emulator submission failed: $submissionResult")
    }

    test("committer fails with wrong committer pubkey hash") {
        val provider = createProvider
        val lockedUtxo = lock(provider)
        val utxos = provider.findUtxos(Eve.address).await().toOption.get

        assertScriptFail(HtlcValidator.UnsignedCommitterTransaction) {
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
        val provider = createProvider
        val lockedUtxo = lock(provider)
        val utxos = provider.findUtxos(Alice.address).await().toOption.get

        assertScriptFail(HtlcValidator.InvalidCommitterTimePoint) {
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
