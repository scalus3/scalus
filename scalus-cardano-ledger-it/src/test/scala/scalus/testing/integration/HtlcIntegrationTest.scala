package scalus.testing.integration

import org.scalatest.funsuite.AnyFunSuite
import scalus.builtin.Builtins.sha3_256
import scalus.cardano.ledger.*
import scalus.examples.htlc.*
import scalus.utils.await

import java.time.Instant
import scala.concurrent.ExecutionContext.Implicits.global

/** Integration tests for HTLC (Hash Time Locked Contracts).
  *
  * These tests run against multiple environments:
  *   - Emulator (default, fast): `sbtn scalusCardanoLedgerIt/testOnly *HtlcIntegrationTest`
  *   - YaciDevKit: `SCALUS_TEST_ENV=yaci sbtn ...`
  *   - Preprod: `SCALUS_TEST_ENV=preprod BLOCKFROST_API_KEY=... WALLET_MNEMONIC_PREPROD=... sbtn
  *     ...`
  *
  * Note: For Emulator environment, each test creates a fresh context to ensure isolation. For
  * Yaci/Blockfrost, a shared context is used since container/connection setup is expensive.
  */
class HtlcIntegrationTest extends AnyFunSuite with IntegrationTest {

    // Test data
    private val validPreimage: Preimage = genByteStringOfN(32).sample.get
    private val wrongPreimage: Preimage = genByteStringOfN(12).sample.get
    private lazy val image: Image = sha3_256(validPreimage)

    // HTLC contract (shared across all tests)
    private lazy val contract = HtlcContract.compiled.withErrorTraces

    // Shared context for non-emulator environments
    private lazy val sharedCtx = createTestContext()

    // Get test context - fresh for emulator, shared for others
    private def ctx: IntegrationTestContext =
        if testEnvName == "emulator" then createTestContext()
        else sharedCtx

    // Timeout slot offset
    private val timeoutSlots = 10L

    /** Test helper containing all state for a single test */
    case class TestSetup(
        ctx: IntegrationTestContext,
        txCreator: HtlcTransactions,
        timeout: Instant
    ) {
        def currentTimeout: Instant = {
            val timeoutSlot = ctx.currentSlot + timeoutSlots
            Instant.ofEpochMilli(ctx.cardanoInfo.slotConfig.slotToTime(timeoutSlot))
        }
    }

    private def setup(): TestSetup = {
        val testCtx = ctx
        val txCreator = HtlcTransactions(testCtx.cardanoInfo, contract)
        val timeout = {
            val timeoutSlot = testCtx.currentSlot + timeoutSlots
            Instant.ofEpochMilli(testCtx.cardanoInfo.slotConfig.slotToTime(timeoutSlot))
        }
        TestSetup(testCtx, txCreator, timeout)
    }

    /** Lock funds in HTLC and return the locked UTxO */
    private def lock(s: TestSetup): Utxo = {
        val utxos = s.ctx.provider.findUtxos(s.ctx.alice.address).await().toOption.get

        val lockTx = s.txCreator.lock(
          utxos = utxos,
          value = Value.ada(10),
          sponsor = s.ctx.alice.address,
          committer = s.ctx.alice.addrKeyHash,
          receiver = s.ctx.bob.addrKeyHash,
          image = image,
          timeout = s.timeout,
          signer = s.ctx.alice.signer
        )

        val result = s.ctx.submit(lockTx).await()
        assert(result.isRight, s"Lock transaction failed: $result")

        // Find locked UTxO at script address
        val scriptAddr = contract.address(s.ctx.cardanoInfo.network)
        val scriptUtxos = s.ctx.provider.findUtxos(scriptAddr).await().toOption.get
        Utxo(scriptUtxos.head)
    }

    // ===== Success Tests =====

    test(s"[${testEnvName}] lock HTLC funds") {
        val s = setup()
        val utxos = s.ctx.provider.findUtxos(s.ctx.alice.address).await().toOption.get

        val lockTx = s.txCreator.lock(
          utxos = utxos,
          value = Value.ada(10),
          sponsor = s.ctx.alice.address,
          committer = s.ctx.alice.addrKeyHash,
          receiver = s.ctx.bob.addrKeyHash,
          image = image,
          timeout = s.timeout,
          signer = s.ctx.alice.signer
        )

        val result = s.ctx.submit(lockTx).await()
        assert(result.isRight, s"Lock failed: $result")
    }

    test(s"[${testEnvName}] receiver reveals preimage before timeout") {
        val s = setup()
        val lockedUtxo = lock(s)
        val bobUtxos = s.ctx.provider.findUtxos(s.ctx.bob.address).await().toOption.get

        val revealTx = s.txCreator.reveal(
          utxos = bobUtxos,
          lockedUtxo = lockedUtxo,
          payeeAddress = s.ctx.bob.address,
          sponsor = s.ctx.bob.address,
          preimage = validPreimage,
          receiverPkh = s.ctx.bob.addrKeyHash,
          validTo = s.timeout,
          signer = s.ctx.bob.signer
        )

        val result = s.ctx.submit(revealTx).await()
        assert(result.isRight, s"Reveal failed: $result")
    }

    test(s"[${testEnvName}] committer reclaims after timeout") {
        val s = setup()
        val lockedUtxo = lock(s)

        // Advance past timeout
        s.ctx.awaitSlots(timeoutSlots + 1).await()

        val aliceUtxos = s.ctx.provider.findUtxos(s.ctx.alice.address).await().toOption.get
        val afterTimeout = Instant.ofEpochMilli(
          s.ctx.cardanoInfo.slotConfig.slotToTime(s.ctx.currentSlot)
        )

        val timeoutTx = s.txCreator.timeout(
          utxos = aliceUtxos,
          lockedUtxo = lockedUtxo,
          payeeAddress = s.ctx.alice.address,
          sponsor = s.ctx.alice.address,
          committerPkh = s.ctx.alice.addrKeyHash,
          validFrom = afterTimeout,
          signer = s.ctx.alice.signer
        )

        val result = s.ctx.submit(timeoutTx).await()
        assert(result.isRight, s"Timeout failed: $result")
    }

    // ===== Failure Tests =====

    test(s"[${testEnvName}] receiver fails with wrong preimage") {
        val s = setup()
        val lockedUtxo = lock(s)
        val bobUtxos = s.ctx.provider.findUtxos(s.ctx.bob.address).await().toOption.get

        assertScriptFail(HtlcValidator.InvalidReceiverPreimage) {
            s.txCreator.reveal(
              utxos = bobUtxos,
              lockedUtxo = lockedUtxo,
              payeeAddress = s.ctx.bob.address,
              sponsor = s.ctx.bob.address,
              preimage = wrongPreimage,
              receiverPkh = s.ctx.bob.addrKeyHash,
              validTo = s.timeout,
              signer = s.ctx.bob.signer
            )
        }
    }

    test(s"[${testEnvName}] receiver fails with wrong receiver") {
        val s = setup()
        val lockedUtxo = lock(s)
        val eveUtxos = s.ctx.provider.findUtxos(s.ctx.eve.address).await().toOption.get

        assertScriptFail(HtlcValidator.UnsignedReceiverTransaction) {
            s.txCreator.reveal(
              utxos = eveUtxos,
              lockedUtxo = lockedUtxo,
              payeeAddress = s.ctx.eve.address,
              sponsor = s.ctx.eve.address,
              preimage = validPreimage,
              receiverPkh = s.ctx.eve.addrKeyHash,
              validTo = s.timeout,
              signer = s.ctx.eve.signer
            )
        }
    }

    test(s"[${testEnvName}] receiver fails after timeout") {
        val s = setup()
        val lockedUtxo = lock(s)

        // Advance past timeout
        s.ctx.awaitSlots(timeoutSlots + 1).await()

        val afterTimeout = Instant.ofEpochMilli(
          s.ctx.cardanoInfo.slotConfig.slotToTime(s.ctx.currentSlot + 1)
        )
        val bobUtxos = s.ctx.provider.findUtxos(s.ctx.bob.address).await().toOption.get

        assertScriptFail(HtlcValidator.InvalidReceiverTimePoint) {
            s.txCreator.reveal(
              utxos = bobUtxos,
              lockedUtxo = lockedUtxo,
              payeeAddress = s.ctx.bob.address,
              sponsor = s.ctx.bob.address,
              preimage = validPreimage,
              receiverPkh = s.ctx.bob.addrKeyHash,
              validTo = afterTimeout,
              signer = s.ctx.bob.signer
            )
        }
    }

    test(s"[${testEnvName}] committer fails with wrong committer") {
        val s = setup()
        val lockedUtxo = lock(s)
        s.ctx.awaitSlots(timeoutSlots + 1).await()

        val afterTimeout = Instant.ofEpochMilli(
          s.ctx.cardanoInfo.slotConfig.slotToTime(s.ctx.currentSlot)
        )
        val eveUtxos = s.ctx.provider.findUtxos(s.ctx.eve.address).await().toOption.get

        assertScriptFail(HtlcValidator.UnsignedCommitterTransaction) {
            s.txCreator.timeout(
              utxos = eveUtxos,
              lockedUtxo = lockedUtxo,
              payeeAddress = s.ctx.eve.address,
              sponsor = s.ctx.eve.address,
              committerPkh = s.ctx.eve.addrKeyHash,
              validFrom = afterTimeout,
              signer = s.ctx.eve.signer
            )
        }
    }

    test(s"[${testEnvName}] committer fails before timeout") {
        val s = setup()
        val lockedUtxo = lock(s)
        // Don't advance time - still before timeout

        val beforeTimeout = Instant.ofEpochMilli(
          s.ctx.cardanoInfo.slotConfig.slotToTime(s.ctx.currentSlot)
        )
        val aliceUtxos = s.ctx.provider.findUtxos(s.ctx.alice.address).await().toOption.get

        assertScriptFail(HtlcValidator.InvalidCommitterTimePoint) {
            s.txCreator.timeout(
              utxos = aliceUtxos,
              lockedUtxo = lockedUtxo,
              payeeAddress = s.ctx.alice.address,
              sponsor = s.ctx.alice.address,
              committerPkh = s.ctx.alice.addrKeyHash,
              validFrom = beforeTimeout,
              signer = s.ctx.alice.signer
            )
        }
    }
}
