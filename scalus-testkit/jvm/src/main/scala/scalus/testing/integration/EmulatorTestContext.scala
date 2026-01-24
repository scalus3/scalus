package scalus.testing.integration

import scalus.cardano.ledger.{CardanoInfo, SlotNo, Transaction, TransactionHash}
import scalus.cardano.node.{BlockchainProvider, Emulator, SubmitError}

import scala.concurrent.{ExecutionContext, Future}

/** Integration test context using the in-memory Emulator.
  *
  * Provides fast, deterministic testing with instant slot advancement.
  *
  * @param cardanoInfo
  *   Cardano network information
  * @param emulator
  *   The in-memory Emulator instance
  * @param parties
  *   Available test parties
  * @param initialSlot
  *   Starting slot number (default: 0)
  */
class EmulatorTestContext(
    val cardanoInfo: CardanoInfo,
    val emulator: Emulator,
    val parties: IndexedSeq[TestParty],
    initialSlot: SlotNo = 0
) extends IntegrationTestContext {

    // Track slot separately since Emulator doesn't expose it
    private var _slot: SlotNo = initialSlot
    emulator.setSlot(initialSlot)

    override def provider: BlockchainProvider = emulator

    override def envName: String = "Emulator"

    override def currentSlot: SlotNo = _slot

    override def awaitSlots(n: Long)(using ExecutionContext): Future[Unit] = {
        Future.successful {
            _slot = _slot + n
            emulator.setSlot(_slot)
        }
    }

    override def submit(
        tx: Transaction
    )(using ExecutionContext): Future[Either[SubmitError, TransactionHash]] = {
        // Emulator submission is synchronous, no waiting needed
        provider.submit(tx)
    }
}
