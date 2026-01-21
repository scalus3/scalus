package scalus.testing.integration

import scalus.cardano.ledger.{CardanoInfo, SlotNo, Transaction, TransactionHash}
import scalus.cardano.node.{Provider, SubmitError}

import scala.concurrent.{ExecutionContext, Future}

/** Integration test context for real Cardano networks via Blockfrost.
  *
  * Uses system time for slot calculation and Thread.sleep for time advancement. Real networks
  * require longer wait times for transaction propagation.
  *
  * @param cardanoInfo
  *   Cardano network information
  * @param provider
  *   BlockfrostProvider connected to Preprod or Mainnet
  * @param parties
  *   Available test parties (derived from wallet mnemonic)
  * @param envName
  *   Environment name ("Preprod" or "Mainnet")
  */
class BlockfrostTestContext(
    val cardanoInfo: CardanoInfo,
    val provider: Provider,
    val parties: IndexedSeq[TestParty],
    val envName: String
) extends IntegrationTestContext {

    override def currentSlot: SlotNo =
        cardanoInfo.slotConfig.timeToSlot(System.currentTimeMillis())

    override def awaitSlots(n: Long)(using ExecutionContext): Future[Unit] = {
        Future {
            // Real network slot length (typically 1 second)
            Thread.sleep(n * cardanoInfo.slotConfig.slotLength)
        }
    }

    override def submit(
        tx: Transaction
    )(using ExecutionContext): Future[Either[SubmitError, TransactionHash]] = {
        for
            result <- provider.submit(tx)
            _ <- if result.isRight then awaitSlots(20) else Future.unit // Wait for propagation
        yield result
    }
}
