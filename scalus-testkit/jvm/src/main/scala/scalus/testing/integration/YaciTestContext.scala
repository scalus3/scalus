package scalus.testing.integration

import scalus.cardano.ledger.{CardanoInfo, SlotNo, Transaction, TransactionHash}
import scalus.cardano.node.{Provider, SubmitError}

import scala.concurrent.{ExecutionContext, Future}

/** Integration test context for YaciDevKit.
  *
  * Uses system time for slot calculation and Thread.sleep for time advancement. YaciDevKit uses
  * 1-second slots starting from zero time.
  *
  * @param cardanoInfo
  *   Cardano network information
  * @param provider
  *   BlockfrostProvider connected to YaciDevKit
  * @param parties
  *   Available test parties
  */
class YaciTestContext(
    val cardanoInfo: CardanoInfo,
    val provider: Provider,
    val parties: IndexedSeq[TestParty]
) extends IntegrationTestContext {

    override def envName: String = "YaciDevKit"

    override def currentSlot: SlotNo =
        cardanoInfo.slotConfig.timeToSlot(System.currentTimeMillis())

    override def awaitSlots(n: Long)(using ExecutionContext): Future[Unit] = {
        Future {
            // Yaci uses 1-second slots
            Thread.sleep(n * cardanoInfo.slotConfig.slotLength)
        }
    }

    override def submit(
        tx: Transaction
    )(using ExecutionContext): Future[Either[SubmitError, TransactionHash]] = {
        for
            result <- provider.submit(tx)
            _ <- if result.isRight then awaitSlots(2) else Future.unit // Wait for block
        yield result
    }
}
