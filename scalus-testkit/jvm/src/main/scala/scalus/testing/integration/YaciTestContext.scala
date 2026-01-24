package scalus.testing.integration

import scalus.cardano.ledger.{CardanoInfo, SlotNo, Transaction, TransactionHash}
import scalus.cardano.node.{BlockchainProvider, SubmitError}
import scalus.utils.await

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.*
import scala.concurrent.{ExecutionContext, Future}

/** Integration test context for YaciDevKit.
  *
  * Uses system time for slot calculation and Thread.sleep for time advancement. YaciDevKit uses
  * 1-second slots starting from zero time.
  *
  * Access test parties via `alice`, `bob`, `eve`. For full HD account access (stake, drep, change
  * keys), use `Party.Alice.account` directly.
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
    val provider: BlockchainProvider,
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

    /** Submit a transaction synchronously and wait for confirmation.
      *
      * @param tx
      *   Transaction to submit
      * @return
      *   Right(txHash) on success, Left(error) on failure
      */
    def submitTx(tx: Transaction): Either[String, String] =
        provider.submit(tx).await(30.seconds).map(_.toHex).left.map(_.toString)

    /** Wait for the next block to be produced (synchronous).
      *
      * Yaci DevKit produces blocks every ~2 seconds.
      */
    def waitForBlock(): Unit = Thread.sleep(2000)
}
