package scalus.testing.integration

import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.{CardanoInfo, SlotNo, Transaction, TransactionHash}
import scalus.cardano.node.{Provider, SubmitError}
import scalus.cardano.txbuilder.TransactionSigner
import scalus.cardano.ledger.AddrKeyHash
import scalus.testing.kit.Party

import scala.concurrent.{ExecutionContext, Future}

/** A test party with all information needed for transaction signing and submission.
  *
  * @param party
  *   The Party enum value
  * @param address
  *   Shelley address for receiving funds
  * @param addrKeyHash
  *   Payment key hash for script requirements
  * @param signer
  *   Transaction signer for signing transactions
  */
case class TestParty(
    party: Party,
    address: ShelleyAddress,
    addrKeyHash: AddrKeyHash,
    signer: TransactionSigner
)

/** Unified test context for integration testing across different environments.
  *
  * All time-dependent methods return Future for async compatibility. Use `.await()` extension from
  * `scalus.utils.await` for synchronous test code.
  */
trait IntegrationTestContext {

    /** Cardano network information (protocol params, network, slot config). */
    def cardanoInfo: CardanoInfo

    /** Provider for blockchain queries and transaction submission. */
    def provider: Provider

    /** All available test parties. */
    def parties: IndexedSeq[TestParty]

    /** Convenience accessor for Alice (first party). */
    def alice: TestParty = parties(Party.Alice.ordinal)

    /** Convenience accessor for Bob (second party). */
    def bob: TestParty = parties(Party.Bob.ordinal)

    /** Convenience accessor for Eve (eavesdropper party). */
    def eve: TestParty = parties(Party.Eve.ordinal)

    /** Current slot number. */
    def currentSlot: SlotNo

    /** Advance time by n slots (async).
      *
      *   - Emulator: Directly sets slot to currentSlot + n (instant)
      *   - Yaci/Real networks: Waits for ~n seconds (1 slot = 1 sec in Yaci)
      *
      * @param n
      *   Number of slots to advance
      */
    def awaitSlots(n: Long)(using ExecutionContext): Future[Unit]

    /** Submit transaction and wait for confirmation (async).
      *
      * @param tx
      *   Transaction to submit
      * @return
      *   Either an error or the transaction hash
      */
    def submit(tx: Transaction)(using
        ExecutionContext
    ): Future[Either[SubmitError, TransactionHash]]

    /** Environment name for logging (e.g., "Emulator", "YaciDevKit", "Preprod"). */
    def envName: String
}
