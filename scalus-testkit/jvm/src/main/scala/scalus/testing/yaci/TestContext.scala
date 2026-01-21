package scalus.testing.yaci

import scalus.cardano.address.{ShelleyAddress, StakeAddress}
import scalus.cardano.ledger.{CardanoInfo, Transaction}
import scalus.cardano.node.Provider
import scalus.cardano.txbuilder.TransactionSigner
import scalus.cardano.wallet.Account
import scalus.utils.await

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.*

/** Test context containing all utilities needed for transaction building and submission
  *
  * This is the main abstraction for working with Yaci DevKit in tests. It provides a unified
  * interface for submitting transactions and waiting for blocks.
  *
  * @param cardanoInfo
  *   Cardano network information (fetched via provider.fetchCardanoInfo())
  * @param provider
  *   Provider for blockchain queries (typically BlockfrostProvider for Yaci DevKit)
  * @param account
  *   Account with HD wallet support (CIP-1852 compatible)
  * @param signer
  *   Transaction signer configured with the account's keys
  * @param address
  *   Base address for the test account
  * @param stakeAddress
  *   Stake address for the test account
  */
case class TestContext(
    cardanoInfo: CardanoInfo,
    provider: Provider,
    account: Account,
    signer: TransactionSigner,
    address: ShelleyAddress,
    stakeAddress: StakeAddress
):
    /** Submit a transaction and wait for confirmation
      *
      * @param tx
      *   Transaction to submit
      * @return
      *   Right(txHash) on success, Left(error) on failure
      */
    def submitTx(tx: Transaction): Either[String, String] =
        provider.submit(tx).await(30.seconds).map(_.toHex).left.map(_.toString)

    /** Wait for the next block to be produced
      *
      * Yaci DevKit produces blocks every ~2 seconds. This is a simple sleep-based wait.
      */
    def waitForBlock(): Unit = Thread.sleep(2000)
