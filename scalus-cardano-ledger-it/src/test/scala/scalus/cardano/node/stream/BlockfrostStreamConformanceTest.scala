package scalus.cardano.node.stream

import scalus.cardano.address.{Address, Network}
import scalus.cardano.ledger.*
import scalus.cardano.node.BlockchainReader
import scalus.cardano.txbuilder.{TransactionSigner, TxBuilder}
import scalus.cardano.wallet.hd.HdAccount
import scalus.testing.integration.IntegrationTest
import scalus.testing.stream.{StreamConformanceFixture, StreamProviderConformance}
import scalus.utils.await

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.*

/** `StreamProviderConformance` against preprod — the acceptance criterion the M2 plan names.
  *
  * The suite reads a provider's declared [[StreamCapabilities]] and holds it to them, so running it
  * here does not merely check that the Blockfrost provider works: it checks that what it advertises
  * is what it does. Everything it declines — `Block`, `TransactionStatus`, replay, anything outside
  * an address pushdown — is refused by the suite's own assertions rather than skipped silently.
  *
  * Slow and metered by nature: each fixture builds a streaming view, and several tests submit a
  * transaction and wait for it to reach a block. Budget tens of minutes and a few hundred requests.
  *
  * Run with: {{{
  *   SCALUS_TEST_ENV=preprod BLOCKFROST_API_KEY=... WALLET_MNEMONIC_PREPROD=... \
  *     sbt "scalusCardanoLedgerIt/testOnly *BlockfrostStreamConformanceTest"
  * }}}
  */
class BlockfrostStreamConformanceTest extends StreamProviderConformance with IntegrationTest {

    /** A chain that produces a block every ~20s, behind a provider that polls every 10s and a
      * backend that indexes afterwards. The emulator's default of five seconds is a different
      * order of magnitude.
      */
    override protected def patience: FiniteDuration = 5.minutes

    /** Shared across fixtures, and started at a band this run has not used before.
      *
      * `withFixture` builds a fresh fixture per test, so a per-fixture counter would hand every
      * test the same "fresh" address — and a fixed base would hand this run the addresses the last
      * run already paid. Neither is fresh, and the seeding assertions cannot tell the difference
      * between a stale UTxO and a live one.
      */
    private val nextIndex = new java.util.concurrent.atomic.AtomicInteger(
      100 + (System.currentTimeMillis() / 1000 % 1000000).toInt
    )

    override protected def newFixture(): StreamConformanceFixture = {
        assume(testEnvName == "preprod", "meaningful only against a real chain")
        new PreprodFixture(createTestContext())
    }

    private class PreprodFixture(ctx: scalus.testing.integration.IntegrationTestContext)
        extends StreamConformanceFixture {

        private val mnemonic = requireEnv("WALLET_MNEMONIC_PREPROD")

        /** Fresh addresses come from further HD indices of the same wallet.
          *
          * They have to be two things at once, and only derivation gives both: never paid before,
          * which the suite's seeding assertions depend on, and *spendable*, since `spendFrom` has
          * to sign for them. Reusing the three configured parties would satisfy the second and
          * fail the first as soon as a second test ran.
          */
        private var signers = Map.empty[Address, TransactionSigner]

        val reader: BlockchainReader = ctx.provider
        val provider: BlockchainStreaming = ctx.provider.streaming()

        def payer: Address = ctx.alice.address

        def freshAddress(): Address = {
            val account = HdAccount.fromMnemonic(mnemonic, "", nextIndex.getAndIncrement())
            val address = account.baseAddress(Network.Testnet)
            signers += address -> new TransactionSigner(Set(account.paymentKeyPair))
            address
        }

        def payTo(address: Address, amount: Value): TransactionHash =
            submit(payer, ctx.alice.signer, address, amount)

        def spendFrom(address: Address, amount: Value): TransactionHash =
            submit(
              address,
              signers.getOrElse(
                address,
                throw new IllegalArgumentException(
                  s"no signer for $address; spendFrom only works for addresses freshAddress() made"
                )
              ),
              payer,
              amount
            )

        private def submit(
            from: Address,
            signer: TransactionSigner,
            to: Address,
            amount: Value
        ): TransactionHash = {
            val tx = TxBuilder(ctx.cardanoInfo)
                .payTo(to, amount)
                .complete(ctx.provider, from)
                .await(2.minutes)
                .sign(signer)
                .transaction
            ctx.provider.submit(tx).await(3.minutes) match
                case Left(error) => throw new AssertionError(s"fixture submit failed: $error")
                case Right(hash) =>
                    awaitSpent(from, tx.body.value.inputs.toSet)
                    hash
        }

        /** Wait until the spender's UTxO listing no longer offers the inputs just consumed.
          *
          * Confirmation is the wrong signal, and using it is what made the first run fail with
          * "All inputs are spent" on six tests: `/txs/{hash}` reports a transaction as confirmed
          * from one index while `/addresses/{a}/utxos` — the one `complete` selects inputs from —
          * is still serving the outputs it spent. Waiting on the index actually read is the only
          * thing that makes consecutive submissions safe.
          */
        private def awaitSpent(spender: Address, consumed: Set[TransactionInput]): Unit = {
            val deadline = System.currentTimeMillis() + 5.minutes.toMillis
            while System.currentTimeMillis() < deadline do
                val utxos = ctx.provider.findUtxos(spender).await(1.minute)
                val stillOffered = utxos.toOption.map(_.keySet.intersect(consumed)).getOrElse(Set.empty)
                if stillOffered.isEmpty then return
                Thread.sleep(5000)
            throw new AssertionError(
              s"$spender still offers inputs spent 5 minutes ago; the UTxO index is not catching up"
            )
        }

        def close(): Unit = provider.close().await(1.minute)
    }
}
