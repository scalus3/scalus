package scalus.testing.stream

import scalus.cardano.address.Address
import scalus.cardano.ledger.rules.STS
import scalus.cardano.ledger.{CardanoInfo, TransactionHash, Value}
import scalus.cardano.node.{BlockchainReader, Emulator, EmulatorBase}
import scalus.cardano.node.stream.{BlockchainStreaming, StreamingEmulator}
import scalus.cardano.txbuilder.TxBuilder
import scalus.testing.kit.Party
import scalus.utils.await

/** The emulator implementation of the streaming facade, held to the shared contract.
  *
  * The provider type carries no stream parameter, and the suite asks it for `ScalusAsyncSource`s —
  * so an implementation can be conformance tested before anyone has written an adapter for it, and
  * the same provider value can still hand fs2 streams to application code.
  */
class StreamingEmulatorConformanceTest extends StreamProviderConformance {

    protected def newFixture(): StreamConformanceFixture = new StreamConformanceFixture {

        private given CardanoInfo = CardanoInfo.mainnet

        private val parties = Party.values.toIndexedSeq
        private var nextParty = 1 // 0 is the payer

        private val emulator = new Emulator(
          initialUtxos = EmulatorBase.createInitialUtxos(Seq(payer)),
          // Witness validation is not what this suite is about, and signing every fixture payment
          // would only test the tx builder.
          validators = Set.empty[STS.Validator]
        )

        // Reads and submission stay on the emulator; the streaming view only subscribes. The
        // view observes every applied transaction through the ledger's own hook, so submitting
        // straight to the emulator — as `submit` below does — still reaches every subscription.
        val reader: BlockchainReader = emulator
        val provider: BlockchainStreaming = emulator.streaming()

        def payer: Address = parties(0).address

        def freshAddress(): Address = {
            val party = parties(nextParty % parties.size)
            nextParty += 1
            party.address
        }

        def payTo(address: Address, amount: Value): TransactionHash = submit(payer, address, amount)

        def spendFrom(address: Address, amount: Value): TransactionHash =
            submit(address, payer, amount)

        private def submit(from: Address, to: Address, amount: Value): TransactionHash = {
            val tx = TxBuilder(CardanoInfo.mainnet)
                .payTo(to, amount)
                .complete(emulator, from)
                .await()
                .transaction
            emulator.submit(tx).await() match
                case Right(hash) => hash
                case Left(error) => throw new AssertionError(s"fixture submit failed: $error")
        }

        def close(): Unit = provider.close().await()
    }
}
