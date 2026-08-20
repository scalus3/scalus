package scalus.testing.stream

import scalus.cardano.address.Address
import scalus.cardano.ledger.rules.STS
import scalus.cardano.ledger.{CardanoInfo, TransactionHash, Value}
import scalus.cardano.node.{Emulator, EmulatorBase}
import scalus.cardano.node.stream.{BlockchainStreamProvider, ScalusAsyncSource, StreamingEmulator}
import scalus.cardano.txbuilder.TxBuilder
import scalus.testing.kit.Party
import scalus.utils.await

/** The emulator implementation of the streaming facade, held to the shared contract.
  *
  * Runs against the zero-dependency `ScalusAsyncSource` stream type, which is also the point: the
  * suite needs no stream library to exercise a provider, so an implementation can be conformance
  * tested before anyone has written an adapter for it.
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

        val provider: BlockchainStreamProvider[ScalusAsyncSource] =
            new StreamingEmulator[ScalusAsyncSource](emulator)

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
                .complete(provider, from)
                .await()
                .transaction
            provider.submit(tx).await() match
                case Right(hash) => hash
                case Left(error) => throw new AssertionError(s"fixture submit failed: $error")
        }

        def close(): Unit = provider.close().await()
    }
}
