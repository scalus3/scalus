package scalus.cardano.txbuilder

import scalus.cardano.address.Address
import scalus.cardano.node.{toAsync, AsyncProvider, Provider}

import scala.concurrent.Await
import scala.concurrent.duration.Duration

/** JVM platform-specific operations for TxBuilder.
  *
  * These operations use blocking calls like `Await.result` which are only available on the JVM
  * platform.
  */
private trait TxBuilderPlatformOps { self: TxBuilder =>

    /** Automatically completes the transaction by selecting UTXOs and collateral.
      *
      * This is a blocking version of `completeAsync`. It queries the provider for available UTXOs
      * at the sponsor address, selects inputs to cover all outputs and fees, selects collateral if
      * needed (for script transactions), and sets up change handling to the sponsor address.
      *
      * This method blocks the calling thread while waiting for provider responses. Not available on
      * JavaScript platform - use `completeAsync` instead.
      *
      * @param provider
      *   the provider to query for UTXOs
      * @param sponsor
      *   the address to use for input selection, collateral, and change
      * @return
      *   a new TxBuilder with additional spend and collateral steps
      * @throws RuntimeException
      *   if insufficient UTXOs are available to cover the transaction requirements
      */
    def complete(provider: Provider, sponsor: Address): TxBuilder = {
        import scala.concurrent.ExecutionContext.Implicits.global
        Await.result(completeAsync(provider.toAsync, sponsor), Duration.Inf)
    }

    /** Automatically completes the transaction using an async provider.
      *
      * This is a blocking version that wraps the async provider.
      *
      * @param provider
      *   the async provider to query for UTXOs
      * @param sponsor
      *   the address to use for input selection, collateral, and change
      */
    def complete(provider: AsyncProvider, sponsor: Address): TxBuilder = {
        import scala.concurrent.ExecutionContext.Implicits.global
        Await.result(completeAsync(provider, sponsor), Duration.Inf)
    }
}
