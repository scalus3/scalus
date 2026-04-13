package scalus.examples.streaming.oxbatcher

import ox.flow.Flow
import scalus.cardano.node.stream.BlockchainStreamProviderTF

import OxScalusAsyncStream.Id

/** A `BlockchainStreamProvider` specialised to ox: identity effect for
  * the snapshot side, `ox.flow.Flow` for the streaming side.
  *
  * With this trait the ox batcher stays in direct style end-to-end —
  * no `Future` bridging at submit call sites.
  *
  * Belongs in a future `scalus-streaming-ox` adapter module.
  */
trait OxBlockchainStreamProvider extends BlockchainStreamProviderTF[Id, Flow]
