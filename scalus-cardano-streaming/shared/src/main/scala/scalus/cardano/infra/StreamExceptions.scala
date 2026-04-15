package scalus.cardano.infra

/** Raised on the consumer side of a [[scalus.cardano.node.stream.engine.Mailbox]] when a bounded
  * delta mailbox runs out of room. The subscriber must treat its view as no-longer-trustworthy and
  * resync.
  */
class ScalusBufferOverflowException(message: String = "subscriber buffer overflow")
    extends RuntimeException(message)
