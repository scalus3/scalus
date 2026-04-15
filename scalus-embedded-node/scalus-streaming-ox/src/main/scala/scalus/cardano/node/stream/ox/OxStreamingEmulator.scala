package scalus.cardano.node.stream.ox

import ox.flow.Flow
import scalus.cardano.node.Emulator
import scalus.cardano.node.stream.StreamingEmulatorOps
import scalus.cardano.node.stream.engine.Engine

import OxScalusAsyncStream.Id

import scala.concurrent.ExecutionContext

/** ox specialisation: an [[OxBlockchainStreamProvider]] backed by an [[Emulator]].
  *
  * Direct-style wrapper: `submit` blocks until the emulator commits and the resulting
  * `AppliedBlock` has been enqueued onto the engine's worker. See [[StreamingEmulatorOps]] for the
  * ledger-validation invariant and tip-numbering semantics.
  */
class OxStreamingEmulator(protected val emulator: Emulator)(using ExecutionContext)
    extends OxBlockchainStreamProvider(
      new Engine(emulator.cardanoInfo, Some(emulator), Engine.DefaultSecurityParam)
    )
    with StreamingEmulatorOps[Id, Flow]
