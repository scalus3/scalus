package scalus.cardano.node.stream

import scalus.cardano.ledger.{Transaction, TransactionHash}
import scalus.cardano.node.{Emulator, SubmitError}
import scalus.cardano.node.stream.engine.EngineDriver

import scala.concurrent.Future

/** Mix-in for a [[BaseStreamProvider]] that is backed by a [[scalus.cardano.node.Emulator]].
  *
  * Turns emulator activity into `AppliedBlock` events fed into the streaming engine:
  *   - Overrides `submit` to run [[Emulator.submitSync]] (ledger-validated: the emulator's
  *     `validators` + `mutators` pipeline runs before state is committed), and on success
  *     hands the result to [[EngineDriver]] so the engine sees `notifySubmit` then
  *     `onRollForward` in order.
  *   - Exposes `newEmptyBlock()` so tests can advance the tip without a transaction.
  *
  * **Correctness invariant.** An `AppliedBlock` is emitted only on `Right(hash)` from
  * `submitSync`, i.e. only after all validators + mutators pass and the emulator commits
  * the new state. Rejected transactions never surface as events.
  *
  * The engine-driving side of the work (block construction, counter, serialised calls on
  * the engine worker) lives in [[EngineDriver]] so the deferred `ImmutableStreamingEmulator`
  * can reuse the same driver from scenario-scoped code.
  */
trait StreamingEmulatorOps[F[_], C[_]] extends BaseStreamProvider[F, C] {

    protected def emulator: Emulator

    protected lazy val driver: EngineDriver = new EngineDriver(engine)

    override def submit(
        transaction: Transaction
    ): F[Either[SubmitError, TransactionHash]] = liftFuture {
        emulator.submitSync(transaction) match
            case Left(e) => Future.successful(Left(e))
            case Right(hash) =>
                given scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.parasitic
                driver.applySubmit(transaction, hash).map(_ => Right(hash))
    }

    /** Produce an empty block — advances the tip without submitting a transaction. */
    def newEmptyBlock(): F[Unit] = liftFuture(driver.emptyBlock())
}
