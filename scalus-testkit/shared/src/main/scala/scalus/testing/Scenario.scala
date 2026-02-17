package scalus.testing

import cps.*
import cps.monads.FutureAsyncMonad
import cps.monads.logic.*
import scalus.cardano.ledger.*
import scalus.cardano.node.*

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}
import scala.util.control.NonFatal

/** State threaded through Scenario computations.
  *
  * @param emulator
  *   the immutable emulator state
  * @param rng
  *   ScalaCheck RNG seed for deterministic generation
  * @param actionLog
  *   log of actions taken during exploration (newest first for O(1) append)
  */
case class ScenarioState(
    emulator: ImmutableEmulator,
    rng: org.scalacheck.rng.Seed,
    actionLog: List[StepAction] = Nil
)

object ScenarioState {

    /** An empty state with no UTxOs and a zero seed. */
    val empty: ScenarioState = ScenarioState(
      emulator = ImmutableEmulator.empty,
      rng = org.scalacheck.rng.Seed(0L)
    )

}

/** A non-deterministic, state-threading monad for testing Cardano transaction scenarios.
  *
  * Scenario is a sealed ADT:
  *   - [[Scenario.Done]] — terminal value with state
  *   - [[Scenario.Leaf]] — state-dependent continuation
  *   - [[Scenario.Branches]] — non-deterministic branching (independent states per branch)
  *   - [[Scenario.WaitFuture]] — async suspension
  *   - [[Scenario.ScenarioError]] — error
  *   - [[Scenario.FromStream]] — already-evaluated stream (from fsplit/msplit)
  *
  * Use `async[Scenario]` (from dotty-cps-async) for imperative-looking syntax.
  *
  * @tparam A
  *   the result type
  */
sealed trait Scenario[A]

object Scenario {

    // =========================================================================
    // ADT cases
    // =========================================================================

    /** Terminal: holds final state and value. */
    case class Done[A](state: ScenarioState, value: A) extends Scenario[A]

    /** State-dependent continuation. */
    case class Leaf[A](state: ScenarioState, run: ScenarioState => Scenario[A]) extends Scenario[A]

    /** Non-deterministic branching — each branch is an independent Scenario. */
    case class Branches[A](branches: LogicStreamT[Future, Scenario[A]]) extends Scenario[A]

    /** Async suspension — wait for Future, then continue. */
    case class WaitFuture[A](future: Future[Scenario[A]]) extends Scenario[A]

    /** Error with state for rollback in flatMapTry. */
    case class ScenarioError[A](state: ScenarioState, error: Throwable) extends Scenario[A]

    /** Already-evaluated stream (from fsplit/msplit tails). */
    case class FromStream[A](stream: LogicStreamT[Future, (ScenarioState, A)]) extends Scenario[A]

    // =========================================================================
    // Internal utilities
    // =========================================================================

    private given ec: ExecutionContext = ExecutionContext.parasitic

    private val futureMonad: cps.monads.FutureAsyncMonadAPI = FutureAsyncMonad
    private given CpsConcurrentMonad[Future] = futureMonad

    /** The underlying LogicStreamT monad instance used for observation/running. */
    private val logicStreamMonad: CpsConcurrentLogicMonad[[A] =>> LogicStreamT[Future, A], Future] =
        LogicStreamT.cpsLogicStreamConcurrentMonad[Future]

    /** Convenience: create a Leaf with empty initial state (unbound). */
    private def leaf[A](run: ScenarioState => Scenario[A]): Scenario[A] =
        Leaf(ScenarioState.empty, run)

    /** Inject state into a scenario, only overriding unbound (empty sentinel) nodes. Nodes with
      * real states (e.g., from mplus of independent branches) are left unchanged.
      */
    private def injectState[A](s: ScenarioState, scenario: Scenario[A]): Scenario[A] =
        scenario match
            case Done(existing, a) =>
                if existing eq ScenarioState.empty then Done(s, a) else scenario
            case Leaf(existing, run) =>
                if existing eq ScenarioState.empty then Leaf(s, run) else scenario
            case Branches(streams) =>
                Branches(streams.map(branch => injectState(s, branch)))
            case WaitFuture(fut) =>
                WaitFuture(fut.map(sc => injectState(s, sc)))
            case ScenarioError(existing, e) =>
                if existing eq ScenarioState.empty then ScenarioError(s, e) else scenario
            case FromStream(_) => scenario // stream states are already bound

    /** Evaluate a Scenario tree to a LogicStreamT of (state, value) pairs. */
    def eval[A](scenario: Scenario[A]): LogicStreamT[Future, (ScenarioState, A)] =
        scenario match
            case Done(s, a) => LogicStreamT.Pure((s, a))
            case Leaf(s, run) =>
                try eval(run(s))
                catch case NonFatal(e) => LogicStreamT.Error(e)
            case Branches(streams)   => streams.flatMap(branch => eval(branch))
            case WaitFuture(fut)     => LogicStreamT.WaitF(fut.map(sc => eval(sc)))
            case ScenarioError(_, e) => LogicStreamT.Error(e)
            case FromStream(stream)  => stream

    // =========================================================================
    // CpsConcurrentLogicMonad instance
    // =========================================================================

    given scenarioLogicMonad: CpsConcurrentLogicMonad[Scenario, Future]
        with CpsConcurrentLogicMonadInstanceContext[Scenario, Future]
        with {

        override val observerCpsMonad: CpsConcurrentMonad[Future] = futureMonad

        override def pure[A](a: A): Scenario[A] =
            Done(ScenarioState.empty, a)

        override def map[A, B](fa: Scenario[A])(f: A => B): Scenario[B] = fa match
            case Done(s, a) =>
                try Done(s, f(a))
                catch case NonFatal(e) => ScenarioError(s, e)
            case Leaf(s, run)        => Leaf(s, state => map(run(state))(f))
            case Branches(streams)   => Branches(streams.map(branch => map(branch)(f)))
            case WaitFuture(fut)     => WaitFuture(fut.map(sc => map(sc)(f)))
            case ScenarioError(s, e) => ScenarioError(s, e)
            case FromStream(stream)  => FromStream(stream.map { case (s, a) => (s, f(a)) })

        override def flatMap[A, B](fa: Scenario[A])(f: A => Scenario[B]): Scenario[B] = fa match
            case Done(s, a) =>
                try injectState(s, f(a))
                catch case NonFatal(e) => ScenarioError(s, e)
            case Leaf(s, run) => Leaf(s, state => flatMap(run(state))(f))
            case Branches(streams) =>
                Branches(streams.map(branch => flatMap(branch)(f)))
            case WaitFuture(fut)     => WaitFuture(fut.map(sc => flatMap(sc)(f)))
            case ScenarioError(s, e) => ScenarioError(s, e)
            case FromStream(stream) =>
                FromStream(stream.flatMap { case (s, a) => eval(injectState(s, f(a))) })

        override def flatMapTry[A, B](fa: Scenario[A])(f: Try[A] => Scenario[B]): Scenario[B] =
            fa match
                case Done(s, a) =>
                    try injectState(s, f(Success(a)))
                    catch case NonFatal(e) => ScenarioError(s, e)
                case ScenarioError(s, e) =>
                    try injectState(s, f(Failure(e)))
                    catch case NonFatal(e2) => ScenarioError(s, e2)
                case Leaf(s, run) => Leaf(s, state => flatMapTry(run(state))(f))
                case Branches(streams) =>
                    Branches(streams.map(branch => flatMapTry(branch)(f)))
                case WaitFuture(fut) =>
                    WaitFuture(fut.map(sc => flatMapTry(sc)(f)))
                case FromStream(stream) =>
                    FromStream(stream.flatMapTry {
                        case Success((s, a)) => eval(injectState(s, f(Success(a))))
                        case Failure(e)      => eval(f(Failure(e)))
                    })

        override def error[A](e: Throwable): Scenario[A] =
            ScenarioError(ScenarioState.empty, e)

        override def mzero[A]: Scenario[A] =
            Branches(LogicStreamT.Empty[Future, Scenario[A]]())

        override def mplus[A](a: Scenario[A], b: => Scenario[A]): Scenario[A] =
            val aStream = a match
                case Branches(streams) => streams
                case other             => LogicStreamT.Pure[Future, Scenario[A]](other)
            Branches(aStream.mplus(LogicStreamT.Pure[Future, Scenario[A]](b)))

        override def msplit[A](
            sa: Scenario[A]
        ): Scenario[Option[(Try[A], Scenario[A])]] =
            leaf { state =>
                val stream = eval(injectState(state, sa))
                FromStream(logicStreamMonad.msplit(stream).map {
                    case None => (state, None)
                    case Some((tryPair, rest)) =>
                        val tail: Scenario[A] = FromStream(rest)
                        tryPair match
                            case Success((s2, a)) =>
                                (s2, Some((Success(a), tail)))
                            case Failure(e) =>
                                (state, Some((Failure(e), tail)))
                })
            }

        override def fsplit[A](sa: Scenario[A]): Future[Option[(Try[A], Scenario[A])]] =
            logicStreamMonad.fsplit(eval(sa)).map {
                case None => None
                case Some((tryPair, restStream)) =>
                    val tail: Scenario[A] = FromStream(restStream)
                    tryPair match
                        case Success((s2, a)) => Some((Success(a), tail))
                        case Failure(e)       => Some((Failure(e), tail))
            }

        override def withMsplit[A, B](
            c: Scenario[A]
        )(f: Option[(Try[A], Scenario[A])] => Scenario[B]): Scenario[B] =
            flatMap(msplit(c))(f)

        override def flattenObserver[A](fsa: Future[Scenario[A]]): Scenario[A] =
            WaitFuture(fsa)
    }

    // =========================================================================
    // State primitives (package-private)
    // =========================================================================

    private[testing] def get: Scenario[ScenarioState] =
        leaf(state => Done(state, state))

    private[testing] def modify(f: ScenarioState => ScenarioState): Scenario[Unit] =
        leaf(state => Done(f(state), ()))

    private[testing] def modifyAndReturn[A](
        f: ScenarioState => (ScenarioState, A)
    ): Scenario[A] =
        leaf { state =>
            val (s2, a) = f(state)
            Done(s2, a)
        }

    // =========================================================================
    // Public DSL
    // =========================================================================

    /** Create non-deterministic choices from the given values. */
    def choices[A](xs: A*): Scenario[A] =
        fromCollection(xs)

    /** Create non-deterministic choices from a collection. */
    def fromCollection[A](xs: Iterable[A]): Scenario[A] =
        leaf { state =>
            Branches(
              xs.foldRight(
                LogicStreamT.Empty[Future, Scenario[A]](): LogicStreamT[Future, Scenario[A]]
              ) { (a, acc) =>
                  LogicStreamT.Pure[Future, Scenario[A]](Done(state, a)).mplus(acc)
              }
            )
        }

    /** Prune this branch if the condition is false. */
    def guard(cond: Boolean): Scenario[Unit] =
        if cond then scenarioLogicMonad.pure(()) else scenarioLogicMonad.mzero

    /** Always-failing computation (prunes this branch). */
    def fail[A]: Scenario[A] = scenarioLogicMonad.mzero

    /** Raise an error in the current branch. */
    def error[A](e: Throwable): Scenario[A] = scenarioLogicMonad.error(e)

    /** Get the current action log (in chronological order). */
    def actionLog: Scenario[Seq[StepAction]] =
        leaf(state => Done(state, state.actionLog.reverse))

    /** Clear the action log and return the cleared entries (in chronological order). */
    private[testing] def clearActionLog: Scenario[Seq[StepAction]] =
        modifyAndReturn { s =>
            (s.copy(actionLog = Nil), s.actionLog.reverse)
        }

    /** Get the current slot number. */
    def now: Scenario[SlotNo] =
        leaf(state => Done(state, state.emulator.currentSlot))

    /** Advance the slot by the given number of slots. */
    def sleep(slots: Long): Scenario[Unit] =
        modify(s =>
            s.copy(
              emulator = s.emulator.advanceSlot(slots),
              actionLog = StepAction.Wait(slots) :: s.actionLog
            )
        )

    /** Get the current emulator state (read-only snapshot). */
    def currentEmulator: Scenario[ImmutableEmulator] =
        leaf(state => Done(state, state.emulator))

    /** Submit a transaction, updating the emulator state if successful.
      *
      * On success, the transaction is logged to the action log.
      */
    def submit(tx: Transaction): Scenario[Either[SubmitError, TransactionHash]] =
        modifyAndReturn { s =>
            s.emulator.submit(tx) match {
                case Right((txHash, newEmulator)) =>
                    (
                      s.copy(
                        emulator = newEmulator,
                        actionLog = StepAction.Submit(tx) :: s.actionLog
                      ),
                      Right(txHash)
                    )
                case Left(err) =>
                    (s, Left(err))
            }
        }

    /** Get a [[BlockchainProviderTF]] that operates within the Scenario monad.
      *
      * Each call reads/updates the latest emulator state.
      */
    def provider: Scenario[BlockchainProviderTF[Scenario]] = {
        val p = new BlockchainProviderTF[Scenario] {
            override def cardanoInfo: CardanoInfo =
                throw new UnsupportedOperationException(
                  "Use Scenario.currentEmulator.map(_.cardanoInfo) instead"
                )

            override def fetchLatestParams: Scenario[ProtocolParams] =
                leaf(s => Done(s, s.emulator.protocolParams))

            override def submit(
                transaction: Transaction
            ): Scenario[Either[SubmitError, TransactionHash]] =
                Scenario.submit(transaction)

            override def findUtxos(
                query: UtxoQuery
            ): Scenario[Either[UtxoQueryError, Utxos]] =
                leaf(s => Done(s, s.emulator.findUtxos(query)))

            override def currentSlot: Scenario[SlotNo] =
                Scenario.now

            override def checkTransaction(
                txHash: TransactionHash
            ): Scenario[TransactionStatus] =
                leaf { s =>
                    Done(
                      s,
                      s.emulator.findUtxos(
                        UtxoQuery(UtxoSource.FromTransaction(txHash))
                      ) match
                          case Right(_) => TransactionStatus.Confirmed
                          case Left(_)  => TransactionStatus.NotFound
                    )
                }

            override def pollForConfirmation(
                txHash: TransactionHash,
                maxAttempts: Int,
                delayMs: Long
            ): Scenario[TransactionStatus] =
                checkTransaction(txHash) // emulator is instant

            override def submitAndPoll(
                transaction: Transaction,
                maxAttempts: Int,
                delayMs: Long
            ): Scenario[Either[SubmitError, TransactionHash]] =
                Scenario.submit(transaction) // emulator is instant, no polling needed
        }
        scenarioLogicMonad.pure(p)
    }

    /** Get a read-only [[BlockchainReader]] snapshot from the current state. */
    def snapshotReader: Scenario[BlockchainReader] =
        leaf(s => Done(s, s.emulator.asReader))

    /** Get a read-only [[BlockchainProvider]] snapshot from the current state.
      *
      * @deprecated
      *   Use [[snapshotReader]] instead. The `submit` method on this provider discards state
      *   changes which is misleading.
      */
    @deprecated("Use snapshotReader instead", "0.14.2")
    def snapshotProvider: Scenario[BlockchainProvider] =
        leaf(s => Done(s, s.emulator.asProvider))

    /** Sample a value from a ScalaCheck generator using the scenario's RNG.
      *
      * The RNG state is advanced deterministically, ensuring reproducible execution across
      * branches. If the generator fails to produce a value, this branch is pruned (returns mzero).
      */
    def sample[A](gen: org.scalacheck.Gen[A]): Scenario[A] =
        leaf { state =>
            val params = org.scalacheck.Gen.Parameters.default.withInitialSeed(state.rng)
            gen(params, state.rng) match
                case Some(value) =>
                    val nextRng = state.rng.next
                    Done(state.copy(rng = nextRng), value)
                case None =>
                    scenarioLogicMonad.mzero
        }

    /** Sample N values from a generator, creating N branches.
      *
      * Each branch receives a different sampled value and its own RNG state. Failed samples are
      * filtered out.
      */
    def sampleN[A](gen: org.scalacheck.Gen[A], n: Int = 10): Scenario[A] =
        leaf { state =>
            val params = org.scalacheck.Gen.Parameters.default
            val (_, result) = (0 until n).foldLeft((state.rng, scenarioLogicMonad.mzero[A])) {
                case ((rng, acc), _) =>
                    val nextRng = rng.next
                    gen(params.withInitialSeed(rng), rng) match
                        case Some(value) =>
                            (
                              nextRng,
                              scenarioLogicMonad.mplus(acc, Done(state.copy(rng = nextRng), value))
                            )
                        case None =>
                            (nextRng, acc)
            }
            result
        }

    /** Check a condition, failing the scenario with [[CheckFailure]] if false.
      *
      * This is an inline macro that captures the predicate expression, optional message, and source
      * location at compile time.
      *
      * Usage:
      * {{{
      * async[Scenario] {
      *     Scenario.check(balance >= 0).await
      *     Scenario.check(owner != null, "owner must be set").await
      * }
      * }}}
      */
    inline def check(inline condition: Boolean, inline message: String = ""): Scenario[Unit] =
        ScenarioCheck.check(condition, message)

    /** Explore contract interactions up to a maximum depth.
      *
      * Convenience method that delegates to [[ScenarioExplorer.explore]].
      *
      * The step function receives a [[BlockchainReader]] and should perform one step of the
      * contract interaction using normal Scenario operations ([[submit]], [[sleep]], etc.). Actions
      * are automatically logged and included in [[Violation.path]] if a check fails.
      *
      * @param maxDepth
      *   maximum number of steps to explore
      * @param step
      *   function that performs one interaction step; use [[Scenario.check]] for invariants,
      *   [[Scenario.choices]]/[[Scenario.fromCollection]] for branching
      * @return
      *   Scenario returning None if all paths succeeded, Some(Violation) if a check failed
      */
    def explore(maxDepth: Int)(
        step: BlockchainReader => Scenario[Unit]
    ): Scenario[Option[Violation]] =
        ScenarioExplorer.explore(maxDepth)(step)

    // =========================================================================
    // Runners
    // =========================================================================

    /** Run a scenario from an Emulator, collecting all successful results (up to maxResults). */
    def runAll[A](
        emulator: Emulator,
        maxResults: Int = 1000,
        seed: org.scalacheck.rng.Seed = org.scalacheck.rng.Seed(0L)
    )(
        s: Scenario[A]
    ): Future[IndexedSeq[(ScenarioState, A)]] =
        continueAll(
          ScenarioState(ImmutableEmulator.fromEmulator(emulator), seed),
          maxResults
        )(s)

    /** Run a scenario from an Emulator, returning the first successful result. */
    def runFirst[A](
        emulator: Emulator,
        seed: org.scalacheck.rng.Seed = org.scalacheck.rng.Seed(0L)
    )(
        s: Scenario[A]
    ): Future[Option[(ScenarioState, A)]] =
        continueFirst(
          ScenarioState(ImmutableEmulator.fromEmulator(emulator), seed)
        )(s)

    /** Continue a scenario from an existing ScenarioState, returning the raw LogicStreamT. */
    private[testing] def continue[A](state: ScenarioState)(
        s: Scenario[A]
    ): LogicStreamT[Future, (ScenarioState, A)] =
        eval(injectState(state, s))

    /** Continue a scenario from an existing ScenarioState, collecting all successful results. */
    private[testing] def continueAll[A](state: ScenarioState, maxResults: Int = 1000)(
        s: Scenario[A]
    ): Future[IndexedSeq[(ScenarioState, A)]] =
        logicStreamMonad.mObserveN(eval(injectState(state, s)), maxResults)

    /** Continue a scenario from an existing ScenarioState, returning the first successful result.
      */
    private[testing] def continueFirst[A](state: ScenarioState)(
        s: Scenario[A]
    ): Future[Option[(ScenarioState, A)]] =
        logicStreamMonad.mObserveOne(eval(injectState(state, s)))

    // =========================================================================
    // CpsMonadConversion instances
    // =========================================================================

    /** Enables implicit conversion from Future to Scenario via WaitFuture.
      *
      * Allows writing:
      * {{{
      *   async[Scenario] {
      *     val slot = reader.currentSlot.await // Future[SlotNo] -> Scenario[SlotNo]
      *     ...
      *   }
      * }}}
      */
    given futureToScenarioConversion: CpsMonadConversion[Future, Scenario] with {
        def apply[A](fa: Future[A]): Scenario[A] =
            leaf(state =>
                WaitFuture(fa.transform {
                    case Success(a) => Success(Done(state, a))
                    case Failure(e) => Success(ScenarioError(state, e))
                }(ExecutionContext.parasitic))
            )
    }

    /** Enables implicit conversion from ScalaCheck Gen to Scenario.
      *
      * Allows writing:
      * {{{
      *   async[Scenario] {
      *     val x = Gen.choose(1, 10).await // implicitly converted via sample
      *     ...
      *   }
      * }}}
      */
    given genToScenarioConversion: CpsMonadConversion[org.scalacheck.Gen, Scenario] with {
        def apply[A](ga: org.scalacheck.Gen[A]): Scenario[A] = sample(ga)
    }
}
