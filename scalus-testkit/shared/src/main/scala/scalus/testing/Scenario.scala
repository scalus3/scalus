package scalus.testing

import cps.*
import cps.monads.{FutureAsyncMonad, given}
import cps.monads.logic.*
import scalus.cardano.ledger.*
import scalus.cardano.node.*

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

/** State threaded through Scenario computations. */
case class ScenarioState(
    emulator: ImmutableEmulator,
    rng: org.scalacheck.rng.Seed
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
            case Done(s, a)          => LogicStreamT.Pure((s, a))
            case Leaf(s, run)        => eval(run(s))
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
            case Done(s, a)          => Done(s, f(a))
            case Leaf(s, run)        => Leaf(s, state => map(run(state))(f))
            case Branches(streams)   => Branches(streams.map(branch => map(branch)(f)))
            case WaitFuture(fut)     => WaitFuture(fut.map(sc => map(sc)(f)))
            case ScenarioError(s, e) => ScenarioError(s, e)
            case FromStream(stream)  => FromStream(stream.map { case (s, a) => (s, f(a)) })

        override def flatMap[A, B](fa: Scenario[A])(f: A => Scenario[B]): Scenario[B] = fa match
            case Done(s, a)   => injectState(s, f(a))
            case Leaf(s, run) => Leaf(s, state => flatMap(run(state))(f))
            case Branches(streams) =>
                Branches(streams.map(branch => flatMap(branch)(f)))
            case WaitFuture(fut)     => WaitFuture(fut.map(sc => flatMap(sc)(f)))
            case ScenarioError(s, e) => ScenarioError(s, e)
            case FromStream(stream) =>
                FromStream(stream.flatMap { case (s, a) => eval(injectState(s, f(a))) })

        override def flatMapTry[A, B](fa: Scenario[A])(f: Try[A] => Scenario[B]): Scenario[B] =
            fa match
                case Done(s, a)          => injectState(s, f(Success(a)))
                case ScenarioError(s, e) => injectState(s, f(Failure(e)))
                case Leaf(s, run)        => Leaf(s, state => flatMapTry(run(state))(f))
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

    /** Get the current slot number. */
    def now: Scenario[SlotNo] =
        leaf(state => Done(state, state.emulator.currentSlot))

    /** Advance the slot by the given number of slots. */
    def sleep(slots: Long): Scenario[Unit] =
        modify(s => s.copy(emulator = s.emulator.advanceSlot(slots)))

    /** Get the current emulator state (read-only snapshot). */
    def currentEmulator: Scenario[ImmutableEmulator] =
        leaf(state => Done(state, state.emulator))

    /** Submit a transaction, updating the emulator state if successful. */
    def submit(tx: Transaction): Scenario[Either[SubmitError, TransactionHash]] =
        modifyAndReturn { s =>
            s.emulator.submit(tx) match {
                case Right((txHash, newEmulator)) =>
                    (s.copy(emulator = newEmulator), Right(txHash))
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
        }
        scenarioLogicMonad.pure(p)
    }

    /** Get a read-only [[BlockchainProvider]] snapshot from the current state. */
    def snapshotProvider: Scenario[BlockchainProvider] =
        leaf(s => Done(s, s.emulator.asProvider))

    // =========================================================================
    // Runners
    // =========================================================================

    /** Evaluate and run a scenario, returning the raw LogicStreamT. */
    def run[A](initial: ScenarioState)(
        s: Scenario[A]
    ): LogicStreamT[Future, (ScenarioState, A)] =
        eval(injectState(initial, s))

    /** Run a scenario and collect all successful results (up to n, default 1000). */
    def runAll[A](initial: ScenarioState, maxResults: Int = 1000)(
        s: Scenario[A]
    ): Future[IndexedSeq[(ScenarioState, A)]] =
        logicStreamMonad.mObserveN(eval(injectState(initial, s)), maxResults)

    /** Run a scenario and return the first successful result. */
    def runFirst[A](initial: ScenarioState)(
        s: Scenario[A]
    ): Future[Option[(ScenarioState, A)]] =
        logicStreamMonad.mObserveOne(eval(injectState(initial, s)))
}
