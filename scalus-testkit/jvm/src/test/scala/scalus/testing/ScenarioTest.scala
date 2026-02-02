package scalus.testing

import cps.*
import cps.monads.logic.*
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.Address
import scalus.cardano.ledger.*
import scalus.cardano.node.*
import scalus.testing.kit.Party
import scalus.testing.kit.Party.{Alice, Bob, Charles}
import scalus.cardano.txbuilder.TxBuilder

import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration

class ScenarioTest extends AnyFunSuite {

    private given CardanoInfo = CardanoInfo.mainnet

    private def mkState(parties: Party*): ScenarioState = {
        val addresses = parties.map(_.address).toSeq
        val emulator = ImmutableEmulator.withAddresses(addresses)
        ScenarioState(emulator, org.scalacheck.rng.Seed(42L))
    }

    private def awaitResult[A](f: Future[A]): A =
        Await.result(f, Duration.Inf)

    // =========================================================================
    // Basic monad operations
    // =========================================================================

    test("pure returns value with state") {
        val initial = mkState(Alice)
        val scenario = Scenario.scenarioLogicMonad.pure(42)
        val result = awaitResult(Scenario.runFirst(initial)(scenario))
        assert(result.isDefined)
        assert(result.get._2 == 42)
    }

    test("map transforms value") {
        val initial = mkState(Alice)
        val scenario = Scenario.scenarioLogicMonad.map(
          Scenario.scenarioLogicMonad.pure(21)
        )(_ * 2)
        val result = awaitResult(Scenario.runFirst(initial)(scenario))
        assert(result.get._2 == 42)
    }

    test("flatMap threads state") {
        val initial = mkState(Alice)
        val scenario = Scenario.scenarioLogicMonad.flatMap(
          Scenario.sleep(5)
        )(_ => Scenario.now)
        val result = awaitResult(Scenario.runFirst(initial)(scenario))
        assert(result.isDefined)
        assert(result.get._2 == initial.emulator.currentSlot + 5)
    }

    // =========================================================================
    // Non-deterministic branching
    // =========================================================================

    test("choices produces all alternatives") {
        val initial = mkState(Alice)
        val scenario = Scenario.choices(1, 2, 3)
        val results = awaitResult(Scenario.runAll(initial)(scenario))
        assert(results.map(_._2).toSet == Set(1, 2, 3))
    }

    test("fromCollection with empty produces no results") {
        val initial = mkState(Alice)
        val scenario = Scenario.fromCollection(Seq.empty[Int])
        val results = awaitResult(Scenario.runAll(initial)(scenario))
        assert(results.isEmpty)
    }

    test("guard prunes branch when false") {
        import Scenario.scenarioLogicMonad
        val initial = mkState(Alice)
        val scenario = scenarioLogicMonad.flatMap(Scenario.choices(1, 2, 3, 4, 5)) { n =>
            scenarioLogicMonad.flatMap(Scenario.guard(n % 2 == 0))(_ => scenarioLogicMonad.pure(n))
        }
        val results = awaitResult(Scenario.runAll(initial)(scenario))
        assert(results.map(_._2).toSet == Set(2, 4))
    }

    test("mzero produces no results") {
        val initial = mkState(Alice)
        val scenario = Scenario.fail[Int]
        val results = awaitResult(Scenario.runAll(initial)(scenario))
        assert(results.isEmpty)
    }

    // =========================================================================
    // State operations
    // =========================================================================

    test("sleep advances slot") {
        val initial = mkState(Alice)
        val scenario = Scenario.scenarioLogicMonad.flatMap(Scenario.sleep(10))(_ => Scenario.now)
        val result = awaitResult(Scenario.runFirst(initial)(scenario))
        assert(result.get._2 == initial.emulator.currentSlot + 10)
    }

    test("sleep per branch is independent") {
        import Scenario.scenarioLogicMonad
        val initial = mkState(Alice)
        val scenario = scenarioLogicMonad.flatMap(Scenario.choices(5L, 10L, 20L)) { slots =>
            scenarioLogicMonad.flatMap(Scenario.sleep(slots))(_ => Scenario.now)
        }
        val results = awaitResult(Scenario.runAll(initial)(scenario))
        val slots = results.map(_._2).toSet
        val base = initial.emulator.currentSlot
        assert(slots == Set(base + 5, base + 10, base + 20))
    }

    test("currentEmulator returns emulator snapshot") {
        val initial = mkState(Alice)
        val scenario = Scenario.currentEmulator
        val result = awaitResult(Scenario.runFirst(initial)(scenario))
        assert(result.get._2.utxos == initial.emulator.utxos)
    }

    // =========================================================================
    // mplus of independent branches
    // =========================================================================

    test("mplus combines independent branches") {
        import Scenario.scenarioLogicMonad
        val stateA = mkState(Alice)
        val stateB = mkState(Bob)

        // Create two independently-stated scenarios via Leaf with their own states
        val branchA: Scenario[String] = Scenario.Leaf(stateA, s => Scenario.Done(s, "alice"))
        val branchB: Scenario[String] = Scenario.Leaf(stateB, s => Scenario.Done(s, "bob"))

        val combined = scenarioLogicMonad.mplus(branchA, branchB)
        // Use ScenarioState.empty since branches have their own states
        val results = awaitResult(
          Scenario.runAll(ScenarioState.empty)(combined)
        )
        assert(results.map(_._2).toSet == Set("alice", "bob"))
    }

    // =========================================================================
    // fsplit
    // =========================================================================

    test("fsplit returns first result and tail") {
        import Scenario.scenarioLogicMonad
        val initial = mkState(Alice)
        // Bind choices to the initial state via Leaf
        val scenario: Scenario[Int] = Scenario.Leaf(
          initial,
          s => {
              val c1: Scenario[Int] = Scenario.Done(s, 1)
              val c2: Scenario[Int] = Scenario.Done(s, 2)
              val c3: Scenario[Int] = Scenario.Done(s, 3)
              scenarioLogicMonad.mplus(c1, scenarioLogicMonad.mplus(c2, c3))
          }
        )

        val splitResult = awaitResult(scenarioLogicMonad.fsplit(scenario))
        assert(splitResult.isDefined)
        val (tryHead, tail) = splitResult.get
        assert(tryHead.isSuccess)
        assert(tryHead.get == 1)

        // tail should have remaining elements
        val restResults = awaitResult(Scenario.runAll(initial)(tail))
        assert(restResults.map(_._2).toSet == Set(2, 3))
    }

    test("fsplit on empty returns None") {
        import Scenario.scenarioLogicMonad
        val splitResult = awaitResult(scenarioLogicMonad.fsplit(Scenario.fail[Int]))
        assert(splitResult.isEmpty)
    }

    // =========================================================================
    // msplit (within Scenario monad)
    // =========================================================================

    test("msplit returns first result inside Scenario") {
        import Scenario.scenarioLogicMonad
        val initial = mkState(Alice)
        val scenario = scenarioLogicMonad.flatMap(
          scenarioLogicMonad.msplit(Scenario.choices(10, 20, 30))
        ) {
            case Some((tryVal, _)) => scenarioLogicMonad.pure(tryVal.get)
            case None              => scenarioLogicMonad.pure(-1)
        }
        val result = awaitResult(Scenario.runFirst(initial)(scenario))
        assert(result.get._2 == 10)
    }

    // =========================================================================
    // flatMapTry / error handling
    // =========================================================================

    test("error produces error in stream") {
        val initial = mkState(Alice)
        val scenario = Scenario.error[Int](new RuntimeException("boom"))
        val caught =
            try {
                awaitResult(Scenario.runFirst(initial)(scenario))
                false
            } catch {
                case e: RuntimeException if e.getMessage == "boom" => true
            }
        assert(caught, "error should propagate as exception")
    }

    test("flatMapTry catches error") {
        import Scenario.scenarioLogicMonad
        val initial = mkState(Alice)
        val scenario = scenarioLogicMonad.flatMapTry(
          Scenario.error[Int](new RuntimeException("boom"))
        ) {
            case scala.util.Success(n) => scenarioLogicMonad.pure(n)
            case scala.util.Failure(e) => scenarioLogicMonad.pure(-1)
        }
        val result = awaitResult(Scenario.runFirst(initial)(scenario))
        assert(result.get._2 == -1)
    }

    // =========================================================================
    // Runners
    // =========================================================================

    test("runAll collects all branches") {
        val initial = mkState(Alice)
        val scenario = Scenario.choices("a", "b", "c")
        val results = awaitResult(Scenario.runAll(initial)(scenario))
        assert(results.size == 3)
        assert(results.map(_._2).toSet == Set("a", "b", "c"))
    }

    test("runAll respects maxResults") {
        val initial = mkState(Alice)
        val scenario = Scenario.choices(1, 2, 3, 4, 5)
        val results = awaitResult(Scenario.runAll(initial, maxResults = 2)(scenario))
        assert(results.size == 2)
    }

    test("runFirst returns first result") {
        val initial = mkState(Alice)
        val scenario = Scenario.choices("first", "second")
        val result = awaitResult(Scenario.runFirst(initial)(scenario))
        assert(result.isDefined)
        assert(result.get._2 == "first")
    }

    test("runFirst on empty returns None") {
        val initial = mkState(Alice)
        val result = awaitResult(Scenario.runFirst(initial)(Scenario.fail[Int]))
        assert(result.isEmpty)
    }

    // =========================================================================
    // ImmutableEmulator
    // =========================================================================

    test("ImmutableEmulator.empty has no utxos") {
        val emu = ImmutableEmulator.empty
        assert(emu.utxos.isEmpty)
    }

    test("ImmutableEmulator.withAddresses funds addresses") {
        val emu = ImmutableEmulator.withAddresses(
          Seq(Alice.address, Bob.address)
        )
        assert(emu.utxos.size == 2)
        assert(emu.utxos.values.forall(_.value == Value.ada(10_000L)))
    }

    test("ImmutableEmulator.advanceSlot returns new instance") {
        val emu = ImmutableEmulator.empty
        val advanced = emu.advanceSlot(100)
        assert(advanced.currentSlot == emu.currentSlot + 100)
        assert(emu.currentSlot == 0) // original unchanged
    }

    test("ImmutableEmulator.fromEmulator captures snapshot") {
        val mutable = Emulator(
          initialUtxos = EmulatorBase.createInitialUtxos(Seq(Alice.address)),
          validators = Set.empty,
          mutators = Emulator.defaultMutators
        )
        val immutable = ImmutableEmulator.fromEmulator(mutable)
        assert(immutable.utxos == mutable.utxos)
    }

    test("ImmutableEmulator.toEmulator round-trips") {
        val original = ImmutableEmulator.withAddresses(Seq(Alice.address))
        val mutable = original.toEmulator
        assert(mutable.utxos == original.utxos)
    }

    // =========================================================================
    // Integration: Scenario + TxBuilder
    // =========================================================================

    test("submit valid transaction updates state") {
        val initial = mkState(Alice, Bob)
        val scenario = {
            import Scenario.scenarioLogicMonad
            scenarioLogicMonad.flatMap(Scenario.snapshotProvider) { provider =>
                val tx = Await
                    .result(
                      TxBuilder(provider.cardanoInfo)
                          .payTo(Bob.address, Value.ada(10))
                          .complete(provider, Alice.address),
                      Duration.Inf
                    )
                    .sign(Alice.signer)
                    .transaction
                scenarioLogicMonad.flatMap(Scenario.submit(tx)) { result =>
                    scenarioLogicMonad.pure(result)
                }
            }
        }
        val result = awaitResult(Scenario.runFirst(initial)(scenario))
        assert(result.isDefined)
        assert(result.get._2.isRight, s"Submit failed: ${result.get._2}")
    }
}
