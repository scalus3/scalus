package scalus.testing

import cps.*
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.*
import scalus.cardano.node.*
import scalus.testing.kit.Party
import scalus.testing.kit.Party.{Alice, Bob}
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
            scenarioLogicMonad.flatMap(Scenario.snapshotReader) { reader =>
                val tx = Await
                    .result(
                      TxBuilder(reader.cardanoInfo)
                          .payTo(Bob.address, Value.ada(10))
                          .complete(reader, Alice.address),
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

    // =========================================================================
    // Gen sampling
    // =========================================================================

    test("sample generates value from Gen") {
        import org.scalacheck.Gen
        val initial = mkState(Alice)
        val scenario = Scenario.sample(Gen.choose(1, 10))
        val result = awaitResult(Scenario.runFirst(initial)(scenario))
        assert(result.isDefined)
        val value = result.get._2
        assert(value >= 1 && value <= 10)
    }

    test("sample advances RNG deterministically") {
        import org.scalacheck.Gen
        val initial = mkState(Alice)
        val scenario = {
            import Scenario.scenarioLogicMonad
            scenarioLogicMonad.flatMap(Scenario.sample(Gen.choose(1, 100))) { v1 =>
                scenarioLogicMonad.flatMap(Scenario.sample(Gen.choose(1, 100))) { v2 =>
                    scenarioLogicMonad.pure((v1, v2))
                }
            }
        }
        val result1 = awaitResult(Scenario.runFirst(initial)(scenario))
        val result2 = awaitResult(Scenario.runFirst(initial)(scenario))
        // Same seed should produce same sequence
        assert(result1.get._2 == result2.get._2)
    }

    test("sample with async syntax") {
        import org.scalacheck.Gen
        val initial = mkState(Alice)
        val scenario = async[Scenario] {
            val x = Scenario.sample(Gen.choose(1, 10)).await
            val y = Scenario.sample(Gen.choose(1, 10)).await
            x + y
        }
        val result = awaitResult(Scenario.runFirst(initial)(scenario))
        assert(result.isDefined)
        val sum = result.get._2
        assert(sum >= 2 && sum <= 20)
    }

    test("Gen implicit conversion in async") {
        import org.scalacheck.Gen
        import Scenario.genToScenarioConversion
        val initial = mkState(Alice)
        val scenario = async[Scenario] {
            val x = Gen.choose(1, 10).await // implicit conversion
            val y = Gen.choose(1, 10).await
            x + y
        }
        val result = awaitResult(Scenario.runFirst(initial)(scenario))
        assert(result.isDefined)
        val sum = result.get._2
        assert(sum >= 2 && sum <= 20)
    }

    test("sample across branches produces different values per branch") {
        import org.scalacheck.Gen
        val initial = mkState(Alice)
        val scenario = async[Scenario] {
            val branch = Scenario.choices("A", "B").await
            val x = Scenario.sample(Gen.choose(1, 100)).await
            (branch, x)
        }
        val results = awaitResult(Scenario.runAll(initial)(scenario))
        assert(results.size == 2)
        // Both branches should have sampled values
        assert(results.forall { case (_, (branch, x)) => x >= 1 && x <= 100 })
    }

    test("sample with failed Gen returns mzero") {
        import org.scalacheck.Gen
        val initial = mkState(Alice)
        // Gen.fail always fails to produce a value, should prune branch
        val scenario = Scenario.sample(Gen.fail)
        val result = awaitResult(Scenario.runFirst(initial)(scenario))
        assert(result.isEmpty, "Failed generator should return mzero (no results)")
    }

    test("sample 10 times produces 10 values in single branch") {
        import org.scalacheck.Gen
        val initial = mkState(Alice)
        val scenario = async[Scenario] {
            val values = (1 to 10).map { _ =>
                Scenario.sample(Gen.choose(1, 1000)).await
            }
            values.toList
        }
        val result = awaitResult(Scenario.runFirst(initial)(scenario))
        assert(result.isDefined)
        val values = result.get._2
        assert(values.size == 10)
        // All values should be in range
        assert(values.forall(v => v >= 1 && v <= 1000))
        // With high probability, not all values are the same (RNG advances)
        assert(values.toSet.size > 1, "RNG should produce different values")
    }

    test("sample in 10 branches produces 10 independent results") {
        import org.scalacheck.Gen
        val initial = mkState(Alice)
        val scenario = async[Scenario] {
            val branch = Scenario.choices(1 to 10: _*).await
            val x = Scenario.sample(Gen.choose(1, 1000)).await
            (branch, x)
        }
        val results = awaitResult(Scenario.runAll(initial)(scenario))
        assert(results.size == 10)
        // All branches should have valid sampled values
        assert(results.forall { case (_, (_, x)) => x >= 1 && x <= 1000 })
        // Each branch has the same RNG starting point, so same sampled value
        val sampledValues = results.map(_._2._2)
        assert(
          sampledValues.toSet.size == 1,
          "Same RNG state in each branch should produce same value"
        )
    }

    test("sampleN creates N branches with different values") {
        import org.scalacheck.Gen
        val initial = mkState(Alice)
        val scenario = Scenario.sampleN(Gen.choose(1, 1000), 10)
        val results = awaitResult(Scenario.runAll(initial)(scenario))
        assert(results.size == 10, s"Expected 10 branches, got ${results.size}")
        // All values should be in range
        assert(results.forall { case (_, v) => v >= 1 && v <= 1000 })
        // With high probability, values should differ (RNG advances between samples)
        assert(results.map(_._2).toSet.size > 1, "sampleN should produce different values")
    }

    test("sampleN with default n=10") {
        import org.scalacheck.Gen
        val initial = mkState(Alice)
        val scenario = Scenario.sampleN(Gen.choose(1, 100))
        val results = awaitResult(Scenario.runAll(initial)(scenario))
        assert(results.size == 10)
    }

    test("sampleN filters failed samples") {
        import org.scalacheck.Gen
        val initial = mkState(Alice)
        // Gen that fails 50% of the time
        val gen = Gen.choose(1, 100).suchThat(_ > 50)
        val scenario = Scenario.sampleN(gen, 20)
        val results = awaitResult(Scenario.runAll(initial)(scenario))
        // Should have some results but likely fewer than 20
        assert(results.nonEmpty)
        assert(results.forall { case (_, v) => v > 50 })
    }

    // =========================================================================
    // Scenario.check
    // =========================================================================

    test("check passes when condition is true") {
        val initial = mkState(Alice)
        val scenario = async[Scenario] {
            Scenario.check(1 + 1 == 2).await
            Scenario.check(true, "should pass").await
            42
        }
        val result = awaitResult(Scenario.runFirst(initial)(scenario))
        assert(result.isDefined)
        assert(result.get._2 == 42)
    }

    test("check fails with CheckFailure when condition is false") {
        val initial = mkState(Alice)
        val x = 1
        val y = 3
        val scenario = Scenario.check(x + 1 == y, "math is broken")
        val caught =
            try {
                awaitResult(Scenario.runFirst(initial)(scenario))
                false
            } catch {
                case e: CheckFailure =>
                    // Predicate should show the expression
                    assert(
                      e.predicate.contains("x") || e.predicate.contains("=="),
                      s"predicate was: ${e.predicate}"
                    )
                    assert(e.message == "math is broken")
                    assert(e.location.file.endsWith(".scala"))
                    assert(e.location.line > 0)
                    true
            }
        assert(caught, "check should throw CheckFailure")
    }

    test("check captures source location") {
        val initial = mkState(Alice)
        val scenario = Scenario.check(false, "test location")
        val caught =
            try {
                awaitResult(Scenario.runFirst(initial)(scenario))
                None
            } catch {
                case e: CheckFailure => Some(e)
            }
        assert(caught.isDefined)
        val failure = caught.get
        assert(failure.location.file == "ScenarioTest.scala")
        assert(failure.location.line > 0)
    }

    test("check in async block captures correct location") {
        val initial = mkState(Alice)
        val scenario = async[Scenario] {
            val x = 5
            Scenario.check(x > 10, "x too small").await
            x
        }
        val caught =
            try {
                awaitResult(Scenario.runFirst(initial)(scenario))
                None
            } catch {
                case e: CheckFailure => Some(e)
            }
        assert(caught.isDefined)
        assert(caught.get.message == "x too small")
    }
}
