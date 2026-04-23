package scalus.benchmarks

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.compiler.{compile, Options}
import scalus.cardano.onchain.plutus.prelude.*
import scalus.compiler.{UplcRepr, UplcRepresentation}
import scalus.testing.kit.ScalusTest
import scalus.uplc.builtin.Data
import scalus.uplc.{Constant, Term}
import scalus.uplc.eval.{PlutusVM, Result}
import scalus.cardano.ledger.MajorProtocolVersion
import scalus.compiler.sir.{SIR, TargetLoweringBackend}

class KnightsTestMinimal extends AnyFunSuite, ScalusTest:
    import KnightsTest.{*, given}

    override protected def plutusVM: PlutusVM =
        PlutusVM.makePlutusV3VM(MajorProtocolVersion.vanRossemPV)

    given Options = Options(
      targetLoweringBackend = TargetLoweringBackend.SirToUplcV3Lowering,
      targetProtocolVersion = MajorProtocolVersion.vanRossemPV,
      generateErrorTraces = true,
      optimizeUplc = true,
      debug = false,
      warnListConversions = true
    )

    /** Evaluate `sir` and assert the script succeeds with `value` as a BigInt. Accepts both the
      * native [[Constant.Integer]] form and the Data-wrapped [[Data.I]] form, because some
      * lowering paths (e.g. a `.x` access on a `TypeVar(Fixed)`-returning extension method)
      * route the result through Data-based extraction and leave the final `unIData` unwrap to
      * a later alignment step that isn't always emitted.
      */
    private def assertBigIntResult(sir: SIR, expected: BigInt): Unit =
        val result = sir.toUplc(optimizeUplc = false).evaluateDebug
        result match
            case Result.Success(Term.Const(Constant.Integer(v), _), _, _, _) =>
                assert(v == expected, s"Expected $expected, got $v")
            case Result.Success(Term.Const(Constant.Data(Data.I(v)), _), _, _, _) =>
                assert(v == expected, s"Expected Data.I($expected), got Data.I($v)")
            case Result.Failure(ex, _, _, _) => fail(s"Runtime failure: $ex")
            case other                       => fail(s"Unexpected: $other")

    test("filterMap over map-ed List[SolutionEntry] counts matches on depth field") {
        val sir = compile {
            val board = startTour(Tile(1, 1), BigInt(4))
            @UplcRepr(UplcRepresentation.UplcConstr)
            val baseList: List[ChessSet] = List.Cons(board, List.Nil)
            val entries = baseList.map { item => SolutionEntry(BigInt(1), item) }
            val filtered = entries.filterMap { item =>
                if item.depth === BigInt(1) then Option.Some(item.board) else Option.None
            }
            filtered.length
        }
        assertBigIntResult(sir, BigInt(1))
    }

    test("map lambda accesses Data-repr Option field via extension method (firstPiece.x)") {
        // Reaches `.x` on a Tile whose enclosing Option[Tile] is Data-encoded (ChessSet's
        // `start` field has no @UplcRepr). Without type-arg substitution in the
        // PairIntDataList → SumUplcConstr conversion, the Tile field was labeled native but
        // its bytes stayed Data — causing `iData` to run on Tile's fields-list at runtime.
        val sir = compile {
            val board = startTour(Tile(1, 1), BigInt(4))
            @UplcRepr(UplcRepresentation.UplcConstr)
            val baseList: List[ChessSet] = List.Cons(board, List.Nil)
            val entries: List[BigInt] = baseList.map { item => item.firstPiece.x }
            entries.head
        }
        assertBigIntResult(sir, BigInt(1))
    }

    test("map lambda accesses Data-repr Option field via direct .get (start.get.x)") {
        // Silent-corruption sibling of the firstPiece.x test. Same lying-relabel root cause,
        // but direct field access surfaced the bug as a wrong result (0) rather than a crash.
        val sir = compile {
            val board = startTour(Tile(1, 1), BigInt(4))
            @UplcRepr(UplcRepresentation.UplcConstr)
            val baseList: List[ChessSet] = List.Cons(board, List.Nil)
            val entries: List[BigInt] = baseList.map { item => item.start.get.x }
            entries.head
        }
        assertBigIntResult(sir, BigInt(1))
    }

    test("SolutionEntry built from possibleMoves.length destructures back to depth") {
        val sir = compile {
            val board = startTour(Tile(1, 1), BigInt(4))
            val entry = SolutionEntry(board.possibleMoves.length, board)
            entry.depth
        }
        assertBigIntResult(sir, BigInt(2))
    }

    test("filterMap after map over Fixed-repr depth field") {
        // Exercises the Fixed → Transparent field-repr path: depth comes from
        // `.length` (Data-encoded BigInt, Fixed repr); the constructor records it as Fixed;
        // `filterMap`'s genSelect on a Transparent-TypeVar scrutinee must not resolve to
        // the default [Constant, ...] repr or EqualsInteger would operate on a Data integer.
        val sir = compile {
            val board = startTour(Tile(1, 1), BigInt(4))
            @UplcRepr(UplcRepresentation.UplcConstr)
            val baseList: List[ChessSet] = List.Cons(board, List.Nil)
            val entries = baseList.map { item =>
                SolutionEntry(item.possibleMoves.length, item)
            }
            val filtered = entries.filterMap { item =>
                if item.depth === BigInt(0) then Option.Some(item.board) else Option.None
            }
            filtered.length
        }
        assertBigIntResult(sir, BigInt(0))
    }

    test("map over single-element List[BigInt] with pure lambda body") {
        val sir = compile {
            val xs: List[BigInt] = List.Cons(BigInt(10), List.Nil)
            val ys = xs.map { x => x + BigInt(1) }
            ys.head
        }
        assertBigIntResult(sir, BigInt(11))
    }

    test("head on UplcConstr List[SolutionEntry] then field access") {
        // Regression for `genMatchUplcConstrCase` branch-repr alignment. Before
        // `BranchLambdaWrapper`, aligning the Cons branch's repr wrapped the whole
        // `λh.λt. h` lambda in `Case(…)`, producing `case (\h.\t.h) […]` at runtime.
        val sir = compile {
            val board = startTour(Tile(1, 1), BigInt(4))
            val entry = SolutionEntry(BigInt(0), board)
            @UplcRepr(UplcRepresentation.UplcConstr)
            val queue: List[SolutionEntry] = List.Cons(entry, List.Nil)
            List.head(queue).depth
        }
        assertBigIntResult(sir, BigInt(0))
    }

    test("++ then head.board.isTourFinished — direct call passes after interpretation (a)") {
        val sir = compile {
            val board = startTour(Tile(1, 1), BigInt(4))
            val entry1 = SolutionEntry(BigInt(0), board)
            @UplcRepr(UplcRepresentation.UplcConstr)
            val queue: List[SolutionEntry] = List.Cons(entry1, List.Nil)
            @UplcRepr(UplcRepresentation.UplcConstr)
            val combined = queue ++ queue
            val h = List.head(combined)
            if h.board.isTourFinished then BigInt(1) else BigInt(0)
        }
        assertBigIntResult(sir, BigInt(0))
    }

    test("isDone(queue.head) via function-parameter call — Knights 4x4 reproducer step 1") {
        // Closer to Knights's `depthSearch`: the predicate is passed as a function parameter,
        // not called directly. This adds an indirection layer that may trigger the
        // MultiplyInteger-on-LamAbs mismatch.
        val sir = compile {
            def step(
                @UplcRepr(UplcRepresentation.UplcConstr) q: List[SolutionEntry],
                done: SolutionEntry => Boolean
            ): BigInt =
                if done(List.head(q)) then BigInt(1) else BigInt(0)
            val board = startTour(Tile(1, 1), BigInt(4))
            val entry1 = SolutionEntry(BigInt(0), board)
            @UplcRepr(UplcRepresentation.UplcConstr)
            val queue: List[SolutionEntry] = List.Cons(entry1, List.Nil)
            @UplcRepr(UplcRepresentation.UplcConstr)
            val combined = queue ++ queue
            step(combined, isDone)
        }
        assertBigIntResult(sir, BigInt(0))
    }

    test("runKnights depth=1 size=4 — smoke test, should succeed") {
        val sir = compile { runKnights(BigInt(1), BigInt(4)).length }
        sir.toUplc(optimizeUplc = false).evaluateDebug match
            case Result.Success(_, _, _, _) => // pass
            case other                      => fail(s"Unexpected: $other")
    }

    test("runKnights depth=2 size=4 — find the failing depth") {
        val sir = compile { runKnights(BigInt(2), BigInt(4)).length }
        sir.toUplc(optimizeUplc = false).evaluateDebug match
            case Result.Success(_, _, _, _) => // pass
            case other                      => fail(s"Unexpected: $other")
    }

    test("runKnights depth=5 size=4 — find the failing depth") {
        val sir = compile { runKnights(BigInt(5), BigInt(4)).length }
        sir.toUplc(optimizeUplc = false).evaluateDebug match
            case Result.Success(_, _, _, _) => // pass
            case other                      => fail(s"Unexpected: $other")
    }

    test("runKnights depth=10 size=4 — find the failing depth") {
        val sir = compile { runKnights(BigInt(10), BigInt(4)).length }
        sir.toUplc(optimizeUplc = false).evaluateDebug match
            case Result.Success(_, _, _, _) => // pass
            case other                      => fail(s"Unexpected: $other")
    }


    test("runKnights depth=20 size=4") {
        val sir = compile { runKnights(BigInt(20), BigInt(4)).length }
        sir.toUplc(optimizeUplc = false).evaluateDebug match
            case Result.Success(_, _, _, _) => // pass
            case other                      => fail(s"Unexpected: $other")
    }

    test("runKnights depth=50 size=4") {
        val sir = compile { runKnights(BigInt(50), BigInt(4)).length }
        sir.toUplc(optimizeUplc = false).evaluateDebug match
            case Result.Success(_, _, _, _) => // pass
            case other                      => fail(s"Unexpected: $other")
    }

    test("runKnights depth=100 size=4 — same as KnightsTest.100_4x4 (no eq check)") {
        val sir = compile { runKnights(BigInt(100), BigInt(4)).length }
        sir.toUplc(optimizeUplc = false).evaluateDebug match
            case Result.Success(_, _, _, _) => // pass
            case other                      => fail(s"Unexpected: $other")
    }

    test("runKnights depth=1 size=4 === List.empty — exact shape of KnightsTest.100_4x4") {
        // KnightsTest.100_4x4 does `require(runKnights(100,4) === expected)`. The Eq
        // comparison recurses into SolutionEntry → ChessSet → Option[Tile]. That's the
        // path where the repr mismatch bites. Smaller depth to isolate.
        val sir = compile {
            val result = runKnights(BigInt(1), BigInt(4))
            val expected: Solution = List.empty
            if result === expected then BigInt(1) else BigInt(0)
        }
        sir.toUplc(optimizeUplc = false).evaluateDebug match
            case Result.Success(_, _, _, _) => // either 0 or 1 is fine; just no crash
            case other                      => fail(s"Unexpected: $other")
    }

    test("empty === empty on Solution — isolated Eq check") {
        val sir = compile {
            val a: Solution = List.empty
            val b: Solution = List.empty
            if a === b then BigInt(1) else BigInt(0)
        }
        assertBigIntResult(sir, BigInt(1))
    }

    test("runKnights depth=100 size=4 === List.empty — full shape of KnightsTest.100_4x4") {
        val sir = compile {
            val result = runKnights(BigInt(100), BigInt(4))
            val expected: Solution = List.empty
            if result === expected then BigInt(1) else BigInt(0)
        }
        sir.toUplc(optimizeUplc = false).evaluateDebug match
            case Result.Success(_, _, _, _) => // no crash
            case other                      => fail(s"Unexpected: $other")
    }

    test("verbatim KnightsTest.100_4x4 copy with evaluateProfile") {
        // Copy of KnightsTest.100_4x4 using the SAME evaluator as the failing test
        // (evaluateProfile via evalWithOptionalProfile with profilingEnabled=true).
        // NOW WITH IDENTICAL COMPILATION: optimizeUplc = false
        val sir = compile {
            val result = runKnights(100, 4)
            val expected: Solution = List.empty
            require(result === expected)
        }
        val uplc = sir.toUplc(optimizeUplc = false)
        val result = uplc.evaluateProfile
        assert(result.isSuccess, s"Runtime failure: $result")
    }

    test("depthSearch-shaped recursion calling appendAllFront — Knights 4x4 reproducer step 2") {
        // Mimics Knights's `depthSearch(depth, queue, grow, done)` pattern:
        // recursive function with function-valued parameters that calls
        // `queue.removeFront.appendAllFront(grow(queue.head))` then recurses. The arity
        // mismatch at `self.size` (MultiplyInteger on 2-lambda residual) only shows up
        // in the full Knights flow — this test tries to trigger it minimally.
        val sir = compile {
            def rec(
                depth: BigInt,
                @UplcRepr(UplcRepresentation.UplcConstr) queue: List[SolutionEntry],
                @UplcRepr(UplcRepresentation.UplcConstr) grow: SolutionEntry => List[SolutionEntry],
                done: SolutionEntry => Boolean
            ): BigInt =
                if depth === BigInt(0) || queue.isEmpty then BigInt(0)
                else if done(List.head(queue)) then BigInt(1)
                else
                    rec(
                      depth - 1,
                      List.tail(queue) ++ grow(List.head(queue)),
                      grow,
                      done
                    )
            val board = startTour(Tile(1, 1), BigInt(4))
            val entry1 = SolutionEntry(BigInt(0), board)
            @UplcRepr(UplcRepresentation.UplcConstr)
            val queue: List[SolutionEntry] = List.Cons(entry1, List.Nil)
            rec(BigInt(2), queue, grow, isDone)
        }
        assertBigIntResult(sir, BigInt(0))
    }

    test("++ then head.depth on List[SolutionEntry] — Knights root cause (UnConstrData on native Constr)") {
        // Minimal reproducer for the remaining KnightsTest 100_4x4/6x6/8x8 failure:
        // `++` on `@UplcRepr(UplcConstr) List[SolutionEntry]` leaves the nested Option
        // field in native form, so the later `Case → unConstrData → ...` conversion trips
        // with "Deserialization error in UnConstrData" — it expected Data-shape bytes.
        // Suspect: `sumUplcConstrToSumUplcConstr` doesn't recursively align nested field
        // reprs when the list element type has non-primitive fields.
        val sir = compile {
            val board = startTour(Tile(1, 1), BigInt(4))
            val entry1 = SolutionEntry(BigInt(0), board)
            val entry2 = SolutionEntry(BigInt(1), board)
            @UplcRepr(UplcRepresentation.UplcConstr)
            val queue: List[SolutionEntry] = List.Cons(entry1, List.Nil)
            @UplcRepr(UplcRepresentation.UplcConstr)
            val extras: List[SolutionEntry] = List.Cons(entry2, List.Nil)
            @UplcRepr(UplcRepresentation.UplcConstr)
            val combined = extras ++ queue
            List.head(combined).depth
        }
        assertBigIntResult(sir, BigInt(1))
    }

end KnightsTestMinimal
