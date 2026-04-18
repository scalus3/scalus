package scalus.benchmarks

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.compiler.{compile, Options}
import scalus.cardano.onchain.plutus.prelude.*
import scalus.compiler.{UplcRepr, UplcRepresentation}
import scalus.uplc.{Constant, Term}
import scalus.uplc.eval.{PlutusVM, Result}
import scalus.cardano.ledger.MajorProtocolVersion
import scalus.compiler.sir.lowering.LoweredValue
import scalus.compiler.sir.TargetLoweringBackend

class KnightsTestMinimal extends AnyFunSuite:
    import KnightsTest.{*, given}

    private given PlutusVM = PlutusVM.makePlutusV3VM(MajorProtocolVersion.vanRossemPV)
    private given Options = Options(
      targetLoweringBackend = TargetLoweringBackend.SirToUplcV3Lowering,
      targetProtocolVersion = MajorProtocolVersion.vanRossemPV,
      generateErrorTraces = true,
      optimizeUplc = true,
      debug = false
    )

    ignore("minimal - SolutionEntry field access in filterMap") {
        val sir = compile {
            // Minimal reproduction of the bug
            val board = startTour(Tile(1, 1), BigInt(4))
            @UplcRepr(UplcRepresentation.UplcConstr)
            val baseList: List[ChessSet] = List.Cons(board, List.Nil)
            // map creates List[SolutionEntry] from List[ChessSet] (both UplcConstr)
            val entries = baseList.map { item => SolutionEntry(BigInt(1), item) }
            // filterMap accesses depth field - this should fail with the bug
            val filtered = entries.filterMap { item =>
                if item.depth === BigInt(1) then Option.Some(item.board) else Option.None
            }
            filtered.length
        }
        val result = sir.toUplc().evaluateDebug
        result match
            case Result.Success(Term.Const(Constant.Integer(v), _), _, _, _) =>
                assert(v == 1, s"Expected 1, got $v")
            case Result.Failure(ex, _, _, _) =>
                fail(s"Bug reproduced: $ex")
            case other => fail(s"Unexpected: $other")
    }

    ignore("minimal - SolutionEntry with .length field") {
        val sir = compile {
            // Bug: depth comes from .length (returns TypeVarRepresentation)
            val board = startTour(Tile(1, 1), BigInt(4))
            @UplcRepr(UplcRepresentation.UplcConstr)
            val baseList: List[ChessSet] = List.Cons(board, List.Nil)
            // Use .length to get depth - this might have TypeVarRepresentation
            val entries = baseList.map { item =>
                SolutionEntry(item.deleteFirst.possibleMoves.length, item)
            }
            val filtered = entries.filterMap { item =>
                if item.depth === BigInt(1) then Option.Some(item.board) else Option.None
            }
            filtered.length
        }

        // Print the lowered SIR value to a file for offline inspection
        val lowered: LoweredValue = sir.toLoweredValue(using summon[Options])()
        val pw = new java.io.PrintWriter("/tmp/knights_min_failing.txt")
        try {
            pw.println("=== lowered.representation ===")
            pw.println(lowered.representation)
            pw.println("=== lowered.show ===")
            pw.println(lowered.show)
        } finally pw.close()

        val result = sir.toUplc().evaluateDebug
        result match
            case Result.Success(Term.Const(Constant.Integer(v), _), _, _, _) =>
                info(s"Result: $v")
                assert(v >= 0, s"Expected non-negative, got $v")
            case Result.Failure(ex, _, _, _) =>
                val pw2 = new java.io.PrintWriter("/tmp/knights_min_failing_err.txt")
                try pw2.println(s"Bug with .length: $ex")
                finally pw2.close()
                fail(s"Bug with .length: $ex")
            case other => fail(s"Unexpected: $other")
    }

    ignore("micro - SolutionEntry with .length directly") {
        // Even smaller: build SolutionEntry(somelist.length, board) directly,
        // skip map/filterMap. Inspect the lowered constructor.
        val sir = compile {
            val board = startTour(Tile(1, 1), BigInt(4))
            val entry = SolutionEntry(board.possibleMoves.length, board)
            entry.depth
        }
        val lowered: LoweredValue = sir.toLoweredValue(using summon[Options])()
        println(s"[MICRO] lowered.representation = ${lowered.representation}")
        println(s"[MICRO] lowered.show =")
        println(lowered.show)
        val result = sir.toUplc().evaluateDebug
        result match
            case Result.Success(Term.Const(Constant.Integer(v), _), _, _, _) =>
                info(s"depth = $v")
            case Result.Failure(ex, _, _, _) =>
                fail(s"Micro repro failed: $ex")
            case other => fail(s"Unexpected: $other")
    }

    ignore("micro2 - field-repr mismatch via map (Fixed -> Transparent)") {
        // Clean reproduction of the Fixed -> Transparent field-repr crash, no
        // empty-list-head landmines. The depth comes from `.length` (returns a
        // Data-encoded BigInt with TypeVar(Fixed) repr). The constructor records
        // the field as Fixed. The list is mapped (TypeVar(B) entry path).
        // filterMap then calls genSelect on a Transparent-TypeVar scrutinee,
        // which resolves to defaultRepresentation [Constant, ...] for depth,
        // but the actual stored value is Data-encoded -> mismatch -> EqualsInteger
        // operates on a Data integer -> evaluation failure.
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
        val lowered = sir.toLoweredValue(using summon[Options])()
        val pw = new java.io.PrintWriter("/tmp/micro2_lowered.txt")
        try {
            pw.println("=== representation ===")
            pw.println(lowered.representation)
            pw.println("=== show ===")
            pw.println(lowered.show)
        } finally pw.close()
        val result = sir.toUplc().evaluateDebug
        result match
            case Result.Success(Term.Const(Constant.Integer(v), _), _, _, _) =>
                info(s"length = $v")
            case Result.Failure(ex, _, _, _) =>
                fail(s"micro2 failed (expected Fixed->Transparent fix): $ex")
            case other => fail(s"Unexpected: $other")
    }

    test("micro3 - tiny multi-arg dispatch repro (just map over BigInts)") {
        // Smallest possible 2-arg intrinsic call that triggers multi-arg dispatch:
        // `entries.map(f)` where entries is a 1-element list and f is a tiny lambda.
        // No nested intrinsics inside f. Should expose any scoping issue with
        // multi-arg dispatch's eager arg lowering + precomputedValues inlining.
        val sir = compile {
            val xs: List[BigInt] = List.Cons(BigInt(10), List.Nil)
            val ys = xs.map { x => x + BigInt(1) }
            ys.head
        }
        val result = sir.toUplc().evaluateDebug
        result match
            case Result.Success(Term.Const(Constant.Integer(v), _), _, _, _) =>
                info(s"head = $v")
                assert(v == 11, s"Expected 11, got $v")
            case Result.Failure(ex, _, _, _) =>
                fail(s"micro3 failed: $ex")
            case other => fail(s"Unexpected: $other")
    }

end KnightsTestMinimal
