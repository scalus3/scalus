package scalus.verify.uplcblaster

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.cardano.onchain.plutus.prelude.Math
import scalus.compiler.Compile
import scalus.compiler.sir.{AnnotationsDecl, SIR, SIRBuiltins, SIRType, TargetLoweringBackend}
import scalus.compiler.sir.lowering.{PrimitiveRepresentation, ProductCaseClassRepresentation, SumCaseClassRepresentation}
import scalus.uplc.{Constant, PlutusV3, Term}
import scalus.uplc.Term.asTerm
import scalus.uplc.builtin.{Builtins, ByteString, Data, FromData, ToData}
import scalus.uplc.eval.{PlutusVM, Result}
import scalus.verify.*
import scalus.verify.Props.*

import java.nio.file.Files

case class BlasterPair(a: BigInt, b: BigInt) derives FromData, ToData

@Compile
object BlasterPair

enum BlasterShape derives FromData, ToData:
    case Circle(r: BigInt)
    case Rect(w: BigInt, h: BigInt)

@Compile
object BlasterShape

class UplcBlasterTest extends AnyFunSuite with LeanProofs {
    private given PlutusVM = PlutusVM.makePlutusV3VM()

    private val div10 = FunctionDef.named("div10", (x: BigInt) => BigInt(10) / x)

    private val makePair =
        FunctionDef.named("makePair", (a: BigInt, b: BigInt) => BlasterPair(a, b))
    private val sum = FunctionDef.named("sum", (p: BlasterPair) => p.a + p.b)
    private val area = FunctionDef.named(
      "area",
      (s: BlasterShape) =>
          s match
              case BlasterShape.Circle(r)  => r
              case BlasterShape.Rect(w, h) => w * h
    )

    private def lowered(prop: Prop, functions: FunctionTable): UplcBlaster.Lowered =
        UplcBlaster.lower(prop, functions) match
            case Right(lowered) => lowered
            case Left(reason)   => fail(reason)

    /** The two binders of a two-value `forAll` and its body. */
    private def split2(prop: Prop): (PropExpr.Ident[?], PropExpr.Ident[?], Prop) = prop match
        case Prop.Forall(x, Prop.Forall(y, body)) => (x, y, body)
        case other => fail(s"expected two universal quantifiers, got $other")

    test("proves an addition identity through compiled UPLC and Lean Blaster") {
        proven(forAll[BigInt](x => x + BigInt(0) == x), budget = 40)
    }

    test("proves that addition is commutative and associative") {
        proven(forAll[BigInt, BigInt]((x, y) => x + y == y + x), budget = 40)
        proven(
          forAll[BigInt, BigInt, BigInt]((x, y, z) => (x + y) + z == x + (y + z)),
          budget = 60
        )
    }

    test("proves that min is a lower bound and min + max is the sum") {
        proven(
          forAll[BigInt, BigInt]((x, y) => Math.min(x, y) <= x && Math.min(x, y) <= y),
          budget = 100
        )
        proven(
          forAll[BigInt, BigInt]((x, y) => Math.min(x, y) + Math.max(x, y) == x + y),
          budget = 100
        )
    }

    test("quantifies over Boolean values") {
        proven(
          forAll[Boolean, BigInt]((flag, x) =>
              if flag then Math.max(x, BigInt(0)) >= BigInt(0)
              else Math.min(x, BigInt(0)) <= BigInt(0)
          ),
          budget = 60
        )
    }

    test("a false property of min is refuted with a replayed counterexample") {
        val prop = forAll[BigInt, BigInt]((x, y) => Math.min(x, y) == x)
        val (x, y, _) = split2(prop)
        val counterexample = refuted(prop, budget = 60)
        assert(counterexample.keySet == Set(x.name, y.name))
        assert(integer(counterexample(x.name)) > integer(counterexample(y.name)), counterexample)
    }

    test("proves connectives over separately compiled tests, with each test read by polarity") {
        val implication =
            forAll[BigInt, BigInt]((x, y) => (x <= y) ==> (Math.min(x, y) == x))
        assert(lowered(implication, FunctionTable.empty).leaves.size == 2)
        proven(implication, budget = 60)
        proven(
          forAll[BigInt, BigInt]((x, y) => (x <= y) <=> (Math.min(x, y) == x)),
          budget = 60
        )
        proven(
          forAll[BigInt, BigInt]((x, y) => !Prop(x <= y) ==> (Math.min(x, y) == y)),
          budget = 60
        )
        proven(
          forAll[BigInt, BigInt]((x, y) => Prop(x <= y) || Prop(Math.min(x, y) == y)),
          budget = 60
        )

        val wrong = forAll[BigInt, BigInt]((x, y) => (x <= y) ==> (Math.min(x, y) == y))
        // println(s"wrong: $wrong")
        val (x, y, _) = split2(wrong)
        val counterexample = refuted(wrong, budget = 60)
        // println(s"counterexample: $counterexample")
        assert(integer(counterexample(x.name)) < integer(counterexample(y.name)), counterexample)
    }

    test("a falsification caused by too small a budget is reported as spurious") {
        run(forAll[BigInt, BigInt]((x, y) => Math.min(x, y) <= x), 3, Nil) match
            case (_, _, VerificationResult.Inconclusive(reason)) =>
                assert(reason.contains("spurious"), reason)
            case (_, _, other) => fail(s"expected an inconclusive result, got $other")
    }

    test("closed statements, equality and denotes") {
        proven(equal(BigInt(2) + BigInt(3), BigInt(5)), budget = 40)
        proven(denotes(BigInt(7) / BigInt(2)), budget = 40)
        refuted(denotes(BigInt(7) / BigInt(0)), budget = 40)
    }

    test("a closed statement is decided by evaluation, one with variables by Blaster") {
        assert(proven(denotes(BigInt(7) / BigInt(2)), budget = 40) == ProofKind.LeanNative)
        assert(
          proven(forAll[BigInt](x => denotes(x + BigInt(1))), budget = 40) == ProofKind.Blaster
        )
    }

    test("proves that a program fails, apart from a budget that runs out") {
        proven(!denotes(BigInt(7) / BigInt(0)), budget = 40)
        refuted(!denotes(BigInt(7) / BigInt(2)), budget = 40)
        run(!denotes(BigInt(7) / BigInt(0)), 2, Nil) match
            case (_, _, VerificationResult.Inconclusive(reason)) =>
                assert(reason.contains("spurious"), reason)
            case (_, _, other) => fail(s"expected an inconclusive result, got $other")
    }

    test("a failing test does not hold, so its negation does") {
        proven(!Prop(BigInt(7) / BigInt(0) > BigInt(0)), budget = 40)
    }

    test("denotes in a premise restricts a statement to the inputs where a program returns") {
        proven(forAll[BigInt](x => denotes(BigInt(10) / x) ==> (x != BigInt(0))), budget = 40)

        val positive = forAll[BigInt](x => denotes(BigInt(10) / x) ==> (x > BigInt(0)))
        val x = positive match
            case Prop.Forall(x, _) => x
            case other             => fail(s"expected a universal proposition, got $other")
        val counterexample = refuted(positive, budget = 40)
        assert(integer(counterexample(x.name)) < 0, counterexample)
    }

    test("requires a positive symbolic execution budget") {
        assertThrows[IllegalArgumentException](UplcBlaster(0))
    }

    test("lowers a universal prefix and a Boolean body to one n-argument UPLC predicate") {
        val annotations = AnnotationsDecl.empty
        val xName = "x"
        val yName = "y"
        val xVar = SIR.Var(xName, SIRType.Integer, annotations)
        val yVar = SIR.Var(yName, SIRType.Integer, annotations)
        val lessThanX = SIR.Apply(
          SIRBuiltins.lessThanInteger,
          xVar,
          SIRType.Fun(SIRType.Integer, SIRType.Boolean),
          annotations
        )
        val body = SIR.Apply(lessThanX, yVar, SIRType.Boolean, annotations)
        val prop = Prop.Forall(
          PropExpr.Ident[BigInt](xName, 1, SIRType.Integer),
          Prop.Forall(
            PropExpr.Ident[BigInt](yName, 2, SIRType.Integer),
            Prop.Bool(PropExpr.SIRExpr(body))
          )
        )

        val lower = lowered(prop, FunctionTable.empty)
        assert(lower.body == UplcBlaster.LeafFormula.Test(0))
        val applied = lower.leaves.head $ BigInt(1).asTerm $ BigInt(2).asTerm
        applied.term.evaluateDebug match
            case success: Result.Success => assert(success.term == Term.Const(Constant.Bool(true)))
            case failure: Result.Failure => fail(failure.exception)
    }

    test("links a registered function into the UPLC predicate and proves it") {
        val clamp = FunctionDef(Math.clamp)
        val prop = forAll[BigInt](x => Math.clamp(x, x, x) == x)

        val applied = lowered(prop, FunctionTable(clamp)).leaves.head $ BigInt(7).asTerm
        applied.term.evaluateDebug match
            case success: Result.Success => assert(success.term == Term.Const(Constant.Bool(true)))
            case failure: Result.Failure => fail(failure.exception)

        proven(prop, 80, clamp)
    }

    test("compiles an unregistered @Compile function together with the test") {
        proven(forAll[BigInt](x => Math.clamp(x, x, x) == x), budget = 80)
    }

    test("proves a total call of a registered function") {
        val increment = FunctionDef.named("increment", (x: BigInt) => x + BigInt(1))
        proven(callRef(increment.ref, BigInt(41))(r => r == BigInt(42)), 40, increment)
    }

    test("a partial call claims its continuation only when the function returns") {
        val partial = forAll[BigInt](x => whenReturns(div10, x)(r => x != BigInt(0)))
        lowered(partial, FunctionTable(div10)).body match
            case UplcBlaster.LeafFormula.Implies(
                  UplcBlaster.LeafFormula.Denotes(0),
                  UplcBlaster.LeafFormula.Test(1)
                ) =>
            case other => fail(s"expected denotes(div10(x)) ==> call(div10, x), got $other")
        proven(partial, 60, div10)

        // The total call also claims that div10 returns, which it does not at 0.
        val total = forAll[BigInt](x => call(div10, x)(r => x != BigInt(0)))
        val x = total match
            case Prop.Forall(x, _) => x
            case other             => fail(s"expected a universal proposition, got $other")
        val counterexample = refuted(total, 60, div10)
        assert(integer(counterexample(x.name)) == 0, counterexample)

        // negative control: where div10 returns, its result is checked
        refuted(forAll[BigInt](x => whenReturns(div10, x)(r => r > BigInt(0))), 60, div10)

        // A call that fails satisfies any continuation.
        assert(
          proven(whenReturns(div10, BigInt(0))(r => r == BigInt(42)), 60, div10) ==
              ProofKind.LeanNative
        )
        refuted(call(div10, BigInt(0))(r => r == BigInt(42)), 60, div10)
    }

    test("quantifies over Data, and reads a Data counterexample back") {
        proven(forAll[Data](d => equal(d, d)), budget = 40)
        proven(forAll[BigInt](x => equal(Builtins.unIData(Builtins.iData(x)), x)), budget = 40)
        proven(
          forAll[Data](d =>
              denotes(Builtins.unIData(d)) ==> equal(Builtins.iData(Builtins.unIData(d)), d)
          ),
          budget = 60
        )

        // negative control: an I value can hold a negative integer
        val nonNegative =
            forAll[Data](d => denotes(Builtins.unIData(d)) ==> (Builtins.unIData(d) >= BigInt(0)))
        val d = nonNegative match
            case Prop.Forall(d, _) => d
            case other             => fail(s"expected a universal proposition, got $other")
        refuted(nonNegative, budget = 60)(d.name) match
            case Constant.Data(Data.I(value)) => assert(value < 0, value)
            case other                        => fail(s"expected an I value, got $other")

        // A counterexample that nests: a list with an element.
        val empty = forAll[Data](d =>
            denotes(Builtins.nullList(Builtins.unListData(d))) ==>
                Builtins.nullList(Builtins.unListData(d))
        )
        val list = empty match
            case Prop.Forall(list, _) => list
            case other                => fail(s"expected a universal proposition, got $other")
        refuted(empty, budget = 60)(list.name) match
            case Constant.Data(Data.List(values)) => assert(!values.isEmpty, values)
            case other                            => fail(s"expected a List value, got $other")
    }

    test("calls take and return Data") {
        val wrap = FunctionDef.named("wrap", (x: BigInt) => Builtins.iData(x))
        val unwrap = FunctionDef.named("unwrap", (d: Data) => Builtins.unIData(d))
        proven(forAll[BigInt](x => callRef(wrap.ref, x)(r => r == Builtins.iData(x))), 60, wrap)
        proven(forAll[Data](d => whenReturns(unwrap, d)(r => Builtins.iData(r) == d)), 60, unwrap)
        // negative control: unwrap does not return on every Data value
        refuted(forAll[Data](d => call(unwrap, d)(r => Builtins.iData(r) == d)), 60, unwrap)
        assert(
          proven(callRef(wrap.ref, BigInt(3))(r => r == Data.I(BigInt(3))), 60, wrap) ==
              ProofKind.LeanNative
        )
    }

    test("a compiled function records how its program takes its arguments and returns") {
        import PrimitiveRepresentation.Constant
        import ProductCaseClassRepresentation.ProdDataList
        import SumCaseClassRepresentation.DataConstr
        def signature(function: FunctionDef[?, ?]) = function.get(Representation.UplcSignature)
        // A plain case class travels as a builtin list of its fields, an enum as Data.
        assert(signature(sum) == Some(UplcSignature.Represented(List(ProdDataList), Constant)))
        assert(
          signature(makePair) ==
              Some(UplcSignature.Represented(List(Constant, Constant), ProdDataList))
        )
        assert(signature(area) == Some(UplcSignature.Represented(List(DataConstr), Constant)))
    }

    test("a function compiled with another lowering backend is not linked into the tests") {
        val options =
            UplcBlaster.options.copy(targetLoweringBackend =
                TargetLoweringBackend.SumOfProductsLowering
            )
        val increment = FunctionDef.fromCompiled(
          FunctionDef.synthetic[BigInt, BigInt]("increment"),
          PlutusV3.compile((x: BigInt) => x + BigInt(1))(using options)
        )
        val lowered = UplcBlaster.lower(
          forAll[BigInt](x => callRef(increment.ref, x)(r => r > x)),
          FunctionTable(increment)
        )
        assert(lowered.left.exists(_.contains("SumOfProductsLowering")), lowered)
    }

    test("a function with only a UPLC program is called with BigInt, Boolean and Data only") {
        // Without SIR there is no declared type, so the tests cannot know how its program takes a
        // case class: `Circle(r)` alone would be passed as a list of fields, not as the enum.
        val areaProgram = FunctionDef
            .synthetic[BlasterShape, BigInt]("areaProgram")
            .withRepresentation(Representation.Uplc, area(Representation.Uplc))
        val shaped = UplcBlaster.lower(
          forAll[BigInt](r => callRef(areaProgram.ref, BlasterShape.Circle(r))(x => x == r)),
          FunctionTable(areaProgram)
        )
        assert(shaped.left.exists(_.contains("has no SIR")), shaped)

        val incrementProgram = FunctionDef
            .synthetic[BigInt, BigInt]("incrementProgram")
            .withRepresentation(
              Representation.Uplc,
              PlutusV3.compile((x: BigInt) => x + BigInt(1))(using UplcBlaster.options).program
            )
        val lowered = UplcBlaster.lower(
          forAll[BigInt](x => callRef(incrementProgram.ref, x)(r => r > x)),
          FunctionTable(incrementProgram)
        )
        assert(lowered.isRight, lowered)
    }

    test("calls take and return case classes, each in its own representation") {
        proven(
          forAll[BigInt, BigInt]((a, b) =>
              callRef(makePair.ref, (a, b))(p => p.a == a && p.b == b)
          ),
          240,
          makePair
        )
        proven(
          forAll[BigInt, BigInt]((a, b) => callRef(sum.ref, BlasterPair(a, b))(r => r == a + b)),
          240,
          sum
        )
        // An enum's constructor is passed as the enum.
        proven(
          forAll[BigInt](r => callRef(area.ref, BlasterShape.Circle(r))(x => x == r)),
          240,
          area
        )
        // One function's result is another's argument.
        proven(
          forAll[BigInt, BigInt]((a, b) =>
              callRef(makePair.ref, (a, b))(p => callRef(sum.ref, p)(r => r == a + b))
          ),
          400,
          makePair,
          sum
        )
        // negative control
        refuted(
          forAll[BigInt, BigInt]((a, b) => callRef(sum.ref, BlasterPair(a, b))(r => r == a)),
          240,
          sum
        )
        // A function of one parameter whose type is a pair takes the whole pair.
        val first = FunctionDef.named("first", (pair: (BigInt, BigInt)) => pair._1)
        proven(callRef(first.ref, (BigInt(1), BigInt(2)))(r => r == BigInt(1)), 240, first)
    }

    test("denotes of a case class, and decoding one from Data") {
        proven(forAll[BigInt](x => denotes(BlasterPair(x, x))), budget = 40)
        refuted(forAll[Data](d => denotes(d.to[BlasterPair])), budget = 160)
    }

    test("calls a function of several parameters through its own compiled program") {
        val clamp = FunctionDef(Math.clamp)
        assert(clamp.arity == 3)
        proven(
          callRef(clamp.ref, (BigInt(9), BigInt(1), BigInt(5)))(r => r == BigInt(5)),
          80,
          clamp
        )
        proven(
          forAll[BigInt, BigInt, BigInt]((x, lo, hi) =>
              (lo <= hi) ==> callRef(clamp.ref, (x, lo, hi))(r => lo <= r && r <= hi)
          ),
          120,
          clamp
        )

        val unguarded = forAll[BigInt, BigInt, BigInt]((x, lo, hi) =>
            callRef(clamp.ref, (x, lo, hi))(r => lo <= r && r <= hi)
        )
        val (lo, hi) = unguarded match
            case Prop.Forall(_, Prop.Forall(lo, Prop.Forall(hi, _))) => (lo, hi)
            case other => fail(s"expected three universal quantifiers, got $other")
        val counterexample = refuted(unguarded, 120, clamp)
        assert(integer(counterexample(lo.name)) > integer(counterexample(hi.name)), counterexample)
    }

    test("a call's continuation can call another function") {
        val min = FunctionDef.named("min", (x: BigInt, y: BigInt) => Math.min(x, y))
        val max = FunctionDef.named("max", (x: BigInt, y: BigInt) => Math.max(x, y))
        proven(
          forAll[BigInt, BigInt]((x, y) =>
              callRef(min.ref, (x, y))(a => callRef(max.ref, (x, y))(b => a + b == x + y))
          ),
          120,
          min,
          max
        )
    }

    test("Blaster's error comes back in the inconclusive result") {
        // For `e >= 0`, `exp2` reaches the bitwise builtins, whose `ByteString` is built on
        // `BitVec`, which Blaster cannot translate (README, Limitations). When it can, this test
        // needs another statement Blaster rejects.
        val exp2 = FunctionDef(Math.exp2)
        run(forAll[BigInt](e => callRef(exp2.ref, e)(r => r >= BigInt(0))), 120, Seq(exp2)) match
            case (verifier, _, VerificationResult.Inconclusive(reason)) =>
                assert(reason.startsWith("Lean exited with code 1: "), reason)
                assert(
                  reason.contains("error: Inductive datatype with instance parameters"),
                  reason
                )
                assert(reason.contains("not supported: `BitVec"), reason)
                assert(!reason.contains("Successfully decoded"), reason)
                assert(verifier.theorems.isEmpty)
            case (_, _, other) => fail(s"expected an inconclusive result, got $other")
    }

    test("a workspace without the ScalusProofs library is reported with Lean's error") {
        requireLean()
        // The workspace's own toolchain, so elan does not look for a default one.
        val empty = Files.createTempDirectory("scalus-empty-lean-workspace-")
        val toolchain = empty.resolve("lean-toolchain")
        Files.copy(leanDirectory.resolve("lean-toolchain"), toolchain)
        try
            val verifier = Verifier.empty
            val statement = verifier.statement(forAll[BigInt](x => x + BigInt(0) == x))
            verifier.verify(statement, UplcBlaster(40, empty)) match
                case VerificationResult.Inconclusive(reason) =>
                    assert(reason.startsWith("Lean exited with code 1: "), reason)
                    assert(reason.contains("unknown module prefix 'ScalusProofs'"), reason)
                case other => fail(s"expected an inconclusive result, got $other")
        finally
            Files.deleteIfExists(toolchain)
            Files.deleteIfExists(empty)
    }

    test("statements outside the fragment are inconclusive") {
        val quantified = forAll[BigInt](x => x > 0) match
            case Prop.Forall(ident, body) =>
                Prop.Forall(ident, body && Prop.Exists(ident, None, body))
            case other => fail(s"expected a universal proposition, got $other")
        assert(
          UplcBlaster.lower(quantified, FunctionTable.empty).left.exists(_.contains("quantifier"))
        )

        // The tactic reports these without running Lean.
        val verifier = Verifier.empty
        val bytes = verifier.statement(forAll[ByteString](_ => true))
        verifier.verify(bytes, UplcBlaster(10, leanDirectory)) match
            case VerificationResult.Inconclusive(reason) =>
                assert(reason.contains("ByteString binder"), reason)
            case other => fail(s"expected an inconclusive result, got $other")

        // A partial call is split into two leaves, which a call's continuation cannot hold.
        val nested = UplcBlaster.lower(
          callRef(div10.ref, BigInt(1))(r => whenReturnsRef(div10.ref, r)(s => s == BigInt(1))),
          FunctionTable(div10)
        )
        assert(nested.left.exists(_.contains("whenReturns inside a call's continuation")), nested)
    }
}
