package scalus.verify.uplcblaster

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.cardano.onchain.plutus.prelude.{require, Math}
import scalus.compiler.Compile
import scalus.compiler.sir.{AnnotationsDecl, SIR, SIRBuiltins, SIRType, TargetLoweringBackend}
import scalus.compiler.sir.lowering.{PrimitiveRepresentation, ProductCaseClassRepresentation, SumCaseClassRepresentation}
import scalus.uplc.{Constant, PlutusV3, Term}
import scalus.uplc.Term.asTerm
import scalus.uplc.builtin.{Builtins, ByteString, Data, FromData, ToData}
import scalus.uplc.builtin.Data.toData
import scalus.uplc.eval.{PlutusVM, Result}
import scalus.verify.*
import scalus.verify.Props.*
import scalus.verify.lean.Directories
import scalus.verify.lean.LeanServer.{Progress, Reached, Started}

import java.nio.file.Files
import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*

case class BlasterPair(a: BigInt, b: BigInt) derives FromData, ToData

@Compile
object BlasterPair

case class BlasterOwner(key: ByteString) derives FromData, ToData

@Compile
object BlasterOwner

case class BlasterOwned(owner: BlasterOwner, amount: BigInt) derives FromData, ToData

@Compile
object BlasterOwned

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

    test("proves existential statements and explicit witnesses") {
        val searched = forAll[BigInt](x => exists[BigInt](y => y == x + BigInt(1)))
        val loweredSearch = lowered(searched, FunctionTable.empty)
        loweredSearch.body match
            case UplcBlaster.LeafFormula.Forall(
                  List(0),
                  UplcBlaster.LeafFormula.Exists(
                    List(1),
                    UplcBlaster.LeafFormula.Test(0)
                  )
                ) =>
            case other => fail(s"expected ∀ x. ∃ y. test, got $other")
        assert(loweredSearch.leafBinders == Vector(List(0, 1)))
        val rendered = Files.createTempDirectory("scalus-exists-check-")
        try
            val source = Files.readString(UplcBlaster.writeCheck(loweredSearch, 60, rendered))
            assert(source.contains("¬ (∀"), source)
            assert(!source.contains("∃"), source)
        finally
            Files.deleteIfExists(rendered.resolve("Check.lean"))
            Files.deleteIfExists(rendered.resolve("Leaf0.flat"))
            Files.deleteIfExists(rendered)
        proven(searched, budget = 60)

        val supplied = forAll[BigInt](x => existsLet(x + BigInt(1))(y => y > x))
        val loweredWitness = lowered(supplied, FunctionTable.empty)
        assert(loweredWitness.binders.size == 1)
        assert(loweredWitness.leafBinders == Vector(List(0)))
        proven(supplied, budget = 60)

        // A witness is applied strictly, even when the body does not use it.
        refuted(existsLet(BigInt(1) / BigInt(0))(_ => true), budget = 20)

        // A leaf outside an existential's scope takes no value for its binder.
        val splitScope = Prop(true) || exists[BigInt](_ => false)
        val loweredScope = lowered(splitScope, FunctionTable.empty)
        assert(loweredScope.leafBinders == Vector(Nil, List(0)))
        proven(splitScope, budget = 20)

        // There is no finite counterexample that CEK replay can use to check every witness.
        run(exists[BigInt](x => x != x), 20, Nil) match
            case (_, _, VerificationResult.Inconclusive(reason)) =>
                assert(reason.contains("establish that no witness exists"), reason)
            case (_, _, other) =>
                fail(s"expected an inconclusive existential refutation, got $other")
    }

    test("a statement that asks for a witness is not refuted by one assignment") {
        def searches(prop: Prop): Boolean = lowered(prop, FunctionTable.empty).hasExistential
        // A quantifier asks for a witness by its position: an `exists` where the statement is
        // read as stated, a `forAll` under a negation or in a premise.
        assert(!searches(forAll[BigInt](n => n == n)))
        assert(searches(exists[BigInt](n => n == n)))
        assert(searches(!forAll[BigInt](n => n != BigInt(2))))
        assert(searches(forAll[BigInt](n => n != BigInt(2)) ==> Prop(false)))
        assert(!searches(!exists[BigInt](n => n != n)))
        assert(!searches(exists[BigInt](n => n != n) ==> Prop(false)))
        assert(searches(forAll[BigInt](n => n == n) <=> Prop(true)))
        assert(!searches(!(!forAll[BigInt](n => n == n))))

        // True, with the witness 2, which Lean does not find at a budget no run finishes in.
        // The value a replay would take for `n` shows nothing about the others.
        val squareRoot = !forAll[BigInt](n => n * n != BigInt(4))
        run(squareRoot, 3, Nil) match
            case (_, _, VerificationResult.Inconclusive(reason)) =>
                assert(reason.contains("establish that no witness exists"), reason)
            case (_, _, other) => fail(s"expected an inconclusive result, got $other")
        proven(squareRoot, budget = 60)

        // A negated `exists` ranges over every value, so one of them refutes it.
        val counterexample = refuted(!exists[BigInt](n => n * n == BigInt(4)), budget = 60)
        assert(counterexample.size == 1, counterexample)
        assert(
          counterexample.values.map(integer).forall(n => n * n == BigInt(4)),
          counterexample
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

    test("a runtime check in an expression is part of its program") {
        // `require(...)` is a statement whose value nothing uses. It must not be dropped as an
        // unused definition: the expression fails where the check does.
        val checked = forAll[BigInt](x => denotes { require(x >= BigInt(0)); x * BigInt(2) })
        val program = lowered(checked, FunctionTable.empty).leaves.head
        assert((program $ BigInt(3).asTerm).term.evaluateDebug.isSuccess)
        assert((program $ BigInt(-1).asTerm).term.evaluateDebug.isFailure)

        val x = checked match
            case Prop.Forall(x, _) => x
            case other             => fail(s"expected a universal proposition, got $other")
        assert(integer(refuted(checked, budget = 80)(x.name)) < 0)
        proven(
          forAll[BigInt](x =>
              (x < BigInt(0)) ==> !denotes { require(x >= BigInt(0)); x * BigInt(2) }
          ),
          budget = 80
        )
        proven(
          forAll[BigInt](x =>
              (x >= BigInt(0)) ==> denotes { require(x >= BigInt(0)); x * BigInt(2) }
          ),
          budget = 80
        )
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
        // No Lean is asked for: a tactic takes its server when it runs a check.
        assertThrows[IllegalArgumentException](UplcBlaster(0, lean))
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
        assert(
          lower.body == UplcBlaster.LeafFormula.Forall(
            List(0),
            UplcBlaster.LeafFormula.Forall(List(1), UplcBlaster.LeafFormula.Test(0))
          )
        )
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
            case UplcBlaster.LeafFormula.Forall(
                  List(0),
                  UplcBlaster.LeafFormula.Implies(
                    UplcBlaster.LeafFormula.Denotes(0),
                    UplcBlaster.LeafFormula.Test(1)
                  )
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

    test("quantifies over byte strings, and reads a byte string counterexample back") {
        proven(
          forAll[ByteString](b => Builtins.lengthOfByteString(b) >= BigInt(0)),
          budget = 40
        )
        proven(
          forAll[ByteString, ByteString]((a, b) =>
              Builtins.lengthOfByteString(Builtins.appendByteString(a, b)) ==
                  Builtins.lengthOfByteString(a) + Builtins.lengthOfByteString(b)
          ),
          budget = 60
        )
        // negative control: a byte string need not be empty
        val empty = forAll[ByteString](b => Builtins.lengthOfByteString(b) == BigInt(0))
        val b = empty match
            case Prop.Forall(b, _) => b
            case other             => fail(s"expected a universal proposition, got $other")
        refuted(empty, budget = 40)(b.name) match
            case Constant.ByteString(bytes) => assert(bytes.size > 0, bytes)
            case other                      => fail(s"expected a byte string, got $other")
    }

    test("a counterexample that is no value of its variable's type is inconclusive") {
        // No Lean here: what the tactic makes of Lean's output, on the Scalus CEK.
        val goal = lowered(
          forAll[ByteString](b => Builtins.lengthOfByteString(b) == BigInt(0)),
          FunctionTable.empty
        )
        def falsified(value: String): VerificationResult =
            UplcBlaster.verdict(
              goal,
              40,
              errors = true,
              s"❌ Falsified\nCounterexample:\n - x0: $value"
            )
        val bytes = "PlutusCore.ByteString.ByteString.mk"
        // a byte string, replayed
        assert(falsified(s"""($bytes "A")""").isInstanceOf[VerificationResult.Refuted])
        // Lean's model stores a byte string as a string: one with a character above 255 is a
        // value of the model only, and shows nothing about byte strings
        falsified(s"""($bytes "\\u{100}")""") match
            case VerificationResult.Inconclusive(reason) =>
                assert(reason.contains("no value of its variable's type"), reason)
                assert(reason.contains("U+100"), reason)
            case other => fail(s"expected an inconclusive result, got $other")
        // output that is not understood is a failure of the tool, not a result about the statement
        falsified("42") match
            case VerificationResult.Failed(reason) =>
                assert(reason.contains("cannot read Lean's counterexample"), reason)
            case other => fail(s"expected a failed result, got $other")
    }

    test("a variable of a case class is one variable per field, and built in each test") {
        val sumOfPair = forAll[BlasterPair](p => callRef(sum.ref, p)(r => r == p.a + p.b))
        // Lean quantifies over the two fields, in order, named after the variable.
        val fields = lowered(sumOfPair, FunctionTable(sum)).binders
        assert(fields.map(_.tp) == List(SIRType.Integer, SIRType.Integer))
        assert(fields.head.name.endsWith(".a") && fields.last.name.endsWith(".b"), fields)
        proven(sumOfPair, 240, sum)
        // The built value is the compiler's: it agrees with one the test constructs.
        proven(forAll[BlasterPair](p => equal(p.toData, BlasterPair(p.a, p.b).toData)), 240)
        // negative control, with the counterexample's value per field
        val wrong = forAll[BlasterPair](p => callRef(sum.ref, p)(r => r == p.a))
        val counterexample = refuted(wrong, 240, sum)
        val names = lowered(wrong, FunctionTable(sum)).binders.map(_.name)
        assert(counterexample.keySet == names.toSet, counterexample)
        assert(integer(counterexample(names.last)) != 0, counterexample)
    }

    test("a case class in a case class is expanded down to its fields") {
        val keyLength = forAll[BlasterOwned](o =>
            Builtins.lengthOfByteString(o.owner.key) >= BigInt(0) && o.amount == o.amount
        )
        val fields = lowered(keyLength, FunctionTable.empty).binders
        assert(fields.map(_.tp) == List(SIRType.ByteString, SIRType.Integer), fields)
        assert(fields.head.name.endsWith(".owner.key"), fields)
        proven(keyLength, budget = 160)
        refuted(forAll[BlasterOwned](o => o.amount >= BigInt(0)), budget = 160)
    }

    test("states that an expression fails, whatever its type") {
        // A builtin that fails: unBData of an I value, for every x.
        proven(forAll[BigInt](x => fails(Builtins.unBData(Builtins.iData(x)))), budget = 60)
        proven(forAll[BigInt](x => (x == BigInt(0)) ==> fails(BigInt(10) / x)), budget = 40)
        proven(
          forAll[BigInt](x => (x < BigInt(0)) ==> fails { require(x >= BigInt(0)); x * BigInt(2) }),
          budget = 80
        )
        proven(forAll[BigInt](x => (x != BigInt(0)) ==> succeeds(BigInt(10) / x)), budget = 40)
        // A comparison returns a Boolean on every input: it never fails.
        refuted(forAll[BigInt](x => fails(x > BigInt(0))), budget = 40)
    }

    test("states when a function returns and when it fails") {
        proven(returnsWhen(div10)(x => x != BigInt(0)), 60, div10)
        proven(failsWhen(div10)(x => x == BigInt(0)), 60, div10)
        // negative controls: div10 fails at 0, and returns elsewhere
        val everywhere = returnsWhen(div10)(x => true)
        val x = everywhere match
            case Prop.Forall(x, _) => x
            case other             => fail(s"expected a universal proposition, got $other")
        assert(integer(refuted(everywhere, 60, div10)(x.name)) == 0)
        refuted(failsWhen(div10)(x => x >= BigInt(0)), 60, div10)
        // Closed forms are decided by evaluation.
        assert(proven(fails(div10, BigInt(0)), 60, div10) == ProofKind.LeanNative)
        assert(proven(succeeds(div10, BigInt(2)), 60, div10) == ProofKind.LeanNative)
        refuted(fails(div10, BigInt(2)), 60, div10)
    }

    test("a contract is proved about its function's compiled program") {
        val clamp = FunctionDef(Math.clamp)
        proven(
          contract(clamp)(
            expects = (x, lo, hi) => lo <= hi,
            ensures = (x, lo, hi) => r => lo <= r && r <= hi
          ).prop,
          120,
          clamp
        )
        proven(
          totalContract(clamp)(
            expects = (x, lo, hi) => lo <= hi,
            ensures = (x, lo, hi) => r => lo <= r && r <= hi
          ).prop,
          120,
          clamp
        )
        // negative control: without its precondition the range can be empty
        refuted(
          contract(clamp)(
            expects = (x, lo, hi) => true,
            ensures = (x, lo, hi) => r => lo <= r && r <= hi
          ).prop,
          120,
          clamp
        )
        // A partial contract holds where the function fails; a total one does not.
        proven(
          contract(div10)(expects = x => true, ensures = x => r => x != BigInt(0)).prop,
          60,
          div10
        )
        refuted(
          totalContract(div10)(expects = x => true, ensures = x => r => x != BigInt(0)).prop,
          60,
          div10
        )
    }

    test("a contract states when its function returns and when it fails") {
        def base = contract(div10)(expects = x => true, ensures = x => r => r <= BigInt(10))
        val specified = base.returnsWhen(x => x != BigInt(0)).failsWhen(x => x == BigInt(0))
        // The two clauses call div10 with the same program, which is one leaf: the precondition,
        // each clause's condition, that call, and the two leaves of the partial postcondition.
        assert(lowered(specified.prop, FunctionTable(div10)).leaves.size == 6)
        proven(specified.prop, 80, div10)

        // negative controls, one per clause, each next to a clause that holds: a contract keeps
        // every clause it was given
        refuted(
          base.returnsWhen(x => x != BigInt(0)).failsWhen(x => x >= BigInt(0)).prop,
          80,
          div10
        )
        refuted(base.returnsWhen(x => true).failsWhen(x => x == BigInt(0)).prop, 80, div10)

        // returnsWhen(_ => true) is the total contract
        val clamp = FunctionDef(Math.clamp)
        proven(
          contract(clamp)(
            expects = (x, lo, hi) => lo <= hi,
            ensures = (x, lo, hi) => r => lo <= r && r <= hi
          ).returnsWhen((x, lo, hi) => true).prop,
          120,
          clamp
        )
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

    test("a call under an explicit witness passes its arguments written out") {
        val clamp = FunctionDef(Math.clamp)
        val bounded = forAll[BigInt](x =>
            existsLet(x + BigInt(1))(y =>
                callRef(clamp.ref, (y, BigInt(0), BigInt(9)))(r => r >= BigInt(0))
            )
        )
        assert(lowered(bounded, FunctionTable(clamp)).leaves.size == 1)
        proven(bounded, 80, clamp)
        // a partial call, whose arguments are also those of the `denotes` that guards it
        proven(
          forAll[BigInt](x =>
              existsLet(x + BigInt(1))(y =>
                  whenReturns(clamp, (y, BigInt(0), BigInt(9)))(r => r <= BigInt(9))
              )
          ),
          80,
          clamp
        )
    }

    test("a function under an explicit witness is called through its own compiled program") {
        // A program that is not the one `Math.clamp` compiles to, registered under its name: a
        // test that calls the function gets this program, and not a copy made from the source.
        val standIn = FunctionDef(Math.clamp).withRepresentation(
          Representation.Uplc,
          PlutusV3
              .compile((x: BigInt, lo: BigInt, hi: BigInt) => BigInt(-1))(using
                UplcBlaster.options
              )
              .program
        )
        def holds(prop: Prop): Boolean =
            lowered(prop, FunctionTable(standIn)).leaves.head.term.evaluateDebug match
                case success: Result.Success => success.term == Term.Const(Constant.Bool(true))
                case failure: Result.Failure => fail(failure.exception)
        assert(holds(Prop(Math.clamp(BigInt(5), BigInt(0), BigInt(9)) == BigInt(-1))))
        // in the body of an `existsLet`, and in its witness
        assert(
          holds(existsLet(BigInt(5))(y => Math.clamp(y, BigInt(0), BigInt(9)) == BigInt(-1)))
        )
        assert(
          holds(existsLet(Math.clamp(BigInt(5), BigInt(0), BigInt(9)))(y => y == BigInt(-1)))
        )
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

    test("Blaster's error comes back in the failed result") {
        // For `e >= 0`, `exp2` reaches the bitwise builtins, whose `ByteString` is built on
        // `BitVec`, which Blaster cannot translate (README, Limitations). When it can, this test
        // needs another statement Blaster rejects.
        val exp2 = FunctionDef(Math.exp2)
        run(forAll[BigInt](e => callRef(exp2.ref, e)(r => r >= BigInt(0))), 120, Seq(exp2)) match
            case (verifier, _, VerificationResult.Failed(reason)) =>
                assert(reason.startsWith("Lean reported an error: "), reason)
                assert(reason.contains("Inductive datatype with instance parameters"), reason)
                assert(reason.contains("not supported: `BitVec"), reason)
                assert(!reason.contains("Successfully decoded"), reason)
                assert(verifier.theorems.isEmpty)
            case (_, _, other) => fail(s"expected a failed result, got $other")
    }

    test("a workspace without the ScalusProofs library is reported as failed") {
        requireLean()
        // The workspace's own toolchain, so elan does not look for a default one.
        val empty = Files.createTempDirectory("scalus-empty-lean-workspace-")
        Files.copy(leanWorkspace.resolve("lean-toolchain"), empty.resolve("lean-toolchain"))
        try
            val server = startLean(empty)
            val verifier = Verifier.empty
            val statement = verifier.statement(forAll[BigInt](x => x + BigInt(0) == x))
            try
                verifier.verify(statement, UplcBlaster(40, () => Right(server))) match
                    case VerificationResult.Failed(reason) =>
                        assert(reason.startsWith("Lean reported an error: "), reason)
                        assert(reason.contains("ScalusProofs"), reason)
                    case other => fail(s"expected a failed result, got $other")
            finally server.close()
            // A server that is closed takes no check, and the tactic says so.
            assert(
              verifier.verify(statement, UplcBlaster(40, () => Right(server))) ==
                  VerificationResult.Failed("the Lean server is closed")
            )
            // So it does where its provider has no server to give.
            assert(
              verifier.verify(statement, UplcBlaster(40, () => Left("no Lean here"))) ==
                  VerificationResult.Failed("no Lean here")
            )
        finally Directories.remove(empty)
    }

    test("a check sets Lean's limit of work, twice Lean's own unless told otherwise") {
        assert(UplcBlaster.defaultMaxHeartbeats == 400000)
        val goal = lowered(forAll[BigInt](x => x + BigInt(0) == x), FunctionTable.empty)
        val directory = Files.createTempDirectory("scalus-heartbeats-")
        try
            def written(check: java.nio.file.Path): String = Files.readString(check)
            assert(
              written(UplcBlaster.writeCheck(goal, 40, directory))
                  .contains("\nset_option maxHeartbeats 400000\n")
            )
            // none at all
            assert(
              written(UplcBlaster.writeCheck(goal, 40, 0, directory))
                  .contains("\nset_option maxHeartbeats 0\n")
            )
        finally Directories.remove(directory)

        // A limit no check keeps to. Lean gives the check up, and nothing is decided about the
        // statement, which the same tactic proves at its own limit.
        val verifier = Verifier.empty
        val statement = verifier.statement(forAll[BigInt](x => x + BigInt(0) == x))
        verifier.verify(statement, UplcBlaster(40, lean).withMaxHeartbeats(1)) match
            case VerificationResult.Inconclusive(reason) =>
                assert(reason.startsWith("Lean gave up at its own limit"), reason)
                assert(reason.contains("maximum number of heartbeats"), reason)
            case other => fail(s"expected an inconclusive result, got $other")
        assert(verifier.theorems.isEmpty)
        assert(
          verifier.verify(statement, UplcBlaster(40, lean)).isInstanceOf[VerificationResult.Proven]
        )
        assertThrows[IllegalArgumentException](UplcBlaster(40, lean).withMaxHeartbeats(-1))
    }

    test("a check has a time limit, of ten minutes unless told otherwise") {
        // No Lean is asked for: these are the tactic's own settings.
        assert(UplcBlaster.defaultTimeout == 10.minutes)
        assert(UplcBlaster(40, lean).timeout.contains(10.minutes))
        assert(UplcBlaster(40, lean, 30.seconds).timeout.contains(30.seconds))
        assert(UplcBlaster(40, lean).withoutTimeout.timeout.isEmpty)
        // One limit is set without the other being lost.
        assert(UplcBlaster(40, lean).withMaxHeartbeats(7).withoutTimeout.maxHeartbeats == 7)
        assert(UplcBlaster(40, lean, 30.seconds).withMaxHeartbeats(7).timeout.contains(30.seconds))
        assertThrows[IllegalArgumentException](UplcBlaster(40, lean, 0.seconds))
    }

    test("a check that is given up says how far it had come") {
        // No Lean is asked for: these are the tactic's readings of what the server reports. The
        // commands are those of checks the tactic writes, so a change of their text shows here.
        val directory = Files.createTempDirectory("scalus-progress-")
        def commands(goal: UplcBlaster.Lowered): List[String] =
            Files.readAllLines(UplcBlaster.writeCheck(goal, 40, directory)).asScala.toList
        def command(of: List[String], start: String): String =
            of.find(_.startsWith(start)).getOrElse(fail(s"no command $start in $of"))
        val (quantified, closed) =
            try
                (
                  commands(lowered(forAll[BigInt](x => x + BigInt(0) == x), FunctionTable.empty)),
                  commands(lowered(Prop(BigInt(1) + BigInt(1) == BigInt(2)), FunctionTable.empty))
                )
            finally Directories.remove(directory)

        val worker = Started("lean", 40.seconds, Some(38.seconds))
        def at(command: String, started: Started*): Progress =
            Progress(Reached.Command(command), 27.seconds, worker :: started.toList)

        // A symbolic run that goes on: the budget lets the program run through a loop.
        val running = UplcBlaster.unfinished(2, 12000, at(command(quantified, "#prep_uplc_run")))
        assert(running.startsWith("after 27 s it was still running the program of test 1 of 2"))
        assert(running.contains("symbolically, for 12000 steps"), running)
        assert(running.contains("had not come to the solver"), running)

        // The statement had come to Blaster, which simplifies it, and then starts the solver,
        // translates the statement for it and asks it.
        val statement = command(quantified, "#blaster")
        assert(
          UplcBlaster.unfinished(2, 400, at(statement)) ==
              "Blaster had been simplifying the statement for 27 s, and had not started the solver"
        )
        assert(
          UplcBlaster.unfinished(
            2,
            400,
            at(statement, Started("z3", 9.seconds, Some(8.seconds)))
          ) ==
              "Blaster had started the solver 9 s before, and the solver had worked for 8 s of them"
        )
        assert(
          UplcBlaster.unfinished(2, 400, at(statement, Started("z3", 9.seconds, None))) ==
              "Blaster had started the solver 9 s before"
        )

        // A closed statement is evaluated.
        assert(
          UplcBlaster.unfinished(1, 400, at(command(closed, "example"))) ==
              "Lean had been evaluating the statement for 27 s"
        )
        assert(
          UplcBlaster.unfinished(2, 400, at(command(quantified, "import"))) ==
              "it was still loading the libraries and the programs"
        )

        // What is no command of the check.
        def reached(where: Reached): String =
            UplcBlaster.unfinished(2, 400, Progress(where, Duration.Zero, Nil))
        assert(reached(Reached.NotStarted).startsWith("the check had not started"))
        assert(reached(Reached.Unreported) == "Lean had not said how far the check had come")
        assert(reached(Reached.End).startsWith("Lean had run every command of the check"))
    }

    test("a check is also written where it is asked to be kept") {
        val kept = Files.createTempDirectory("scalus-kept-checks-")
        try
            val goal = lowered(forAll[BigInt](x => x + BigInt(0) == x), FunctionTable.empty)
            val result =
                UplcBlaster.check(
                  goal,
                  40,
                  UplcBlaster.defaultMaxHeartbeats,
                  leanServer,
                  None,
                  Some(kept)
                )
            assert(result.isInstanceOf[VerificationResult.Proven], result)
            // one directory for the check, with the text and the program it refers to
            val List(directory) = Files.list(kept).iterator().asScala.toList
            val leaf = directory.resolve("Leaf0.flat")
            assert(Files.isRegularFile(leaf))
            assert(Files.readString(directory.resolve("Check.lean")).contains(leaf.toString))
        finally Directories.remove(kept)
    }

    test("statements outside the fragment are unsupported") {
        // The tactic reports these without running Lean. An enum has several constructors, so
        // it is not built from one set of fields.
        given Quantifiable[BlasterShape] = new Quantifiable[BlasterShape] {}
        val shape = forAll[BlasterShape](_ => true)
        val shapes = UplcBlaster.lower(shape, FunctionTable.empty)
        assert(shapes.left.exists(_.contains("BlasterShape binder")), shapes)

        // One constructor of `Data` is no type of Lean's: its variable would be any `Data`, and
        // the statement would be about values the Scala type does not have.
        val integers = UplcBlaster.lower(
          forAll[Data.I](i => i.value >= BigInt(0) || i.value < BigInt(0)),
          FunctionTable.empty
        )
        assert(integers.left.exists(_.contains("binder is not supported")), integers)

        // A partial call is split into two leaves, which a call's continuation cannot hold.
        val nested = UplcBlaster.lower(
          callRef(div10.ref, BigInt(1))(r => whenReturnsRef(div10.ref, r)(s => s == BigInt(1))),
          FunctionTable(div10)
        )
        assert(nested.left.exists(_.contains("whenReturns inside a call's continuation")), nested)

        // The verifier gives the reason as an unsupported result, and Lean is not asked.
        val verifier = Verifier.empty
        verifier.verify(verifier.statement(shape), UplcBlaster(10, lean)) match
            case VerificationResult.Unsupported(report) =>
                val reasons = report.issues.collect {
                    case CompatibilityIssue.UnsupportedFeature(_, reason) => reason
                }
                assert(reasons.exists(_.contains("BlasterShape binder")), report)
            case other => fail(s"expected an unsupported result, got $other")
    }
}
