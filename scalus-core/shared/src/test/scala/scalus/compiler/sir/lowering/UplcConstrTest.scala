package scalus.compiler.sir.lowering

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.compiler.{compile, Compile}
import scalus.compiler.Options
import scalus.cardano.onchain.plutus.prelude.*
import scalus.uplc.builtin.Data
import scalus.uplc.builtin.Data.{fromData, toData}
import scalus.uplc.{Constant, Term}
import scalus.compiler.{UplcRepr, UplcRepresentation}
import scalus.cardano.ledger.MajorProtocolVersion
import scalus.uplc.eval.{PlutusVM, Result}

/** Data-compatible case class with @UplcRepr(UplcConstr). */
@UplcRepr(UplcRepresentation.UplcConstr)
case class Point(x: BigInt, y: BigInt)

/** Container with a UplcConstr list of UplcConstr Points. */
@UplcRepr(UplcRepresentation.UplcConstr)
case class PointSet(
    name: BigInt,
    @UplcRepr(UplcRepresentation.UplcConstr)
    points: List[Point]
)

@Compile
object PointModule {
    given Eq[Point] = Eq.derived

    def mkPoint(x: BigInt, y: BigInt): Point = Point(x, y)
    def sumFields(p: Point): BigInt = p.x + p.y
    def eqPoints(a: Point, b: Point): Boolean = a === b
    def listContainsPoint(lst: List[Point], p: Point): Boolean = lst.contains(p)

    def mkPointSet(name: BigInt, pts: List[Point]): PointSet = PointSet(name, pts)
    def pointSetContains(ps: PointSet, p: Point): Boolean = ps.points.contains(p)
    def pointSetSize(ps: PointSet): BigInt = ps.points.length
    def pointSetPrepend(ps: PointSet, p: Point): PointSet =
        PointSet(ps.name, ps.points.prepended(p))
    def pointSetReverse(ps: PointSet): List[Point] = ps.points.reverse
    def pointSetFilter(ps: PointSet, x: BigInt): List[Point] =
        ps.points.filter(p => p.x === x)
}

enum Action:
    case Const(value: BigInt)
    case Transform(f: BigInt => BigInt)

@Compile
object ActionModule {
    def run(action: Action, input: BigInt): BigInt = action match
        case Action.Const(v)     => v
        case Action.Transform(f) => f(input)
}

/** A case class with two function fields: decoder and encoder for type T. Cannot be Data-encoded
  * (functions aren't Data), so uses ProdUplcConstr.
  */
case class Codec[T](decode: Data => Option[T], encode: T => Data)

// TODO: test SumUplcConstr → DataConstr when @UplcRepr("UplcConstr") annotation is wired.
// Requires a data-compatible enum annotated to use UplcConstr, stored in a Data-encoded field.

@Compile
object CodecModule {
    import Option.*

    def roundtrip[T](codec: Codec[T], d: Data): Option[Data] =
        codec.decode(d) match
            case Some(value) => Some(codec.encode(value))
            case None        => None
}

/** Tests for UplcConstr representation — case classes/enums with function fields.
  *
  * Case classes containing function fields (like `BigInt => BigInt`) cannot use ProdDataConstr
  * because functions can't be converted to Data. They use ProdUplcConstr/SumUplcConstr instead,
  * stored as UPLC Constr(tag, [fields]).
  */
class UplcConstrTest extends AnyFunSuite {

    private given PlutusVM = PlutusVM.makePlutusV3VM()
    private given Options = Options()

    test("case class with two function fields: codec roundtrip") {
        val sir = compile {
            import Option.*

            val intCodec = Codec[BigInt](
              decode = d => Some(fromData[BigInt](d)),
              encode = x => toData[BigInt](x)
            )

            // roundtrip: decode Data(42) → Some(42) → encode → Some(Data(42))
            CodecModule.roundtrip(intCodec, toData[BigInt](BigInt(42))) match
                case Some(d) => fromData[BigInt](d)
                case None    => BigInt(0)
        }
        val result = sir.toUplc().evaluateDebug
        result match
            case Result.Success(Term.Const(Constant.Integer(v), _), _, _, _) =>
                assert(v == 42, s"Expected 42, got $v")
            case Result.Success(term, _, _, _) =>
                fail(s"Expected Integer constant, got ${term.show}")
            case Result.Failure(ex, _, _, _) =>
                fail(s"Codec roundtrip failed: $ex")
    }

    test("codec with ByteString type") {
        val sir = compile {
            import Option.*
            import scalus.uplc.builtin.ByteString

            val bsCodec = Codec[ByteString](
              decode = d => Some(fromData[ByteString](d)),
              encode = x => toData[ByteString](x)
            )

            val testBs = ByteString.fromString("hello")
            CodecModule.roundtrip(bsCodec, toData[ByteString](testBs)) match
                case Some(d) => fromData[ByteString](d)
                case None    => ByteString.empty
        }
        val result = sir.toUplc().evaluateDebug
        result match
            case Result.Success(Term.Const(Constant.ByteString(bs), _), _, _, _) =>
                assert(
                  bs == scalus.uplc.builtin.ByteString.fromString("hello"),
                  s"Expected 'hello', got $bs"
                )
            case Result.Success(term, _, _, _) =>
                fail(s"Expected ByteString constant, got ${term.show}")
            case Result.Failure(ex, _, _, _) =>
                fail(s"ByteString codec failed: $ex")
    }

    test("enum with function variant: construct and match") {
        val sir = compile {
            val a1 = Action.Const(BigInt(10))
            val a2 = Action.Transform(x => x + BigInt(1))

            // run(Const(10), 5) + run(Transform(_ + 1), 5)  = 10 + 6 = 16
            ActionModule.run(a1, BigInt(5)) + ActionModule.run(a2, BigInt(5))
        }
        val result = sir.toUplc().evaluateDebug
        result match
            case Result.Success(Term.Const(Constant.Integer(v), _), _, _, _) =>
                assert(v == 16, s"Expected 16, got $v")
            case Result.Success(term, _, _, _) =>
                fail(s"Expected Integer constant, got ${term.show}")
            case Result.Failure(ex, _, _, _) =>
                fail(s"UplcConstr enum with function variant failed: $ex")
    }

    // === @UplcRepr(UplcConstr) Data-compatible tests ===

    test("UplcConstr: construct and access fields") {
        val sir = compile {
            val p = PointModule.mkPoint(BigInt(3), BigInt(4))
            PointModule.sumFields(p)
        }
        val result = sir.toUplc().evaluateDebug
        result match
            case Result.Success(Term.Const(Constant.Integer(v), _), _, _, _) =>
                assert(v == 7, s"Expected 7, got $v")
            case Result.Failure(ex, _, _, _) =>
                fail(s"UplcConstr field access failed: $ex")
            case other => fail(s"Unexpected: $other")
    }

    test("UplcConstr: equality comparison") {
        val sir = compile {
            val a = PointModule.mkPoint(BigInt(1), BigInt(2))
            val b = PointModule.mkPoint(BigInt(1), BigInt(2))
            val c = PointModule.mkPoint(BigInt(3), BigInt(4))
            // a === b should be true, a === c should be false
            // encode as: if a===b && !(a===c) then 1 else 0
            if PointModule.eqPoints(a, b) && !PointModule.eqPoints(a, c) then BigInt(1)
            else BigInt(0)
        }
        val result = sir.toUplc().evaluateDebug
        result match
            case Result.Success(Term.Const(Constant.Integer(v), _), _, _, _) =>
                assert(v == 1, s"Expected 1, got $v")
            case Result.Failure(ex, _, _, _) =>
                fail(s"UplcConstr equality failed: $ex")
            case other => fail(s"Unexpected: $other")
    }

    test("UplcConstr: list contains") {
        val sir = compile {
            val lst = List(
              PointModule.mkPoint(BigInt(1), BigInt(2)),
              PointModule.mkPoint(BigInt(3), BigInt(4)),
              PointModule.mkPoint(BigInt(5), BigInt(6))
            )
            val yes = PointModule.listContainsPoint(lst, PointModule.mkPoint(BigInt(3), BigInt(4)))
            val no = PointModule.listContainsPoint(lst, PointModule.mkPoint(BigInt(7), BigInt(8)))
            if yes && !no then BigInt(1) else BigInt(0)
        }
        val result = sir.toUplc().evaluateDebug
        result match
            case Result.Success(Term.Const(Constant.Integer(v), _), _, _, _) =>
                assert(v == 1, s"Expected 1, got $v")
            case Result.Failure(ex, _, _, _) =>
                fail(s"UplcConstr list contains failed: $ex")
            case other => fail(s"Unexpected: $other")
    }

    test("UplcConstr: field-level @UplcRepr for native list") {
        val sir = compile {
            // Build list with Cons/Nil to match UplcConstr representation directly
            val pts: List[Point] = List.Cons(
              PointModule.mkPoint(BigInt(1), BigInt(2)),
              List.Cons(PointModule.mkPoint(BigInt(3), BigInt(4)), List.Nil)
            )
            val ps = PointModule.mkPointSet(BigInt(42), pts)
            val contains =
                PointModule.pointSetContains(ps, PointModule.mkPoint(BigInt(3), BigInt(4)))
            if contains then BigInt(1) else BigInt(0)
        }
        java.nio.file.Files
            .writeString(java.nio.file.Path.of("/tmp/pointset_sir.txt"), sir.pretty.render(200))
        val lw = sir.toLoweredValue()
        java.nio.file.Files
            .writeString(java.nio.file.Path.of("/tmp/pointset_lowered.txt"), lw.pretty.render(200))
        val uplc = sir.toUplc()
        java.nio.file.Files
            .writeString(java.nio.file.Path.of("/tmp/pointset_uplc.txt"), uplc.pretty.render(120))
        val result = uplc.evaluateDebug
        result match
            case Result.Success(Term.Const(Constant.Integer(v), _), _, _, _) =>
                assert(v == 1, s"Expected 1, got $v")
            case Result.Failure(ex, _, _, _) =>
                fail(s"UplcConstr field-level native list failed: $ex")
            case other => fail(s"Unexpected: $other")
    }

    test("UplcConstr: list prepend and contains on modified list") {
        val sir = compile {
            val pts: List[Point] = List.Cons(
              PointModule.mkPoint(BigInt(1), BigInt(2)),
              List.Cons(PointModule.mkPoint(BigInt(3), BigInt(4)), List.Nil)
            )
            val ps = PointModule.mkPointSet(BigInt(42), pts)
            // Prepend a new point, then check contains
            val ps2 = PointModule.pointSetPrepend(ps, PointModule.mkPoint(BigInt(5), BigInt(6)))
            val has5 = PointModule.pointSetContains(ps2, PointModule.mkPoint(BigInt(5), BigInt(6)))
            val size = PointModule.pointSetSize(ps2)
            if has5 && size === BigInt(3) then BigInt(1) else BigInt(0)
        }
        val result = sir.toUplc().evaluateDebug
        result match
            case Result.Success(Term.Const(Constant.Integer(v), _), _, _, _) =>
                assert(v == 1, s"Expected 1, got $v")
            case Result.Failure(ex, _, _, _) =>
                fail(s"UplcConstr list prepend+contains failed: $ex")
            case other => fail(s"Unexpected: $other")
    }

    test("UplcConstr: list dropRight/init on native list") {
        val sir = compile {
            val pts: List[Point] = List.Cons(
              PointModule.mkPoint(BigInt(1), BigInt(2)),
              List.Cons(
                PointModule.mkPoint(BigInt(3), BigInt(4)),
                List.Cons(PointModule.mkPoint(BigInt(5), BigInt(6)), List.Nil)
              )
            )
            val ps = PointModule.mkPointSet(BigInt(42), pts)
            // init removes the last element
            val initPts = ps.points.init
            initPts.length
        }
        java.nio.file.Files
            .writeString(java.nio.file.Path.of("/tmp/init_sir.txt"), sir.pretty.render(200))
        val lw = sir.toLoweredValue()
        java.nio.file.Files
            .writeString(java.nio.file.Path.of("/tmp/init_lowered.txt"), lw.pretty.render(200))
        val uplc = sir.toUplc()
        java.nio.file.Files
            .writeString(java.nio.file.Path.of("/tmp/init_uplc.txt"), uplc.pretty.render(120))
        val result = uplc.evaluateDebug
        result match
            case Result.Success(Term.Const(Constant.Integer(v), _), _, _, _) =>
                assert(v == 2, s"Expected 2, got $v")
            case Result.Failure(ex, _, _, _) =>
                fail(s"UplcConstr list dropRight/init failed: $ex")
            case other => fail(s"Unexpected: $other")
    }

    test("@UplcRepr parameter: match extracts value and constructs Option.Some") {
        val sir = compile {
            extension [A](@UplcRepr(UplcRepresentation.UplcConstr) self: List[A])
                def headOption2: Option[A] =
                    self match
                        case List.Nil            => Option.None
                        case List.Cons(value, _) => Option.Some(value)

            val pts = List(Point(1, 2), Point(3, 4))
            val ps = PointModule.mkPointSet(BigInt(1), pts)
            val result = ps.points.headOption2
            result match
                case Option.Some(p) => p.x
                case Option.None    => BigInt(-1)
        }
        val result = sir.toUplc().evaluateDebug
        result match
            case Result.Success(Term.Const(Constant.Integer(v), _), _, _, _) =>
                assert(v == 1, s"Expected 1, got $v")
            case Result.Success(term, _, _, _) =>
                fail(s"Expected Integer constant, got ${term.show}")
            case Result.Failure(ex, _, _, _) =>
                fail(s"@UplcRepr parameter headOption2 failed: $ex")
    }

    test("UplcConstr: prepended preserves repr through Cast") {
        val sir = compile {
            // Reproduces the issue: Cons(tile, visited) is Cast to List[Point],
            // which strips UplcConstr repr. Then PointSet construction tries
            // builtinListToUplcConstr on an already-UplcConstr runtime value.
            val ps = PointModule.mkPointSet(BigInt(1), List(Point(1, 2)))
            val newPt = Point(3, 4)
            val ps2 = PointModule.pointSetPrepend(ps, newPt)
            PointModule.pointSetSize(ps2)
        }
        val result = sir.toUplc().evaluateDebug
        result match
            case Result.Success(Term.Const(Constant.Integer(v), _), _, _, _) =>
                assert(v == 2, s"Expected 2, got $v")
            case Result.Success(term, _, _, _) =>
                fail(s"Expected Integer constant, got ${term.show}")
            case Result.Failure(ex, _, _, _) =>
                fail(s"UplcConstr prepended through Cast failed: $ex")
    }

    test("UplcConstr: flatMap from BuiltinList to UplcConstr list") {
        val sir = compile {
            // BuiltinList[BigInt].flatMap returning List[Point] (UplcConstr)
            // This crosses repr boundaries: input is BuiltinList, output is SumUplcConstr
            val numbers = List(BigInt(1), BigInt(2))
            val result = numbers.flatMap { n =>
                List(Point(n, n + 1))
            }
            result.length
        }
        val result = sir.toUplc().evaluateDebug
        result match
            case Result.Success(Term.Const(Constant.Integer(v), _), _, _, _) =>
                assert(v == 2, s"Expected 2, got $v")
            case Result.Failure(ex, _, _, _) =>
                fail(s"UplcConstr cross-repr flatMap failed: $ex")
            case other => fail(s"Unexpected: $other")
    }

    test("UplcConstr: nested flatMap+map like KnightsTest.makeStarts") {
        val sir = compile {
            // Reproduces the KnightsTest.makeStarts pattern:
            // List[BigInt].flatMap { x => List[BigInt].map { y => UplcConstrType } }
            // Then zip + map on the result
            val it = List(BigInt(1), BigInt(2), BigInt(3))
            val l = it.flatMap { x => it.map { y => Point(x, y) } }
            val length = l.length
            require(length == BigInt(9))
            val fill = List.fill(BigInt(1), length)
            val zipped = fill.zip(l)
            val result = zipped.map { pair => PointModule.sumFields(pair._2) }
            result.length
        }
        val result = sir.toUplc().evaluateDebug
        result match
            case Result.Success(Term.Const(Constant.Integer(v), _), _, _, _) =>
                assert(v == 9, s"Expected 9, got $v")
            case Result.Failure(ex, _, _, _) =>
                fail(s"UplcConstr nested flatMap+map failed: $ex")
            case other => fail(s"Unexpected: $other")
    }

    test("tag > 0 constructor: direct use of Transform variant") {
        val sir = compile {
            // Action.Transform has tag=1 — exercises Case branch padding
            val t = Action.Transform(x => x + BigInt(1))
            ActionModule.run(t, BigInt(5))
        }
        val result = sir.toUplc().evaluateDebug
        result match
            case Result.Success(Term.Const(Constant.Integer(v), _), _, _, _) =>
                assert(v == 6, s"Expected 6, got $v")
            case Result.Success(term, _, _, _) =>
                fail(s"Expected Integer constant, got ${term.show}")
            case Result.Failure(ex, _, _, _) =>
                fail(s"Tag > 0 constructor failed: $ex")
    }

}
