package scalus.compiler.sir.lowering

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.compiler.{Compile, Options, UplcRepr, UplcRepresentation}
import scalus.cardano.onchain.plutus.prelude.{===, Eq, List, Option}
import scalus.uplc.PlutusV3
import scalus.uplc.Term.asTerm
import scalus.uplc.eval.PlutusVM

@Compile
object EqOpsHelpers:
    // Abstract element type `A` — exercises `equalsRepr` on an unresolved TypeVar element, the
    // case the dropped-`eq` path must handle for the dropped-`eq` ops.
    def genericIndexOf[A](xs: List[A], y: A)(using Eq[A]): BigInt = xs.indexOf(y)
    def genericOptionContains[A](o: Option[A], y: A)(using Eq[A]): Boolean = o.contains(y)
    def genericDeleteFirst[A](xs: List[A], y: A, expected: List[A])(using Eq[A]): Boolean =
        xs.deleteFirst(y) === expected
    def genericDistinct[A](xs: List[A], expected: List[A])(using Eq[A]): Boolean =
        xs.distinct === expected
    def genericDiff[A](xs: List[A], ys: List[A], expected: List[A])(using Eq[A]): Boolean =
        xs.diff(ys) === expected

/** Phase 2: `indexOf` and `Option.contains` drop their `eq` argument and compare via `equalsRepr`.
  * See docs/local/claude/compiler/v3-eq-eliminating.md.
  */
class EqOpsLoweringTest extends AnyFunSuite {

    private given PlutusVM = PlutusVM.makePlutusV3VM()
    private given Options = Options.release.copy(noWarn = true)

    test("indexOf on a case-class element list (UplcConstr, eq dropped)") {
        val compiled = PlutusV3.compile {
            List
                .Cons(CPoint(BigInt(1), BigInt(2)), List.single(CPoint(BigInt(3), BigInt(4))))
                .indexOf(CPoint(BigInt(3), BigInt(4)))
        }
        assert(compiled.program.term.evaluate == BigInt(1).asTerm)
    }

    test("indexOf — element not present returns -1") {
        val compiled = PlutusV3.compile {
            List.single(CPoint(BigInt(1), BigInt(2))).indexOf(CPoint(BigInt(9), BigInt(9)))
        }
        assert(compiled.program.term.evaluate == BigInt(-1).asTerm)
    }

    test("indexOf on a BigInt list (builtin/native repr)") {
        val compiled = PlutusV3.compile {
            List.Cons(BigInt(10), List.Cons(BigInt(20), List.single(BigInt(30))))
                .indexOf(BigInt(20))
        }
        assert(compiled.program.term.evaluate == BigInt(1).asTerm)
    }

    test("indexOf via a generic function (abstract element type)") {
        val compiled = PlutusV3.compile {
            EqOpsHelpers.genericIndexOf(
              List.single(CPoint(BigInt(5), BigInt(6))),
              CPoint(BigInt(5), BigInt(6))
            )
        }
        assert(compiled.program.term.evaluate == BigInt(0).asTerm)
    }

    test("Option.contains — Some matching (eq dropped)") {
        val compiled = PlutusV3.compile {
            Option.Some(CPoint(BigInt(1), BigInt(2))).contains(CPoint(BigInt(1), BigInt(2)))
        }
        assert(compiled.program.term.evaluate == true.asTerm)
    }

    test("Option.contains — Some not matching") {
        val compiled = PlutusV3.compile {
            Option.Some(CPoint(BigInt(1), BigInt(2))).contains(CPoint(BigInt(9), BigInt(9)))
        }
        assert(compiled.program.term.evaluate == false.asTerm)
    }

    test("Option.contains — None") {
        val compiled = PlutusV3.compile {
            (Option.None: Option[CPoint]).contains(CPoint(BigInt(1), BigInt(2)))
        }
        assert(compiled.program.term.evaluate == false.asTerm)
    }

    test("Option.contains via a generic function (abstract element type)") {
        val compiled = PlutusV3.compile {
            EqOpsHelpers.genericOptionContains(
              Option.Some(CPoint(BigInt(7), BigInt(8))),
              CPoint(BigInt(7), BigInt(8))
            )
        }
        assert(compiled.program.term.evaluate == true.asTerm)
    }

    // -- deleteFirst -------------------------------------------------------------------------

    test("deleteFirst on a BigInt list (native repr)") {
        val compiled = PlutusV3.compile {
            val xs: List[BigInt] =
                List.Cons(
                  BigInt(1),
                  List.Cons(BigInt(2), List.Cons(BigInt(2), List.single(BigInt(3))))
                )
            val expected: List[BigInt] =
                List.Cons(BigInt(1), List.Cons(BigInt(2), List.single(BigInt(3))))
            xs.deleteFirst(BigInt(2)) === expected
        }
        assert(compiled.program.term.evaluate == true.asTerm)
    }

    test("deleteFirst — element not present leaves the list unchanged") {
        val compiled = PlutusV3.compile {
            val xs: List[BigInt] = List.Cons(BigInt(1), List.single(BigInt(2)))
            xs.deleteFirst(BigInt(9)) === xs
        }
        assert(compiled.program.term.evaluate == true.asTerm)
    }

    test("deleteFirst on a case-class list (Data repr)") {
        val compiled = PlutusV3.compile {
            val xs: List[CPoint] = List.Cons(
              CPoint(BigInt(1), BigInt(1)),
              List.Cons(CPoint(BigInt(2), BigInt(2)), List.single(CPoint(BigInt(2), BigInt(2))))
            )
            val expected: List[CPoint] =
                List.Cons(CPoint(BigInt(1), BigInt(1)), List.single(CPoint(BigInt(2), BigInt(2))))
            xs.deleteFirst(CPoint(BigInt(2), BigInt(2))) === expected
        }
        assert(compiled.program.term.evaluate == true.asTerm)
    }

    test("deleteFirst via a generic function (abstract element type)") {
        val compiled = PlutusV3.compile {
            EqOpsHelpers.genericDeleteFirst(
              List.Cons(CPoint(BigInt(1), BigInt(1)), List.single(CPoint(BigInt(2), BigInt(2)))),
              CPoint(BigInt(1), BigInt(1)),
              List.single(CPoint(BigInt(2), BigInt(2)))
            )
        }
        assert(compiled.program.term.evaluate == true.asTerm)
    }

    // -- distinct ----------------------------------------------------------------------------

    test("distinct on a BigInt list (native repr)") {
        val compiled = PlutusV3.compile {
            val xs: List[BigInt] = List.Cons(
              BigInt(1),
              List.Cons(
                BigInt(2),
                List.Cons(BigInt(1), List.Cons(BigInt(3), List.single(BigInt(2))))
              )
            )
            val expected: List[BigInt] =
                List.Cons(BigInt(1), List.Cons(BigInt(2), List.single(BigInt(3))))
            xs.distinct === expected
        }
        assert(compiled.program.term.evaluate == true.asTerm)
    }

    test("distinct on a case-class list (Data repr)") {
        val compiled = PlutusV3.compile {
            val xs: List[CPoint] = List.Cons(
              CPoint(BigInt(1), BigInt(1)),
              List.Cons(CPoint(BigInt(2), BigInt(2)), List.single(CPoint(BigInt(1), BigInt(1))))
            )
            val expected: List[CPoint] =
                List.Cons(CPoint(BigInt(1), BigInt(1)), List.single(CPoint(BigInt(2), BigInt(2))))
            xs.distinct === expected
        }
        assert(compiled.program.term.evaluate == true.asTerm)
    }

    test("distinct via a generic function (abstract element type)") {
        val compiled = PlutusV3.compile {
            EqOpsHelpers.genericDistinct(
              List.Cons(BigInt(5), List.Cons(BigInt(5), List.single(BigInt(6)))),
              List.Cons(BigInt(5), List.single(BigInt(6)))
            )
        }
        assert(compiled.program.term.evaluate == true.asTerm)
    }

    // -- diff --------------------------------------------------------------------------------

    test("diff on a BigInt list (native repr)") {
        val compiled = PlutusV3.compile {
            val xs: List[BigInt] =
                List.Cons(BigInt(1), List.Cons(BigInt(2), List.single(BigInt(3))))
            val ys: List[BigInt] = List.single(BigInt(2))
            val expected: List[BigInt] = List.Cons(BigInt(1), List.single(BigInt(3)))
            xs.diff(ys) === expected
        }
        assert(compiled.program.term.evaluate == true.asTerm)
    }

    test("diff on a case-class list (Data repr)") {
        val compiled = PlutusV3.compile {
            val xs: List[CPoint] = List.Cons(
              CPoint(BigInt(1), BigInt(1)),
              List.Cons(CPoint(BigInt(2), BigInt(2)), List.single(CPoint(BigInt(3), BigInt(3))))
            )
            val ys: List[CPoint] = List.single(CPoint(BigInt(2), BigInt(2)))
            val expected: List[CPoint] =
                List.Cons(CPoint(BigInt(1), BigInt(1)), List.single(CPoint(BigInt(3), BigInt(3))))
            xs.diff(ys) === expected
        }
        assert(compiled.program.term.evaluate == true.asTerm)
    }

    test("diff via a generic function (abstract element type)") {
        val compiled = PlutusV3.compile {
            EqOpsHelpers.genericDiff(
              List.Cons(BigInt(1), List.Cons(BigInt(2), List.single(BigInt(3)))),
              List.single(BigInt(2)),
              List.Cons(BigInt(1), List.single(BigInt(3)))
            )
        }
        assert(compiled.program.term.evaluate == true.asTerm)
    }

    // -- UplcConstr repr (annotated element type) --------------------------------------------
    //
    // A `@UplcRepr(UplcConstr)`-annotated `val` makes the list use SumUplcConstr repr (case classes
    // are Data-repr by default, UplcConstr only when annotated). This drives the
    // `UplcConstrListOperations.{deleteFirst,distinct,diff,indexOf}` leaves. We assert via the
    // intrinsified scalar ops (`length`/`indexOf`) rather than whole-list `===`: the named generic
    // `listEq` over a UplcConstr list hits the separate UplcConstr→Data boundary gap.

    test("indexOf / deleteFirst on a UplcConstr-annotated case-class list") {
        val compiled = PlutusV3.compile {
            @UplcRepr(UplcRepresentation.UplcConstr)
            val xs: List[CPoint] = List.Cons(
              CPoint(BigInt(1), BigInt(1)),
              List.Cons(CPoint(BigInt(2), BigInt(2)), List.single(CPoint(BigInt(2), BigInt(2))))
            )
            // delete first (2,2) -> [(1,1), (2,2)]
            val r = xs.deleteFirst(CPoint(BigInt(2), BigInt(2)))
            xs.indexOf(CPoint(BigInt(2), BigInt(2))) === BigInt(1) &&
            r.length === BigInt(2) &&
            r.indexOf(CPoint(BigInt(1), BigInt(1))) === BigInt(0) &&
            r.indexOf(CPoint(BigInt(2), BigInt(2))) === BigInt(1)
        }
        assert(compiled.program.term.evaluate == true.asTerm)
    }

    test("distinct / diff on a UplcConstr-annotated case-class list") {
        val compiled = PlutusV3.compile {
            @UplcRepr(UplcRepresentation.UplcConstr)
            val xs: List[CPoint] = List.Cons(
              CPoint(BigInt(1), BigInt(1)),
              List.Cons(CPoint(BigInt(2), BigInt(2)), List.single(CPoint(BigInt(1), BigInt(1))))
            )
            @UplcRepr(UplcRepresentation.UplcConstr)
            val ys: List[CPoint] = List.single(CPoint(BigInt(2), BigInt(2)))
            val d = xs.distinct // -> [(1,1), (2,2)]
            val f = xs.diff(ys) // -> [(1,1), (1,1)]
            d.length === BigInt(2) &&
            d.indexOf(CPoint(BigInt(2), BigInt(2))) === BigInt(1) &&
            f.length === BigInt(2) &&
            f.indexOf(CPoint(BigInt(2), BigInt(2))) === BigInt(-1)
        }
        assert(compiled.program.term.evaluate == true.asTerm)
    }
}
