package scalus.uplc.internal

import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.cardano.ledger.Word64
import scalus.uplc.Term.*
import scalus.uplc.transform.TermAnalysis.freeVars
import scalus.uplc.{Constant, DefaultFun, NamedDeBruijn, Term}

import scala.collection.mutable.ArrayBuffer
import scalus.uplc.internal.TermTraversal.{exists, foldLeft, foreach, foreachChild, foreachChildIndexed, mapChildren, mapChildrenIndexed, nodeCount, transformDown, transformUp, visit}

/** Tests for [[TermTraversal]]. Rule IDs refer to
  * docs/superpowers/specs/2026-10-27-term-traversal-design.md.
  */
class TermTraversalTest extends AnyFunSuite with ScalaCheckPropertyChecks {
    import TermTraversalTest.*

    given PropertyCheckConfiguration = PropertyCheckConfiguration(minSuccessful = 300)

    // ---- child order --------------------------------------------------------------------------

    test("[TT-1] foreach visits Apply function before argument, Case scrutinee before branches") {
        val term = Apply(f, Case(s, List(b0, b1)))
        val seen = ArrayBuffer.empty[Term]
        term.foreach(seen += _)
        assert(seen.toList == List(term, f, Case(s, List(b0, b1)), s, b0, b1))
    }

    test("[TT-1] foreachChild gives Constr arguments in order, and nothing for a leaf") {
        val seen = ArrayBuffer.empty[Term]
        Constr(Word64(0), List(b0, b1, s)).foreachChild(seen += _)
        s.foreachChild(seen += _)
        assert(seen.toList == List(b0, b1, s))
    }

    test("[TT-2] mapChildrenIndexed numbers children from 0 in child order") {
        def indices(t: Term): List[(Term, Int)] = {
            val seen = ArrayBuffer.empty[(Term, Int)]
            t.mapChildrenIndexed { (c, i) =>
                seen += c -> i
                c
            }
            seen.toList
        }
        assert(indices(Apply(f, s)) == List(f -> 0, s -> 1))
        assert(indices(Case(s, List(b0, b1))) == List(s -> 0, b0 -> 1, b1 -> 2))
        assert(indices(Constr(Word64(1), List(b0, b1))) == List(b0 -> 0, b1 -> 1))
        assert(indices(LamAbs("x", s)) == List(s -> 0))
        assert(indices(Force(s)) == List(s -> 0))
        assert(indices(Delay(s)) == List(s -> 0))
        assert(indices(Error()) == Nil)
    }

    test("[TT-2] foreachChildIndexed numbers children like mapChildrenIndexed") {
        forAll(genTerm) { t =>
            val visited = ArrayBuffer.empty[(Term, Int)]
            t.foreachChildIndexed((c, i) => visited += c -> i)
            val mapped = ArrayBuffer.empty[(Term, Int)]
            t.mapChildrenIndexed { (c, i) =>
                mapped += c -> i
                c
            }
            assert(visited.map(_._2) == mapped.map(_._2))
            assert(visited.lazyZip(mapped).forall((a, b) => a._1 eq b._1))
        }
        val seen = ArrayBuffer.empty[(Term, Int)]
        Case(s, List(b0, b1)).foreachChildIndexed((c, i) => seen += c -> i)
        assert(seen.toList == List(s -> 0, b0 -> 1, b1 -> 2))
    }

    // ---- traversal shape ----------------------------------------------------------------------

    test("[TT-3] [TT-5] visit enters a node before its children and exits it after them") {
        val inner = Delay(s)
        val term = Apply(LamAbs("x", inner), b0)
        val events = ArrayBuffer.empty[String]
        term.visit { n =>
            events += s"enter ${label(n)}"
            true
        } { n => events += s"exit ${label(n)}" }
        assert(
          events.toList == List(
            "enter Apply",
            "enter LamAbs",
            "enter Delay",
            "enter s",
            "exit s",
            "exit Delay",
            "exit LamAbs",
            "enter b0",
            "exit b0",
            "exit Apply"
          )
        )
    }

    test("[TT-4] [TT-5] visit skips the children of a node whose enter returns false") {
        val term = Apply(LamAbs("x", Delay(s)), b0)
        val entered = ArrayBuffer.empty[String]
        val exited = ArrayBuffer.empty[String]
        term.visit { n =>
            entered += label(n)
            !n.isInstanceOf[LamAbs]
        } { n => exited += label(n) }
        assert(entered.toList == List("Apply", "LamAbs", "b0"))
        assert(exited.toList == List("b0", "Apply"))
    }

    test("[TT-6] foreach visits every node in pre-order") {
        forAll(genTerm) { t =>
            val seen = ArrayBuffer.empty[Term]
            t.foreach(seen += _)
            assert(sameNodes(seen.toList, preorder(t)))
        }
    }

    test("[TT-7] [TT-21] exists stops at the first match and tests no later node") {
        val term = Apply(Apply(f, s), Case(b0, List(b1)))
        var tested = 0
        val found = term.exists { n =>
            tested += 1
            n eq s
        }
        assert(found)
        // Apply, Apply, f, s: four nodes, then stop.
        assert(tested == 4)
    }

    test("[TT-7] exists agrees with a recursive reference") {
        forAll(genTerm) { t =>
            assert(t.exists(_.isInstanceOf[Delay]) == preorder(t).exists {
                case _: Delay => true
                case _        => false
            })
            assert(!t.exists(_ => false))
        }
    }

    test("[TT-8] foldLeft applies f in pre-order") {
        forAll(genTerm) { t =>
            val seen = t.foldLeft(List.empty[Term])((acc, n) => n :: acc).reverse
            assert(sameNodes(seen, preorder(t)))
        }
    }

    test("nodeCount counts every node once, a leaf too") {
        assert(f.nodeCount == 1)
        assert(Apply(f, s).nodeCount == 3)
        assert(Case(s, List(b0, Delay(b1))).nodeCount == 5)
        forAll(genTerm)(t => assert(t.nodeCount == preorder(t).size))
    }

    test("[TT-9] transformUp rewrites children before their parent") {
        // The parent sees its child already rewritten: Force(Delay(1)) -> Force(Delay(2)) -> 2.
        val term = Force(Delay(int(1)))
        val result = term.transformUp(incrementThenCancel)
        assert(result == int(2))
    }

    test("[TT-9] transformUp agrees with a recursive reference") {
        forAll(genTerm) { t =>
            assert(t.transformUp(incrementThenCancel) == refUp(t))
        }
    }

    test("[TT-10] [TT-11] transformDown walks the result of f and does not call f on it again") {
        // Spec example: f turns the root into Force(Delay(c)), then walks Delay(c) and c only.
        val c = int(7)
        val term = Force(Delay(Force(Delay(c))))
        val result = term.transformDown(cancelForceDelay)
        assert(result == Force(Delay(c)))
    }

    test("[TT-10] transformDown agrees with a recursive reference") {
        forAll(genTerm) { t =>
            assert(t.transformDown(cancelForceDelay) == refDown(t))
        }
    }

    test("[TT-11] transformUp calls f exactly once per node") {
        forAll(genTerm) { t =>
            var calls = 0
            t.transformUp { n =>
                calls += 1
                n
            }
            assert(calls == preorder(t).size)
        }
    }

    // ---- identity preservation ----------------------------------------------------------------

    test("[TT-12] rewrites return the input node when nothing changes") {
        forAll(genTerm) { t =>
            assert(t.transformUp(n => n) eq t)
            assert(t.transformDown(n => n) eq t)
            assert(t.mapChildren(n => n) eq t)
            assert(t.mapChildrenIndexed((n, _) => n) eq t)
        }
    }

    test("[TT-12] a rewrite of one leaf rebuilds only the path above it") {
        val target = int(1)
        val untouched = Delay(s)
        val term = Apply(LamAbs("x", Case(untouched, List(b0, target))), b1)
        val result = term.transformUp(n => if n eq target then int(2) else n)
        val Apply(LamAbs(_, Case(scrutinee, List(first, second), _), _), arg, _) =
            result: @unchecked
        assert(second == int(2))
        assert(scrutinee eq untouched)
        assert(first eq b0)
        assert(arg eq b1)
    }

    test("[TT-13] a rewrite reuses an unchanged branch list when only the scrutinee changes") {
        val branches = List(b0, b1)
        val term = Case(s, branches)
        val down = term.transformDown(n => if n eq s then int(9) else n)
        val up = term.transformUp(n => if n eq s then int(9) else n)
        val mapped = term.mapChildren(n => if n eq s then int(9) else n)
        for result <- List(down, up, mapped) do
            val Case(scrutinee, cases, _) = result: @unchecked
            assert(scrutinee == int(9))
            assert(cases eq branches)
    }

    test("[TT-13] mapChildren copies a list once and keeps the unchanged prefix elements") {
        val args = List(b0, b1, s)
        val Constr(_, next, _) =
            Constr(Word64(0), args).mapChildren(n => if n eq b1 then int(5) else n): @unchecked
        assert(next == List(b0, int(5), s))
        assert(next.head eq b0)
        assert(next(2) eq s)
    }

    // ---- freeVars -------------------------------------------------------------------------

    test("freeVars agrees with TermAnalysis.freeVars, including shadowing") {
        forAll(genTerm) { t =>
            assert(TermTraversal.freeVars(t) == t.freeVars)
        }
        val shadowed = Apply(LamAbs("x", Apply(LamAbs("x", x), x)), x)
        assert(TermTraversal.freeVars(shadowed) == Set("x"))
        assert(TermTraversal.freeVars(LamAbs("x", LamAbs("x", x))) == Set.empty)
    }
}

object TermTraversalTest {
    val f: Term = Builtin(DefaultFun.AddInteger)
    val s: Term = Var(NamedDeBruijn("s"))
    val b0: Term = Const(Constant.Unit)
    val b1: Term = Error()
    val x: Term = Var(NamedDeBruijn("x"))

    def int(n: Int): Term = Const(Constant.Integer(n))

    def label(t: Term): String =
        if t eq s then "s"
        else if t eq b0 then "b0"
        else t.getClass.getSimpleName

    /** Recursive reference for pre-order. */
    def preorder(t: Term): List[Term] = {
        val out = List.newBuilder[Term]
        def go(t: Term): Unit = {
            out += t
            t match
                case LamAbs(_, body, _)    => go(body)
                case Apply(fn, arg, _)     => go(fn); go(arg)
                case Force(body, _)        => go(body)
                case Delay(body, _)        => go(body)
                case Constr(_, args, _)    => args.foreach(go)
                case Case(scrut, cases, _) => go(scrut); cases.foreach(go)
                case _                     => ()
        }
        go(t)
        out.result()
    }

    def sameNodes(a: List[Term], b: List[Term]): Boolean =
        a.size == b.size && a.lazyZip(b).forall(_ eq _)

    /** Adds 1 to integer constants, and turns `Force(Delay(t))` into `t`. */
    def incrementThenCancel(t: Term): Term = t match
        case Const(Constant.Integer(n), ann) => Const(Constant.Integer(n + 1), ann)
        case Force(Delay(inner, _), _)       => inner
        case other                           => other

    def cancelForceDelay(t: Term): Term = t match
        case Force(Delay(inner, _), _) => inner
        case other                     => other

    private def mapKids(t: Term)(g: Term => Term): Term = t match
        case LamAbs(n, body, a)    => LamAbs(n, g(body), a)
        case Apply(fn, arg, a)     => Apply(g(fn), g(arg), a)
        case Force(body, a)        => Force(g(body), a)
        case Delay(body, a)        => Delay(g(body), a)
        case Constr(tag, args, a)  => Constr(tag, args.map(g), a)
        case Case(scrut, cases, a) => Case(g(scrut), cases.map(g), a)
        case leaf                  => leaf

    def refUp(t: Term): Term = incrementThenCancel(mapKids(t)(refUp))

    def refDown(t: Term): Term = mapKids(cancelForceDelay(t))(refDown)

    /** Small name pool, so terms have free variables and shadowing. Lists of length 0 to 3. */
    val genTerm: Gen[Term] = {
        val name = Gen.oneOf("x", "y", "z")
        val leaf: Gen[Term] = Gen.oneOf(
          name.map(n => Var(NamedDeBruijn(n))),
          Gen.choose(0, 3).map(int),
          Gen.const(Builtin(DefaultFun.AddInteger)),
          Gen.const(Error())
        )
        def go(depth: Int): Gen[Term] =
            if depth <= 0 then leaf
            else
                val sub = Gen.lzy(go(depth - 1))
                Gen.frequency(
                  2 -> leaf,
                  1 -> Gen.zip(name, sub).map((n, b) => LamAbs(n, b)),
                  1 -> Gen.zip(sub, sub).map((a, b) => Apply(a, b)),
                  1 -> sub.map(Force(_)),
                  1 -> sub.map(Delay(_)),
                  1 -> Gen.choose(0, 3).flatMap(Gen.listOfN(_, sub)).map(Constr(Word64(0), _)),
                  1 -> Gen.zip(sub, Gen.choose(0, 3).flatMap(Gen.listOfN(_, sub))).map(Case(_, _))
                )
        Gen.choose(0, 6).flatMap(go)
    }
}
