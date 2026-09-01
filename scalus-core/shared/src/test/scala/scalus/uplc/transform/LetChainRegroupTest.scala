package scalus.uplc.transform

import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.Term.*
import scalus.uplc.{Constant, DefaultFun, Term}

class LetChainRegroupTest extends AnyFunSuite {

    /** `[(lam name body) rhs]` — a let, as lowered UPLC encodes it. */
    private def let(name: String, rhs: Term)(body: Term): Term = Apply(LamAbs(name, body), rhs)

    private def c(n: Int): Term = Const(Constant.Integer(BigInt(n)))

    /** `[[[(lam n1 (lam n2 (lam n3 body))) a1] a2] a3]` — what a grouped run looks like. */
    private def group(names: List[String], args: List[Term])(body: Term): Term =
        args.foldLeft(names.foldRight(body)(LamAbs(_, _)))(Apply(_, _))

    /** The shape tests below exercise the grouping logic, so they lower the profitability
      * threshold; the default threshold has its own tests.
      */
    private def regroup(t: Term): Term = new LetChainRegroup(minRunSize = 3)(t)

    test("a run of three independent lets becomes one three-argument application") {
        val input = let("a", c(1))(let("b", c(2))(let("c", c(3))(vr"a")))
        val expected = group(List("a", "b", "c"), List(c(1), c(2), c(3)))(vr"a")
        assert(regroup(input) ~=~ expected)
    }

    test("a dependency closes the run and a new one starts at the dependent binding") {
        // b's rhs mentions a, so a cannot join b's group
        val input =
            let("a", c(1))(let("b", vr"a")(let("c", c(3))(let("d", c(4))(vr"b"))))
        val expected =
            let("a", c(1))(group(List("b", "c", "d"), List(vr"a", c(3), c(4)))(vr"b"))
        assert(regroup(input) ~=~ expected)
    }

    test("a repeated binder name closes the run") {
        val input = let("a", c(1))(
          let("b", c(2))(let("c", c(3))(let("a", c(4))(let("d", c(5))(let("e", c(6))(vr"a")))))
        )
        val expected = group(List("a", "b", "c"), List(c(1), c(2), c(3)))(
          group(List("a", "d", "e"), List(c(4), c(5), c(6)))(vr"a")
        )
        assert(regroup(input) ~=~ expected)
    }

    test("runs shorter than the threshold are left in their original nested shape") {
        val input = let("a", c(1))(let("b", vr"a")(vr"b"))
        assert(regroup(input) ~=~ input)
    }

    /** A group of N saves N-2 machine steps but costs about one script byte. At mainnet rates a
      * step is 6.92 lovelace and a byte is 15, so N must reach 5 to pay for itself.
      */
    private def chainOf(n: Int): Term =
        (1 to n).foldRight(vr"a")((i, body) => let(s"v$i", c(i))(body))

    test("by default a run of four is left nested — it would not pay for its byte") {
        val input = chainOf(4)
        assert(LetChainRegroup(input) ~=~ input)
    }

    test("by default a run of five is grouped") {
        val input = chainOf(5)
        val expected =
            group(List("v1", "v2", "v3", "v4", "v5"), List(c(1), c(2), c(3), c(4), c(5)))(vr"a")
        assert(LetChainRegroup(input) ~=~ expected)
    }

    test("regrouping is idempotent") {
        val input = let("a", c(1))(let("b", c(2))(let("c", c(3))(vr"a")))
        val once = regroup(input)
        assert(regroup(once) ~=~ once)
    }

    test("chains inside a bound expression are regrouped too") {
        val inner = let("x", c(1))(let("y", c(2))(let("z", c(3))(vr"x")))
        val innerGrouped = group(List("x", "y", "z"), List(c(1), c(2), c(3)))(vr"x")
        val input = let("a", inner)(vr"a")
        assert(regroup(input) ~=~ let("a", innerGrouped)(vr"a"))
    }

    test("chains inside case branches and delays are regrouped too") {
        val chain = let("x", c(1))(let("y", c(2))(let("z", c(3))(vr"x")))
        val grouped = group(List("x", "y", "z"), List(c(1), c(2), c(3)))(vr"x")
        val input = Case(vr"s", List(Delay(chain), chain))
        assert(regroup(input) ~=~ Case(vr"s", List(Delay(grouped), grouped)))
    }

    test("a lambda that is not applied is not treated as a let") {
        val input = LamAbs("a", LamAbs("b", LamAbs("c", vr"a")))
        assert(regroup(input) ~=~ input)
    }

    /** Five independent bindings — the profitability threshold — each used twice so the inliner
      * keeps them (a `Many` occurrence of a non-trivial term is not inlined) and each depending
      * only on the outer parameter, so they form a single run.
      */
    private val pipelineInput: Term = {
        def add(a: Term, b: Term): Term = Apply(Apply(Builtin(DefaultFun.AddInteger), a), b)
        val names = (1 to 5).map(i => s"v$i")
        val body = names.map(n => add(vr(n), vr(n))).reduce(add)
        LamAbs(
          "p",
          names.zipWithIndex.foldRight(body) { case ((n, i), acc) =>
              let(n, add(vr"p", c(i + 1)))(acc)
          }
        )
    }

    test("V3Optimizer runs the pass when letChainRegroup is enabled") {
        val optimizer =
            new V3Optimizer(cseIterations = 2, cceEnabled = false, letChainRegroup = true)
        optimizer(pipelineInput)
        assert(optimizer.logs.exists(_.startsWith("LetChainRegroup:")))
    }

    test("V3Optimizer leaves the pass out by default") {
        val optimizer = new V3Optimizer()
        optimizer(pipelineInput)
        assert(!optimizer.logs.exists(_.startsWith("LetChainRegroup:")))
    }
}
