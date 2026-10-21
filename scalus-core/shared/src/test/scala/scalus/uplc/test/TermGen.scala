package scalus.uplc.test

import org.scalacheck.Gen
import scalus.uplc.DefaultFun.{AddInteger, MultiplyInteger, SubtractInteger}
import scalus.uplc.Term.*
import scalus.uplc.TermDSL.given
import scalus.uplc.{Constant, DefaultFun, Term}

import scala.language.implicitConversions

/** Building blocks shared by the UPLC term generators of the optimizer tests.
  *
  * Each suite keeps its own generator, because each one targets the shapes of a different pass
  * (eta-redexes, let chains, duplicated subexpressions); see finding F-29 in
  * docs/internal/test-refactor/findings.md. These are only the pieces they had copied.
  *
  * The random term generator for syntax round-trips is `ArbitraryInstances.arbitraryTerm`, not this
  * object.
  */
object TermGen {

    /** A weighted branch for `Gen.frequency`, built only when `frequency` picks it.
      *
      * Use it for every branch that recurses. A recursive generator written as
      * `Gen.frequency(3 -> genTerm(depth - 1).map(Force(_)), ...)` builds all of its branches, and
      * through them its whole subtree, on every call: about (branches)^depth `Gen` objects per
      * sample. `Gen.lzy` passes the seed through unchanged, so wrapping a branch does not change
      * the generated values. Example:
      * {{{
      * Gen.frequency(
      *   1 -> Gen.const(Error(): Term),
      *   branch(3)(genTerm(depth - 1).map(Delay(_)))
      * )
      * }}}
      */
    def branch[A](weight: Int)(gen: => Gen[A]): (Int, Gen[A]) = (weight, Gen.lzy(gen))

    /** An Integer constant in `[min, max]`, a Bool constant, or Unit, with equal chance. */
    def smallConst(min: Long, max: Long): Gen[Term] = Gen.oneOf(
      Gen.chooseNum(min, max).map(i => Const(Constant.Integer(BigInt(i)))),
      Gen.oneOf(true, false).map(b => Const(Constant.Bool(b))),
      Gen.const(Const(Constant.Unit))
    )

    /** An Integer constant in [-100, 100]. */
    val smallInt: Gen[Term] = Gen.choose(-100, 100).map(n => Const(Constant.Integer(n)))

    /** `addInteger`, `multiplyInteger` or `subtractInteger`: total on integers, so a tree of them
      * always evaluates.
      */
    val arithOp: Gen[DefaultFun] = Gen.oneOf(AddInteger, MultiplyInteger, SubtractInteger)

    /** A full binary tree of [[arithOp]] applications of the given depth over [[smallInt]] leaves.
      * Closed, well-typed and terminating. Depth 0 is a single leaf; depth 2 is, for example,
      * `[(builtin addInteger) [(builtin multiplyInteger) 3 -7] [(builtin subtractInteger) 0 42]]`.
      */
    def arithTree(depth: Int): Gen[Term] =
        if depth <= 0 then smallInt
        else
            for
                a <- arithTree(depth - 1)
                b <- arithTree(depth - 1)
                op <- arithOp
            yield op $ a $ b
}
