package scalus.verify.uplcblaster

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.cardano.onchain.plutus.prelude
import scalus.cardano.onchain.plutus.prelude.require
import scalus.compiler.Compile
import scalus.verify.*
import scalus.verify.Props.*

import scala.concurrent.duration.*

/** A helper of a contract's own, and not of the library: the rewrite inlines it. */
@Compile
object CallsAsUnknownsHelpers {
    def size(xs: prelude.List[BigInt]): BigInt = xs.length

    /** A helper that does not loop: the rewrite leaves its call, and its key is that of its code.
      */
    def firstList(xss: prelude.List[prelude.List[BigInt]]): prelude.List[BigInt] = xss match
        case prelude.List.Cons(head, _) => head
        case prelude.List.Nil           => prelude.List.Nil
}

/** The tactic of calls as unknowns ([[CallsAsUnknowns]]): what it makes unknowns of, and that a
  * statement of every list, which Lean does not finish with the walk, it proves with the walk as an
  * unknown.
  */
class CallsAsUnknownsTest extends AnyFunSuite with LeanProofsSuite {
    import proofs.*

    private val longerThanNothing =
        forAll[prelude.List[BigInt]](xs => (xs.length > BigInt(0)) ==>: (xs.length >= BigInt(1)))

    private def unknownsOf(prop: Prop): List[String] =
        CallsAsUnknowns
            .rewrite(prop, CallsAsUnknowns.inLibrary)
            .fold(reason => fail(reason), _.unknowns.map(_.written))

    test("the premise's and the claim's walk of one list are one unknown") {
        assert(
          unknownsOf(longerThanNothing) == List(
            "scalus.cardano.onchain.plutus.prelude.List$.length"
          )
        )
    }

    test("a helper of the contract is inlined, so that its walk is the claim's") {
        val prop = forAll[prelude.List[BigInt]](xs =>
            (CallsAsUnknownsHelpers.size(xs) > BigInt(0)) ==>: (xs.length >= BigInt(1))
        )
        assert(unknownsOf(prop) == List("scalus.cardano.onchain.plutus.prelude.List$.length"))
    }

    test("a walk of a helper's result is the walk of the helper's code written out") {
        // Vesting's clause reads its datum with `configOf(datum)`, and the validator with that
        // code written out: the two signature checks are one walk.
        val prop = forAll[prelude.List[prelude.List[BigInt]]](xss =>
            (CallsAsUnknownsHelpers.firstList(xss).length > BigInt(0)) ==>: (xss match {
                case prelude.List.Cons(head, _) => head
                case prelude.List.Nil           => prelude.List.Nil
            }).length >= BigInt(1)
        )
        assert(unknownsOf(prop) == List("scalus.cardano.onchain.plutus.prelude.List$.length"))
    }

    test("a helper's argument that may fail stays in the key of a walk of its result") {
        // `firstList({ require(...); xss })` fails where `firstList(xss)` returns.
        val prop = forAll[BigInt, prelude.List[prelude.List[BigInt]]]((x, xss) =>
            (CallsAsUnknownsHelpers.firstList(xss).length >= BigInt(0)) ==>: (
              CallsAsUnknownsHelpers.firstList { require(x > BigInt(0)); xss }.length >= BigInt(0)
            )
        )
        assert(unknownsOf(prop).size == 2)
    }

    test("walks of two lists are two unknowns") {
        val prop = forAll[prelude.List[BigInt], prelude.List[BigInt]]((xs, ys) =>
            (xs.length > ys.length) ==>: (xs.length >= BigInt(1))
        )
        assert(unknownsOf(prop).size == 2)
    }

    test("a walk whose argument may fail is not one with an argument that cannot") {
        // `{ require(...); xs }.length` fails where `xs.length` returns: the two calls have two
        // unknowns, or one unknown's `None` would stand for both.
        val prop = forAll[BigInt, prelude.List[BigInt]]((x, xs) =>
            (xs.length >= BigInt(0)) ==>: ({ require(x > BigInt(0)); xs }.length >= BigInt(0))
        )
        assert(unknownsOf(prop).size == 2)
    }

    test("a walk of constants is no unknown") {
        // It has one value, which Lean computes: an unknown of it would lose it.
        val prop =
            forAll[BigInt](x => prelude.List.Cons(BigInt(1), prelude.List.Nil).length + x > x)
        assert(unknownsOf(prop).isEmpty)
    }

    test("a statement with no walk is left as it is") {
        val prop = forAll[BigInt](x => x + BigInt(0) == x)
        assert(CallsAsUnknowns.rewrite(prop, CallsAsUnknowns.inLibrary).map(_.prop) == Right(prop))
    }

    private val unknowns = new CallsAsUnknowns(UplcBlaster(Budget.Auto, lean, 5.minutes))

    private val longerThanNothingStatement =
        verifier.statement("calls_as_unknowns.longer_than_nothing", longerThanNothing)

    test("with its walk as an unknown, a statement of every list is proved") {
        val result = verify(VerificationRequest(longerThanNothingStatement, unknowns))
        assert(result.isProven, describe(result))
    }

    test("a refutation whose unknown is no value its walk returns is spurious") {
        // A length is never negative, but an unknown of it can be: the replay on the statement as
        // written says that the refutation is of no list.
        val result = verifyWithoutKeeping(
          verifier.statement(
            "calls_as_unknowns.length_not_negative",
            forAll[prelude.List[BigInt]](xs => xs.length >= BigInt(0))
          ),
          unknowns
        )
        assert(result.isInconclusive, describe(result))
        val reason = result.why.getOrElse("")
        assert(reason.contains("values that their walks do not return"), reason)
    }
}
