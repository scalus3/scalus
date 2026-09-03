package scalus.uplc

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.cardano.onchain.plutus.prelude.*
import scalus.compiler.{Compile, Options}
import scalus.uplc.Term.asTerm
import scalus.uplc.builtin.Data
import scalus.uplc.eval.PlutusVM
import scalus.uplc.transform.Inliner

/** A validator that can fail, so its compiled term has `(error)` nodes to carry the tag. */
@Compile
object ScalusTagFixture {
    def check(d: Data): Unit = require(d.to[BigInt] >= 0, "negative")
}

class ScalusTagTest extends AnyFunSuite {

    private given PlutusVM = PlutusVM.makePlutusV3VM()

    /** An argument the fixture accepts, so evaluation takes the success path. */
    private val okArg: Term = Data.I(1).asTerm

    test("Options.default does not tag V3 programs") {
        given Options = Options.default
        val compiled = PlutusV3.compile(ScalusTagFixture.check)
        assert(!ScalusTag.isTagged(compiled.program.term))
    }

    test("Options.default does not tag V1 programs") {
        given Options = Options.default
        val compiled =
            PlutusV1.compile((d: Data) => (_: Data) => (_: Data) => ScalusTagFixture.check(d))
        assert(!ScalusTag.isTagged(compiled.program.term))
    }

    test("Options.default does not tag V2 programs") {
        given Options = Options.default
        val compiled =
            PlutusV2.compile((d: Data) => (_: Data) => (_: Data) => ScalusTagFixture.check(d))
        assert(!ScalusTag.isTagged(compiled.program.term))
    }

    test("Options.debug does not tag programs") {
        given Options = Options.debug
        val compiled = PlutusV3.compile(ScalusTagFixture.check)
        assert(!ScalusTag.isTagged(compiled.program.term))
    }

    test("Options.release tags V3 programs") {
        given Options = Options.release
        val compiled = PlutusV3.compile(ScalusTagFixture.check)
        assert(ScalusTag.isTagged(compiled.program.term))
    }

    test("Options.release tags V1 programs") {
        given Options = Options.release
        val compiled =
            PlutusV1.compile((d: Data) => (_: Data) => (_: Data) => ScalusTagFixture.check(d))
        assert(ScalusTag.isTagged(compiled.program.term))
    }

    test("Options.release tags V2 programs") {
        given Options = Options.release
        val compiled =
            PlutusV2.compile((d: Data) => (_: Data) => (_: Data) => ScalusTagFixture.check(d))
        assert(ScalusTag.isTagged(compiled.program.term))
    }

    test("Options.releaseUntagged produces no tag") {
        given Options = Options.releaseUntagged
        val compiled = PlutusV3.compile(ScalusTagFixture.check)
        assert(!ScalusTag.isTagged(compiled.program.term))
    }

    test("withScalusTag(true) enables tagging on a custom Options") {
        given Options = Options.default.withScalusTag(true)
        val compiled = PlutusV3.compile(ScalusTagFixture.check)
        assert(ScalusTag.isTagged(compiled.program.term))
    }

    test("withScalusTag(false) disables tagging on release") {
        given Options = Options.release.withScalusTag(false)
        val compiled = PlutusV3.compile(ScalusTagFixture.check)
        assert(!ScalusTag.isTagged(compiled.program.term))
    }

    test("Tagged program evaluates to the same result as untagged") {
        val untagged =
            PlutusV3.compile(ScalusTagFixture.check)(using Options.releaseUntagged)
        val tagged = PlutusV3.compile(ScalusTagFixture.check)(using Options.release)
        val u = (untagged.program.term $ okArg).evaluate
        val t = (tagged.program.term $ okArg).evaluate
        assert(u == t)
    }

    test("Tagging costs ZERO execution budget on the success path") {
        val untagged =
            PlutusV3.compile(ScalusTagFixture.check)(using Options.releaseUntagged)
        val tagged = PlutusV3.compile(ScalusTagFixture.check)(using Options.release)

        val u = (untagged.program.term $ okArg).evaluateDebug
        val t = (tagged.program.term $ okArg).evaluateDebug
        assert(u.isSuccess && t.isSuccess, "both programs must take the success path")
        assert(
          u.budget == t.budget,
          s"the tag must not cost any budget: untagged=${u.budget}, tagged=${t.budget}"
        )
    }

    test("Tagging adds 2 or 3 bytes to the flat-encoded program") {
        // The marker is ~20 bits (Apply tag + Const tag + type list + zigzag integer), so the
        // byte delta depends on where the untagged program ends within its final byte.
        val untagged =
            PlutusV3.compile(ScalusTagFixture.check)(using Options.releaseUntagged)
        val tagged = PlutusV3.compile(ScalusTagFixture.check)(using Options.release)
        val overhead = tagged.program.flatEncoded.length - untagged.program.flatEncoded.length
        assert(overhead >= 2 && overhead <= 3, s"tag overhead should be 2-3 bytes, got $overhead")
    }

    test("A validator with no (error) node is left untagged") {
        given Options = Options.release
        val compiled = PlutusV3.compile((_: Data) => ())
        assert(
          !hasErrorNode(compiled.program.term),
          "fixture precondition: this validator must have no (error) node"
        )
        assert(!ScalusTag.isTagged(compiled.program.term))
    }

    test("The Inliner preserves the tag") {
        // Unlike the pre-1.1 root wrapper, `[(error) x]` is not reducible, so the optimizer
        // cannot eliminate it. Injection still happens post-optimization so the set of
        // (error) nodes is final.
        val tagged = PlutusV3.compile(ScalusTagFixture.check)(using Options.release).program.term
        assert(ScalusTag.isTagged(tagged))
        assert(ScalusTag.isTagged(Inliner(tagged)), "Inliner unexpectedly dropped the tag")
    }

    test("wrap marks the first (error) node and only that one") {
        val body = Term.LamAbs(
          "x",
          Term.Apply(Term.Force(Term.Error()), Term.Delay(Term.Error()))
        )
        val wrapped = ScalusTag.wrap(body)
        assert(ScalusTag.isTagged(wrapped))
        assert(countMarkers(wrapped) == 1, s"expected exactly one marker, got: ${wrapped.show}")
    }

    test("wrap is the identity on a term with no (error) node") {
        val body = BigInt(42).asTerm
        assert(ScalusTag.wrap(body) == body)
        assert(!ScalusTag.isTagged(body))
    }

    test("the legacy root-wrapper tag is still recognised") {
        val legacy = Term.Apply(
          Term.LamAbs("_scalusTag", BigInt(42).asTerm),
          Term.Const(ScalusTag.legacyMarker)
        )
        assert(ScalusTag.isLegacyTagged(legacy))
        assert(ScalusTag.isTagged(legacy))
    }

    private def hasErrorNode(t: Term): Boolean = t match
        case _: Term.Error           => true
        case Term.LamAbs(_, b, _)    => hasErrorNode(b)
        case Term.Apply(f, a, _)     => hasErrorNode(f) || hasErrorNode(a)
        case Term.Force(b, _)        => hasErrorNode(b)
        case Term.Delay(b, _)        => hasErrorNode(b)
        case Term.Constr(_, args, _) => args.exists(hasErrorNode)
        case Term.Case(s, cs, _)     => hasErrorNode(s) || cs.exists(hasErrorNode)
        case _                       => false

    private def countMarkers(t: Term): Int = t match
        case Term.Apply(_: Term.Error, Term.Const(ScalusTag.marker, _), _) => 1
        case Term.LamAbs(_, b, _)                                          => countMarkers(b)
        case Term.Apply(f, a, _)     => countMarkers(f) + countMarkers(a)
        case Term.Force(b, _)        => countMarkers(b)
        case Term.Delay(b, _)        => countMarkers(b)
        case Term.Constr(_, args, _) => args.map(countMarkers).sum
        case Term.Case(s, cs, _)     => countMarkers(s) + cs.map(countMarkers).sum
        case _                       => 0
}
