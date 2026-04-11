package scalus.uplc

import org.scalatest.funsuite.AnyFunSuite
import scalus.compiler.Options
import scalus.uplc.Term.asTerm
import scalus.uplc.builtin.Data
import scalus.uplc.eval.PlutusVM
import scalus.uplc.transform.Inliner

class ScalusTagTest extends AnyFunSuite {

    private given PlutusVM = PlutusVM.makePlutusV3VM()

    test("Options.default does not tag V3 programs") {
        given Options = Options.default
        val compiled = PlutusV3.compile((_: Data) => ())
        assert(!ScalusTag.isTagged(compiled.program.term))
    }

    test("Options.default does not tag V1 programs") {
        given Options = Options.default
        val compiled = PlutusV1.compile((_: Data) => (_: Data) => (_: Data) => ())
        assert(!ScalusTag.isTagged(compiled.program.term))
    }

    test("Options.default does not tag V2 programs") {
        given Options = Options.default
        val compiled = PlutusV2.compile((_: Data) => (_: Data) => (_: Data) => ())
        assert(!ScalusTag.isTagged(compiled.program.term))
    }

    test("Options.debug does not tag programs") {
        given Options = Options.debug
        val compiled = PlutusV3.compile((_: Data) => ())
        assert(!ScalusTag.isTagged(compiled.program.term))
    }

    test("Options.release tags V3 programs") {
        given Options = Options.release
        val compiled = PlutusV3.compile((_: Data) => ())
        assert(ScalusTag.isTagged(compiled.program.term))
    }

    test("Options.release tags V1 programs") {
        given Options = Options.release
        val compiled = PlutusV1.compile((_: Data) => (_: Data) => (_: Data) => ())
        assert(ScalusTag.isTagged(compiled.program.term))
    }

    test("Options.release tags V2 programs") {
        given Options = Options.release
        val compiled = PlutusV2.compile((_: Data) => (_: Data) => (_: Data) => ())
        assert(ScalusTag.isTagged(compiled.program.term))
    }

    test("Options.releaseUntagged produces no tag") {
        given Options = Options.releaseUntagged
        val compiled = PlutusV3.compile((_: Data) => ())
        assert(!ScalusTag.isTagged(compiled.program.term))
    }

    test("withScalusTag(true) enables tagging on a custom Options") {
        given Options = Options.default.withScalusTag(true)
        val compiled = PlutusV3.compile((_: Data) => ())
        assert(ScalusTag.isTagged(compiled.program.term))
    }

    test("withScalusTag(false) disables tagging on release") {
        given Options = Options.release.withScalusTag(false)
        val compiled = PlutusV3.compile((_: Data) => ())
        assert(!ScalusTag.isTagged(compiled.program.term))
    }

    test("Tagged program evaluates to the same result as untagged") {
        val untagged = PlutusV3.compile((_: Data) => ())(using Options.release.withScalusTag(false))
        val tagged = PlutusV3.compile((_: Data) => ())(using Options.release)
        // Both validators, applied to a dummy Data argument, should succeed.
        val arg = Data.unit.asTerm
        val u = (untagged.program.term $ arg).evaluate
        val t = (tagged.program.term $ arg).evaluate
        assert(u == t)
    }

    test("Tagging adds at most 8 bytes to the flat-encoded program") {
        val untagged = PlutusV3.compile((_: Data) => ())(using Options.release.withScalusTag(false))
        val tagged = PlutusV3.compile((_: Data) => ())(using Options.release)
        val overhead = tagged.program.flatEncoded.length - untagged.program.flatEncoded.length
        assert(overhead > 0, s"tagged program should be larger than untagged, got $overhead")
        assert(overhead <= 8, s"tag overhead should be <= 8 bytes, got $overhead")
    }

    test("Tagging adds exactly 300 memory and 48000 steps to the evaluation budget") {
        val untagged = PlutusV3.compile((_: Data) => ())(using Options.release.withScalusTag(false))
        val tagged = PlutusV3.compile((_: Data) => ())(using Options.release)
        val arg = Data.unit.asTerm

        val u = (untagged.program.term $ arg).evaluateDebug.budget
        val t = (tagged.program.term $ arg).evaluateDebug.budget

        val memDelta = t.memory - u.memory
        val stepsDelta = t.steps - u.steps
        assert(
          memDelta == 300L,
          s"expected tag to add exactly 300 memory, but got +$memDelta (untagged=$u, tagged=$t)"
        )
        assert(
          stepsDelta == 48000L,
          s"expected tag to add exactly 48000 steps, but got +$stepsDelta (untagged=$u, tagged=$t)"
        )
    }

    test("Inliner deletes the Scalus tag when run directly") {
        // Guards the invariant that tag injection must happen POST-optimization.
        val body = BigInt(42).asTerm
        val wrapped = ScalusTag.wrap(body)
        assert(ScalusTag.isTagged(wrapped))
        val optimized = Inliner(wrapped)
        assert(
          !ScalusTag.isTagged(optimized),
          s"Inliner unexpectedly preserved the tag: $optimized"
        )
    }
}
