package scalus.uplc

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.cardano.ledger.{MajorProtocolVersion, Word64}
import scalus.cardano.onchain.plutus.prelude.*
import scalus.compiler.sir.lowering.LoweringException
import scalus.compiler.{Compile, Options}
import scalus.uplc.builtin.Data
import scalus.uplc.eval.PlutusVM

/** A validator that pattern-matches on `Data`, so the lowering emits `case` at van Rossem. */
@Compile
object UplcVersionFixture {
    def check(d: Data): Unit = require(d.to[BigInt] >= 0, "negative")
}

/** A validator that sorts, which goes through `Order` - a `@UplcRepr(UplcConstr)` type. That
  * annotation is not gated on the protocol version, so this is the fixture that exposed
  * `constr`/`case` leaking into a 1.0.0 PlutusV1/V2 program at plomin.
  */
@Compile
object UplcVersionSortFixture {
    def check(d: Data): Unit = {
        val n = d.to[BigInt]
        val xs = List.Cons(n + 1, List.Cons(n, List.Nil))
        require(xs.sort.head === n, "not sorted")
    }
}

/** A type holding a function has no Data form: `constr`/`case` is its only encoding. */
case class UplcVersionStep(f: BigInt => BigInt, bias: BigInt)

@Compile
object UplcVersionClosureFixture {
    def check(d: Data): Unit = {
        val n = d.to[BigInt]
        val step = UplcVersionStep(x => x + 1, n)
        require(step.f(step.bias) >= 0, "negative")
    }
}

/** UPLC 1.1.0 (`constr`/`case`) became legal for PlutusV1/V2 at the van Rossem hard fork. Plutus
  * rejects `constr`/`case` inside a 1.0.0 program at deserialization, so a PlutusV1/V2 program
  * declares the lowest version its term needs ([[Program.minVersionFor]]), and a target below van
  * Rossem must not emit those terms at all.
  */
class UplcVersionTest extends AnyFunSuite {

    private val plomin = Options.release.copy(targetProtocolVersion = MajorProtocolVersion.plominPV)
    private val v1Args: Vector[Term] =
        Vector(Data.I(1), Data.unit, Data.unit).map(d => Term.Const(Constant.Data(d)))

    test("minVersionFor is 1.1.0 exactly when the term uses constr/case") {
        assert(Program.minVersionFor(Term.Const(Constant.Unit)) == (1, 0, 0))
        assert(Program.minVersionFor(Term.Constr(Word64(0), Nil)) == (1, 1, 0))
        assert(
          Program.minVersionFor(Term.Case(Term.Constr(Word64(0), Nil), Term.Error() :: Nil)) ==
              (1, 1, 0)
        )
    }

    test("a constr-free V1/V2 script keeps 1.0.0 even when compiled for van Rossem") {
        // The version is a property of the term, not of the target: a script that needs nothing
        // from UPLC 1.1.0 must not be made PV11-only, and its hash must not move. alwaysOk is the
        // canonical example - a published constant that can never contain constr/case.
        val v1 = PlutusV1.alwaysOk.program
        val v2 = PlutusV2.alwaysOk.program
        assert(!v1.term.usesConstrOrCase && !v2.term.usesConstrOrCase)
        assert(v1.version == (1, 0, 0))
        assert(v2.version == (1, 0, 0))
    }

    test("V1 declares 1.1.0 at van Rossem and 1.0.0 at plomin") {
        // This fixture's term has `case` at van Rossem (case-on-Data) and none at plomin, so the
        // declared version tracks the term.
        val vanRossem = PlutusV1.compile((d: Data) =>
            (_: Data) => (_: Data) => UplcVersionFixture.check(d)
        )(using Options.release)
        val atPlomin = PlutusV1.compile((d: Data) =>
            (_: Data) => (_: Data) => UplcVersionFixture.check(d)
        )(using plomin)

        assert(vanRossem.program.version == (1, 1, 0))
        assert(atPlomin.program.version == (1, 0, 0))
    }

    test("V2 declares 1.1.0 at van Rossem and 1.0.0 at plomin") {
        val vanRossem = PlutusV2.compile((d: Data) =>
            (_: Data) => (_: Data) => UplcVersionFixture.check(d)
        )(using Options.release)
        val atPlomin = PlutusV2.compile((d: Data) =>
            (_: Data) => (_: Data) => UplcVersionFixture.check(d)
        )(using plomin)

        assert(vanRossem.program.version == (1, 1, 0))
        assert(atPlomin.program.version == (1, 0, 0))
    }

    test("V1 at van Rossem uses constr/case for a @UplcRepr(UplcConstr) type") {
        val compiled = PlutusV1.compile((d: Data) =>
            (_: Data) => (_: Data) => UplcVersionSortFixture.check(d)
        )(using Options.release)
        assert(compiled.program.version == (1, 1, 0))
        assert(compiled.program.term.usesConstrOrCase, "expected the UplcConstr representation")
    }

    test("V1 at plomin falls back to Data for a @UplcRepr(UplcConstr) type and still runs") {
        // Below van Rossem PlutusV1 has no UPLC 1.1.0, so the annotation is a hint the target
        // cannot honour: the lowering must pick the Data representation instead and produce a
        // valid 1.0.0 program - not stamp 1.0.0 over a term the ledger will reject.
        val compiled = PlutusV1.compile((d: Data) =>
            (_: Data) => (_: Data) => UplcVersionSortFixture.check(d)
        )(using plomin.copy(noWarn = true))
        val program = compiled.program
        assert(program.version == (1, 0, 0))
        assert(!program.term.usesConstrOrCase, s"constr/case leaked into a 1.0.0 program")

        given PlutusVM = PlutusVM.makePlutusV1VM(MajorProtocolVersion.plominPV)
        val applied = v1Args.foldLeft(program)(_ $ _)
        val result = applied.evaluateDebug
        assert(result.isSuccess, s"the Data-represented program must still run: $result")
        assert(program.flatEncoded.nonEmpty)
    }

    test("V1 at plomin fails to lower a type that holds a function") {
        // No Data form exists for a closure, so there is nothing to fall back to.
        val compiled = PlutusV1.compile((d: Data) =>
            (_: Data) => (_: Data) => UplcVersionClosureFixture.check(d)
        )(using plomin)
        val e = intercept[LoweringException](compiled.program)
        assert(e.getMessage.contains("UPLC 1.1.0"), e.getMessage)
        assert(e.getMessage.contains("UplcVersionStep"), e.getMessage)
    }

    test("V1 at van Rossem lowers a type that holds a function") {
        // The same fixture that fails at plomin is fine once the target has UPLC 1.1.0. (Whether
        // the constr survives to the output depends on the optimizer, so only lowering is asserted.)
        val compiled = PlutusV1.compile((d: Data) =>
            (_: Data) => (_: Data) => UplcVersionClosureFixture.check(d)
        )(using Options.release)
        assert(compiled.program.version == (1, 1, 0))
    }

    test("V3 at plomin still uses constr/case: UPLC 1.1.0 is not protocol-version gated there") {
        val compiled = PlutusV3.compile(UplcVersionSortFixture.check)(using plomin)
        assert(compiled.program.version == (1, 1, 0))
        assert(compiled.program.term.usesConstrOrCase)
    }

    test("the raw plutusV1/plutusV2 helpers stamp the lowest version the term needs") {
        // `sir.toUplc()` lowers at the default (van Rossem) protocol version and so may emit
        // `case`; a fixed 1.0.0 stamp on it would be undeployable. These helpers have no protocol
        // version to consult, so they look at the term.
        val plain = Term.LamAbs("x", Term.Const(Constant.Unit))
        assert(plain.plutusV1.version == (1, 0, 0))
        assert(plain.plutusV2.version == (1, 0, 0))

        val withConstr = Term.LamAbs("x", Term.Constr(Word64(0), Nil))
        assert(withConstr.plutusV1.version == (1, 1, 0))
        assert(withConstr.plutusV2.version == (1, 1, 0))
        assert(withConstr.plutusV1.flatEncoded.nonEmpty)
    }

    test("the flat encoder refuses constr/case in a program below 1.1.0") {
        // Mirrors the Plutus decoder ("'constr' is not allowed before version 1.1.0"), so Scalus
        // cannot produce bytes the ledger will reject, whichever path built the program.
        val term = Term.Constr(Word64(0), Nil)
        val e = intercept[IllegalArgumentException](Program((1, 0, 0), term).flatEncoded)
        assert(e.getMessage.contains("1,1,0"), e.getMessage)
        assert(Program((1, 1, 0), term).flatEncoded.nonEmpty)
    }

    test("a 1.1.0 PlutusV1 text envelope is readable") {
        val program = PlutusV1
            .compile((d: Data) => (_: Data) => (_: Data) => UplcVersionFixture.check(d))(using
              Options.release
            )
            .program
        assert(program.version == (1, 1, 0))
        val envelope =
            s"""{"type":"PlutusScriptV1","description":"","cborHex":"${program.doubleCborHex}"}"""
        val read = scalus.utils.Utils.readPlutusFileContent(envelope)
        assert(read.version == (1, 1, 0))
        assert(read.doubleCborHex == program.doubleCborHex)
    }
}
