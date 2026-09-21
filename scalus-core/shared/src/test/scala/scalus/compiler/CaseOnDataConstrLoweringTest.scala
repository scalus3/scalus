package scalus.compiler

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.MajorProtocolVersion
import scalus.cardano.onchain.plutus.prelude.{List as PList, Option as POption}
import scalus.cardano.onchain.plutus.v1.Credential
import scalus.compiler.sir.{AnnotationsDecl, SIR, SIRType}
import scalus.compiler.{compile, Options}
import scalus.toUplc
import scalus.uplc.*
import scalus.uplc.Term.asTerm
import scalus.uplc.builtin.{ByteString, Data, FromData, ToData}
import scalus.uplc.eval.{CaseDataNotSupportedError, PlutusVM}

object CaseOnDataConstrLoweringTest {

    /** Zero-field, one-field and multi-field constructors: the fields list supplied by the Case
      * instruction is walked with headList/tailList, so every arity has to be covered.
      */
    enum Shape derives FromData, ToData:
        case Empty
        case Circle(radius: BigInt)
        case Box(width: BigInt, label: ByteString, height: BigInt)
}

/** Pattern matching on a sum type in the Data-Constr representation.
  *
  * From Dijkstra (PV12) on, the Case instruction scrutinizes a Data.Constr directly: the branch is
  * selected by the constructor tag and receives the fields list. Earlier targets have no Case on
  * Data, so the match has to go through unConstrData. Both lowerings must agree on every result.
  */
class CaseOnDataConstrLoweringTest extends AnyFunSuite:
    import CaseOnDataConstrLoweringTest.*

    private val pv11 = MajorProtocolVersion.vanRossemPV
    private val pv12 = MajorProtocolVersion.dijkstraPV

    private def constr(tag: Int, fields: Data*): Data =
        Data.Constr(BigInt(tag), PList.from(fields.toList))

    private val pubKeyHash = ByteString.fromHex("AABBCC")
    private val scriptHash = ByteString.fromHex("DDEEFF00")
    private val pubKeyCredential = constr(0, Data.B(pubKeyHash))
    private val scriptCredential = constr(1, Data.B(scriptHash))

    private val empty = constr(0)
    private val circle = constr(1, Data.I(7))
    private val box = constr(2, Data.I(3), Data.B(ByteString.fromHex("CAFE")), Data.I(5))

    private def options(pv: MajorProtocolVersion): Options =
        Options.default.copy(targetProtocolVersion = pv)

    private def applied(sirFun: SIR, arg: Data): SIR =
        sirFun $ SIR.Const(Constant.Data(arg), SIRType.Data.tp, AnnotationsDecl.empty)

    private def lower(sir: SIR, pv: MajorProtocolVersion): Term = {
        given Options = options(pv)
        sir.toUplc()
    }

    private def run(sirFun: SIR, arg: Data, pv: MajorProtocolVersion): Term = {
        given PlutusVM = PlutusVM.makePlutusV3VM(pv)
        lower(applied(sirFun, arg), pv).evaluate
    }

    /** Is there a Case whose branches are all single-argument lambdas over a non-constr scrutinee
      * \- the shape of a Case on Data? (`contains("(case")` would also match Case on
      * integer/bool/list.)
      */
    private def hasCaseOnDataShape(term: Term, branchCount: Int): Boolean = {
        def go(t: Term): Boolean = t match
            case Term.Case(scrutinee, cases, _) =>
                val isShape = cases.length == branchCount && cases.forall {
                    case Term.LamAbs(_, body, _) =>
                        body match
                            case _: Term.LamAbs => false
                            case _              => true
                    case _ => false
                }
                isShape || go(scrutinee) || cases.exists(go)
            case Term.LamAbs(_, body, _) => go(body)
            case Term.Apply(f, arg, _)   => go(f) || go(arg)
            case Term.Force(inner, _)    => go(inner)
            case Term.Delay(inner, _)    => go(inner)
            case Term.Constr(_, args, _) => args.exists(go)
            case _                       => false
        go(term)
    }

    private def fieldsSirFun(using Options): SIR = compile { (d: Data) =>
        d.to[Credential] match
            case Credential.PubKeyCredential(pkh) => pkh.hash
            case Credential.ScriptCredential(vh)  => vh
    }

    private def wildcardSirFun(using Options): SIR = compile { (d: Data) =>
        d.to[Credential] match
            case Credential.ScriptCredential(_) => BigInt(1)
            case _                              => BigInt(0)
    }

    private def shapeSirFun(using Options): SIR = compile { (d: Data) =>
        d.to[Shape] match
            case Shape.Empty            => BigInt(-1)
            case Shape.Circle(r)        => r
            case Shape.Box(w, label, h) => w * h + label.length
    }

    // the wildcard expands to the first two constructors, the explicit case is the last one
    private def shapeWildcardSirFun(using Options): SIR = compile { (d: Data) =>
        d.to[Shape] match
            case Shape.Box(w, _, h) => w + h
            case _                  => BigInt(-1)
    }

    // nested matches, the outer one with branches that return a sum type
    private def nestedSirFun(using Options): SIR = compile { (d: Data) =>
        val area: POption[BigInt] = d.to[POption[Shape]] match
            case POption.Some(shape) =>
                shape match
                    case Shape.Empty        => POption.None
                    case Shape.Circle(r)    => POption.Some(r * r)
                    case Shape.Box(w, _, h) => POption.Some(w * h)
            case POption.None => POption.None
        area match
            case POption.Some(a) => a
            case POption.None    => BigInt(-1)
    }

    test("PV12: match on a Data-Constr sum type is a Case on the Data scrutinee") {
        given Options = options(pv12)
        val term = lower(fieldsSirFun, pv12)
        val uplcStr = term.pretty.render(200)
        assert(hasCaseOnDataShape(term, 2), s"Expected Case on Data in UPLC, got:\n$uplcStr")
        assert(!uplcStr.contains("unConstrData"), s"Unexpected unConstrData in UPLC:\n$uplcStr")
    }

    test("PV12: one Case branch per constructor, wildcard included") {
        given Options = options(pv12)
        val term = lower(shapeWildcardSirFun, pv12)
        assert(hasCaseOnDataShape(term, 3), s"got:\n${term.pretty.render(200)}")
    }

    test("PV11: match on a Data-Constr sum type goes through unConstrData") {
        given Options = options(pv11)
        val uplcStr = lower(fieldsSirFun, pv11).pretty.render(200)
        assert(uplcStr.contains("unConstrData"), s"Expected unConstrData in UPLC, got:\n$uplcStr")
    }

    for pv <- Seq(pv11, pv12) do
        test(s"PV${pv.version}: branches receive the constructor fields") {
            given Options = options(pv)
            val sirFun = fieldsSirFun
            assert(run(sirFun, pubKeyCredential, pv) == Term.Const(Constant.ByteString(pubKeyHash)))
            assert(run(sirFun, scriptCredential, pv) == Term.Const(Constant.ByteString(scriptHash)))
        }

        test(s"PV${pv.version}: wildcard covers the remaining constructors") {
            given Options = options(pv)
            val sirFun = wildcardSirFun
            assert(run(sirFun, pubKeyCredential, pv) == 0.asTerm)
            assert(run(sirFun, scriptCredential, pv) == 1.asTerm)
        }

        test(s"PV${pv.version}: zero-, one- and multi-field constructors") {
            given Options = options(pv)
            val sirFun = shapeSirFun
            assert(run(sirFun, empty, pv) == (-1).asTerm)
            assert(run(sirFun, circle, pv) == 7.asTerm)
            assert(run(sirFun, box, pv) == 17.asTerm)
        }

        test(s"PV${pv.version}: wildcard expands to the leading constructors") {
            given Options = options(pv)
            val sirFun = shapeWildcardSirFun
            assert(run(sirFun, empty, pv) == (-1).asTerm)
            assert(run(sirFun, circle, pv) == (-1).asTerm)
            assert(run(sirFun, box, pv) == 8.asTerm)
        }

        test(s"PV${pv.version}: nested matches with sum-typed branches") {
            given Options = options(pv)
            val sirFun = nestedSirFun
            assert(run(sirFun, constr(1), pv) == (-1).asTerm) // None
            assert(run(sirFun, constr(0, empty), pv) == (-1).asTerm)
            assert(run(sirFun, constr(0, circle), pv) == 49.asTerm)
            assert(run(sirFun, constr(0, box), pv) == 15.asTerm)
        }

    test("PV12: a constructor tag without a branch fails evaluation") {
        given Options = options(pv12)
        given PlutusVM = PlutusVM.makePlutusV3VM(pv12)
        val term = lower(applied(shapeSirFun, constr(3)), pv12)
        assert(term.evaluateDebug.isFailure)
    }

    test("a PV12-targeted script does not run on a PV11 VM") {
        given Options = options(pv12)
        val vm11 = PlutusVM.makePlutusV3VM(pv11)
        val term = lower(applied(fieldsSirFun, pubKeyCredential), pv12)
        assertThrows[CaseDataNotSupportedError](
          vm11.evaluateDeBruijnedTerm(DeBruijn.deBruijnTerm(term))
        )
    }
