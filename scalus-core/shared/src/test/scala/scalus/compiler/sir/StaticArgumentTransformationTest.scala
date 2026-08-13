package scalus.compiler.sir

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.cardano.ledger.MajorProtocolVersion
import scalus.compiler.Options
import scalus.compiler.sir.SirDSL.*
import scalus.uplc.{Constant, Term}
import scalus.uplc.eval.{PlutusVM, Result}

class StaticArgumentTransformationTest extends AnyFunSuite {
    private given PlutusVM = PlutusVM.makePlutusV3VM(MajorProtocolVersion.vanRossemPV)

    private val ann = AnnotationsDecl.empty
    private val intTp = SIRType.Integer
    private val intToInt = SIRType.Fun(intTp, intTp)

    /** go : (Int -> Int) -> Int -> Int -> Int */
    private val goTp = SIRType.Fun(intToInt, SIRType.Fun(intTp, intToInt))

    private def intConst(value: Int) = SIR.Const(Constant.Integer(value), intTp, ann)
    private def v(name: String, tp: SIRType) = SIR.Var(name, tp, ann)

    /** Collects the names of all `Var`/`ExternalVar` occurrences, for shape assertions. */
    private def varNames(sir: SIR): List[String] = sir match
        case SIR.Decl(_, term)              => varNames(term)
        case SIR.Var(name, _, _)            => List(name)
        case SIR.ExternalVar(_, name, _, _) => List(name)
        case SIR.Let(bs, body, _, _)        => bs.flatMap(b => varNames(b.value)) ++ varNames(body)
        case SIR.LamAbs(_, term, _, _)      => varNames(term)
        case SIR.Apply(f, a, _, _)          => varNames(f) ++ varNames(a)
        case SIR.Select(s, _, _, _)         => varNames(s)
        case SIR.IfThenElse(c, t, f, _, _)  => varNames(c) ++ varNames(t) ++ varNames(f)
        case SIR.And(a, b, _)               => varNames(a) ++ varNames(b)
        case SIR.Or(a, b, _)                => varNames(a) ++ varNames(b)
        case SIR.Not(a, _)                  => varNames(a)
        case SIR.Match(s, cases, _, _)      => varNames(s) ++ cases.flatMap(c => varNames(c.body))
        case SIR.Constr(_, _, args, _, _)   => args.flatMap(varNames)
        case SIR.Cast(e, _, _)              => varNames(e)
        case SIR.Error(msg, _, _)           => varNames(msg)
        case _: SIR.Builtin | _: SIR.Const  => Nil

    /** λf. λn. λacc. if n == 0 then acc else go f (n - 1) (acc + f n) */
    private def goRhs: SIR =
        SIR.LamAbs(
          v("f", intToInt),
          SIR.LamAbs(
            v("n", intTp),
            SIR.LamAbs(
              v("acc", intTp),
              SIR.IfThenElse(
                extractAnnotated(SIRBuiltins.equalsInteger $ v("n", intTp) $ intConst(0)),
                v("acc", intTp),
                extractAnnotated(
                  v("go", goTp) $ v("f", intToInt)
                      $ (SIRBuiltins.subtractInteger $ v("n", intTp) $ intConst(1))
                      $ (SIRBuiltins.addInteger $ v("acc", intTp)
                          $ (v("f", intToInt) $ v("n", intTp)))
                ),
                intTp,
                ann
              ),
              List.empty,
              ann
            ),
            List.empty,
            ann
          ),
          List.empty,
          ann
        )

    /** λx. x + x */
    private def double: SIR =
        SIR.LamAbs(
          v("x", intTp),
          extractAnnotated(SIRBuiltins.addInteger $ v("x", intTp) $ v("x", intTp)),
          List.empty,
          ann
        )

    /** let rec go = goRhs in go double 4 seed */
    private def goProgram(seed: AnnotatedSIR): AnnotatedSIR =
        SIR.Let(
          List(Binding("go", goTp, goRhs)),
          extractAnnotated(v("go", goTp) $ double $ intConst(4) $ seed),
          SIR.LetFlags.Recursivity,
          ann
        )

    private def program: AnnotatedSIR = goProgram(intConst(0))

    test("static param f is lifted, n and acc keep recursing") {
        StaticArgumentTransformation(program) match
            case SIR.Let(List(Binding("go", _, wrapper)), _, outerFlags, _) =>
                assert(!outerFlags.isRec, "outer let must become non-recursive")
                wrapper match
                    case SIR.LamAbs(
                          f1,
                          SIR.LamAbs(n1, SIR.LamAbs(a1, inner, _, _), _, _),
                          _,
                          _
                        ) =>
                        assert(List(f1.name, n1.name, a1.name) == List("f", "n", "acc"))
                        inner match
                            case SIR.Let(
                                  List(Binding("go$sat", satTp, satLam)),
                                  entry,
                                  innerFlags,
                                  _
                                ) =>
                                assert(innerFlags.isRec, "inner let must be recursive")
                                assert(satTp == SIRType.Fun(intTp, intToInt))
                                satLam match
                                    case SIR.LamAbs(p1, SIR.LamAbs(p2, _, _, _), _, _) =>
                                        assert(List(p1.name, p2.name) == List("n", "acc"))
                                    case other => fail(s"expected 2-param sat lambda: $other")
                                entry match
                                    case SIR.Apply(
                                          SIR.Apply(
                                            SIR.Var("go$sat", _, _),
                                            SIR.Var("n", _, _),
                                            _,
                                            _
                                          ),
                                          SIR.Var("acc", _, _),
                                          _,
                                          _
                                        ) =>
                                    case other => fail(s"expected entry `go$$sat n acc`: $other")
                            case other => fail(s"expected inner go$$sat letrec: $other")
                    case other => fail(s"expected 3-param wrapper: $other")
            case other => fail(s"expected outer go let: $other")
    }

    test("no self-call through the original name remains inside the rewritten rhs") {
        val out = StaticArgumentTransformation(program)
        val wrapper = out match
            case SIR.Let(List(Binding("go", _, w)), _, _, _) => w
            case other                                       => fail(s"unexpected: $other")
        val names = varNames(wrapper)
        assert(!names.contains("go"), s"rewritten rhs still references `go`: $names")
        assert(names.count(_ == "go$sat") == 2, s"expected 2 go$$sat refs, got $names")
    }

    test("transformed program evaluates to the same result on all backends") {
        for backend <- TargetLoweringBackend.values do {
            val opts = Options(
              targetLoweringBackend = backend,
              targetProtocolVersion = MajorProtocolVersion.vanRossemPV
            )
            val transformed: SIR = StaticArgumentTransformation(program)
            val uplc = transformed.toUplc(using opts)()
            uplc.evaluateDebug match
                case s: Result.Success =>
                    assert(s.term == Term.Const(Constant.Integer(20)), s"backend $backend")
                case f => fail(s"backend $backend failed: $f")
        }
    }

    test("untransformed program evaluates to the same result (sanity baseline)") {
        val opts = Options(
          targetLoweringBackend = TargetLoweringBackend.SirToUplcV3Lowering,
          targetProtocolVersion = MajorProtocolVersion.vanRossemPV
        )
        (program: SIR).toUplc(using opts)().evaluateDebug match
            case s: Result.Success => assert(s.term == Term.Const(Constant.Integer(20)))
            case f                 => fail(s"baseline failed: $f")
    }
}
