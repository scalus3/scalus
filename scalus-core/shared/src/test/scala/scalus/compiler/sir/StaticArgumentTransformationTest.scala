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

    // ------------------------------------------------------------------ edge cases

    private def app(f: SIR, a: SIR, tp: SIRType): AnnotatedSIR =
        SIR.Apply(extractAnnotated(f), extractAnnotated(a), tp, ann)

    /** let rec count = λn. if n == 0 then 0 else count (n - 1) in count 3 */
    private def countProgram: AnnotatedSIR =
        SIR.Let(
          List(
            Binding(
              "count",
              intToInt,
              SIR.LamAbs(
                v("n", intTp),
                SIR.IfThenElse(
                  extractAnnotated(SIRBuiltins.equalsInteger $ v("n", intTp) $ intConst(0)),
                  intConst(0),
                  app(
                    v("count", intToInt),
                    SIRBuiltins.subtractInteger $ v("n", intTp) $ intConst(1),
                    intTp
                  ),
                  intTp,
                  ann
                ),
                List.empty,
                ann
              )
            )
          ),
          app(v("count", intToInt), intConst(3), intTp),
          SIR.LetFlags.Recursivity,
          ann
        )

    test("zero static params: the let is returned unchanged") {
        assert(StaticArgumentTransformation(countProgram) == countProgram)
    }

    test("all params static: the last one is demoted to changing") {
        // let rec spin = λa. λb. spin a b   (diverges if run; shape-checked only)
        val spinTp = SIRType.Fun(intTp, intToInt)
        val spin = SIR.Let(
          List(
            Binding(
              "spin",
              spinTp,
              SIR.LamAbs(
                v("a", intTp),
                SIR.LamAbs(
                  v("b", intTp),
                  app(app(v("spin", spinTp), v("a", intTp), intToInt), v("b", intTp), intTp),
                  List.empty,
                  ann
                ),
                List.empty,
                ann
              )
            )
          ),
          intConst(0),
          SIR.LetFlags.Recursivity,
          ann
        )
        StaticArgumentTransformation(spin) match
            case SIR.Let(List(Binding("spin", _, wrapper)), _, flags, _) =>
                assert(!flags.isRec)
                wrapper match
                    case SIR.LamAbs(a, SIR.LamAbs(b, inner, _, _), _, _) =>
                        assert(List(a.name, b.name) == List("a", "b"))
                        inner match
                            case SIR.Let(List(Binding("spin$sat", satTp, satLam)), _, f, _) =>
                                assert(f.isRec)
                                // only `b` remains a parameter of the fixpoint
                                assert(satTp == intToInt)
                                satLam match
                                    case SIR.LamAbs(p, body, _, _) =>
                                        assert(p.name == "b")
                                        assert(varNames(body) == List("spin$sat", "b"))
                                    case other => fail(s"expected 1-param sat lambda: $other")
                            case other => fail(s"expected spin$$sat letrec: $other")
                    case other => fail(s"expected 2-param wrapper: $other")
            case other => fail(s"expected spin let: $other")
    }

    /** Wraps `selfUse` (an Int-typed body fragment) as the else-branch of the standard 2-param
      * recursion `let rec go2 = λf. λn. if n == 0 then 0 else <selfUse> in go2 double 2`.
      */
    private def go2Program(selfUse: AnnotatedSIR): AnnotatedSIR = {
        val go2Tp = SIRType.Fun(intToInt, intToInt)
        SIR.Let(
          List(
            Binding(
              "go2",
              go2Tp,
              SIR.LamAbs(
                v("f", intToInt),
                SIR.LamAbs(
                  v("n", intTp),
                  SIR.IfThenElse(
                    extractAnnotated(SIRBuiltins.equalsInteger $ v("n", intTp) $ intConst(0)),
                    intConst(0),
                    selfUse,
                    intTp,
                    ann
                  ),
                  List.empty,
                  ann
                ),
                List.empty,
                ann
              )
            )
          ),
          app(app(v("go2", go2Tp), double, intToInt), intConst(2), intTp),
          SIR.LetFlags.Recursivity,
          ann
        )
    }

    private val go2Tp = SIRType.Fun(intToInt, intToInt)

    test("under-saturated self-call blocks the transform") {
        // (λh. h (n - 1)) (go2 f)      -- `go2 f` is a 1-arg spine, arity is 2
        val partial = app(v("go2", go2Tp), v("f", intToInt), intToInt)
        val consumer = SIR.LamAbs(
          v("h", intToInt),
          app(
            v("h", intToInt),
            SIRBuiltins.subtractInteger $ v("n", intTp) $ intConst(1),
            intTp
          ),
          List.empty,
          ann
        )
        val prog = go2Program(app(consumer, partial, intTp))
        assert(StaticArgumentTransformation(prog) == prog)
    }

    test("bare self-reference blocks the transform") {
        // (λh. h f (n - 1)) go2        -- `go2` appears as a plain argument
        val consumer = SIR.LamAbs(
          v("h", go2Tp),
          app(
            app(v("h", go2Tp), v("f", intToInt), intToInt),
            SIRBuiltins.subtractInteger $ v("n", intTp) $ intConst(1),
            intTp
          ),
          List.empty,
          ann
        )
        val prog = go2Program(app(consumer, v("go2", go2Tp), intTp))
        assert(StaticArgumentTransformation(prog) == prog)
    }

    test("param shadowed at the call site is not static") {
        // (λf. go2 f (n - 1)) f        -- the inner λf shadows the outer param
        val shadowing = SIR.LamAbs(
          v("f", intToInt),
          app(
            app(v("go2", go2Tp), v("f", intToInt), intToInt),
            SIRBuiltins.subtractInteger $ v("n", intTp) $ intConst(1),
            intTp
          ),
          List.empty,
          ann
        )
        val prog = go2Program(app(shadowing, v("f", intToInt), intTp))
        // f is shadowed at the call site, n is changing -> no static param at all
        assert(StaticArgumentTransformation(prog) == prog)
    }

    test("over-saturated self-call keeps its trailing arguments") {
        // let rec mk = λc. λn. if n == 0 then (λx. x + c) else (λy. mk c (n - 1) y)
        val mkTp = SIRType.Fun(intTp, SIRType.Fun(intTp, intToInt))
        val selfCall = app(
          app(
            app(v("mk", mkTp), v("c", intTp), SIRType.Fun(intTp, intToInt)),
            SIRBuiltins.subtractInteger $ v("n", intTp) $ intConst(1),
            intToInt
          ),
          v("y", intTp),
          intTp
        )
        val mk = SIR.Let(
          List(
            Binding(
              "mk",
              mkTp,
              SIR.LamAbs(
                v("c", intTp),
                SIR.LamAbs(
                  v("n", intTp),
                  SIR.IfThenElse(
                    extractAnnotated(SIRBuiltins.equalsInteger $ v("n", intTp) $ intConst(0)),
                    SIR.LamAbs(
                      v("x", intTp),
                      extractAnnotated(SIRBuiltins.addInteger $ v("x", intTp) $ v("c", intTp)),
                      List.empty,
                      ann
                    ),
                    SIR.LamAbs(v("y", intTp), selfCall, List.empty, ann),
                    intToInt,
                    ann
                  ),
                  List.empty,
                  ann
                ),
                List.empty,
                ann
              )
            )
          ),
          app(
            app(
              app(v("mk", mkTp), intConst(5), SIRType.Fun(intTp, intToInt)),
              intConst(3),
              intToInt
            ),
            intConst(1),
            intTp
          ),
          SIR.LetFlags.Recursivity,
          ann
        )
        StaticArgumentTransformation(mk) match
            case SIR.Let(List(Binding("mk", _, wrapper)), _, _, _) =>
                val names = varNames(wrapper)
                assert(!names.contains("mk"), s"still references mk: $names")
                assert(names.contains("mk$sat"))
                // the trailing `y` argument must survive: mk$sat (n - 1) y
                assert(names.contains("y"), s"trailing arg dropped: $names")
            case other => fail(s"expected mk let: $other")
    }

    test("multi-binding recursive let (mutual group) is left untouched") {
        val mutual = SIR.Let(
          List(
            Binding("isEven", intToInt, SIR.LamAbs(v("n", intTp), intConst(1), List.empty, ann)),
            Binding("isOdd", intToInt, SIR.LamAbs(v("n", intTp), intConst(0), List.empty, ann))
          ),
          app(v("isEven", intToInt), intConst(4), intTp),
          SIR.LetFlags.Recursivity,
          ann
        )
        assert(StaticArgumentTransformation(mutual) == mutual)
    }

    test("non-recursive let is left untouched") {
        val nonRec = SIR.Let(
          List(Binding("id", intToInt, SIR.LamAbs(v("x", intTp), v("x", intTp), List.empty, ann))),
          app(v("id", intToInt), intConst(7), intTp),
          SIR.LetFlags.None,
          ann
        )
        assert(StaticArgumentTransformation(nonRec) == nonRec)
    }

    test("a recursive let nested in another recursive rhs is transformed independently") {
        // let rec outer = λf. λn.
        //     if n == 0 then 0
        //     else let rec inner = λg. λm.
        //              if m == 0 then outer f (n - 1) else inner g (m - 1)
        //          in inner f n
        val innerSelf = app(
          app(v("inner", go2Tp), v("g", intToInt), intToInt),
          SIRBuiltins.subtractInteger $ v("m", intTp) $ intConst(1),
          intTp
        )
        val outerSelf = app(
          app(v("outer", go2Tp), v("f", intToInt), intToInt),
          SIRBuiltins.subtractInteger $ v("n", intTp) $ intConst(1),
          intTp
        )
        val innerLet = SIR.Let(
          List(
            Binding(
              "inner",
              go2Tp,
              SIR.LamAbs(
                v("g", intToInt),
                SIR.LamAbs(
                  v("m", intTp),
                  SIR.IfThenElse(
                    extractAnnotated(SIRBuiltins.equalsInteger $ v("m", intTp) $ intConst(0)),
                    outerSelf,
                    innerSelf,
                    intTp,
                    ann
                  ),
                  List.empty,
                  ann
                ),
                List.empty,
                ann
              )
            )
          ),
          app(app(v("inner", go2Tp), v("f", intToInt), intToInt), v("n", intTp), intTp),
          SIR.LetFlags.Recursivity,
          ann
        )
        val outer = SIR.Let(
          List(
            Binding(
              "outer",
              go2Tp,
              SIR.LamAbs(
                v("f", intToInt),
                SIR.LamAbs(
                  v("n", intTp),
                  SIR.IfThenElse(
                    extractAnnotated(SIRBuiltins.equalsInteger $ v("n", intTp) $ intConst(0)),
                    intConst(0),
                    innerLet,
                    intTp,
                    ann
                  ),
                  List.empty,
                  ann
                ),
                List.empty,
                ann
              )
            )
          ),
          app(app(v("outer", go2Tp), double, intToInt), intConst(2), intTp),
          SIR.LetFlags.Recursivity,
          ann
        )
        val out = StaticArgumentTransformation(outer)
        val names = varNames(out)
        // both fixpoints exist, and each is referenced twice: once by its own
        // self-call, once by its wrapper's entry call
        assert(names.count(_ == "outer$sat") == 2, s"outer not transformed: $names")
        assert(names.count(_ == "inner$sat") == 2, s"inner not transformed: $names")
        // the only surviving references under the original names are the two
        // call sites outside the recursive rhs: `inner f n` (inner let body)
        // and `outer double 2` (top-level program body)
        assert(names.count(_ == "inner") == 1, s"inner self-call left behind: $names")
        assert(names.count(_ == "outer") == 1, s"outer self-call left behind: $names")
    }

    // ---------------------------------------------- ExternalVar and polymorphism

    test("ExternalVar self-calls are detected (linked top-level defs)") {
        // SIRLinker emits top-level defs under dotted names, with self-references
        // as ExternalVar("Mod$", "Mod$.go", ...)
        val fq = "Mod$.go"
        def selfRef = SIR.ExternalVar("Mod$", fq, go2Tp, ann)
        val rhs = SIR.LamAbs(
          v("f", intToInt),
          SIR.LamAbs(
            v("n", intTp),
            SIR.IfThenElse(
              extractAnnotated(SIRBuiltins.equalsInteger $ v("n", intTp) $ intConst(0)),
              intConst(0),
              app(
                app(selfRef, v("f", intToInt), intToInt),
                SIRBuiltins.subtractInteger $ v("n", intTp) $ intConst(1),
                intTp
              ),
              intTp,
              ann
            ),
            List.empty,
            ann
          ),
          List.empty,
          ann
        )
        val prog = SIR.Let(
          List(Binding(fq, go2Tp, rhs)),
          app(app(SIR.ExternalVar("Mod$", fq, go2Tp, ann), double, intToInt), intConst(3), intTp),
          SIR.LetFlags.Recursivity,
          ann
        )
        StaticArgumentTransformation(prog) match
            case SIR.Let(List(Binding(`fq`, _, wrapper)), _, flags, _) =>
                assert(!flags.isRec)
                wrapper match
                    case SIR.LamAbs(_, SIR.LamAbs(_, inner, _, _), _, _) =>
                        inner match
                            case SIR.Let(List(Binding(satName, satTp, _)), _, f, _) =>
                                assert(satName == fq + "$sat")
                                assert(satTp == intToInt, "only `n` stays in the fixpoint")
                                assert(f.isRec)
                            case other => fail(s"expected sat letrec: $other")
                    case other => fail(s"expected 2-param wrapper: $other")
            case other => fail(s"expected $fq let: $other")
    }

    test("typeParams stay on the wrapper lambda") {
        // let rec go = Λ[A]. λf: A -> A. λn: Int. λx: A.
        //     if n == 0 then x else go f (n - 1) (f x)
        val tvA = SIRType.TypeVar("A", Some(1L), SIRType.TypeVarKind.Fixed)
        val aToA = SIRType.Fun(tvA, tvA)
        val polyTp = SIRType.TypeLambda(
          List(tvA),
          SIRType.Fun(aToA, SIRType.Fun(intTp, SIRType.Fun(tvA, tvA)))
        )
        val selfCall = app(
          app(
            app(v("go", polyTp), v("f", aToA), SIRType.Fun(intTp, SIRType.Fun(tvA, tvA))),
            SIRBuiltins.subtractInteger $ v("n", intTp) $ intConst(1),
            SIRType.Fun(tvA, tvA)
          ),
          app(v("f", aToA), v("x", tvA), tvA),
          tvA
        )
        val rhs = SIR.LamAbs(
          v("f", aToA),
          SIR.LamAbs(
            v("n", intTp),
            SIR.LamAbs(
              v("x", tvA),
              SIR.IfThenElse(
                extractAnnotated(SIRBuiltins.equalsInteger $ v("n", intTp) $ intConst(0)),
                v("x", tvA),
                selfCall,
                tvA,
                ann
              ),
              List.empty,
              ann
            ),
            List.empty,
            ann
          ),
          List(tvA), // type params sit on the outermost lambda
          ann
        )
        val prog = SIR.Let(
          List(Binding("go", polyTp, rhs)),
          intConst(0),
          SIR.LetFlags.Recursivity,
          ann
        )
        StaticArgumentTransformation(prog) match
            case SIR.Let(List(Binding("go", _, wrapper)), _, _, _) =>
                wrapper match
                    case SIR.LamAbs(
                          fp,
                          SIR.LamAbs(np, SIR.LamAbs(xp, inner, xt, _), nt, _),
                          ft,
                          _
                        ) =>
                        assert(ft == List(tvA), "type params must stay on the wrapper")
                        assert(nt.isEmpty && xt.isEmpty)
                        assert(List(fp.name, np.name, xp.name) == List("f", "n", "x"))
                        inner match
                            case SIR.Let(List(Binding("go$sat", satTp, satLam)), _, f, _) =>
                                assert(f.isRec)
                                // f is static; n and x keep recursing
                                assert(satTp == SIRType.Fun(intTp, SIRType.Fun(tvA, tvA)))
                                satLam match
                                    case SIR.LamAbs(p1, SIR.LamAbs(p2, _, tp2, _), tp1, _) =>
                                        assert(List(p1.name, p2.name) == List("n", "x"))
                                        assert(tp1.isEmpty && tp2.isEmpty)
                                    case other => fail(s"expected 2-param sat lambda: $other")
                            case other => fail(s"expected go$$sat letrec: $other")
                    case other => fail(s"expected 3-param wrapper: $other")
            case other => fail(s"expected go let: $other")
    }
}
