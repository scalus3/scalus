package scalus.compiler.sir

import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.DefaultFun

class RemoveTracesSpec extends AnyFunSuite {

    private val anns = AnnotationsDecl.empty

    private def traceCall(msg: AnnotatedSIR, value: AnnotatedSIR): SIR.Apply = {
        val traceBuiltin = SIR.Builtin(
          DefaultFun.Trace,
          SIRType.Fun(SIRType.String, SIRType.Fun(SIRType.Unit, SIRType.Unit)),
          anns
        )
        val partial = SIR.Apply(traceBuiltin, msg, SIRType.Fun(SIRType.Unit, SIRType.Unit), anns)
        SIR.Apply(partial, value, SIRType.Unit, anns)
    }

    private val unitConst = SIR.Const.unit(anns)
    private val intConst = SIR.Const.integer(42, anns)
    private val strConst = SIR.Const.string("hello", anns)

    test("fully applied Trace is replaced by its value argument") {
        val trace = traceCall(strConst, unitConst)
        val result = RemoveTraces.transform(trace)
        assert(result == unitConst)
    }

    test("Trace with non-unit value preserves the value") {
        val trace = traceCall(strConst, intConst)
        val result = RemoveTraces.transform(trace)
        assert(result == intConst)
    }

    test("message subtree with appendString is dropped") {
        val appendStr = SIR.Builtin(
          DefaultFun.AppendString,
          SIRType.Fun(SIRType.String, SIRType.Fun(SIRType.String, SIRType.String)),
          anns
        )
        val concat = SIR.Apply(
          SIR.Apply(
            appendStr,
            SIR.Const.string("prefix: ", anns),
            SIRType.Fun(SIRType.String, SIRType.String),
            anns
          ),
          SIR.Const.string("value", anns),
          SIRType.String,
          anns
        )
        val trace = traceCall(concat, intConst)
        val result = RemoveTraces.transform(trace)
        assert(result == intConst)
    }

    test("non-trace nodes are preserved unchanged") {
        val add = SIR.Apply(
          SIR.Apply(
            SIR.Builtin(
              DefaultFun.AddInteger,
              SIRType.Fun(SIRType.Integer, SIRType.Fun(SIRType.Integer, SIRType.Integer)),
              anns
            ),
            intConst,
            SIRType.Fun(SIRType.Integer, SIRType.Integer),
            anns
          ),
          SIR.Const.integer(1, anns),
          SIRType.Integer,
          anns
        )
        val result = RemoveTraces.transform(add)
        assert(result == add)
    }

    test("nested traces are handled") {
        // trace(msg1)(trace(msg2)(value))
        val inner = traceCall(SIR.Const.string("inner", anns), intConst)
        val outer = traceCall(SIR.Const.string("outer", anns), inner)
        val result = RemoveTraces.transform(outer)
        assert(result == intConst)
    }

    test("trace inside let binding is removed") {
        val trace = traceCall(strConst, unitConst)
        val body = intConst
        val let = SIR.Let(
          List(Binding("_log", SIRType.Unit, trace)),
          body,
          SIR.LetFlags.None,
          anns
        )
        val result = RemoveTraces.transform(let)
        // The binding becomes Const(Unit) and _log is unused in body → binding removed
        assert(result == intConst)
    }

    test("trace inside lambda is removed") {
        val trace = traceCall(strConst, unitConst)
        val param = SIR.Var("x", SIRType.Integer, anns)
        val body = SIR.Let(
          List(Binding("_log", SIRType.Unit, trace)),
          param,
          SIR.LetFlags.None,
          anns
        )
        val lam = SIR.LamAbs(param, body, Nil, anns)
        val result = RemoveTraces.transform(lam)
        // After transform: LamAbs(x, x) with the dead let removed
        val expected = SIR.LamAbs(param, param, Nil, anns)
        assert(result == expected)
    }

    test("trace inside match case body is removed") {
        val trace = traceCall(strConst, unitConst)
        val body = SIR.Let(
          List(Binding("_log", SIRType.Unit, trace)),
          intConst,
          SIR.LetFlags.None,
          anns
        )
        val caseExpr = SIR.Case(SIR.Pattern.Wildcard, body, anns)
        val scrutinee = SIR.Const.boolean(true, anns)
        val matchExpr = SIR.Match(scrutinee, List(caseExpr), SIRType.Integer, anns)
        val result = RemoveTraces.transform(matchExpr)
        val expectedCase = SIR.Case(SIR.Pattern.Wildcard, intConst, anns)
        val expected = SIR.Match(scrutinee, List(expectedCase), SIRType.Integer, anns)
        assert(result == expected)
    }

    test("dead let binding cleanup does not remove used bindings") {
        val xVar = SIR.Var("x", SIRType.Integer, anns)
        val let = SIR.Let(
          List(Binding("x", SIRType.Integer, intConst)),
          xVar,
          SIR.LetFlags.None,
          anns
        )
        val result = RemoveTraces.transform(let)
        assert(result == let) // binding is used, preserved
    }

    test("dead let binding cleanup removes only Unit bindings that are unused") {
        val xVar = SIR.Var("x", SIRType.Integer, anns)
        val trace = traceCall(strConst, unitConst)
        val let = SIR.Let(
          List(
            Binding("_log", SIRType.Unit, trace),
            Binding("x", SIRType.Integer, intConst)
          ),
          xVar,
          SIR.LetFlags.None,
          anns
        )
        val result = RemoveTraces.transform(let)
        // _log binding removed (Unit, unused), x binding preserved (used)
        val expected = SIR.Let(
          List(Binding("x", SIRType.Integer, intConst)),
          xVar,
          SIR.LetFlags.None,
          anns
        )
        assert(result == expected)
    }

    test("trace inside IfThenElse branches is removed") {
        val trace = traceCall(SIR.Const.string("then", anns), intConst)
        val cond = SIR.Const.boolean(true, anns)
        val ite = SIR.IfThenElse(cond, trace, intConst, SIRType.Integer, anns)
        val result = RemoveTraces.transform(ite)
        val expected = SIR.IfThenElse(cond, intConst, intConst, SIRType.Integer, anns)
        assert(result == expected)
    }

    test("partially applied Trace is not removed") {
        val traceBuiltin = SIR.Builtin(
          DefaultFun.Trace,
          SIRType.Fun(SIRType.String, SIRType.Fun(SIRType.Unit, SIRType.Unit)),
          anns
        )
        // Only one Apply - partially applied
        val partial =
            SIR.Apply(traceBuiltin, strConst, SIRType.Fun(SIRType.Unit, SIRType.Unit), anns)
        val result = RemoveTraces.transform(partial)
        assert(result == partial) // preserved as-is
    }
}
