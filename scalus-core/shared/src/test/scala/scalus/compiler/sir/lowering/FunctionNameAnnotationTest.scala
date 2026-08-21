package scalus.compiler.sir.lowering

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.cardano.ledger.MajorProtocolVersion
import scalus.compiler.{compile, Compile, Options}
import scalus.compiler.sir.TargetLoweringBackend
import scalus.uplc.Term

@Compile
object FunctionNameAnnotationFixtures {
    def triple(x: BigInt): BigInt = x + x + x
}

/** V3 lowering stamps the enclosing source function into every UPLC term annotation, so tooling
  * (the VS Code UPLC source view) can group compiled UPLC by the function it came from.
  *
  * The UPLC optimizer is disabled here: this suite is about what the lowering emits, not about how
  * later passes preserve annotations.
  */
class FunctionNameAnnotationTest extends AnyFunSuite {

    private given Options = Options(
      targetLoweringBackend = TargetLoweringBackend.SirToUplcV3Lowering,
      targetProtocolVersion = MajorProtocolVersion.vanRossemPV,
      optimizeUplc = false
    )

    private def collectFunctionNames(t: Term): Set[String] = {
        def go(t: Term, acc: Set[String]): Set[String] = {
            val acc1 =
                if t.annotation.functionName.nonEmpty then acc + t.annotation.functionName else acc
            t match
                case Term.LamAbs(_, body, _)  => go(body, acc1)
                case Term.Apply(f, arg, _)    => go(arg, go(f, acc1))
                case Term.Force(b, _)         => go(b, acc1)
                case Term.Delay(b, _)         => go(b, acc1)
                case Term.Constr(_, args, _)  => args.foldLeft(acc1)((a, x) => go(x, a))
                case Term.Case(arg, cases, _) => cases.foldLeft(go(arg, acc1))((a, x) => go(x, a))
                case _                        => acc1
        }
        go(t, Set.empty)
    }

    test("lowered terms carry the enclosing function name") {
        val sir = compile {
            def double(x: BigInt): BigInt = x + x
            double(21)
        }
        val term = sir.toUplc()
        val names = collectFunctionNames(term)
        assert(names.contains("double"), s"expected 'double' in $names")
    }

    test("linked top-level module functions carry their simple name") {
        val sir = compile {
            FunctionNameAnnotationFixtures.triple(7)
        }
        val names = collectFunctionNames(sir.toUplc())
        assert(names.contains("triple"), s"expected 'triple' in $names")
    }

    test("shared sumEq helper is attributed to its site label, not the triggering function") {
        // NestedListsModule.eqLists compares UplcConstr lists, which makes the lowering emit a
        // cached top-level sumEq helper. The helper is shared by every later call site, so its
        // body must carry the stable label "sumEq" — not "eqLists", the function that happened
        // to trigger its generation first.
        val sir = compile { (a: NestedLists, b: NestedLists) =>
            NestedListsModule.eqLists(a, b)
        }
        val term = sir.toUplc()
        val names = collectFunctionNames(term)
        assert(names.contains("sumEq"), s"expected 'sumEq' in $names")
        // References to the helper variable re-emit the variable's own functionName (see
        // VariableLoweredValue.termInternal), so they must carry the label too, not the name of
        // whichever function triggered helper generation first.
        def collectVars(t: Term): List[Term.Var] = t match
            case v: Term.Var           => List(v)
            case Term.LamAbs(_, b, _)  => collectVars(b)
            case Term.Apply(f, a, _)   => collectVars(f) ++ collectVars(a)
            case Term.Force(b, _)      => collectVars(b)
            case Term.Delay(b, _)      => collectVars(b)
            case Term.Constr(_, as, _) => as.flatMap(collectVars)
            case Term.Case(a, cs, _)   => collectVars(a) ++ cs.flatMap(collectVars)
            case _                     => Nil
        val helperRefs = collectVars(term).filter(_.name.name.contains("sumEq"))
        assert(helperRefs.nonEmpty, "expected at least one reference to the sumEq helper")
        // "" is fine (the let-rec wrapper is emitted outside any function scope); any other
        // user-function name means the construction-order capture bug is back.
        assert(
          helperRefs.forall(v =>
              v.annotation.functionName == "sumEq" || v.annotation.functionName.isEmpty
          ),
          s"helper refs carry ${helperRefs.map(_.annotation.functionName).distinct}"
        )
    }

    test("simpleBindingName strips only what the producer appended") {
        // local binding: the plugin appends `-<symbolId>`
        assert(Lowering.simpleBindingName("double-432208") == "double")
        // a backticked local `retry-2` gets a suffix on top; only the suffix goes
        assert(Lowering.simpleBindingName("retry-2-432208") == "retry-2")
        // linked top-level defs are never suffixed: keep a digit tail
        assert(Lowering.simpleBindingName("pkg.Obj$.bar") == "bar")
        assert(Lowering.simpleBindingName("pkg.Obj$.retry-2") == "retry-2")
    }
}
