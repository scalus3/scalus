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
}
