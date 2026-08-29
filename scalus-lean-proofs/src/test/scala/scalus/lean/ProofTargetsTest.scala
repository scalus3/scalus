package scalus.lean

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.uplc.Term.asTerm
import scalus.uplc.eval.{PlutusVM, Result}

/** Every declared sample must actually hold when the compiled UPLC is run on the JVM. This catches
  * a mis-declared expectation here rather than in Lean, where it would surface as a confusing proof
  * failure.
  */
class ProofTargetsTest extends AnyFunSuite {
    private given PlutusVM = PlutusVM.makePlutusV3VM()

    test("target names are unique and file-name safe") {
        val names = ProofTargets.all.map(_.name)
        assert(names.distinct == names, s"duplicate target names: ${names.diff(names.distinct)}")
        names.foreach { n =>
            assert(n.matches("[a-z0-9_]+"), s"target name '$n' must be lower_snake_case")
        }
    }

    test("declared arity matches the number of arguments in every sample") {
        ProofTargets.all.foreach { t =>
            t.samples.foreach { case (args, _) =>
                assert(
                  args.length == t.arity,
                  s"${t.name}: sample ${args} does not match arity ${t.arity}"
                )
            }
        }
    }

    test("every sample evaluates to its declared expected value") {
        ProofTargets.all.foreach { t =>
            t.samples.foreach { case (args, expected) =>
                val applied = args.foldLeft(t.program)((acc, a) => acc $ a.asTerm)
                applied.term.evaluateDebug match
                    case s: Result.Success =>
                        assert(s.term == expected.asTerm, s"${t.name}(${args.mkString(",")})")
                    case f: Result.Failure =>
                        fail(s"${t.name}(${args.mkString(",")}) failed: ${f.exception}")
            }
        }
    }

    test("leanName converts snake_case to camelCase") {
        assert(ProofTarget.leanNameOf("math_gcd") == "mathGcd")
        assert(ProofTarget.leanNameOf("math_is_sqrt") == "mathIsSqrt")
        assert(ProofTarget.leanNameOf("always_ok") == "alwaysOk")
    }
}
