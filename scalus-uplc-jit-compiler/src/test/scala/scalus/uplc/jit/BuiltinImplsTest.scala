package scalus.uplc.jit

import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.builtin.platform
import scalus.uplc.eval.{ConstCostingFun, DefaultCostingFun, MachineParams}
import scalus.uplc.{BuiltinSemanticsVariant, CardanoBuiltins, DefaultFun}

import java.lang.reflect.Modifier

/** The methods of [[BuiltinImpls]] map to builtins by name ([[BuiltinEmitter.implName]]), and a
  * name that maps to nothing is not a compile error. This test guards the mapping, and the rules of
  * the spec for hand-written builtins: every polymorphic builtin has a method ([BI-10]), and every
  * override has a plain costing function ([BI-9], which checks [BI-5]).
  */
class BuiltinImplsTest extends AnyFunSuite {

    /** The public methods `BuiltinImpls` declares, without those of `Object`. */
    private val methods = BuiltinImpls.getClass.getDeclaredMethods.toList
        .filter(m => Modifier.isPublic(m.getModifiers) && !m.isSynthetic)

    private val byName = DefaultFun.values.map(f => BuiltinEmitter.implName(f) -> f).toMap

    /** Whether `bn` has a method in [[BuiltinImpls]], found by its name. */
    private def hasImpl(bn: DefaultFun): Boolean =
        methods.exists(_.getName == BuiltinEmitter.implName(bn))

    test("every polymorphic builtin has a method (15) [BI-10]") {
        val polymorphic = DefaultFun.values.filter(BuiltinEmitter.isPolymorphic).toList
        assert(polymorphic.size == 15)
        assert(polymorphic.filterNot(hasImpl).isEmpty)
    }

    test("every override has a method") {
        assert(BuiltinEmitter.Overrides.filterNot(hasImpl).isEmpty)
    }

    test("every override has a plain DefaultCostingFun or ConstCostingFun in variant E [BI-9]") {
        val costModel = MachineParams.defaultPlutusV3Params.builtinCostModel
        val builtins = CardanoBuiltins(costModel, platform, BuiltinSemanticsVariant.E)
        for fun <- BuiltinEmitter.Overrides do
            builtins.getBuiltinRuntime(fun).costFunction match
                case _: DefaultCostingFun[?] | _: ConstCostingFun => ()
                case other => fail(s"$fun is costed by ${other.getClass.getSimpleName}")
    }

    test("every method is a polymorphic builtin or an override") {
        val stray = methods.map(_.getName).filterNot { name =>
            byName
                .get(name)
                .exists(f => BuiltinEmitter.isPolymorphic(f) || BuiltinEmitter.Overrides(f))
        }
        assert(stray.isEmpty, s"BuiltinImpls methods that are not used: $stray")
    }

    test("no method is overloaded") {
        val overloaded = methods.groupBy(_.getName).collect { case (n, ms) if ms.size > 1 => n }
        assert(overloaded.isEmpty, s"overloaded: $overloaded")
    }

    test("each method takes logger, budget, params and one parameter per term argument") {
        for m <- methods do
            val fun = byName(m.getName)
            val termArgs = BuiltinEmitter.argKinds(fun).count(identity)
            assert(m.getParameterCount == 3 + termArgs, s"${m.getName}")
    }
}
