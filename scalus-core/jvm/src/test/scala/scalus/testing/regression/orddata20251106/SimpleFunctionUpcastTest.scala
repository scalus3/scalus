package scalus.testing.regression.orddata20251106

import org.scalatest.funsuite.AnyFunSuite
import scalus.*

/** Test simple function upcasting without enum representation issues */
class SimpleFunctionUpcastTest extends AnyFunSuite {

    given scalus.Compiler.Options = scalus.Compiler.Options(
      targetLoweringBackend = scalus.Compiler.TargetLoweringBackend.SirToUplcV3Lowering,
      generateErrorTraces = true,
      optimizeUplc = false,
      debug = false
    )

    test("Simple function upcast should work") {
        sealed trait Animal
        case class Dog() extends Animal
        case class Cat() extends Animal

        // Test upcasting () => Dog to () => Animal
        val sir = Compiler.compile {
            val getDog: () => Dog = () => Dog()
            val getAnimal: () => Animal = getDog // This should trigger upcast
            getAnimal()
        }

        val uplc = sir.toUplc(generateErrorTraces = true)
        assert(uplc != null)
    }

    test("BigInt function upcast should work") {
        // Test upcasting () => BigInt to () => Any
        val sir = Compiler.compile {
            val getNum: () => BigInt = () => BigInt(42)
            val getAny: () => Any = getNum // This should trigger upcast
            getAny()
        }

        val uplc = sir.toUplc(generateErrorTraces = true)
        assert(uplc != null)
    }
}
