package scalus.compiler.sir.lowering

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.compiler.compile
import scalus.compiler.Options
import scalus.compiler.sir.TargetLoweringBackend
import scalus.uplc.eval.{PlutusVM, Result}
import scalus.cardano.onchain.plutus.prelude.List
import scalus.cardano.onchain.plutus.prelude.List.{Cons, Nil}

/** Tests for nativeTypeVarRepresentation flag.
  *
  * Explores how type variables interact with native representations. The core question: when a
  * generic function like `fill[A]` creates Nil inside its body, what UPLC element type should the
  * empty list have? UPLC has no polymorphism — `List(Integer, [])` and `List(Data, [])` are
  * different values.
  *
  * Scenarios:
  *   1. identity[A](x: A) = x — no list creation, should work
  *   2. singleton[A](x: A) = Cons(x, Nil) — creates typed Nil
  *   3. fill[A](x: A, n) = if n > 0 then Cons(x, fill(x, n-1)) else Nil — recursive, creates Nil
  *   4. map[A,B](f, list) — transforms elements, creates Nil at base case
  *   5. foldLeft[A,B](list, z)(f) — accumulates, B is type var for accumulator
  */
class TypeVarRepresentationTest extends AnyFunSuite {

    private given PlutusVM = PlutusVM.makePlutusV3VM()

    private val allModes: scala.List[(Boolean, Boolean, String)] = scala.List(
      (false, false, "native=off, typevar=off"),
      (true, false, "native=on, typevar=off")
      // (true, true) disabled: nativeTypeVarRepresentation requires NativeList + NativeRepr infrastructure
    )

    private def withAllModes(testName: String)(body: (Boolean, Boolean) => Unit): Unit =
        allModes.foreach { case (native, typevar, label) =>
            test(s"$testName [$label]") {
                body(native, typevar)
            }
        }

    private def opts(native: Boolean, typevar: Boolean): Options = Options(
      targetLoweringBackend = TargetLoweringBackend.SirToUplcV3Lowering,
      generateErrorTraces = true,
      nativeListElements = native,
      nativeTypeVarRepresentation = typevar
    )

    // === Scenario 1: identity — no list ops, just pass-through ===
    withAllModes("identity[BigInt] — no list, just type var pass-through") { (native, typevar) =>
        given Options = opts(native, typevar)
        val sir = compile {
            def identity[A](x: A): A = x
            identity[BigInt](BigInt(42))
        }
        val result = sir.toUplc().evaluateDebug
        result match
            case Result.Success(term, _, _, _) =>
                info(s"Result: ${term.show}")
            case Result.Failure(ex, _, _, _) =>
                fail(s"identity failed: $ex")
    }

    // === Scenario 2: singleton — creates Nil for List[A] ===
    withAllModes("singleton[BigInt] — Cons(x, Nil) with type var") { (native, typevar) =>
        given Options = opts(native, typevar)
        val sir = compile {
            def singleton[A](x: A): List[A] = Cons(x, Nil)
            singleton[BigInt](BigInt(7))
        }
        if native && typevar then info(s"singleton SIR:\n${sir.show}")
        val result = sir.toUplc().evaluateDebug
        result match
            case Result.Success(term, _, _, _) =>
                info(s"Result: ${term.show}")
            case Result.Failure(ex, _, _, _) =>
                fail(s"singleton failed: $ex")
    }

    // === Scenario 3: fill — recursive, creates Nil at base case ===
    withAllModes("fill[BigInt] — recursive generic with Nil base case") { (native, typevar) =>
        given Options = opts(native, typevar)
        val sir = compile {
            List.fill[BigInt](BigInt(1), BigInt(3))
        }
        val result = sir.toUplc().evaluateDebug
        result match
            case Result.Success(term, _, _, _) =>
                info(s"Result: ${term.show}")
            case Result.Failure(ex, _, _, _) =>
                fail(s"fill failed: $ex")
    }

    // === Scenario 4: map — transforms elements, Nil at base ===
    withAllModes("List.map[BigInt, BigInt] — generic map with Nil base") { (native, typevar) =>
        given Options = opts(native, typevar)
        val sir = compile {
            val list = Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil)))
            list.map[BigInt](x => x + BigInt(10))
        }
        if native && typevar then info(s"map SIR:\n${sir.show}")
        val result =
            if native && !typevar then
                // Debug mode for the failing combination
                scalus.compiler.sir.lowering.SirToUplcV3Lowering
                    .fromOptions(sir, summon[Options], debug = true)
                    .lower()
                    .evaluateDebug
            else sir.toUplc().evaluateDebug
        result match
            case Result.Success(term, _, _, _) =>
                info(s"Result: ${term.show}")
            case Result.Failure(ex, _, _, _) =>
                fail(s"map failed: $ex")
    }

    // === Scenario 5: foldLeft — accumulator is type var B ===
    withAllModes("List.foldLeft[BigInt] — accumulator type var") { (native, typevar) =>
        given Options = opts(native, typevar)
        val sir = compile {
            val list = Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil)))
            list.foldLeft[BigInt](BigInt(0))((acc, x) => acc + x)
        }
        val result = sir.toUplc().evaluateDebug
        result match
            case Result.Success(term, _, _, _) =>
                info(s"Result: ${term.show}")
            case Result.Failure(ex, _, _, _) =>
                fail(s"foldLeft failed: $ex")
    }

    // === Scenario 6: non-recursive generic returning List ===
    withAllModes("non-recursive wrap[BigInt] — single Cons, no recursion") { (native, typevar) =>
        given Options = opts(native, typevar)
        val sir = compile {
            def wrap[A](x: A, y: A): List[A] = Cons(x, Cons(y, Nil))
            wrap[BigInt](BigInt(10), BigInt(20))
        }
        val result = sir.toUplc().evaluateDebug
        result match
            case Result.Success(term, _, _, _) =>
                info(s"Result: ${term.show}")
            case Result.Failure(ex, _, _, _) =>
                fail(s"wrap failed: $ex")
    }
}
