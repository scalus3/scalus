package scalus.compiler.sir

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.compiler.compile
import scalus.compiler.Compile

// Scope without @Compile - used for type-only tests
object OpaqueTypeTestScope {

    opaque type MyInt = BigInt

    opaque type MyBS <: scalus.uplc.builtin.ByteString = scalus.uplc.builtin.ByteString

    opaque type Wrapped[A] = (A, A)
}

// Scope with @Compile - extension methods on opaque types
@Compile
object OpaqueTypeCompiledScope {

    opaque type MyInt = BigInt

    def mkMyInt(x: BigInt): MyInt = x

    extension (x: MyInt) def add(y: MyInt): MyInt = x + y
}

class OpaqueTypeTest extends AnyFunSuite {

    test("simple opaque type resolves to underlying type") {
        import OpaqueTypeTestScope.*

        val sir = compile { (x: MyInt) =>
            x
        }

        sir.tp match {
            case SIRType.Fun(SIRType.Integer, SIRType.Integer) => succeed
            case _ => fail(s"expected Fun(Integer, Integer) but got ${sir.tp.show}")
        }
    }

    test("opaque type with upper bound resolves to underlying type") {
        import OpaqueTypeTestScope.*

        val sir = compile { (x: MyBS) =>
            x
        }

        sir.tp match {
            case SIRType.Fun(SIRType.ByteString, SIRType.ByteString) => succeed
            case _ => fail(s"expected Fun(ByteString, ByteString) but got ${sir.tp.show}")
        }
    }

    test("parameterized opaque type resolves to underlying type") {
        import OpaqueTypeTestScope.*

        val sir = compile { (x: Wrapped[BigInt]) =>
            x
        }

        sir.tp match {
            case SIRType.Fun(tp1, tp2) =>
                // Wrapped[BigInt] resolves to (BigInt, BigInt) = Tuple2[BigInt, BigInt]
                assert(
                  tp1.show.contains("Tuple2") || tp1.isInstanceOf[SIRType.CaseClass],
                  s"expected Tuple2 type but got ${tp1.show}"
                )
                assert(
                  tp1 == tp2,
                  s"expected same type for input and output but got ${tp1.show} and ${tp2.show}"
                )
            case _ =>
                fail(s"unexpected type ${sir.tp.show}")
        }
    }

    test("extension method on opaque type in @Compile object") {
        import OpaqueTypeCompiledScope.*

        val sir = compile { (x: MyInt, y: MyInt) =>
            x.add(y)
        }

        sir.tp match {
            case SIRType.Fun(SIRType.Integer, SIRType.Fun(SIRType.Integer, SIRType.Integer)) =>
                succeed
            case _ =>
                fail(
                  s"expected Fun(Integer, Fun(Integer, Integer)) but got ${sir.tp.show}"
                )
        }
    }

    test("factory method on opaque type in @Compile object") {
        import OpaqueTypeCompiledScope.*

        val sir = compile { (x: BigInt) =>
            mkMyInt(x)
        }

        sir.tp match {
            case SIRType.Fun(SIRType.Integer, SIRType.Integer) => succeed
            case _ => fail(s"expected Fun(Integer, Integer) but got ${sir.tp.show}")
        }
    }

    test("opaque type in @Compile object: factory method via companion pattern") {
        import scalus.compiler.sir.opaquetypes.TopMyInt
        import TopMyInt.*

        val sir = compile { (x: BigInt) =>
            TopMyInt(x)
        }

        sir.tp match {
            case SIRType.Fun(SIRType.Integer, SIRType.Integer) => succeed
            case _ => fail(s"expected Fun(Integer, Integer) but got ${sir.tp.show}")
        }
    }

    test("opaque type in @Compile object: extension method via companion pattern") {
        import scalus.compiler.sir.opaquetypes.TopMyInt
        import TopMyInt.*

        val sir = compile { (x: T, y: T) =>
            x.add(y)
        }

        sir.tp match {
            case SIRType.Fun(SIRType.Integer, SIRType.Fun(SIRType.Integer, SIRType.Integer)) =>
                succeed
            case _ =>
                fail(
                  s"expected Fun(Integer, Fun(Integer, Integer)) but got ${sir.tp.show}"
                )
        }
    }

    test("top-level opaque type with @Compile companion: factory method") {
        import scalus.compiler.sir.toplevelope.*

        val sir = compile { (x: BigInt) =>
            TopMyInt(x)
        }

        sir.tp match {
            case SIRType.Fun(SIRType.Integer, SIRType.Integer) => succeed
            case _ => fail(s"expected Fun(Integer, Integer) but got ${sir.tp.show}")
        }
    }

    test("top-level opaque type with @Compile companion: extension method") {
        import scalus.compiler.sir.toplevelope.*

        val sir = compile { (x: TopMyInt, y: TopMyInt) =>
            x.add(y)
        }

        sir.tp match {
            case SIRType.Fun(SIRType.Integer, SIRType.Fun(SIRType.Integer, SIRType.Integer)) =>
                succeed
            case _ =>
                fail(
                  s"expected Fun(Integer, Fun(Integer, Integer)) but got ${sir.tp.show}"
                )
        }
    }
}
