package scalus.compiler.sir.lowering

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.compiler.{Compile, compile}
import scalus.compiler.Options
import scalus.cardano.onchain.plutus.prelude.*
import scalus.cardano.onchain.plutus.prelude.Option.*
import scalus.uplc.builtin.Data
import scalus.uplc.builtin.Data.{fromData, toData}
import scalus.uplc.eval.{PlutusVM, Result}

enum Action:
    case Const(value: BigInt)
    case Transform(f: BigInt => BigInt)

@Compile
object ActionModule {
    def run(action: Action, input: BigInt): BigInt = action match
        case Action.Const(v)     => v
        case Action.Transform(f) => f(input)
}

/** A case class with two function fields: decoder and encoder for type T.
  * Cannot be Data-encoded (functions aren't Data), so uses ProdUplcConstr.
  */
case class Codec[T](decode: Data => Option[T], encode: T => Data)

@Compile
object CodecModule {
    import Option.*

    def roundtrip[T](codec: Codec[T], d: Data): Option[Data] =
        codec.decode(d) match
            case Some(value) => Some(codec.encode(value))
            case None        => None
}

/** Tests for UplcConstr representation — case classes/enums with function fields.
  *
  * Case classes containing function fields (like `BigInt => BigInt`) cannot use
  * ProdDataConstr because functions can't be converted to Data. They use
  * ProdUplcConstr/SumUplcConstr instead, stored as UPLC Constr(tag, [fields]).
  */
class UplcConstrTest extends AnyFunSuite {

    private given PlutusVM = PlutusVM.makePlutusV3VM()
    private given Options = Options()

    test("case class with two function fields: codec roundtrip") {
        val sir = compile {
            import Option.*

            val intCodec = Codec[BigInt](
              decode = d => Some(fromData[BigInt](d)),
              encode = x => toData[BigInt](x)
            )

            // roundtrip: decode Data(42) → Some(42) → encode → Some(Data(42))
            CodecModule.roundtrip(intCodec, toData[BigInt](BigInt(42))) match
                case Some(d) => fromData[BigInt](d)
                case None    => BigInt(0)
        }
        val result = sir.toUplc().evaluateDebug
        result match
            case Result.Success(term, _, _, _) =>
                info(s"Result: ${term.show}")
                assert(term.show.contains("42"))
            case Result.Failure(ex, _, _, _) =>
                fail(s"Codec roundtrip failed: $ex")
    }

    test("codec with ByteString type") {
        val sir = compile {
            import Option.*
            import scalus.uplc.builtin.ByteString

            val bsCodec = Codec[ByteString](
              decode = d => Some(fromData[ByteString](d)),
              encode = x => toData[ByteString](x)
            )

            val testBs = ByteString.fromString("hello")
            CodecModule.roundtrip(bsCodec, toData[ByteString](testBs)) match
                case Some(d) => fromData[ByteString](d)
                case None    => ByteString.empty
        }
        val result = sir.toUplc().evaluateDebug
        result match
            case Result.Success(term, _, _, _) =>
                info(s"Result: ${term.show}")
                assert(term.show.contains("68656c6c6f")) // "hello" in hex
            case Result.Failure(ex, _, _, _) =>
                fail(s"ByteString codec failed: $ex")
    }

    test("enum with function variant: construct and match") {
        val sir = compile {
            val a1 = Action.Const(BigInt(10))
            val a2 = Action.Transform(x => x + BigInt(1))

            // run(Const(10), 5) + run(Transform(_ + 1), 5)  = 10 + 6 = 16
            ActionModule.run(a1, BigInt(5)) + ActionModule.run(a2, BigInt(5))
        }
        val result = sir.toUplc().evaluateDebug
        result match
            case Result.Success(term, _, _, _) =>
                info(s"Result: ${term.show}")
                assert(term.show.contains("16"))
            case Result.Failure(ex, _, _, _) =>
                fail(s"UplcConstr enum with function variant failed: $ex")
    }
}
