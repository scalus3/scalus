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

/** Tests for UplcConstr representation — case classes/enums with function fields.
  *
  * Case classes containing function fields (like `BigInt => BigInt`) cannot use
  * ProdDataConstr because functions can't be converted to Data. They use
  * ProdUplcConstr/SumUplcConstr instead, stored as UPLC Constr(tag, [fields]).
  */
class UplcConstrTest extends AnyFunSuite {

    private given PlutusVM = PlutusVM.makePlutusV3VM()
    private given Options = Options()

    // TODO: needs ProdUplcConstr → ProdDataList conversion path in ProductCaseSirTypeGenerator
    ignore("enum with function variant: construct and match") {
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
