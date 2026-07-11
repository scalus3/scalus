package scalus.compiler

import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.builtin.{ByteString, Data}
import scalus.compiler.sir.{AnnotationsDecl, SIR, SIRType}
import scalus.compiler.{compile, Options}
import scalus.cardano.onchain.plutus.prelude.List as PList
import scalus.uplc.*
import scalus.uplc.Term.asTerm
import scalus.uplc.eval.PlutusVM
import scalus.toUplc

/** Regression tests for audit finding R1 (docs/local/audits/2026_07_10/R1.md): pattern matching on
  * a Data scrutinee must work on the default compilation target (PlutusV3, changPV), where
  * Case-on-Data is not available and the match must be lowered via the chooseData builtin.
  */
class DataPatternMatchingDefaultTargetTest extends AnyFunSuite:

    // Default V3 VM at the default protocol version: Case on builtin values is not enabled
    private given PlutusVM = PlutusVM.makePlutusV3VM()

    private given Options = Options.default

    private def run(sirFun: SIR, arg: Data): Term = {
        val applied =
            sirFun $ SIR.Const(Constant.Data(arg), SIRType.Data.tp, AnnotationsDecl.empty)
        applied.toUplc().evaluate
    }

    val testConstr =
        Data.Constr(BigInt(42), PList.from(List(Data.I(BigInt(1)), Data.I(BigInt(2)))))
    val testI = Data.I(BigInt(17))
    val testB = Data.B(ByteString.fromHex("DEADBEEF"))
    val testList = Data.List(
      PList.from(List(Data.I(BigInt(1)), Data.I(BigInt(2)), Data.I(BigInt(3))))
    )
    val testMap = Data.Map(
      PList.from(List((Data.I(BigInt(1)), Data.I(BigInt(10)))))
    )

    test("type discrimination on all five Data variants") {
        val sirFun = compile { (d: Data) =>
            d match
                case Data.Constr(_, _) => BigInt(0)
                case Data.Map(_)       => BigInt(1)
                case Data.List(_)      => BigInt(2)
                case Data.I(_)         => BigInt(3)
                case Data.B(_)         => BigInt(4)
        }
        assert(run(sirFun, testConstr) == 0.asTerm)
        assert(run(sirFun, testMap) == 1.asTerm)
        assert(run(sirFun, testList) == 2.asTerm)
        assert(run(sirFun, testI) == 3.asTerm)
        assert(run(sirFun, testB) == 4.asTerm)
    }

    test("extract integer from Data.I with wildcard fallback") {
        val sirFun = compile { (d: Data) =>
            d match
                case Data.I(i) => i
                case _         => BigInt(-1)
        }
        assert(run(sirFun, testI) == 17.asTerm)
        assert(run(sirFun, testB) == (-1).asTerm)
        assert(run(sirFun, testConstr) == (-1).asTerm)
    }

    test("extract bytestring from Data.B") {
        val sirFun = compile { (d: Data) =>
            d match
                case Data.B(b) => b
                case _         => ByteString.empty
        }
        assert(run(sirFun, testB) == Term.Const(Constant.ByteString(testB.value)))
        assert(run(sirFun, testI) == Term.Const(Constant.ByteString(ByteString.empty)))
    }

    test("extract both tag and args from Data.Constr") {
        val sirFun = compile { (d: Data) =>
            d match
                case Data.Constr(tag, args) => tag + args.length
                case _                      => BigInt(-1)
        }
        assert(run(sirFun, testConstr) == 44.asTerm)
        assert(run(sirFun, testI) == (-1).asTerm)
    }

    test("extract elements from Data.List") {
        val sirFun = compile { (d: Data) =>
            d match
                case Data.List(elements) => elements.length
                case _                   => BigInt(-1)
        }
        assert(run(sirFun, testList) == 3.asTerm)
        assert(run(sirFun, testMap) == (-1).asTerm)
    }

    test("extract entries from Data.Map") {
        val sirFun = compile { (d: Data) =>
            d match
                case Data.Map(entries) => entries.length
                case _                 => BigInt(-1)
        }
        assert(run(sirFun, testMap) == 1.asTerm)
        assert(run(sirFun, testList) == (-1).asTerm)
    }
