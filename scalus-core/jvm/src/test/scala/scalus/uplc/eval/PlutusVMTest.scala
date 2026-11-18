package scalus.uplc.eval

import org.scalatest.funsuite.AnyFunSuiteLike
import scalus.uplc.Constant
import scalus.uplc.DeBruijn
import scalus.uplc.Program
import scalus.uplc.Term.*

class PlutusVMTest extends AnyFunSuiteLike {
    private val v2vm = PlutusVM.makePlutusV2VM()
    private val v3vm = PlutusVM.makePlutusV3VM()

    test(
      "Plutus V3 VM throws InvalidReturnValue on non Unit result according to CIP-117"
    ) {
        assertThrows[InvalidReturnValue] {
            v3vm.evaluateScript(
              Program.plutusV3(Const(Constant.Bool(true))).deBruijnedProgram,
              NoBudgetSpender,
              NoLogger
            )
        }
        assert(
          !v3vm
              .evaluateScriptDebug(Program.plutusV3(Const(Constant.Bool(true))).deBruijnedProgram)
              .isSuccess
        )
    }

    test("Plutus V3 VM succeeds on Unit result") {
        assert(
          v3vm.evaluateScript(
            Program.plutusV3(Const(Constant.Unit)).deBruijnedProgram,
            NoBudgetSpender,
            NoLogger
          ) == Const(Constant.Unit)
        )
        assert(
          v3vm.evaluateScriptDebug(Program.plutusV3(Const(Constant.Unit)).deBruijnedProgram)
              .isSuccess
        )
    }

    test("Plutus V2 VM succeeds on non Unit result") {
        assert(
          v2vm.evaluateScript(
            Program.plutusV2(Const(Constant.Bool(true))).deBruijnedProgram,
            NoBudgetSpender,
            NoLogger
          ) == Const(Constant.Bool(true))
        )
        assert(
          v2vm.evaluateScriptDebug(Program.plutusV2(Const(Constant.Bool(true))).deBruijnedProgram)
              .isSuccess
        )
    }

    test("evaluateDeBruijnedTerm fails on non-debruijned term") {
        // A named variable has index 0, which the machine rejects when it looks the variable up.
        // The term must look it up: like Plutus, the machine returns an unapplied lambda as it is.
        val term = λ(x => x) $ Const(Constant.Bool(true))
        val e = intercept[MachineError] {
            v2vm.evaluateDeBruijnedTerm(term, NoBudgetSpender, NoLogger)
        }
        assert(e.getMessage.contains("DeBruijn.deBruijnTerm"))
    }

    test("evaluateDeBruijnedTerm evaluates debruijned term") {
        val term = λ(x => x) $ Const(Constant.Bool(true))
        assert(
          v2vm.evaluateDeBruijnedTerm(
            DeBruijn.deBruijnTerm(term),
            NoBudgetSpender,
            NoLogger
          ) == Const(Constant.Bool(true))
        )
    }
}
