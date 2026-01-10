package scalus.uplc

import org.scalatest.funsuite.AnyFunSuite
import scalus.builtin.Data
import scalus.builtin.Data.toData
import scalus.compiler.Options
import scalus.uplc.Term.asTerm
import scalus.uplc.eval.PlutusVM

class CompiledTest extends AnyFunSuite {
    private given Options = Options.release
    private given PlutusVM = PlutusVM.makePlutusV3VM()

    test("Compiled apply should work for compiled functions") {
        val dataToBigInt = PlutusV3.compile((sc: Data) => sc.to[BigInt])
        assert(dataToBigInt(1.toData).program.term.evaluate == 1.asTerm)
        assert(dataToBigInt(1.toData).code == BigInt(1))
    }
}
