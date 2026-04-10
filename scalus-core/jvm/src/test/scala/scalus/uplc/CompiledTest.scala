package scalus.uplc

import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.builtin.Data
import scalus.uplc.builtin.Data.toData
import scalus.compiler.Options
import scalus.uplc.Term.asTerm
import scalus.uplc.eval.PlutusVM

class CompiledTest extends AnyFunSuite {
    private given Options = Options.release

    test("PlutusV1 compile and apply should work") {
        given PlutusVM = PlutusVM.makePlutusV1VM()
        val dataToBigInt = PlutusV1.compile((sc: Data) => sc.to[BigInt])
        assert(dataToBigInt.language == scalus.cardano.ledger.Language.PlutusV1)
        assert(dataToBigInt(1.toData).program.term.evaluate == 1.asTerm)
        assert(dataToBigInt(1.toData).code == BigInt(1))
    }

    test("PlutusV2 compile and apply should work") {
        given PlutusVM = PlutusVM.makePlutusV2VM()
        val dataToBigInt = PlutusV2.compile((sc: Data) => sc.to[BigInt])
        assert(dataToBigInt.language == scalus.cardano.ledger.Language.PlutusV2)
        assert(dataToBigInt(2.toData).program.term.evaluate == 2.asTerm)
        assert(dataToBigInt(2.toData).code == BigInt(2))
    }

    test("PlutusV3 compile and apply should work") {
        given PlutusVM = PlutusVM.makePlutusV3VM()
        val dataToBigInt = PlutusV3.compile((sc: Data) => sc.to[BigInt])
        assert(dataToBigInt.language == scalus.cardano.ledger.Language.PlutusV3)
        assert(dataToBigInt(3.toData).program.term.evaluate == 3.asTerm)
        assert(dataToBigInt(3.toData).code == BigInt(3))
    }

    test("PlutusV1 should produce correct script hash") {
        val alwaysOk = PlutusV1.alwaysOk
        assert(
          alwaysOk.script.scriptHash.toHex == "fc61e623d413aa67dc9367e8e48f5ab7f38093e871af6d9dd27b717e"
        )
        val expected = Program
            .parseUplc("(program 1.0.0 (lam x (lam y (lam z (con unit ())))))")
            .toOption
            .get
            .deBruijnedProgram
            .term
        assert(alwaysOk.program.deBruijnedProgram.term α_== expected)
    }

    test("PlutusV2 should produce correct script hash") {
        val alwaysOk = PlutusV2.alwaysOk
        assert(
          alwaysOk.script.scriptHash.toHex == "52c6af0c9b744b4eecce838538a52ceb155038b3de68e2bb2fa8fc37"
        )
        val expected = Program
            .parseUplc("(program 1.0.0 (lam x (lam y (lam z (con unit ())))))")
            .toOption
            .get
            .deBruijnedProgram
            .term
        assert(alwaysOk.program.deBruijnedProgram.term α_== expected)
    }

    test("PlutusV3 should produce correct script hash") {
        val alwaysOk = PlutusV3.alwaysOk
        assert(
          alwaysOk.script.scriptHash.toHex == "186e32faa80a26810392fda6d559c7ed4721a65ce1c9d4ef3e1c87b4"
        )
        val expected = Program
            .parseUplc("(program 1.1.0 (lam x (con unit ())))")
            .toOption
            .get
            .deBruijnedProgram
            .term
        assert(alwaysOk.program.deBruijnedProgram.term α_== expected)
    }

    test("withErrorTraces should work for all versions") {
        val v1 = PlutusV1.alwaysOk
        val v2 = PlutusV2.alwaysOk
        val v3 = PlutusV3.alwaysOk

        assert(!v1.options.generateErrorTraces)
        assert(!v2.options.generateErrorTraces)
        assert(!v3.options.generateErrorTraces)

        assert(v1.withErrorTraces.options.generateErrorTraces)
        assert(v2.withErrorTraces.options.generateErrorTraces)
        assert(v3.withErrorTraces.options.generateErrorTraces)
    }
}
