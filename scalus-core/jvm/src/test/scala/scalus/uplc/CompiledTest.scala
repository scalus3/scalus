package scalus.uplc

import org.scalatest.funsuite.AnyFunSuite
import scalus.builtin.Data
import scalus.builtin.Data.toData
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

    test("PlutusV1 should produce correct script hash prefix") {
        val validator = PlutusV1.compile((_: Data) => ())
        assert(validator.script.isInstanceOf[scalus.cardano.ledger.Script.PlutusV1])
    }

    test("PlutusV2 should produce correct script hash prefix") {
        val validator = PlutusV2.compile((_: Data) => ())
        assert(validator.script.isInstanceOf[scalus.cardano.ledger.Script.PlutusV2])
    }

    test("PlutusV3 should produce correct script hash prefix") {
        val validator = PlutusV3.compile((_: Data) => ())
        assert(validator.script.isInstanceOf[scalus.cardano.ledger.Script.PlutusV3])
    }

    test("withErrorTraces should work for all versions") {
        val v1 = PlutusV1.compile((_: Data) => ())
        val v2 = PlutusV2.compile((_: Data) => ())
        val v3 = PlutusV3.compile((_: Data) => ())

        assert(!v1.options.generateErrorTraces)
        assert(!v2.options.generateErrorTraces)
        assert(!v3.options.generateErrorTraces)

        assert(v1.withErrorTraces.options.generateErrorTraces)
        assert(v2.withErrorTraces.options.generateErrorTraces)
        assert(v3.withErrorTraces.options.generateErrorTraces)
    }
}
