package scalus.builtin

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.Compiler.compile
import scalus.builtin.Builtins.*
import scalus.uplc.{Constant, DeBruijnedProgram, Term}
import scalus.uplc.eval.{CountingBudgetSpender, PlutusVM, Result, TallyingBudgetSpenderLogger}

class BuiltinArrayTest extends AnyFunSuite {

    given PlutusVM = PlutusVM.makePlutusV3VM()

    test("BuiltinArray constructor compile") {
        val sir = compile {
            val array = BuiltinArray(iData(1), iData(2), iData(3))
            lengthOfArray(array)
        }
        val term = sir.toUplc()
        val result = term.evaluateDebug
        result match {
            case Result.Success(evaled, _, _, _) =>
                assert(evaled == Term.Const(scalus.uplc.Constant.Integer(3)))
            case Result.Failure(err, _, _, _) =>
                fail(s"Expected success but got failure: $err")
        }
    }

    test("lengthOfArray returns correct length") {
        val sir = compile {
            val list = mkCons(iData(1), mkCons(iData(2), mkCons(iData(3), mkNilData())))
            val array = listToArray(list)
            lengthOfArray(array)
        }
        val term = sir.toUplc()
        val result = term.evaluateDebug
        result match {
            case Result.Success(evaled, _, _, _) =>
                assert(evaled == Term.Const(scalus.uplc.Constant.Integer(3)))
            case Result.Failure(err, _, _, _) =>
                fail(s"Expected success but got failure: $err")
        }
    }

    test("indexArray returns correct element") {
        val sir = compile {
            val list = mkCons(iData(10), mkCons(iData(20), mkCons(iData(30), mkNilData())))
            val array = listToArray(list)
            indexArray(array, BigInt(1))
        }
        val term = sir.toUplc()
        val result = term.evaluateDebug
        result match {
            case Result.Success(evaled, _, _, _) =>
                // Index 1 should be the second element (20)
                assert(evaled == Term.Const(scalus.uplc.Constant.Data(Data.I(20))))
            case Result.Failure(err, _, _, _) =>
                fail(s"Expected success but got failure: $err")
        }
    }

    test("listToArray converts list to array") {
        val sir = compile {
            val list = mkCons(iData(1), mkCons(iData(2), mkNilData()))
            val array = listToArray(list)
            lengthOfArray(array)
        }
        val term = sir.toUplc()
        val result = term.evaluateDebug
        result match {
            case Result.Success(evaled, _, _, _) =>
                assert(evaled == Term.Const(scalus.uplc.Constant.Integer(2)))
            case Result.Failure(err, _, _, _) =>
                fail(s"Expected success but got failure: $err")
        }
    }

    test("empty array has length 0") {
        val sir = compile {
            val emptyList: BuiltinList[Data] = mkNilData()
            val array = listToArray(emptyList)
            lengthOfArray(array)
        }
        val term = sir.toUplc()
        val result = term.evaluateDebug
        result match {
            case Result.Success(evaled, _, _, _) =>
                assert(evaled == Term.Const(scalus.uplc.Constant.Integer(0)))
            case Result.Failure(err, _, _, _) =>
                fail(s"Expected success but got failure: $err")
        }
    }

    test("indexArray with out of bounds index fails") {
        val sir = compile {
            val list = mkCons(iData(1), mkNilData())
            val array = listToArray(list)
            indexArray(array, BigInt(5)) // Out of bounds
        }
        val term = sir.toUplc()
        val result = term.evaluateDebug
        assert(result.isFailure, s"Expected failure but got: $result")
    }

    test("indexArray with first element (index 0)") {
        val sir = compile {
            val list = mkCons(iData(100), mkCons(iData(200), mkNilData()))
            val array = listToArray(list)
            indexArray(array, BigInt(0))
        }
        val term = sir.toUplc()
        val result = term.evaluateDebug
        result match {
            case Result.Success(evaled, _, _, _) =>
                assert(evaled == Term.Const(scalus.uplc.Constant.Data(Data.I(100))))
            case Result.Failure(err, _, _, _) =>
                fail(s"Expected success but got failure: $err")
        }
    }

    test("indexArray with last element") {
        val sir = compile {
            val list = mkCons(iData(1), mkCons(iData(2), mkCons(iData(3), mkNilData())))
            val array = listToArray(list)
            indexArray(array, BigInt(2))
        }
        val term = sir.toUplc()
        val result = term.evaluateDebug
        result match {
            case Result.Success(evaled, _, _, _) =>
                assert(evaled == Term.Const(scalus.uplc.Constant.Data(Data.I(3))))
            case Result.Failure(err, _, _, _) =>
                fail(s"Expected success but got failure: $err")
        }
    }

    test("indexArray with negative index fails") {
        val sir = compile {
            val list = mkCons(iData(1), mkCons(iData(2), mkNilData()))
            val array = listToArray(list)
            indexArray(array, BigInt(-1))
        }
        val term = sir.toUplc()
        val result = term.evaluateDebug
        assert(result.isFailure, s"Expected failure but got: $result")
    }

    test("BuiltinArray flat encoding roundtrip with Data elements") {
        // Test flat encoding through the compile -> toUplc -> flatEncoded path
        val sir = compile {
            val list = mkCons(iData(1), mkCons(iData(2), mkCons(iData(3), mkNilData())))
            lengthOfArray(listToArray(list))
        }
        val term = sir.toUplc()
        val program = term.plutusV3
        val flatEncoded = program.flatEncoded

        // Decode and verify roundtrip
        val decoded = DeBruijnedProgram.fromFlatEncoded(flatEncoded)
        assert(decoded.version == program.version)

        val result = summon[PlutusVM].evaluateDeBruijnedTerm(decoded.term)
        assert(result == Term.Const(Constant.Integer(3)))
    }

    test("BuiltinArray flat encoding roundtrip with empty array") {
        val sir = compile {
            val emptyList: BuiltinList[Data] = mkNilData()
            lengthOfArray(listToArray(emptyList))
        }
        val program = sir.toUplc().plutusV3
        val flatEncoded = program.flatEncoded

        val decoded = DeBruijnedProgram.fromFlatEncoded(flatEncoded)
        assert(decoded.version == program.version)

        val result = summon[PlutusVM].evaluateDeBruijnedTerm(decoded.term)
        assert(result == Term.Const(Constant.Integer(0)))
    }

    test("BuiltinArray flat encoding roundtrip with ByteString Data elements") {
        val sir = compile {
            val list = mkCons(
              bData(ByteString.fromHex("deadbeef")),
              mkCons(bData(ByteString.fromHex("cafebabe")), mkNilData())
            )
            lengthOfArray(listToArray(list))
        }
        val program = sir.toUplc().plutusV3
        val flatEncoded = program.flatEncoded

        val decoded = DeBruijnedProgram.fromFlatEncoded(flatEncoded)
        assert(decoded.version == program.version)

        val result = summon[PlutusVM].evaluateDeBruijnedTerm(decoded.term)
        assert(result == Term.Const(Constant.Integer(2)))
    }
}
