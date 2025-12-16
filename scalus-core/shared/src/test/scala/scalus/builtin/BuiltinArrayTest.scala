package scalus.builtin

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.Compiler.compile
import scalus.builtin.Builtins.*
import scalus.uplc.{Constant, DeBruijnedProgram, DefaultUni, Term}
import scalus.uplc.eval.{PlutusVM, Result}

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

    // multiIndexArray tests (CIP-0156)
    // Note: multiIndexArray takes List[Integer] as the first argument (per CIP-0156).
    // Since Scalus compiles BuiltinList[BigInt] to List[Data], we test the CEK
    // implementation directly using UPLC Term construction.

    test("multiIndexArray returns elements at specified indices") {
        // Build: multiIndexArray [0, 2] (listToArray [Data(10), Data(20), Data(30)])
        val indices = Term.Const(
          Constant.List(DefaultUni.Integer, List(Constant.Integer(0), Constant.Integer(2)))
        )
        val array = Term.Const(
          Constant.Array(
            DefaultUni.Data,
            Vector(Constant.Data(Data.I(10)), Constant.Data(Data.I(20)), Constant.Data(Data.I(30)))
          )
        )
        val term = Term.Apply(
          Term.Apply(Term.Force(Term.Builtin(scalus.uplc.DefaultFun.MultiIndexArray)), indices),
          array
        )
        val result = term.evaluateDebug
        result match {
            case Result.Success(evaled, _, _, _) =>
                // Should return List(Data(10), Data(30))
                val expected = Term.Const(
                  Constant.List(
                    DefaultUni.Data,
                    List(Constant.Data(Data.I(10)), Constant.Data(Data.I(30)))
                  )
                )
                assert(evaled == expected)
            case Result.Failure(err, _, _, _) =>
                fail(s"Expected success but got failure: $err")
        }
    }

    test("multiIndexArray with repeated indices returns duplicates") {
        val indices = Term.Const(
          Constant.List(
            DefaultUni.Integer,
            List(Constant.Integer(0), Constant.Integer(0), Constant.Integer(1))
          )
        )
        val array = Term.Const(
          Constant.Array(
            DefaultUni.Data,
            Vector(Constant.Data(Data.I(100)), Constant.Data(Data.I(200)))
          )
        )
        val term = Term.Apply(
          Term.Apply(Term.Force(Term.Builtin(scalus.uplc.DefaultFun.MultiIndexArray)), indices),
          array
        )
        val result = term.evaluateDebug
        result match {
            case Result.Success(evaled, _, _, _) =>
                // Should return List(Data(100), Data(100), Data(200))
                val expected = Term.Const(
                  Constant.List(
                    DefaultUni.Data,
                    List(
                      Constant.Data(Data.I(100)),
                      Constant.Data(Data.I(100)),
                      Constant.Data(Data.I(200))
                    )
                  )
                )
                assert(evaled == expected)
            case Result.Failure(err, _, _, _) =>
                fail(s"Expected success but got failure: $err")
        }
    }

    test("multiIndexArray with empty indices returns empty list") {
        val indices = Term.Const(Constant.List(DefaultUni.Integer, List.empty))
        val array = Term.Const(
          Constant.Array(
            DefaultUni.Data,
            Vector(Constant.Data(Data.I(1)), Constant.Data(Data.I(2)))
          )
        )
        val term = Term.Apply(
          Term.Apply(Term.Force(Term.Builtin(scalus.uplc.DefaultFun.MultiIndexArray)), indices),
          array
        )
        val result = term.evaluateDebug
        result match {
            case Result.Success(evaled, _, _, _) =>
                // Should return empty list
                val expected = Term.Const(Constant.List(DefaultUni.Data, List.empty))
                assert(evaled == expected)
            case Result.Failure(err, _, _, _) =>
                fail(s"Expected success but got failure: $err")
        }
    }

    test("multiIndexArray with out of bounds index fails") {
        val indices = Term.Const(
          Constant.List(DefaultUni.Integer, List(Constant.Integer(0), Constant.Integer(5)))
        )
        val array = Term.Const(
          Constant.Array(
            DefaultUni.Data,
            Vector(Constant.Data(Data.I(1)), Constant.Data(Data.I(2)))
          )
        )
        val term = Term.Apply(
          Term.Apply(Term.Force(Term.Builtin(scalus.uplc.DefaultFun.MultiIndexArray)), indices),
          array
        )
        val result = term.evaluateDebug
        assert(result.isFailure, s"Expected failure but got: $result")
    }

    test("multiIndexArray with negative index fails") {
        val indices = Term.Const(Constant.List(DefaultUni.Integer, List(Constant.Integer(-1))))
        val array = Term.Const(
          Constant.Array(
            DefaultUni.Data,
            Vector(Constant.Data(Data.I(1)), Constant.Data(Data.I(2)))
          )
        )
        val term = Term.Apply(
          Term.Apply(Term.Force(Term.Builtin(scalus.uplc.DefaultFun.MultiIndexArray)), indices),
          array
        )
        val result = term.evaluateDebug
        assert(result.isFailure, s"Expected failure but got: $result")
    }

    test("multiIndexArray preserves order from index list") {
        // Request indices in reverse order: [2, 1, 0]
        val indices = Term.Const(
          Constant.List(
            DefaultUni.Integer,
            List(Constant.Integer(2), Constant.Integer(1), Constant.Integer(0))
          )
        )
        val array = Term.Const(
          Constant.Array(
            DefaultUni.Data,
            Vector(Constant.Data(Data.I(10)), Constant.Data(Data.I(20)), Constant.Data(Data.I(30)))
          )
        )
        val term = Term.Apply(
          Term.Apply(Term.Force(Term.Builtin(scalus.uplc.DefaultFun.MultiIndexArray)), indices),
          array
        )
        val result = term.evaluateDebug
        result match {
            case Result.Success(evaled, _, _, _) =>
                // Should return List(Data(30), Data(20), Data(10))
                val expected = Term.Const(
                  Constant.List(
                    DefaultUni.Data,
                    List(
                      Constant.Data(Data.I(30)),
                      Constant.Data(Data.I(20)),
                      Constant.Data(Data.I(10))
                    )
                  )
                )
                assert(evaled == expected)
            case Result.Failure(err, _, _, _) =>
                fail(s"Expected success but got failure: $err")
        }
    }
}
