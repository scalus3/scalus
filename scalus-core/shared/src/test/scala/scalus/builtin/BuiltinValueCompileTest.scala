package scalus.builtin

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.uplc.builtin.{BuiltinValue, Builtins, ByteString, Data}
import scalus.compiler.compile
import scalus.cardano.onchain.plutus.prelude.List as PList
import scalus.uplc.eval.{BuiltinValueOps, PlutusVM, Result}
import scalus.uplc.{Constant, DefaultFun, DefaultUni, Term}

/** Tests for BuiltinValue compilation and evaluation through @Compile blocks.
  *
  * Tests CIP-0153 MaryEraValue builtins compiled from Scala code:
  *   - BuiltinValue.empty
  *   - Builtins.unionValue
  *   - Builtins.valueContains
  *   - Builtins.valueData
  *   - Builtins.unValueData
  *   - Builtins.scaleValue
  */
class BuiltinValueCompileTest extends AnyFunSuite {

    given PlutusVM = PlutusVM.makePlutusV3VM()

    // Helper to extract BuiltinValue from evaluation result
    def evalToBuiltinValue(term: Term)(using vm: PlutusVM): BuiltinValue = {
        term.evaluateDebug match {
            case Result.Success(Term.Const(Constant.BuiltinValue(v), _), _, _, _) => v
            case Result.Success(other, _, _, _) =>
                fail(s"Expected Const(BuiltinValue), got: $other")
            case Result.Failure(err, _, _, _) =>
                fail(s"Evaluation failed: $err")
        }
    }

    // Helper to extract Boolean from evaluation result
    def evalToBool(term: Term)(using vm: PlutusVM): Boolean = {
        term.evaluateDebug match {
            case Result.Success(Term.Const(Constant.Bool(b), _), _, _, _) => b
            case Result.Success(other, _, _, _) =>
                fail(s"Expected Const(Bool), got: $other")
            case Result.Failure(err, _, _, _) =>
                fail(s"Evaluation failed: $err")
        }
    }

    // Helper to extract Data from evaluation result
    def evalToData(term: Term)(using vm: PlutusVM): Data = {
        term.evaluateDebug match {
            case Result.Success(Term.Const(Constant.Data(d), _), _, _, _) => d
            case Result.Success(other, _, _, _) =>
                fail(s"Expected Const(Data), got: $other")
            case Result.Failure(err, _, _, _) =>
                fail(s"Evaluation failed: $err")
        }
    }

    // ==================== BuiltinValue.empty tests ====================

    test("BuiltinValue.empty compiles to unValueData builtin call") {
        val sir = compile { BuiltinValue.empty }
        val uplc = sir.toUplc()
        // Should compile to: unValueData(emptyDataMap)
        val expected = Term.Apply(
          Term.Builtin(DefaultFun.UnValueData),
          Term.Const(Constant.Data(Data.Map(PList.Nil)))
        )
        assert(uplc ~=~ expected)
    }

    test("BuiltinValue.empty evaluates to empty value") {
        val sir = compile { BuiltinValue.empty }
        val uplc = sir.toUplc()
        val result = evalToBuiltinValue(uplc)
        assert(result == BuiltinValue.empty)
    }

    // ==================== Builtins.unValueData tests ====================

    test("Builtins.unValueData compiles and evaluates empty Data.Map") {
        val emptyMap = Data.Map(PList.Nil)
        val sir = compile { (d: Data) => Builtins.unValueData(d) }
        val uplc = sir.toUplc()
        val applied = Term.Apply(uplc, Term.Const(Constant.Data(emptyMap)))
        val result = evalToBuiltinValue(applied)
        assert(result == BuiltinValue.empty)
    }

    test("Builtins.unValueData compiles and evaluates non-empty Data.Map") {
        val policyA = ByteString.fromHex("aabbccdd")
        val tokenA = ByteString.fromString("TokenA")
        val valueData = Data.Map(
          PList((Data.B(policyA), Data.Map(PList((Data.B(tokenA), Data.I(BigInt(100)))))))
        )
        val sir = compile { (d: Data) => Builtins.unValueData(d) }
        val uplc = sir.toUplc()
        val applied = Term.Apply(uplc, Term.Const(Constant.Data(valueData)))
        val result = evalToBuiltinValue(applied)
        assert(BuiltinValueOps.lookupCoin(policyA, tokenA, result) == BigInt(100))
    }

    // ==================== Builtins.valueData tests ====================

    test("Builtins.valueData compiles and evaluates empty BuiltinValue") {
        val sir = compile { (v: BuiltinValue) => Builtins.valueData(v) }
        val uplc = sir.toUplc()
        val applied = Term.Apply(uplc, Term.Const(Constant.BuiltinValue(BuiltinValue.empty)))
        val result = evalToData(applied)
        assert(result == Data.Map(PList.Nil))
    }

    test("Builtins.valueData compiles and evaluates non-empty BuiltinValue") {
        val policyA = ByteString.fromHex("aabbccdd")
        val tokenA = ByteString.fromString("TokenA")
        val value = BuiltinValueOps.insertCoin(policyA, tokenA, BigInt(100), BuiltinValue.empty)
        val sir = compile { (v: BuiltinValue) => Builtins.valueData(v) }
        val uplc = sir.toUplc()
        val applied = Term.Apply(uplc, Term.Const(Constant.BuiltinValue(value)))
        val result = evalToData(applied)
        // Convert back to verify
        val restored = BuiltinValueOps.fromData(result)
        assert(BuiltinValueOps.lookupCoin(policyA, tokenA, restored) == BigInt(100))
    }

    test("valueData and unValueData roundtrip via compile") {
        val policyA = ByteString.fromHex("aabbccdd")
        val tokenA = ByteString.fromString("TokenA")
        val value = BuiltinValueOps.insertCoin(policyA, tokenA, BigInt(100), BuiltinValue.empty)

        val sir = compile { (v: BuiltinValue) => Builtins.unValueData(Builtins.valueData(v)) }
        val uplc = sir.toUplc()
        val applied = Term.Apply(uplc, Term.Const(Constant.BuiltinValue(value)))
        val result = evalToBuiltinValue(applied)
        assert(BuiltinValueOps.lookupCoin(policyA, tokenA, result) == BigInt(100))
    }

    // ==================== Builtins.unionValue tests ====================

    test("Builtins.unionValue compiles and evaluates empty + empty") {
        val sir = compile { (v1: BuiltinValue, v2: BuiltinValue) => Builtins.unionValue(v1, v2) }
        val uplc = sir.toUplc()
        val applied = Term.Apply(
          Term.Apply(uplc, Term.Const(Constant.BuiltinValue(BuiltinValue.empty))),
          Term.Const(Constant.BuiltinValue(BuiltinValue.empty))
        )
        val result = evalToBuiltinValue(applied)
        assert(result == BuiltinValue.empty)
    }

    test("Builtins.unionValue merges different tokens") {
        val adaSymbol = ByteString.empty
        val adaToken = ByteString.empty
        val policyA = ByteString.fromHex("aabbccdd")
        val tokenA = ByteString.fromString("TokenA")
        val v1 =
            BuiltinValueOps.insertCoin(adaSymbol, adaToken, BigInt(1000000), BuiltinValue.empty)
        val v2 = BuiltinValueOps.insertCoin(policyA, tokenA, BigInt(100), BuiltinValue.empty)

        val sir = compile { (v1: BuiltinValue, v2: BuiltinValue) => Builtins.unionValue(v1, v2) }
        val uplc = sir.toUplc()
        val applied = Term.Apply(
          Term.Apply(uplc, Term.Const(Constant.BuiltinValue(v1))),
          Term.Const(Constant.BuiltinValue(v2))
        )
        val result = evalToBuiltinValue(applied)
        assert(BuiltinValueOps.lookupCoin(adaSymbol, adaToken, result) == BigInt(1000000))
        assert(BuiltinValueOps.lookupCoin(policyA, tokenA, result) == BigInt(100))
    }

    test("Builtins.unionValue adds amounts for same token") {
        val policyA = ByteString.fromHex("aabbccdd")
        val tokenA = ByteString.fromString("TokenA")
        val v1 = BuiltinValueOps.insertCoin(policyA, tokenA, BigInt(100), BuiltinValue.empty)
        val v2 = BuiltinValueOps.insertCoin(policyA, tokenA, BigInt(50), BuiltinValue.empty)

        val sir = compile { (v1: BuiltinValue, v2: BuiltinValue) => Builtins.unionValue(v1, v2) }
        val uplc = sir.toUplc()
        val applied = Term.Apply(
          Term.Apply(uplc, Term.Const(Constant.BuiltinValue(v1))),
          Term.Const(Constant.BuiltinValue(v2))
        )
        val result = evalToBuiltinValue(applied)
        assert(BuiltinValueOps.lookupCoin(policyA, tokenA, result) == BigInt(150))
    }

    // ==================== Builtins.valueContains tests ====================

    test("Builtins.valueContains compiles and evaluates empty contains empty") {
        val sir =
            compile { (v1: BuiltinValue, v2: BuiltinValue) => Builtins.valueContains(v1, v2) }
        val uplc = sir.toUplc()
        val applied = Term.Apply(
          Term.Apply(uplc, Term.Const(Constant.BuiltinValue(BuiltinValue.empty))),
          Term.Const(Constant.BuiltinValue(BuiltinValue.empty))
        )
        assert(evalToBool(applied) == true)
    }

    test("Builtins.valueContains returns true when first contains second") {
        val policyA = ByteString.fromHex("aabbccdd")
        val tokenA = ByteString.fromString("TokenA")
        val v1 = BuiltinValueOps.insertCoin(policyA, tokenA, BigInt(100), BuiltinValue.empty)
        val v2 = BuiltinValueOps.insertCoin(policyA, tokenA, BigInt(50), BuiltinValue.empty)

        val sir =
            compile { (v1: BuiltinValue, v2: BuiltinValue) => Builtins.valueContains(v1, v2) }
        val uplc = sir.toUplc()
        val applied = Term.Apply(
          Term.Apply(uplc, Term.Const(Constant.BuiltinValue(v1))),
          Term.Const(Constant.BuiltinValue(v2))
        )
        assert(evalToBool(applied) == true)
    }

    test("Builtins.valueContains returns false when second has more") {
        val policyA = ByteString.fromHex("aabbccdd")
        val tokenA = ByteString.fromString("TokenA")
        val v1 = BuiltinValueOps.insertCoin(policyA, tokenA, BigInt(50), BuiltinValue.empty)
        val v2 = BuiltinValueOps.insertCoin(policyA, tokenA, BigInt(100), BuiltinValue.empty)

        val sir =
            compile { (v1: BuiltinValue, v2: BuiltinValue) => Builtins.valueContains(v1, v2) }
        val uplc = sir.toUplc()
        val applied = Term.Apply(
          Term.Apply(uplc, Term.Const(Constant.BuiltinValue(v1))),
          Term.Const(Constant.BuiltinValue(v2))
        )
        assert(evalToBool(applied) == false)
    }

    // ==================== Builtins.scaleValue tests ====================

    test("Builtins.scaleValue compiles and evaluates scale by 0") {
        val policyA = ByteString.fromHex("aabbccdd")
        val tokenA = ByteString.fromString("TokenA")
        val value = BuiltinValueOps.insertCoin(policyA, tokenA, BigInt(100), BuiltinValue.empty)

        val sir = compile { (s: BigInt, v: BuiltinValue) => Builtins.scaleValue(s, v) }
        val uplc = sir.toUplc()
        val applied = Term.Apply(
          Term.Apply(uplc, Term.Const(Constant.Integer(BigInt(0)))),
          Term.Const(Constant.BuiltinValue(value))
        )
        val result = evalToBuiltinValue(applied)
        assert(BuiltinValueOps.lookupCoin(policyA, tokenA, result) == BigInt(0))
    }

    test("Builtins.scaleValue compiles and evaluates scale by positive") {
        val policyA = ByteString.fromHex("aabbccdd")
        val tokenA = ByteString.fromString("TokenA")
        val value = BuiltinValueOps.insertCoin(policyA, tokenA, BigInt(100), BuiltinValue.empty)

        val sir = compile { (s: BigInt, v: BuiltinValue) => Builtins.scaleValue(s, v) }
        val uplc = sir.toUplc()
        val applied = Term.Apply(
          Term.Apply(uplc, Term.Const(Constant.Integer(BigInt(3)))),
          Term.Const(Constant.BuiltinValue(value))
        )
        val result = evalToBuiltinValue(applied)
        assert(BuiltinValueOps.lookupCoin(policyA, tokenA, result) == BigInt(300))
    }

    test("Builtins.scaleValue compiles and evaluates scale by negative") {
        val policyA = ByteString.fromHex("aabbccdd")
        val tokenA = ByteString.fromString("TokenA")
        val value = BuiltinValueOps.insertCoin(policyA, tokenA, BigInt(100), BuiltinValue.empty)

        val sir = compile { (s: BigInt, v: BuiltinValue) => Builtins.scaleValue(s, v) }
        val uplc = sir.toUplc()
        val applied = Term.Apply(
          Term.Apply(uplc, Term.Const(Constant.Integer(BigInt(-1)))),
          Term.Const(Constant.BuiltinValue(value))
        )
        val result = evalToBuiltinValue(applied)
        assert(BuiltinValueOps.lookupCoin(policyA, tokenA, result) == BigInt(-100))
    }

    // ==================== Complex compiled expressions ====================

    test("Complex: union then scale via compile") {
        val policyA = ByteString.fromHex("aabbccdd")
        val tokenA = ByteString.fromString("TokenA")
        val v1 = BuiltinValueOps.insertCoin(policyA, tokenA, BigInt(100), BuiltinValue.empty)
        val v2 = BuiltinValueOps.insertCoin(policyA, tokenA, BigInt(50), BuiltinValue.empty)

        val sir = compile { (v1: BuiltinValue, v2: BuiltinValue) =>
            Builtins.scaleValue(BigInt(2), Builtins.unionValue(v1, v2))
        }
        val uplc = sir.toUplc()
        val applied = Term.Apply(
          Term.Apply(uplc, Term.Const(Constant.BuiltinValue(v1))),
          Term.Const(Constant.BuiltinValue(v2))
        )
        val result = evalToBuiltinValue(applied)
        // (100 + 50) * 2 = 300
        assert(BuiltinValueOps.lookupCoin(policyA, tokenA, result) == BigInt(300))
    }

    test("Complex: contains after union via compile") {
        val policyA = ByteString.fromHex("aabbccdd")
        val tokenA = ByteString.fromString("TokenA")
        val v1 = BuiltinValueOps.insertCoin(policyA, tokenA, BigInt(100), BuiltinValue.empty)
        val v2 = BuiltinValueOps.insertCoin(policyA, tokenA, BigInt(50), BuiltinValue.empty)
        val v3 = BuiltinValueOps.insertCoin(policyA, tokenA, BigInt(140), BuiltinValue.empty)

        val sir = compile { (v1: BuiltinValue, v2: BuiltinValue, v3: BuiltinValue) =>
            Builtins.valueContains(Builtins.unionValue(v1, v2), v3)
        }
        val uplc = sir.toUplc()
        val applied = Term.Apply(
          Term.Apply(
            Term.Apply(uplc, Term.Const(Constant.BuiltinValue(v1))),
            Term.Const(Constant.BuiltinValue(v2))
          ),
          Term.Const(Constant.BuiltinValue(v3))
        )
        // 150 >= 140 -> true
        assert(evalToBool(applied) == true)
    }

    // ==================== Builtins.insertCoin tests ====================

    test("Builtins.insertCoin compiles and inserts into empty value") {
        val policyA = ByteString.fromHex("aabbccdd")
        val tokenA = ByteString.fromString("TokenA")

        val sir = compile {
            (currency: ByteString, token: ByteString, amount: BigInt, value: BuiltinValue) =>
                Builtins.insertCoin(currency, token, amount, value)
        }
        val uplc = sir.toUplc()
        val applied = Term.Apply(
          Term.Apply(
            Term.Apply(
              Term.Apply(uplc, Term.Const(Constant.ByteString(policyA))),
              Term.Const(Constant.ByteString(tokenA))
            ),
            Term.Const(Constant.Integer(BigInt(100)))
          ),
          Term.Const(Constant.BuiltinValue(BuiltinValue.empty))
        )
        val result = evalToBuiltinValue(applied)
        assert(BuiltinValueOps.lookupCoin(policyA, tokenA, result) == BigInt(100))
    }

    test("Builtins.insertCoin compiles and updates existing token") {
        val policyA = ByteString.fromHex("aabbccdd")
        val tokenA = ByteString.fromString("TokenA")
        val existing = BuiltinValueOps.insertCoin(policyA, tokenA, BigInt(50), BuiltinValue.empty)

        val sir = compile {
            (currency: ByteString, token: ByteString, amount: BigInt, value: BuiltinValue) =>
                Builtins.insertCoin(currency, token, amount, value)
        }
        val uplc = sir.toUplc()
        val applied = Term.Apply(
          Term.Apply(
            Term.Apply(
              Term.Apply(uplc, Term.Const(Constant.ByteString(policyA))),
              Term.Const(Constant.ByteString(tokenA))
            ),
            Term.Const(Constant.Integer(BigInt(200)))
          ),
          Term.Const(Constant.BuiltinValue(existing))
        )
        val result = evalToBuiltinValue(applied)
        // insertCoin replaces, doesn't add
        assert(BuiltinValueOps.lookupCoin(policyA, tokenA, result) == BigInt(200))
    }

    test("Builtins.insertCoin with zero removes token") {
        val policyA = ByteString.fromHex("aabbccdd")
        val tokenA = ByteString.fromString("TokenA")
        val existing = BuiltinValueOps.insertCoin(policyA, tokenA, BigInt(100), BuiltinValue.empty)

        val sir = compile {
            (currency: ByteString, token: ByteString, amount: BigInt, value: BuiltinValue) =>
                Builtins.insertCoin(currency, token, amount, value)
        }
        val uplc = sir.toUplc()
        val applied = Term.Apply(
          Term.Apply(
            Term.Apply(
              Term.Apply(uplc, Term.Const(Constant.ByteString(policyA))),
              Term.Const(Constant.ByteString(tokenA))
            ),
            Term.Const(Constant.Integer(BigInt(0)))
          ),
          Term.Const(Constant.BuiltinValue(existing))
        )
        val result = evalToBuiltinValue(applied)
        assert(BuiltinValueOps.lookupCoin(policyA, tokenA, result) == BigInt(0))
    }

    // ==================== Builtins.lookupCoin tests ====================

    test("Builtins.lookupCoin compiles and returns 0 for missing token") {
        val policyA = ByteString.fromHex("aabbccdd")
        val tokenA = ByteString.fromString("TokenA")

        val sir = compile { (currency: ByteString, token: ByteString, value: BuiltinValue) =>
            Builtins.lookupCoin(currency, token, value)
        }
        val uplc = sir.toUplc()
        val applied = Term.Apply(
          Term.Apply(
            Term.Apply(uplc, Term.Const(Constant.ByteString(policyA))),
            Term.Const(Constant.ByteString(tokenA))
          ),
          Term.Const(Constant.BuiltinValue(BuiltinValue.empty))
        )
        val result = evalToInteger(applied)
        assert(result == BigInt(0))
    }

    test("Builtins.lookupCoin compiles and returns correct amount") {
        val policyA = ByteString.fromHex("aabbccdd")
        val tokenA = ByteString.fromString("TokenA")
        val value = BuiltinValueOps.insertCoin(policyA, tokenA, BigInt(12345), BuiltinValue.empty)

        val sir = compile { (currency: ByteString, token: ByteString, value: BuiltinValue) =>
            Builtins.lookupCoin(currency, token, value)
        }
        val uplc = sir.toUplc()
        val applied = Term.Apply(
          Term.Apply(
            Term.Apply(uplc, Term.Const(Constant.ByteString(policyA))),
            Term.Const(Constant.ByteString(tokenA))
          ),
          Term.Const(Constant.BuiltinValue(value))
        )
        val result = evalToInteger(applied)
        assert(result == BigInt(12345))
    }

    test("Builtins.lookupCoin returns 0 for wrong token name") {
        val policyA = ByteString.fromHex("aabbccdd")
        val tokenA = ByteString.fromString("TokenA")
        val tokenB = ByteString.fromString("TokenB")
        val value = BuiltinValueOps.insertCoin(policyA, tokenA, BigInt(100), BuiltinValue.empty)

        val sir = compile { (currency: ByteString, token: ByteString, value: BuiltinValue) =>
            Builtins.lookupCoin(currency, token, value)
        }
        val uplc = sir.toUplc()
        val applied = Term.Apply(
          Term.Apply(
            Term.Apply(uplc, Term.Const(Constant.ByteString(policyA))),
            Term.Const(Constant.ByteString(tokenB))
          ),
          Term.Const(Constant.BuiltinValue(value))
        )
        val result = evalToInteger(applied)
        assert(result == BigInt(0))
    }

    // Helper to extract BigInt from evaluation result
    def evalToInteger(term: Term)(using vm: PlutusVM): BigInt = {
        term.evaluateDebug match {
            case Result.Success(Term.Const(Constant.Integer(i), _), _, _, _) => i
            case Result.Success(other, _, _, _) =>
                fail(s"Expected Const(Integer), got: $other")
            case Result.Failure(err, _, _, _) =>
                fail(s"Evaluation failed: $err")
        }
    }

    // ==================== Type parameter tests ====================

    test("BuiltinValue as type parameter argument in foldLeft") {
        // Test that BuiltinValue works as accumulator type in foldLeft,
        // where the accumulator type B is a type parameter
        val policyA = ByteString.fromHex("aabbccdd")
        val tokenA = ByteString.fromString("TokenA")
        val v1 = BuiltinValueOps.insertCoin(policyA, tokenA, BigInt(100), BuiltinValue.empty)
        val v2 = BuiltinValueOps.insertCoin(policyA, tokenA, BigInt(50), BuiltinValue.empty)
        val v3 = BuiltinValueOps.insertCoin(policyA, tokenA, BigInt(25), BuiltinValue.empty)

        // foldLeft has signature: def foldLeft[B](init: B)(combiner: (B, A) => B): B
        // Here BuiltinValue is passed as type parameter B
        val sir = compile { (values: PList[BuiltinValue]) =>
            values.foldLeft(BuiltinValue.empty) { (acc, v) =>
                Builtins.unionValue(acc, v)
            }
        }
        val uplc = sir.toUplc()
        // Build the list as UPLC terms - list elements are in Data representation
        // because type parameter A uses PackedData representation for BuiltinValue
        val listTerm = Term.Const(
          Constant.List(
            DefaultUni.Data,
            scala.List(
              Constant.Data(BuiltinValueOps.toData(v1)),
              Constant.Data(BuiltinValueOps.toData(v2)),
              Constant.Data(BuiltinValueOps.toData(v3))
            )
          )
        )
        val applied = Term.Apply(uplc, listTerm)
        val result = evalToBuiltinValue(applied)
        // 100 + 50 + 25 = 175
        assert(BuiltinValueOps.lookupCoin(policyA, tokenA, result) == BigInt(175))
    }

    test("BuiltinValue as type parameter in generic function") {
        // Test passing BuiltinValue to a function with type parameter
        val policyA = ByteString.fromHex("aabbccdd")
        val tokenA = ByteString.fromString("TokenA")
        val v1 = BuiltinValueOps.insertCoin(policyA, tokenA, BigInt(100), BuiltinValue.empty)

        val sir = compile { (v: BuiltinValue) =>
            // identity function has type parameter A
            def identity[A](x: A): A = x
            identity(v)
        }
        val uplc = sir.toUplc()
        val applied = Term.Apply(uplc, Term.Const(Constant.BuiltinValue(v1)))
        val result = evalToBuiltinValue(applied)
        assert(BuiltinValueOps.lookupCoin(policyA, tokenA, result) == BigInt(100))
    }

    test("BuiltinValue as type parameter in higher-order function") {
        // Test BuiltinValue passed through a higher-order function with type parameters
        val policyA = ByteString.fromHex("aabbccdd")
        val tokenA = ByteString.fromString("TokenA")
        val v1 = BuiltinValueOps.insertCoin(policyA, tokenA, BigInt(100), BuiltinValue.empty)
        val v2 = BuiltinValueOps.insertCoin(policyA, tokenA, BigInt(50), BuiltinValue.empty)

        val sir = compile { (x: BuiltinValue, y: BuiltinValue) =>
            // apply has type parameter A, B
            def apply[A, B](f: A => B, a: A): B = f(a)
            apply((v: BuiltinValue) => Builtins.unionValue(v, y), x)
        }
        val uplc = sir.toUplc()
        val applied = Term.Apply(
          Term.Apply(uplc, Term.Const(Constant.BuiltinValue(v1))),
          Term.Const(Constant.BuiltinValue(v2))
        )
        val result = evalToBuiltinValue(applied)
        // 100 + 50 = 150
        assert(BuiltinValueOps.lookupCoin(policyA, tokenA, result) == BigInt(150))
    }
}
