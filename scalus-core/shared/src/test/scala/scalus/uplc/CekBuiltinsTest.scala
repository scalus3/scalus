package scalus.uplc

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.builtin.ByteString.*
import scalus.builtin.{ByteString, Data}
import scalus.compiler.compile
import scalus.prelude.List as PList
import scalus.testing.kit.EvalTestKit
import scalus.uplc.Constant.{asConstant, Pair}
import scalus.uplc.DefaultFun.*
import scalus.uplc.Term.*
import scalus.uplc.TermDSL.given
import scalus.uplc.eval.*

import scala.language.implicitConversions

open class CekBuiltinsTest extends AnyFunSuite with EvalTestKit:

    test("Lazy builtin evaluation") {
        assertTermEvalEq(AddInteger $ "wrong", Apply(Builtin(AddInteger), "wrong"))
    }

    test("AddInteger") {
        forAll { (a: BigInt, b: BigInt) =>
            assertTermEvalEq(AddInteger $ a $ b, Const(asConstant(a + b)))
        }

        forAll { (a: Term, b: Term) =>
            (a, b) match
                case (Const(Constant.Integer(aa)), Const(Constant.Integer(bb))) =>
                    val r = aa + bb
                    assertTermEvalEq(AddInteger $ a $ b, Const(Constant.Integer(r)))
                case _ => assertTermEvalThrows[Exception](AddInteger $ a $ b)
        }
    }

    test("SubstractInteger") {
        forAll { (a: BigInt, b: BigInt) =>
            assertTermEvalEq(SubtractInteger $ a $ b, Const(asConstant(a - b)))
        }

        forAll { (a: Term, b: Term) =>
            (a, b) match
                case (Const(Constant.Integer(aa)), Const(Constant.Integer(bb))) =>
                    val r = aa - bb
                    assertTermEvalEq(SubtractInteger $ a $ b, Const(Constant.Integer(r)))
                case _ => assertTermEvalThrows[Exception](AddInteger $ a $ b)
        }
    }

    test("DivideInteger") {
        // integer division truncated toward negative infinity
        assertTermEvalEq(DivideInteger $ 20 $ 3, Const(Constant.Integer(6)))
        assertTermEvalEq(DivideInteger $ -20 $ -3, Const(Constant.Integer(6)))
        assertTermEvalEq(DivideInteger $ 20 $ -3, Const(Constant.Integer(-7)))
        assertTermEvalEq(DivideInteger $ -20 $ 3, Const(Constant.Integer(-7)))
        assertTermEvalThrows[BuiltinError](DivideInteger $ 1 $ 0)
    }

    test("DivideInteger Long.MinValue / -1") {
        val minValue = BigInt("-9223372036854775808") // Long.MinValue
        // Floor division of Long.MinValue by -1 should give positive result
        // (div x y)*y + (mod x y) == x must hold
        val q = DivideInteger $ minValue $ BigInt(-1)
        val r = ModInteger $ minValue $ BigInt(-1)
        val x1 = AddInteger $ (MultiplyInteger $ q $ BigInt(-1)) $ r
        assertTermEvalEq(x1, Const(Constant.Integer(minValue)))
    }

    test("QuotientInteger") {
        assertTermEvalEq(QuotientInteger $ -20 $ 3, Const(Constant.Integer(-6)))
        assertTermEvalThrows[BuiltinError](QuotientInteger $ 20 $ 0)
        forAll { (a: BigInt, b: BigInt) =>
            if b == 0 then assertTermEvalThrows[BuiltinError](QuotientInteger $ a $ b)
            else assertTermEvalEq(QuotientInteger $ a $ b, Const(asConstant(a / b)))
        }
    }

    test("RemainderInteger") {
        assertTermEvalEq(RemainderInteger $ 20 $ 3, Const(Constant.Integer(2)))
        assertTermEvalEq(RemainderInteger $ -20 $ 3, Const(Constant.Integer(-2)))
        assertTermEvalEq(RemainderInteger $ 20 $ -3, Const(Constant.Integer(2)))
        assertTermEvalEq(RemainderInteger $ -20 $ -3, Const(Constant.Integer(-2)))
        assertTermEvalThrows[BuiltinError](RemainderInteger $ 20 $ 0)
        forAll { (a: BigInt, b: BigInt) =>
            if b == 0 then assertTermEvalThrows[BuiltinError](RemainderInteger $ a $ b)
            else assertTermEvalEq(RemainderInteger $ a $ b, Const(asConstant(a % b)))
        }
    }

    test("ModInteger") {
        assertTermEvalEq(ModInteger $ 20 $ 3, Const(Constant.Integer(2)))
        assertTermEvalEq(ModInteger $ -20 $ 3, Const(Constant.Integer(1)))
        assertTermEvalEq(ModInteger $ 20 $ -3, Const(Constant.Integer(-1)))
        assertTermEvalEq(ModInteger $ -20 $ -3, Const(Constant.Integer(-2)))
        assertTermEvalThrows[BuiltinError](ModInteger $ 20 $ 0)
    }

    test("(x `quot` y)*y + (x `rem` y) == x") {
        forAll { (x: BigInt, y: BigInt) =>
            whenever(y != 0) {
                val q = QuotientInteger $ x $ y
                val r = RemainderInteger $ x $ y
                val x1 = AddInteger $ (MultiplyInteger $ q $ y) $ r
                assertTermEvalEq(x1, Const(Constant.Integer(x)))
            }
        }
    }

    test("(x `div` y)*y + (x `mod` y) == x") {
        forAll { (x: BigInt, y: BigInt) =>
            whenever(y != 0) {
                val q = DivideInteger $ x $ y
                val r = ModInteger $ x $ y
                val x1 = AddInteger $ (MultiplyInteger $ q $ y) $ r
                assertTermEvalEq(x1, Const(Constant.Integer(x)))
            }
        }
    }

    test("EqualsInteger") {
        forAll { (a: BigInt, b: BigInt) =>
            assertTermEvalEq(EqualsInteger $ a $ b, Const(asConstant(a == b)))
        }
    }

    test("UnConstrData") {
        assert(
          (UnConstrData $ Data.Constr(12, PList(Data.I(1)))).evaluate ==
              Const(
                Pair(asConstant(12), Constant.List(DefaultUni.Data, List(Constant.Data(Data.I(1)))))
              )
        )
    }

    test("UnMapData") {
        assert(
          (UnMapData $ Data.Map(PList((Data.I(12), Data.I(1))))).evaluate ==
              Const(
                Constant.List(
                  DefaultUni.Pair(DefaultUni.Data, DefaultUni.Data),
                  Pair(Constant.Data(Data.I(12)), Constant.Data(Data.I(1))) :: Nil
                )
              )
        )
    }

    test("UnListData") {
        assert(
          (UnListData $ Data.List(PList(Data.I(12), Data.I(1)))).evaluate ==
              Const(Constant.List(DefaultUni.Data, Constant.Data(12) :: Constant.Data(1) :: Nil))
        )
    }

    test("UnIData") {
        assert((UnIData $ Data.I(12)).evaluate == Const(Constant.Integer(12)))
    }

    test("UnBData") {
        assert(
          (UnBData $ Data.B(hex"deadbeef")).evaluate == Const(Constant.ByteString(hex"deadbeef"))
        )
    }

    test("ChooseList") {
        // check empty case
        assertTermEvalEq(
          !(!ChooseList) $ Const(Constant.List(DefaultUni.Integer, Nil)) $ asConstant(
            1
          ) $ asConstant(2),
          Const(asConstant(1))
        )
        // check non-empty case
        assertTermEvalEq(
          !(!ChooseList) $ Const(
            Constant.List(DefaultUni.Integer, asConstant(333) :: Nil)
          ) $ asConstant(1) $ asConstant(2),
          Const(asConstant(2))
        )

        forAll { (t: Constant) =>
            t match
                case Constant.List(_, v) =>
                    assertTermEvalEq(
                      !(!ChooseList) $ t $ asConstant(true) $ asConstant(false),
                      Const(Constant.Bool(v.isEmpty))
                    )
                case _ =>
                    assertTermEvalThrows[Exception](!ChooseList $ t)
        }
    }

    test("NullList") {
        assertTermEvalEq(
          !NullList $ Const(Constant.List(DefaultUni.Integer, Nil)),
          Const(asConstant(true))
        )

        forAll { (t: Constant) =>
            t match
                case Constant.List(_, v) =>
                    assertTermEvalEq(!NullList $ t, Const(Constant.Bool(v.isEmpty)))
                case _ =>
                    assertTermEvalThrows[Exception](!NullList $ t)
        }
    }

    test("MkCons") {
        assertTermEvalEq(
          !MkCons $ Constant.Integer(0) $ Const(
            Constant.List(DefaultUni.Integer, asConstant(1) :: Nil)
          ),
          Const(Constant.List(DefaultUni.Integer, asConstant(0) :: asConstant(1) :: Nil))
        )
        // should throw if constant types don't match
        assertTermEvalThrows[Exception](
          !MkCons $ Constant.Unit $ Constant.List(DefaultUni.Integer, Nil)
        )
    }

    test("HeadList") {
        assertTermEvalEq(
          !HeadList $ Const(Constant.List(DefaultUni.Integer, asConstant(1) :: Nil)),
          Const(asConstant(1))
        )

        forAll { (t: Constant) =>
            t match
                case Constant.List(_, v) if v.nonEmpty =>
                    assertTermEvalEq(!HeadList $ t, Const(v.head))
                case _ =>
                    assertTermEvalThrows[Exception](!HeadList $ t)
        }
    }

    test("TailList") {
        assertTermEvalEq(
          !TailList $ Const(
            Constant.List(DefaultUni.Integer, asConstant(1) :: asConstant(2) :: Nil)
          ),
          Const(Constant.List(DefaultUni.Integer, asConstant(2) :: Nil))
        )

        forAll { (t: Constant) =>
            t match
                case Constant.List(tpe, v) if v.nonEmpty =>
                    assertTermEvalEq(!TailList $ t, Const(Constant.List(tpe, v.tail)))
                case _ =>
                    assertTermEvalThrows[Exception](!TailList $ t)
        }
    }

    test("FstPair") {
        assertTermEvalEq(
          !(!FstPair) $ Const(Constant.Pair(asConstant(1), asConstant(false))),
          Const(asConstant(1))
        )

        /*forAll { (t: Constant) =>
      t match
        case Constant.Pair(a, _) =>
          assertTermEvalEq(!(!FstPair) $ t, Const(a))
        case _ =>
          assertTermEvalThrows[Exception](!(!FstPair) $ t)
    }*/
    }

    test("SndPair") {
        assertTermEvalEq(
          !(!SndPair) $ Const(Constant.Pair(asConstant(1), asConstant(false))),
          Const(asConstant(false))
        )

        forAll { (t: Constant) =>
            t match
                case Constant.Pair(_, b) =>
                    assertTermEvalEq(!(!SndPair) $ t, Const(b))
                case _ =>
                    assertTermEvalThrows[Exception](!(!SndPair) $ t)
        }
    }

    test("EqualsByteString") {
        assertTermEvalEq(
          EqualsByteString $ Const(Constant.ByteString(hex"deadbeef")) $ Const(
            Constant.ByteString(hex"deadbeef")
          ),
          Const(asConstant(true))
        )
        assertTermEvalEq(
          EqualsByteString $ Const(Constant.ByteString(hex"")) $ Const(
            Constant.ByteString(hex"deadbeef")
          ),
          Const(asConstant(false))
        )

        forAll { (t: Constant) =>
            t match
                case Constant.ByteString(_) =>
                    assertTermEvalEq(EqualsByteString $ t $ t, Const(asConstant(true)))
                case _ =>
                    assertTermEvalThrows[Exception](EqualsByteString $ t $ t)
        }
    }

    test("SliceByteString") {
        assertTermEvalEq(
          SliceByteString $ asConstant(1) $ asConstant(3) $ Const(
            Constant.ByteString(hex"deadbeef")
          ),
          Const(Constant.ByteString(hex"adbeef"))
        )

        assertTermEvalEq(
          SliceByteString $ asConstant(1) $ asConstant(5) $ Const(
            Constant.ByteString(hex"deadbeef")
          ),
          Const(Constant.ByteString(hex"adbeef"))
        )
    }

    test("verifyEd25519Signature") {
        val sir = compile { scalus.builtin.Builtins.verifyEd25519Signature }
        val verify = sir.toUplc()
        val valid = verify $
            hex"9518c18103cbdab9c6e60b58ecc3e2eb439fef6519bb22570f391327381900a8" $
            utf8"hello" $
            hex"f13fa9acffb108114ec060561b58005fb2d69184de0a2d7400b2ea1f111c0794831cc832c92daf4807820dd9458324935e90bec855e8bf076bbbc4e42b727b07"

        assertTermEvalEq(valid, true)

        val wrongMessage = verify $
            hex"9518c18103cbdab9c6e60b58ecc3e2eb439fef6519bb22570f391327381900a8" $
            utf8"NOT hello" $
            hex"f13fa9acffb108114ec060561b58005fb2d69184de0a2d7400b2ea1f111c0794831cc832c92daf4807820dd9458324935e90bec855e8bf076bbbc4e42b727b07"

        assertTermEvalEq(wrongMessage, false)

        val wrongPubKey = verify $
            hex"AA18c18103cbdab9c6e60b58ecc3e2eb439fef6519bb22570f391327381900a8" $
            utf8"hello" $
            hex"f13fa9acffb108114ec060561b58005fb2d69184de0a2d7400b2ea1f111c0794831cc832c92daf4807820dd9458324935e90bec855e8bf076bbbc4e42b727b07"

        assertTermEvalEq(wrongPubKey, false)

        val wrongSignature = verify $
            hex"9518c18103cbdab9c6e60b58ecc3e2eb439fef6519bb22570f391327381900a8" $
            utf8"NOT hello" $
            hex"FF3fa9acffb108114ec060561b58005fb2d69184de0a2d7400b2ea1f111c0794831cc832c92daf4807820dd9458324935e90bec855e8bf076bbbc4e42b727b07"

        assertTermEvalEq(wrongSignature, false)
    }

    test("verifyEcdsaSecp256k1Signature follows CIP-49") {
        // https://cips.cardano.org/cip/CIP-49
        val sir = compile { scalus.builtin.Builtins.verifyEcdsaSecp256k1Signature }
        val verify = sir.toUplc()
        val pubKey = hex"03427d3132a06e31bf66791dda478b5ebec79bd045247126396fccdf11e42a3627"

        // valid public key, message and signature
        val valid = verify $
            pubKey $
            hex"2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824" $
            hex"040f5b6a2bb4e024d47eab02d4073da655af77c0cf0efdb19c6771378da175c45ffac010a1bd9b9a275ad685ea4052f4bc72c0dc27094422ba9379e7bf44b29b"

        assertTermEvalEq(valid, true)

        // invalid message size
        val invalidMessageSize = verify $
            pubKey $
            hex"deadbeef" $
            hex"040f5b6a2bb4e024d47eab02d4073da655af77c0cf0efdb19c6771378da175c45ffac010a1bd9b9a275ad685ea4052f4bc72c0dc27094422ba9379e7bf44b29b"

        assertTermEvalThrows[BuiltinError](invalidMessageSize)

        val invalidPubKey = verify $
            hex"FFFF7d3132a06e31bf66791dda478b5ebec79bd045247126396fccdf11e42a3627" $
            hex"2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824" $
            hex"040f5b6a2bb4e024d47eab02d4073da655af77c0cf0efdb19c6771378da175c45ffac010a1bd9b9a275ad685ea4052f4bc72c0dc27094422ba9379e7bf44b29b"

        assertTermEvalThrows[BuiltinError](invalidPubKey)

        val invalidSignature = verify $
            pubKey $
            hex"2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824" $
            hex"deadbeef"

        assertTermEvalThrows[BuiltinError](invalidSignature)

        val wrongSignature = verify $
            pubKey $
            hex"2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824" $
            hex"FF0f5b6a2bb4e024d47eab02d4073da655af77c0cf0efdb19c6771378da175c45ffac010a1bd9b9a275ad685ea4052f4bc72c0dc27094422ba9379e7bf44b29b"

        assertTermEvalEq(wrongSignature, false)
    }

    test("verifySchnorrSecp256k1Signature follows CIP-49") {
        // https://cips.cardano.org/cip/CIP-49
        val sir = compile { scalus.builtin.Builtins.verifySchnorrSecp256k1Signature }
        val verify = sir.toUplc()
        val pubKey = hex"427d3132a06e31bf66791dda478b5ebec79bd045247126396fccdf11e42a3627"

        // valid public key, message and signature
        val valid = verify $
            pubKey $
            hex"2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824" $
            hex"4fd97a0c4ad719f89cba68a522e0dee13bcf656ae9c0a395404cda858a7992d8dea979dbc4c83659d695b7d380fe8a75264ba51a63a53fc2a8bd225e50f223f4"

        assertTermEvalEq(valid, true)

        val invalidPubKey = verify $
            hex"FFFF7d3132a06e31bf66791dda478b5ebec79bd045247126396fccdf11e42a3627" $
            hex"2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824" $
            hex"4fd97a0c4ad719f89cba68a522e0dee13bcf656ae9c0a395404cda858a7992d8dea979dbc4c83659d695b7d380fe8a75264ba51a63a53fc2a8bd225e50f223f4"

        assertTermEvalThrows[BuiltinError](invalidPubKey)

        val invalidSignature = verify $
            pubKey $
            hex"2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824" $
            hex"deadbeef"

        assertTermEvalThrows[BuiltinError](invalidSignature)

        val wrongSignature = verify $
            pubKey $
            hex"2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824" $
            hex"FFd97a0c4ad719f89cba68a522e0dee13bcf656ae9c0a395404cda858a7992d8dea979dbc4c83659d695b7d380fe8a75264ba51a63a53fc2a8bd225e50f223f4"

        assertTermEvalEq(wrongSignature, false)
    }

    test("AndByteString follows CIP-122") {
        val AndByteString = compile(scalus.builtin.Builtins.andByteString).toUplc()

        assertTermEvalEq(AndByteString $ false $ hex"" $ hex"", hex"")

        assertTermEvalEq(AndByteString $ false $ hex"00" $ hex"00", hex"00")
        assertTermEvalEq(AndByteString $ false $ hex"FF" $ hex"00", hex"00")
        assertTermEvalEq(AndByteString $ false $ hex"00" $ hex"FF", hex"00")
        assertTermEvalEq(AndByteString $ false $ hex"FF" $ hex"FF", hex"FF")

        assertTermEvalEq(AndByteString $ false $ hex"00FF" $ hex"00", hex"00")
        assertTermEvalEq(AndByteString $ false $ hex"FFFF" $ hex"00", hex"00")
        assertTermEvalEq(AndByteString $ false $ hex"00FF" $ hex"FF", hex"00")
        assertTermEvalEq(AndByteString $ false $ hex"FFFF" $ hex"FF", hex"FF")

        assertTermEvalEq(AndByteString $ false $ hex"00" $ hex"00FF", hex"00")
        assertTermEvalEq(AndByteString $ false $ hex"FF" $ hex"00FF", hex"00")
        assertTermEvalEq(AndByteString $ false $ hex"00" $ hex"FFFF", hex"00")
        assertTermEvalEq(AndByteString $ false $ hex"FF" $ hex"FFFF", hex"FF")

        assertTermEvalEq(AndByteString $ true $ hex"" $ hex"", hex"")

        assertTermEvalEq(AndByteString $ true $ hex"00" $ hex"00", hex"00")
        assertTermEvalEq(AndByteString $ true $ hex"FF" $ hex"00", hex"00")
        assertTermEvalEq(AndByteString $ true $ hex"00" $ hex"FF", hex"00")
        assertTermEvalEq(AndByteString $ true $ hex"FF" $ hex"FF", hex"FF")

        assertTermEvalEq(AndByteString $ true $ hex"00FF" $ hex"00", hex"00FF")
        assertTermEvalEq(AndByteString $ true $ hex"FFFF" $ hex"00", hex"00FF")
        assertTermEvalEq(AndByteString $ true $ hex"00FF" $ hex"FF", hex"00FF")
        assertTermEvalEq(AndByteString $ true $ hex"FFFF" $ hex"FF", hex"FFFF")

        assertTermEvalEq(AndByteString $ true $ hex"00" $ hex"00FF", hex"00FF")
        assertTermEvalEq(AndByteString $ true $ hex"FF" $ hex"00FF", hex"00FF")
        assertTermEvalEq(AndByteString $ true $ hex"00" $ hex"FFFF", hex"00FF")
        assertTermEvalEq(AndByteString $ true $ hex"FF" $ hex"FFFF", hex"FFFF")
    }

    test("OrByteString follows CIP-122") {
        val OrByteString = compile(scalus.builtin.Builtins.orByteString).toUplc()

        assertTermEvalEq(OrByteString $ false $ hex"" $ hex"", hex"")

        assertTermEvalEq(OrByteString $ false $ hex"00" $ hex"00", hex"00")
        assertTermEvalEq(OrByteString $ false $ hex"FF" $ hex"00", hex"FF")
        assertTermEvalEq(OrByteString $ false $ hex"00" $ hex"FF", hex"FF")
        assertTermEvalEq(OrByteString $ false $ hex"FF" $ hex"FF", hex"FF")

        assertTermEvalEq(OrByteString $ false $ hex"00FF" $ hex"00", hex"00")
        assertTermEvalEq(OrByteString $ false $ hex"FFFF" $ hex"00", hex"FF")
        assertTermEvalEq(OrByteString $ false $ hex"00FF" $ hex"FF", hex"FF")
        assertTermEvalEq(OrByteString $ false $ hex"FFFF" $ hex"FF", hex"FF")

        assertTermEvalEq(OrByteString $ false $ hex"00" $ hex"00FF", hex"00")
        assertTermEvalEq(OrByteString $ false $ hex"FF" $ hex"00FF", hex"FF")
        assertTermEvalEq(OrByteString $ false $ hex"00" $ hex"FFFF", hex"FF")
        assertTermEvalEq(OrByteString $ false $ hex"FF" $ hex"FFFF", hex"FF")

        assertTermEvalEq(OrByteString $ true $ hex"" $ hex"", hex"")

        assertTermEvalEq(OrByteString $ true $ hex"00" $ hex"00", hex"00")
        assertTermEvalEq(OrByteString $ true $ hex"FF" $ hex"00", hex"FF")
        assertTermEvalEq(OrByteString $ true $ hex"00" $ hex"FF", hex"FF")
        assertTermEvalEq(OrByteString $ true $ hex"FF" $ hex"FF", hex"FF")

        assertTermEvalEq(OrByteString $ true $ hex"00FF" $ hex"00", hex"00FF")
        assertTermEvalEq(OrByteString $ true $ hex"FFFF" $ hex"00", hex"FFFF")
        assertTermEvalEq(OrByteString $ true $ hex"00FF" $ hex"FF", hex"FFFF")
        assertTermEvalEq(OrByteString $ true $ hex"FFFF" $ hex"FF", hex"FFFF")

        assertTermEvalEq(OrByteString $ true $ hex"00" $ hex"00FF", hex"00FF")
        assertTermEvalEq(OrByteString $ true $ hex"FF" $ hex"00FF", hex"FFFF")
        assertTermEvalEq(OrByteString $ true $ hex"00" $ hex"FFFF", hex"FFFF")
        assertTermEvalEq(OrByteString $ true $ hex"FF" $ hex"FFFF", hex"FFFF")
    }

    test("XorByteString follows CIP-122") {
        val XorByteString = compile(scalus.builtin.Builtins.xorByteString).toUplc()

        assertTermEvalEq(XorByteString $ false $ hex"" $ hex"", hex"")

        assertTermEvalEq(XorByteString $ false $ hex"00" $ hex"00", hex"00")
        assertTermEvalEq(XorByteString $ false $ hex"FF" $ hex"00", hex"FF")
        assertTermEvalEq(XorByteString $ false $ hex"00" $ hex"FF", hex"FF")
        assertTermEvalEq(XorByteString $ false $ hex"FF" $ hex"FF", hex"00")

        assertTermEvalEq(XorByteString $ false $ hex"00FF" $ hex"00", hex"00")
        assertTermEvalEq(XorByteString $ false $ hex"FFFF" $ hex"00", hex"FF")
        assertTermEvalEq(XorByteString $ false $ hex"00FF" $ hex"FF", hex"FF")
        assertTermEvalEq(XorByteString $ false $ hex"FFFF" $ hex"FF", hex"00")

        assertTermEvalEq(XorByteString $ false $ hex"00" $ hex"00FF", hex"00")
        assertTermEvalEq(XorByteString $ false $ hex"FF" $ hex"00FF", hex"FF")
        assertTermEvalEq(XorByteString $ false $ hex"00" $ hex"FFFF", hex"FF")
        assertTermEvalEq(XorByteString $ false $ hex"FF" $ hex"FFFF", hex"00")

        assertTermEvalEq(XorByteString $ true $ hex"" $ hex"", hex"")

        assertTermEvalEq(XorByteString $ true $ hex"00" $ hex"00", hex"00")
        assertTermEvalEq(XorByteString $ true $ hex"FF" $ hex"00", hex"FF")
        assertTermEvalEq(XorByteString $ true $ hex"00" $ hex"FF", hex"FF")
        assertTermEvalEq(XorByteString $ true $ hex"FF" $ hex"FF", hex"00")

        assertTermEvalEq(XorByteString $ true $ hex"00FF" $ hex"00", hex"00FF")
        assertTermEvalEq(XorByteString $ true $ hex"FFFF" $ hex"00", hex"FFFF")
        assertTermEvalEq(XorByteString $ true $ hex"00FF" $ hex"FF", hex"FFFF")
        assertTermEvalEq(XorByteString $ true $ hex"FFFF" $ hex"FF", hex"00FF")

        assertTermEvalEq(XorByteString $ true $ hex"00" $ hex"00FF", hex"00FF")
        assertTermEvalEq(XorByteString $ true $ hex"FF" $ hex"00FF", hex"FFFF")
        assertTermEvalEq(XorByteString $ true $ hex"00" $ hex"FFFF", hex"FFFF")
        assertTermEvalEq(XorByteString $ true $ hex"FF" $ hex"FFFF", hex"00FF")
    }

    test("ComplementByteString follows CIP-122") {
        val ComplementByteString = compile(scalus.builtin.Builtins.complementByteString).toUplc()

        assertTermEvalEq(ComplementByteString $ hex"", hex"")

        assertTermEvalEq(ComplementByteString $ hex"00", hex"FF")
        assertTermEvalEq(ComplementByteString $ hex"F0", hex"0F")
        assertTermEvalEq(ComplementByteString $ hex"0F", hex"F0")
        assertTermEvalEq(ComplementByteString $ hex"FF", hex"00")
    }

    test("ReadBit follows CIP-122") {
        val ReadBit = compile(scalus.builtin.Builtins.readBit).toUplc()

        assertTermEvalThrows[BuiltinError](ReadBit $ hex"" $ 0)
        assertTermEvalThrows[BuiltinError](ReadBit $ hex"" $ 345)
        assertTermEvalThrows[BuiltinError](ReadBit $ hex"" $ -1)
        assertTermEvalThrows[BuiltinError](ReadBit $ hex"FF" $ -1)

        assertTermEvalEq(ReadBit $ hex"F4" $ 0, false)
        assertTermEvalEq(ReadBit $ hex"F4" $ 1, false)
        assertTermEvalEq(ReadBit $ hex"F4" $ 2, true)
        assertTermEvalEq(ReadBit $ hex"F4" $ 3, false)
        assertTermEvalEq(ReadBit $ hex"F4" $ 4, true)
        assertTermEvalEq(ReadBit $ hex"F4" $ 5, true)
        assertTermEvalEq(ReadBit $ hex"F4" $ 6, true)
        assertTermEvalEq(ReadBit $ hex"F4" $ 7, true)

        assertTermEvalThrows[BuiltinError](ReadBit $ hex"F4" $ 8)
        assertTermEvalThrows[BuiltinError](ReadBit $ hex"FFF4" $ 16)

        assertTermEvalEq(ReadBit $ hex"F4FF" $ 10, true)
    }

    test("WriteBits follows CIP-122") {
        val WriteBits = compile(scalus.builtin.Builtins.writeBits).toUplc()

        assertTermEvalThrows[BuiltinError](WriteBits $ hex"" $ List(0) $ false)
        assertTermEvalThrows[BuiltinError](WriteBits $ hex"" $ List(15) $ false)
        assertTermEvalThrows[BuiltinError](WriteBits $ hex"" $ List(0) $ true)
        assertTermEvalThrows[BuiltinError](WriteBits $ hex"" $ List(0, 1) $ false)
        assertTermEvalThrows[BuiltinError](WriteBits $ hex"FF" $ List(-1) $ false)
        assertTermEvalThrows[BuiltinError](WriteBits $ hex"FF" $ List(0, -1) $ true)
        assertTermEvalThrows[BuiltinError](WriteBits $ hex"FF" $ List(-1, 0) $ true)
        assertTermEvalThrows[BuiltinError](WriteBits $ hex"FF" $ List(8) $ false)
        assertTermEvalThrows[BuiltinError](WriteBits $ hex"FF" $ List(1, 8) $ false)
        assertTermEvalThrows[BuiltinError](WriteBits $ hex"FF" $ List(8, 1) $ false)

        assertTermEvalEq(WriteBits $ hex"FF" $ List(0) $ false, hex"FE")
        assertTermEvalEq(WriteBits $ hex"FF" $ List(1) $ false, hex"FD")
        assertTermEvalEq(WriteBits $ hex"FF" $ List(2) $ false, hex"FB")
        assertTermEvalEq(WriteBits $ hex"FF" $ List(3) $ false, hex"F7")
        assertTermEvalEq(WriteBits $ hex"FF" $ List(4) $ false, hex"EF")
        assertTermEvalEq(WriteBits $ hex"FF" $ List(5) $ false, hex"DF")
        assertTermEvalEq(WriteBits $ hex"FF" $ List(6) $ false, hex"BF")
        assertTermEvalEq(WriteBits $ hex"FF" $ List(7) $ false, hex"7F")

        assertTermEvalEq(WriteBits $ hex"00" $ List(5) $ true, hex"20")
        assertTermEvalEq(WriteBits $ hex"FF" $ List(5) $ false, hex"DF")
        assertTermEvalEq(WriteBits $ hex"F4FF" $ List(10) $ false, hex"F0FF")
        assertTermEvalEq(WriteBits $ hex"F4FF" $ List(10, 1) $ false, hex"F0FD")
        assertTermEvalEq(WriteBits $ hex"F4FF" $ List(1, 10) $ false, hex"F0FD")

        assertTermEvalEq(WriteBits $ hex"FF" $ List(0) $ true, hex"FF")
        assertTermEvalEq(WriteBits $ hex"00" $ List(0) $ false, hex"00")

    }

    test("ReplicateByte follows CIP-122") {
        val ReplicateByte = compile(scalus.builtin.Builtins.replicateByte).toUplc()

        assertTermEvalThrows[BuiltinError](ReplicateByte $ -1 $ 0)
        assertTermEvalThrows[BuiltinError](ReplicateByte $ -1 $ 3)
        assertTermEvalThrows[BuiltinError](ReplicateByte $ 8193 $ 0)
        assertTermEvalThrows[BuiltinError](ReplicateByte $ 8193 $ 3)
        assertTermEvalThrows[BuiltinError](ReplicateByte $ 1 $ -1)
        assertTermEvalThrows[BuiltinError](ReplicateByte $ 1 $ 256)
        assertTermEvalThrows[BuiltinError](ReplicateByte $ 4 $ -1)
        assertTermEvalThrows[BuiltinError](ReplicateByte $ 4 $ 256)

        assertTermEvalEq(ReplicateByte $ 0 $ 0xff, hex"")
        assertTermEvalEq(ReplicateByte $ 4 $ 0xff, hex"FFFFFFFF")
    }

    test("ShiftByteString follows CIP-123") {
        val ShiftByteString = compile(scalus.builtin.Builtins.shiftByteString).toUplc()

        assertTermEvalEq(ShiftByteString $ hex"" $ 3, hex"")
        assertTermEvalEq(ShiftByteString $ hex"" $ -3, hex"")
        assertTermEvalEq(ShiftByteString $ hex"EBFC" $ 0, hex"EBFC")
        assertTermEvalEq(ShiftByteString $ hex"EBFC" $ 5, hex"7F80")
        assertTermEvalEq(ShiftByteString $ hex"EBFC" $ -5, hex"075F")
        assertTermEvalEq(ShiftByteString $ hex"EBFC" $ 16, hex"0000")
        assertTermEvalEq(ShiftByteString $ hex"EBFC" $ -16, hex"0000")

        val size = 20
        val byteString = ByteString.unsafeFromArray(
          Array.fill(size)((scala.util.Random.nextInt(256) - 128).toByte)
        )
        val binaryStr = byteString.toBinaryString

        for i <- 0 to size do
            assertResult(binaryStr.drop(i) + "0" * i)(
              scalus.builtin.Builtins.shiftByteString(byteString, i).toBinaryString
            )

            assertResult("0" * i + binaryStr.dropRight(i))(
              scalus.builtin.Builtins.shiftByteString(byteString, -i).toBinaryString
            )
    }

    test("RotateByteString follows CIP-123") {
        val RotateByteString = compile(scalus.builtin.Builtins.rotateByteString).toUplc()

        assertTermEvalEq(RotateByteString $ hex"" $ 3, hex"")
        assertTermEvalEq(RotateByteString $ hex"" $ -3, hex"")
        assertTermEvalEq(RotateByteString $ hex"EBFC" $ 0, hex"EBFC")
        assertTermEvalEq(RotateByteString $ hex"EBFC" $ 5, hex"7F9D")
        assertTermEvalEq(RotateByteString $ hex"EBFC" $ -5, hex"E75F")
        assertTermEvalEq(RotateByteString $ hex"EBFC" $ 16, hex"EBFC")
        assertTermEvalEq(RotateByteString $ hex"EBFC" $ -16, hex"EBFC")
        assertTermEvalEq(RotateByteString $ hex"EBFC" $ 21, hex"7F9D")
        assertTermEvalEq(RotateByteString $ hex"EBFC" $ -21, hex"E75F")

        val size = 20
        val byteString = ByteString.unsafeFromArray(
          Array.fill(size)((scala.util.Random.nextInt(256) - 128).toByte)
        )
        val binaryStr = byteString.toBinaryString

        for i <- 0 to size do
            assertResult(binaryStr.drop(i) + binaryStr.take(i))(
              scalus.builtin.Builtins.rotateByteString(byteString, i).toBinaryString
            )

            assertResult(binaryStr.takeRight(i) + binaryStr.dropRight(i))(
              scalus.builtin.Builtins.rotateByteString(byteString, -i).toBinaryString
            )
    }

    test("CountSetBits follows CIP-123") {
        val CountSetBits = compile(scalus.builtin.Builtins.countSetBits).toUplc()

        assertTermEvalEq(CountSetBits $ hex"", 0)
        assertTermEvalEq(CountSetBits $ hex"0000", 0)
        assertTermEvalEq(CountSetBits $ hex"0100", 1)
        assertTermEvalEq(CountSetBits $ hex"0001", 1)
        assertTermEvalEq(CountSetBits $ hex"000F", 4)
        assertTermEvalEq(CountSetBits $ hex"FFFF", 16)
    }

    test("FindFirstSetBit follows CIP-123") {
        val FindFirstSetBit = compile(scalus.builtin.Builtins.findFirstSetBit).toUplc()

        assertTermEvalEq(FindFirstSetBit $ hex"", -1)
        assertTermEvalEq(FindFirstSetBit $ hex"0000", -1)
        assertTermEvalEq(FindFirstSetBit $ hex"0002", 1)
        assertTermEvalEq(FindFirstSetBit $ hex"FFF2", 1)
        assertTermEvalEq(FindFirstSetBit $ hex"8000", 15)
    }
