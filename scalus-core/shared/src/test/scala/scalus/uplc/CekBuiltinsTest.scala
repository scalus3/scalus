package scalus.uplc

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.*
import scalus.Compiler.compile
import scalus.builtin.{ByteString, Data}
import scalus.builtin.ByteString.*
import scalus.uplc.Constant.Pair
import scalus.uplc.DefaultFun.*
import scalus.uplc.Constant.asConstant
import scalus.uplc.Term.*
import scalus.uplc.TermDSL.given
import scalus.uplc.eval.*
import scalus.uplc.test.ArbitraryInstances

import scala.language.implicitConversions
import scala.reflect.ClassTag

open class CekBuiltinsTest
    extends AnyFunSuite
    with ScalaCheckPropertyChecks
    with ArbitraryInstances:

    protected given PlutusVM = PlutusVM.makePlutusV3VM()

    def assertEvalEq(a: Term, b: Term): Unit =
        assert(a.evaluate == b, s"$a != $b")

    def assertEvalThrows[A <: AnyRef: ClassTag](a: Term): Unit =
        assertThrows[A](a.evaluate)

    test("Lazy builtin evaluation") {
        assertEvalEq(AddInteger $ "wrong", Apply(Builtin(AddInteger), "wrong"))
    }

    test("AddInteger") {
        forAll { (a: BigInt, b: BigInt) =>
            assertEvalEq(AddInteger $ a $ b, Const(asConstant(a + b)))
        }

        forAll { (a: Term, b: Term) =>
            (a, b) match
                case (Const(Constant.Integer(aa)), Const(Constant.Integer(bb))) =>
                    val r = aa + bb
                    assertEvalEq(AddInteger $ a $ b, Const(Constant.Integer(r)))
                case _ => assertEvalThrows[Exception](AddInteger $ a $ b)
        }
    }

    test("SubstractInteger") {
        forAll { (a: BigInt, b: BigInt) =>
            assertEvalEq(SubtractInteger $ a $ b, Const(asConstant(a - b)))
        }

        forAll { (a: Term, b: Term) =>
            (a, b) match
                case (Const(Constant.Integer(aa)), Const(Constant.Integer(bb))) =>
                    val r = aa - bb
                    assertEvalEq(SubtractInteger $ a $ b, Const(Constant.Integer(r)))
                case _ => assertEvalThrows[Exception](AddInteger $ a $ b)
        }
    }

    test("DivideInteger") {
        // integer division truncated toward negative infinity
        assertEvalEq(DivideInteger $ 20 $ 3, Const(Constant.Integer(6)))
        assertEvalEq(DivideInteger $ -20 $ -3, Const(Constant.Integer(6)))
        assertEvalEq(DivideInteger $ 20 $ -3, Const(Constant.Integer(-7)))
        assertEvalEq(DivideInteger $ -20 $ 3, Const(Constant.Integer(-7)))
        assertEvalThrows[BuiltinError](DivideInteger $ 1 $ 0)
    }

    test("QuotientInteger") {
        assertEvalEq(QuotientInteger $ -20 $ 3, Const(Constant.Integer(-6)))
        assertEvalThrows[BuiltinError](QuotientInteger $ 20 $ 0)
        forAll { (a: BigInt, b: BigInt) =>
            if b == 0 then assertEvalThrows[BuiltinError](QuotientInteger $ a $ b)
            else assertEvalEq(QuotientInteger $ a $ b, Const(asConstant(a / b)))
        }
    }

    test("RemainderInteger") {
        assertEvalEq(RemainderInteger $ 20 $ 3, Const(Constant.Integer(2)))
        assertEvalEq(RemainderInteger $ -20 $ 3, Const(Constant.Integer(-2)))
        assertEvalEq(RemainderInteger $ 20 $ -3, Const(Constant.Integer(2)))
        assertEvalEq(RemainderInteger $ -20 $ -3, Const(Constant.Integer(-2)))
        assertEvalThrows[BuiltinError](RemainderInteger $ 20 $ 0)
        forAll { (a: BigInt, b: BigInt) =>
            if b == 0 then assertEvalThrows[BuiltinError](RemainderInteger $ a $ b)
            else assertEvalEq(RemainderInteger $ a $ b, Const(asConstant(a % b)))
        }
    }

    test("ModInteger") {
        assertEvalEq(ModInteger $ 20 $ 3, Const(Constant.Integer(2)))
        assertEvalEq(ModInteger $ -20 $ 3, Const(Constant.Integer(1)))
        assertEvalEq(ModInteger $ 20 $ -3, Const(Constant.Integer(-1)))
        assertEvalEq(ModInteger $ -20 $ -3, Const(Constant.Integer(-2)))
        assertEvalThrows[BuiltinError](ModInteger $ 20 $ 0)
    }

    test("(x `quot` y)*y + (x `rem` y) == x") {
        forAll { (x: BigInt, y: BigInt) =>
            whenever(y != 0) {
                val q = QuotientInteger $ x $ y
                val r = RemainderInteger $ x $ y
                val x1 = AddInteger $ (MultiplyInteger $ q $ y) $ r
                assertEvalEq(x1, Const(Constant.Integer(x)))
            }
        }
    }

    test("(x `div` y)*y + (x `mod` y) == x") {
        forAll { (x: BigInt, y: BigInt) =>
            whenever(y != 0) {
                val q = DivideInteger $ x $ y
                val r = ModInteger $ x $ y
                val x1 = AddInteger $ (MultiplyInteger $ q $ y) $ r
                assertEvalEq(x1, Const(Constant.Integer(x)))
            }
        }
    }

    test("EqualsInteger") {
        forAll { (a: BigInt, b: BigInt) =>
            assertEvalEq(EqualsInteger $ a $ b, Const(asConstant(a == b)))
        }
    }

    test("UnConstrData") {
        assert(
          (UnConstrData $ Data.Constr(12, 1 :: Nil)).evaluate ==
              Const(
                Pair(asConstant(12), Constant.List(DefaultUni.Data, List(Constant.Data(Data.I(1)))))
              )
        )
    }

    test("UnMapData") {
        assert(
          (UnMapData $ Data.Map((12, 1) :: Nil)).evaluate ==
              Const(
                Constant.List(
                  DefaultUni.Pair(DefaultUni.Data, DefaultUni.Data),
                  Pair(Constant.Data(12), Constant.Data(1)) :: Nil
                )
              )
        )
    }

    test("UnListData") {
        assert(
          (UnListData $ Data.List(Data.I(12) :: Data.I(1) :: Nil)).evaluate ==
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
        assertEvalEq(
          !(!ChooseList) $ Const(Constant.List(DefaultUni.Integer, Nil)) $ asConstant(
            1
          ) $ asConstant(2),
          Const(asConstant(1))
        )
        // check non-empty case
        assertEvalEq(
          !(!ChooseList) $ Const(
            Constant.List(DefaultUni.Integer, asConstant(333) :: Nil)
          ) $ asConstant(1) $ asConstant(2),
          Const(asConstant(2))
        )

        forAll { (t: Constant) =>
            t match
                case Constant.List(_, v) =>
                    assertEvalEq(
                      !(!ChooseList) $ t $ asConstant(true) $ asConstant(false),
                      Const(Constant.Bool(v.isEmpty))
                    )
                case _ =>
                    assertEvalThrows[Exception](!ChooseList $ t)
        }
    }

    test("NullList") {
        assertEvalEq(
          !NullList $ Const(Constant.List(DefaultUni.Integer, Nil)),
          Const(asConstant(true))
        )

        forAll { (t: Constant) =>
            t match
                case Constant.List(_, v) =>
                    assertEvalEq(!NullList $ t, Const(Constant.Bool(v.isEmpty)))
                case _ =>
                    assertEvalThrows[Exception](!NullList $ t)
        }
    }

    test("MkCons") {
        assertEvalEq(
          !MkCons $ Constant.Integer(0) $ Const(
            Constant.List(DefaultUni.Integer, asConstant(1) :: Nil)
          ),
          Const(Constant.List(DefaultUni.Integer, asConstant(0) :: asConstant(1) :: Nil))
        )
        // should throw if constant types don't match
        assertEvalThrows[Exception](
          !MkCons $ Constant.Unit $ Constant.List(DefaultUni.Integer, Nil)
        )
    }

    test("HeadList") {
        assertEvalEq(
          !HeadList $ Const(Constant.List(DefaultUni.Integer, asConstant(1) :: Nil)),
          Const(asConstant(1))
        )

        forAll { (t: Constant) =>
            t match
                case Constant.List(_, v) if v.nonEmpty =>
                    assertEvalEq(!HeadList $ t, Const(v.head))
                case _ =>
                    assertEvalThrows[Exception](!HeadList $ t)
        }
    }

    test("TailList") {
        assertEvalEq(
          !TailList $ Const(
            Constant.List(DefaultUni.Integer, asConstant(1) :: asConstant(2) :: Nil)
          ),
          Const(Constant.List(DefaultUni.Integer, asConstant(2) :: Nil))
        )

        forAll { (t: Constant) =>
            t match
                case Constant.List(tpe, v) if v.nonEmpty =>
                    assertEvalEq(!TailList $ t, Const(Constant.List(tpe, v.tail)))
                case _ =>
                    assertEvalThrows[Exception](!TailList $ t)
        }
    }

    test("FstPair") {
        assertEvalEq(
          !(!FstPair) $ Const(Constant.Pair(asConstant(1), asConstant(false))),
          Const(asConstant(1))
        )

        /*forAll { (t: Constant) =>
      t match
        case Constant.Pair(a, _) =>
          assertEvalEq(!(!FstPair) $ t, Const(a))
        case _ =>
          assertEvalThrows[Exception](!(!FstPair) $ t)
    }*/
    }

    test("SndPair") {
        assertEvalEq(
          !(!SndPair) $ Const(Constant.Pair(asConstant(1), asConstant(false))),
          Const(asConstant(false))
        )

        forAll { (t: Constant) =>
            t match
                case Constant.Pair(_, b) =>
                    assertEvalEq(!(!SndPair) $ t, Const(b))
                case _ =>
                    assertEvalThrows[Exception](!(!SndPair) $ t)
        }
    }

    test("EqualsByteString") {
        assertEvalEq(
          EqualsByteString $ Const(Constant.ByteString(hex"deadbeef")) $ Const(
            Constant.ByteString(hex"deadbeef")
          ),
          Const(asConstant(true))
        )
        assertEvalEq(
          EqualsByteString $ Const(Constant.ByteString(hex"")) $ Const(
            Constant.ByteString(hex"deadbeef")
          ),
          Const(asConstant(false))
        )

        forAll { (t: Constant) =>
            t match
                case Constant.ByteString(_) =>
                    assertEvalEq(EqualsByteString $ t $ t, Const(asConstant(true)))
                case _ =>
                    assertEvalThrows[Exception](EqualsByteString $ t $ t)
        }
    }

    test("SliceByteString") {
        assertEvalEq(
          SliceByteString $ asConstant(1) $ asConstant(3) $ Const(
            Constant.ByteString(hex"deadbeef")
          ),
          Const(Constant.ByteString(hex"adbeef"))
        )

        assertEvalEq(
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
            ByteString.fromString("hello") $
            hex"f13fa9acffb108114ec060561b58005fb2d69184de0a2d7400b2ea1f111c0794831cc832c92daf4807820dd9458324935e90bec855e8bf076bbbc4e42b727b07"

        assertEvalEq(valid, true)

        val wrongMessage = verify $
            hex"9518c18103cbdab9c6e60b58ecc3e2eb439fef6519bb22570f391327381900a8" $
            ByteString.fromString("NOT hello") $
            hex"f13fa9acffb108114ec060561b58005fb2d69184de0a2d7400b2ea1f111c0794831cc832c92daf4807820dd9458324935e90bec855e8bf076bbbc4e42b727b07"

        assertEvalEq(wrongMessage, false)

        val wrongPubKey = verify $
            hex"AA18c18103cbdab9c6e60b58ecc3e2eb439fef6519bb22570f391327381900a8" $
            ByteString.fromString("hello") $
            hex"f13fa9acffb108114ec060561b58005fb2d69184de0a2d7400b2ea1f111c0794831cc832c92daf4807820dd9458324935e90bec855e8bf076bbbc4e42b727b07"

        assertEvalEq(wrongPubKey, false)

        val wrongSignature = verify $
            hex"9518c18103cbdab9c6e60b58ecc3e2eb439fef6519bb22570f391327381900a8" $
            ByteString.fromString("NOT hello") $
            hex"FF3fa9acffb108114ec060561b58005fb2d69184de0a2d7400b2ea1f111c0794831cc832c92daf4807820dd9458324935e90bec855e8bf076bbbc4e42b727b07"

        assertEvalEq(wrongSignature, false)
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

        assertEvalEq(valid, true)

        // invalid message size
        val invalidMessageSize = verify $
            pubKey $
            hex"deadbeef" $
            hex"040f5b6a2bb4e024d47eab02d4073da655af77c0cf0efdb19c6771378da175c45ffac010a1bd9b9a275ad685ea4052f4bc72c0dc27094422ba9379e7bf44b29b"

        assertEvalThrows[BuiltinError](invalidMessageSize)

        val invalidPubKey = verify $
            hex"FFFF7d3132a06e31bf66791dda478b5ebec79bd045247126396fccdf11e42a3627" $
            hex"2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824" $
            hex"040f5b6a2bb4e024d47eab02d4073da655af77c0cf0efdb19c6771378da175c45ffac010a1bd9b9a275ad685ea4052f4bc72c0dc27094422ba9379e7bf44b29b"

        assertEvalThrows[BuiltinError](invalidPubKey)

        val invalidSignature = verify $
            pubKey $
            hex"2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824" $
            hex"deadbeef"

        assertEvalThrows[BuiltinError](invalidSignature)

        val wrongSignature = verify $
            pubKey $
            hex"2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824" $
            hex"FF0f5b6a2bb4e024d47eab02d4073da655af77c0cf0efdb19c6771378da175c45ffac010a1bd9b9a275ad685ea4052f4bc72c0dc27094422ba9379e7bf44b29b"

        assertEvalEq(wrongSignature, false)
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

        assertEvalEq(valid, true)

        val invalidPubKey = verify $
            hex"FFFF7d3132a06e31bf66791dda478b5ebec79bd045247126396fccdf11e42a3627" $
            hex"2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824" $
            hex"4fd97a0c4ad719f89cba68a522e0dee13bcf656ae9c0a395404cda858a7992d8dea979dbc4c83659d695b7d380fe8a75264ba51a63a53fc2a8bd225e50f223f4"

        assertEvalThrows[BuiltinError](invalidPubKey)

        val invalidSignature = verify $
            pubKey $
            hex"2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824" $
            hex"deadbeef"

        assertEvalThrows[BuiltinError](invalidSignature)

        val wrongSignature = verify $
            pubKey $
            hex"2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824" $
            hex"FFd97a0c4ad719f89cba68a522e0dee13bcf656ae9c0a395404cda858a7992d8dea979dbc4c83659d695b7d380fe8a75264ba51a63a53fc2a8bd225e50f223f4"

        assertEvalEq(wrongSignature, false)
    }

    test("AndByteString follows CIP-122") {
        val AndByteString = compile(scalus.builtin.Builtins.andByteString).toUplc()

        assertEvalEq(AndByteString $ false $ hex"" $ hex"", hex"")

        assertEvalEq(AndByteString $ false $ hex"00" $ hex"00", hex"00")
        assertEvalEq(AndByteString $ false $ hex"FF" $ hex"00", hex"00")
        assertEvalEq(AndByteString $ false $ hex"00" $ hex"FF", hex"00")
        assertEvalEq(AndByteString $ false $ hex"FF" $ hex"FF", hex"FF")

        assertEvalEq(AndByteString $ false $ hex"00FF" $ hex"00", hex"00")
        assertEvalEq(AndByteString $ false $ hex"FFFF" $ hex"00", hex"00")
        assertEvalEq(AndByteString $ false $ hex"00FF" $ hex"FF", hex"00")
        assertEvalEq(AndByteString $ false $ hex"FFFF" $ hex"FF", hex"FF")

        assertEvalEq(AndByteString $ false $ hex"00" $ hex"00FF", hex"00")
        assertEvalEq(AndByteString $ false $ hex"FF" $ hex"00FF", hex"00")
        assertEvalEq(AndByteString $ false $ hex"00" $ hex"FFFF", hex"00")
        assertEvalEq(AndByteString $ false $ hex"FF" $ hex"FFFF", hex"FF")

        assertEvalEq(AndByteString $ true $ hex"" $ hex"", hex"")

        assertEvalEq(AndByteString $ true $ hex"00" $ hex"00", hex"00")
        assertEvalEq(AndByteString $ true $ hex"FF" $ hex"00", hex"00")
        assertEvalEq(AndByteString $ true $ hex"00" $ hex"FF", hex"00")
        assertEvalEq(AndByteString $ true $ hex"FF" $ hex"FF", hex"FF")

        assertEvalEq(AndByteString $ true $ hex"00FF" $ hex"00", hex"00FF")
        assertEvalEq(AndByteString $ true $ hex"FFFF" $ hex"00", hex"00FF")
        assertEvalEq(AndByteString $ true $ hex"00FF" $ hex"FF", hex"00FF")
        assertEvalEq(AndByteString $ true $ hex"FFFF" $ hex"FF", hex"FFFF")

        assertEvalEq(AndByteString $ true $ hex"00" $ hex"00FF", hex"00FF")
        assertEvalEq(AndByteString $ true $ hex"FF" $ hex"00FF", hex"00FF")
        assertEvalEq(AndByteString $ true $ hex"00" $ hex"FFFF", hex"00FF")
        assertEvalEq(AndByteString $ true $ hex"FF" $ hex"FFFF", hex"FFFF")
    }

    test("OrByteString follows CIP-122") {
        val OrByteString = compile(scalus.builtin.Builtins.orByteString).toUplc()

        assertEvalEq(OrByteString $ false $ hex"" $ hex"", hex"")

        assertEvalEq(OrByteString $ false $ hex"00" $ hex"00", hex"00")
        assertEvalEq(OrByteString $ false $ hex"FF" $ hex"00", hex"FF")
        assertEvalEq(OrByteString $ false $ hex"00" $ hex"FF", hex"FF")
        assertEvalEq(OrByteString $ false $ hex"FF" $ hex"FF", hex"FF")

        assertEvalEq(OrByteString $ false $ hex"00FF" $ hex"00", hex"00")
        assertEvalEq(OrByteString $ false $ hex"FFFF" $ hex"00", hex"FF")
        assertEvalEq(OrByteString $ false $ hex"00FF" $ hex"FF", hex"FF")
        assertEvalEq(OrByteString $ false $ hex"FFFF" $ hex"FF", hex"FF")

        assertEvalEq(OrByteString $ false $ hex"00" $ hex"00FF", hex"00")
        assertEvalEq(OrByteString $ false $ hex"FF" $ hex"00FF", hex"FF")
        assertEvalEq(OrByteString $ false $ hex"00" $ hex"FFFF", hex"FF")
        assertEvalEq(OrByteString $ false $ hex"FF" $ hex"FFFF", hex"FF")

        assertEvalEq(OrByteString $ true $ hex"" $ hex"", hex"")

        assertEvalEq(OrByteString $ true $ hex"00" $ hex"00", hex"00")
        assertEvalEq(OrByteString $ true $ hex"FF" $ hex"00", hex"FF")
        assertEvalEq(OrByteString $ true $ hex"00" $ hex"FF", hex"FF")
        assertEvalEq(OrByteString $ true $ hex"FF" $ hex"FF", hex"FF")

        assertEvalEq(OrByteString $ true $ hex"00FF" $ hex"00", hex"00FF")
        assertEvalEq(OrByteString $ true $ hex"FFFF" $ hex"00", hex"FFFF")
        assertEvalEq(OrByteString $ true $ hex"00FF" $ hex"FF", hex"FFFF")
        assertEvalEq(OrByteString $ true $ hex"FFFF" $ hex"FF", hex"FFFF")

        assertEvalEq(OrByteString $ true $ hex"00" $ hex"00FF", hex"00FF")
        assertEvalEq(OrByteString $ true $ hex"FF" $ hex"00FF", hex"FFFF")
        assertEvalEq(OrByteString $ true $ hex"00" $ hex"FFFF", hex"FFFF")
        assertEvalEq(OrByteString $ true $ hex"FF" $ hex"FFFF", hex"FFFF")
    }

    test("XorByteString follows CIP-122") {
        val XorByteString = compile(scalus.builtin.Builtins.xorByteString).toUplc()

        assertEvalEq(XorByteString $ false $ hex"" $ hex"", hex"")

        assertEvalEq(XorByteString $ false $ hex"00" $ hex"00", hex"00")
        assertEvalEq(XorByteString $ false $ hex"FF" $ hex"00", hex"FF")
        assertEvalEq(XorByteString $ false $ hex"00" $ hex"FF", hex"FF")
        assertEvalEq(XorByteString $ false $ hex"FF" $ hex"FF", hex"00")

        assertEvalEq(XorByteString $ false $ hex"00FF" $ hex"00", hex"00")
        assertEvalEq(XorByteString $ false $ hex"FFFF" $ hex"00", hex"FF")
        assertEvalEq(XorByteString $ false $ hex"00FF" $ hex"FF", hex"FF")
        assertEvalEq(XorByteString $ false $ hex"FFFF" $ hex"FF", hex"00")

        assertEvalEq(XorByteString $ false $ hex"00" $ hex"00FF", hex"00")
        assertEvalEq(XorByteString $ false $ hex"FF" $ hex"00FF", hex"FF")
        assertEvalEq(XorByteString $ false $ hex"00" $ hex"FFFF", hex"FF")
        assertEvalEq(XorByteString $ false $ hex"FF" $ hex"FFFF", hex"00")

        assertEvalEq(XorByteString $ true $ hex"" $ hex"", hex"")

        assertEvalEq(XorByteString $ true $ hex"00" $ hex"00", hex"00")
        assertEvalEq(XorByteString $ true $ hex"FF" $ hex"00", hex"FF")
        assertEvalEq(XorByteString $ true $ hex"00" $ hex"FF", hex"FF")
        assertEvalEq(XorByteString $ true $ hex"FF" $ hex"FF", hex"00")

        assertEvalEq(XorByteString $ true $ hex"00FF" $ hex"00", hex"00FF")
        assertEvalEq(XorByteString $ true $ hex"FFFF" $ hex"00", hex"FFFF")
        assertEvalEq(XorByteString $ true $ hex"00FF" $ hex"FF", hex"FFFF")
        assertEvalEq(XorByteString $ true $ hex"FFFF" $ hex"FF", hex"00FF")

        assertEvalEq(XorByteString $ true $ hex"00" $ hex"00FF", hex"00FF")
        assertEvalEq(XorByteString $ true $ hex"FF" $ hex"00FF", hex"FFFF")
        assertEvalEq(XorByteString $ true $ hex"00" $ hex"FFFF", hex"FFFF")
        assertEvalEq(XorByteString $ true $ hex"FF" $ hex"FFFF", hex"00FF")
    }

    test("ComplementByteString follows CIP-122") {
        val ComplementByteString = compile(scalus.builtin.Builtins.complementByteString).toUplc()

        assertEvalEq(ComplementByteString $ hex"", hex"")

        assertEvalEq(ComplementByteString $ hex"00", hex"FF")
        assertEvalEq(ComplementByteString $ hex"F0", hex"0F")
        assertEvalEq(ComplementByteString $ hex"0F", hex"F0")
        assertEvalEq(ComplementByteString $ hex"FF", hex"00")
    }

    test("ReadBit follows CIP-122") {
        val ReadBit = compile(scalus.builtin.Builtins.readBit).toUplc()

        assertEvalThrows[BuiltinError](ReadBit $ hex"" $ 0)
        assertEvalThrows[BuiltinError](ReadBit $ hex"" $ 345)
        assertEvalThrows[BuiltinError](ReadBit $ hex"" $ -1)
        assertEvalThrows[BuiltinError](ReadBit $ hex"FF" $ -1)

        assertEvalEq(ReadBit $ hex"F4" $ 0, false)
        assertEvalEq(ReadBit $ hex"F4" $ 1, false)
        assertEvalEq(ReadBit $ hex"F4" $ 2, true)
        assertEvalEq(ReadBit $ hex"F4" $ 3, false)
        assertEvalEq(ReadBit $ hex"F4" $ 4, true)
        assertEvalEq(ReadBit $ hex"F4" $ 5, true)
        assertEvalEq(ReadBit $ hex"F4" $ 6, true)
        assertEvalEq(ReadBit $ hex"F4" $ 7, true)

        assertEvalThrows[BuiltinError](ReadBit $ hex"F4" $ 8)
        assertEvalThrows[BuiltinError](ReadBit $ hex"FFF4" $ 16)

        assertEvalEq(ReadBit $ hex"F4FF" $ 10, true)
    }

    test("WriteBits follows CIP-122") {
        val WriteBits = compile(scalus.builtin.Builtins.writeBits).toUplc()

        assertEvalThrows[BuiltinError](WriteBits $ hex"" $ List(0) $ false)
        assertEvalThrows[BuiltinError](WriteBits $ hex"" $ List(15) $ false)
        assertEvalThrows[BuiltinError](WriteBits $ hex"" $ List(0) $ true)
        assertEvalThrows[BuiltinError](WriteBits $ hex"" $ List(0, 1) $ false)
        assertEvalThrows[BuiltinError](WriteBits $ hex"FF" $ List(-1) $ false)
        assertEvalThrows[BuiltinError](WriteBits $ hex"FF" $ List(0, -1) $ true)
        assertEvalThrows[BuiltinError](WriteBits $ hex"FF" $ List(-1, 0) $ true)
        assertEvalThrows[BuiltinError](WriteBits $ hex"FF" $ List(8) $ false)
        assertEvalThrows[BuiltinError](WriteBits $ hex"FF" $ List(1, 8) $ false)
        assertEvalThrows[BuiltinError](WriteBits $ hex"FF" $ List(8, 1) $ false)

        assertEvalEq(WriteBits $ hex"FF" $ List(0) $ false, hex"FE")
        assertEvalEq(WriteBits $ hex"FF" $ List(1) $ false, hex"FD")
        assertEvalEq(WriteBits $ hex"FF" $ List(2) $ false, hex"FB")
        assertEvalEq(WriteBits $ hex"FF" $ List(3) $ false, hex"F7")
        assertEvalEq(WriteBits $ hex"FF" $ List(4) $ false, hex"EF")
        assertEvalEq(WriteBits $ hex"FF" $ List(5) $ false, hex"DF")
        assertEvalEq(WriteBits $ hex"FF" $ List(6) $ false, hex"BF")
        assertEvalEq(WriteBits $ hex"FF" $ List(7) $ false, hex"7F")

        assertEvalEq(WriteBits $ hex"00" $ List(5) $ true, hex"20")
        assertEvalEq(WriteBits $ hex"FF" $ List(5) $ false, hex"DF")
        assertEvalEq(WriteBits $ hex"F4FF" $ List(10) $ false, hex"F0FF")
        assertEvalEq(WriteBits $ hex"F4FF" $ List(10, 1) $ false, hex"F0FD")
        assertEvalEq(WriteBits $ hex"F4FF" $ List(1, 10) $ false, hex"F0FD")

        assertEvalEq(WriteBits $ hex"FF" $ List(0) $ true, hex"FF")
        assertEvalEq(WriteBits $ hex"00" $ List(0) $ false, hex"00")

    }

    test("ReplicateByte follows CIP-122") {
        val ReplicateByte = compile(scalus.builtin.Builtins.replicateByte).toUplc()

        assertEvalThrows[BuiltinError](ReplicateByte $ -1 $ 0)
        assertEvalThrows[BuiltinError](ReplicateByte $ -1 $ 3)
        assertEvalThrows[BuiltinError](ReplicateByte $ 8193 $ 0)
        assertEvalThrows[BuiltinError](ReplicateByte $ 8193 $ 3)
        assertEvalThrows[BuiltinError](ReplicateByte $ 1 $ -1)
        assertEvalThrows[BuiltinError](ReplicateByte $ 1 $ 256)
        assertEvalThrows[BuiltinError](ReplicateByte $ 4 $ -1)
        assertEvalThrows[BuiltinError](ReplicateByte $ 4 $ 256)

        assertEvalEq(ReplicateByte $ 0 $ 0xff, hex"")
        assertEvalEq(ReplicateByte $ 4 $ 0xff, hex"FFFFFFFF")
    }

    test("ShiftByteString follows CIP-123") {
        val ShiftByteString = compile(scalus.builtin.Builtins.shiftByteString).toUplc()

        assertEvalEq(ShiftByteString $ hex"" $ 3, hex"")
        assertEvalEq(ShiftByteString $ hex"" $ -3, hex"")
        assertEvalEq(ShiftByteString $ hex"EBFC" $ 0, hex"EBFC")
        assertEvalEq(ShiftByteString $ hex"EBFC" $ 5, hex"7F80")
        assertEvalEq(ShiftByteString $ hex"EBFC" $ -5, hex"075F")
        assertEvalEq(ShiftByteString $ hex"EBFC" $ 16, hex"0000")
        assertEvalEq(ShiftByteString $ hex"EBFC" $ -16, hex"0000")

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

        assertEvalEq(RotateByteString $ hex"" $ 3, hex"")
        assertEvalEq(RotateByteString $ hex"" $ -3, hex"")
        assertEvalEq(RotateByteString $ hex"EBFC" $ 0, hex"EBFC")
        assertEvalEq(RotateByteString $ hex"EBFC" $ 5, hex"7F9D")
        assertEvalEq(RotateByteString $ hex"EBFC" $ -5, hex"E75F")
        assertEvalEq(RotateByteString $ hex"EBFC" $ 16, hex"EBFC")
        assertEvalEq(RotateByteString $ hex"EBFC" $ -16, hex"EBFC")
        assertEvalEq(RotateByteString $ hex"EBFC" $ 21, hex"7F9D")
        assertEvalEq(RotateByteString $ hex"EBFC" $ -21, hex"E75F")

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

        assertEvalEq(CountSetBits $ hex"", 0)
        assertEvalEq(CountSetBits $ hex"0000", 0)
        assertEvalEq(CountSetBits $ hex"0100", 1)
        assertEvalEq(CountSetBits $ hex"0001", 1)
        assertEvalEq(CountSetBits $ hex"000F", 4)
        assertEvalEq(CountSetBits $ hex"FFFF", 16)
    }

    test("FindFirstSetBit follows CIP-123") {
        val FindFirstSetBit = compile(scalus.builtin.Builtins.findFirstSetBit).toUplc()

        assertEvalEq(FindFirstSetBit $ hex"", -1)
        assertEvalEq(FindFirstSetBit $ hex"0000", -1)
        assertEvalEq(FindFirstSetBit $ hex"0002", 1)
        assertEvalEq(FindFirstSetBit $ hex"FFF2", 1)
        assertEvalEq(FindFirstSetBit $ hex"8000", 15)
    }
