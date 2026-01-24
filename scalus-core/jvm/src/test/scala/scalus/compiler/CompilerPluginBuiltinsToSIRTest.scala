package scalus.compiler

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.uplc.builtin
import scalus.uplc.builtin.ByteString.*
import scalus.uplc.builtin.{Builtins, ByteString, Data, JVMPlatformSpecific}
import scalus.compiler.sir.*
import scalus.compiler.sir.SIR.*
import scalus.compiler.sir.SIRType.Fun
import scalus.compiler.sir.SirDSL.{*, given}
import scalus.compiler.{compile, Options}
import scalus.uplc.*
import scalus.uplc.Constant.asConstant
import scalus.uplc.DefaultFun.*
import scalus.uplc.Term.asTerm
import scalus.uplc.eval.PlutusVM
import scalus.toUplc

import scala.collection.immutable
import scala.language.implicitConversions

class CompilerPluginBuiltinsToSIRTest extends AnyFunSuite with ScalaCheckPropertyChecks {
    private given PlutusVM = PlutusVM.makePlutusV2VM()

    private val deadbeef = Constant.ByteString(hex"deadbeef")

    private val sirData = SIRType.Data.tp
    private val sirBool = SIRType.Boolean
    private val sirInt = SIRType.Integer
    private val sirString = SIRType.String
    private val sirByteString = SIRType.ByteString
    private val sirVoid = SIRType.Unit

    def sirList(tpe: SIRType) = SIRType.List(tpe)

    def sirBuiltinList(tpe: SIRType) = SIRType.BuiltinList(tpe)

    def sirPair(t1: SIRType, t2: SIRType) = SIRType.BuiltinPair(t1, t2)

    def sirConst(x: Int) = Const(Constant.Integer(x), SIRType.Integer, AnnotationsDecl.empty)

    def sirConst(x: BigInt) = Const(Constant.Integer(x), SIRType.Integer, AnnotationsDecl.empty)

    def sirConst(x: Boolean) = Const(Constant.Bool(x), SIRType.Boolean, AnnotationsDecl.empty)

    def sirConst(x: String) = Const(Constant.String(x), SIRType.String, AnnotationsDecl.empty)

    def sirConst(x: ByteString) =
        Const(Constant.ByteString(x), SIRType.ByteString, AnnotationsDecl.empty)

    def sirConst(x: Data) = Const(Constant.Data(x), SIRType.Data.tp, AnnotationsDecl.empty)

    def sirConstUnit = Const(Constant.Unit, SIRType.Unit, AnnotationsDecl.empty)

    def AnE = AnnotationsDecl.empty

    given Options = Options(
      targetLoweringBackend = TargetLoweringBackend.SirToUplcV3Lowering,
      generateErrorTraces = true,
      optimizeUplc = true,
      debug = false
    )

    test("compile chooseList builtins") {

        val x: SIR = List(1, 2, 3)

        assert(
          compile(
            Builtins.chooseList(scalus.uplc.builtin.BuiltinList[BigInt](1, 2, 3), true, false)
          ) ~=~ (DefaultFun.ChooseList $ List(1, 2, 3) $ true $ false)
        )
    }

    test("compile mkCons builtins") {
        assert(
          compile(
            Builtins.mkCons(BigInt(4), scalus.uplc.builtin.BuiltinList[BigInt](1, 2, 3))
          ) ~=~ (DefaultFun.MkCons $ 4 $ List(1, 2, 3))
        )
    }

    test("compile headList builtins") {
        assert(
          compile(
            Builtins.headList(scalus.uplc.builtin.BuiltinList[BigInt](1, 2, 3))
          ) ~=~ (DefaultFun.HeadList $ List(1, 2, 3))
        )
    }

    test("compile tailList builtins") {
        assert(
          compile(
            Builtins.tailList(scalus.uplc.builtin.BuiltinList[BigInt](1, 2, 3))
          ) ~=~ (DefaultFun.TailList $ List(1, 2, 3))
        )
    }

    test("compile nullList builtins") {
        assert(
          compile(
            Builtins.nullList(scalus.uplc.builtin.BuiltinList[BigInt](1, 2, 3))
          ) ~=~ (DefaultFun.NullList $ List(1, 2, 3))
        )
    }

    test("compile empty List") {
        assert(
          compile {
              scalus.uplc.builtin.BuiltinList.empty[BigInt]
          } ~=~ Const(
            Constant.List(DefaultUni.Integer, List()),
            SIRType.List(SIRType.Integer),
            AnE
          )
        )
    }

    test("compile List literal") {
        assert(
          compile {
              scalus.uplc.builtin.BuiltinList[BigInt](1, 2, 3)
          } ~=~ Const(
            Constant.List(
              DefaultUni.Integer,
              List(Constant.Integer(1), Constant.Integer(2), Constant.Integer(3))
            ),
            SIRType.List(SIRType.Integer),
            AnE
          )
        )
    }

    test("compile MkCons builtin") {

        val compiled = compile {
            val a = "foo"
            "bar" :: scalus.uplc.builtin.BuiltinList(a)
        }

        val expected = Let(
          List(Binding("a", sirString, Const(Constant.String("foo"), sirString, AnE))),
          Apply(
            Apply(
              SIRBuiltins.mkCons,
              Const(Constant.String("bar"), sirString, AnE),
              Fun(SIRType.BuiltinList(sirString), SIRType.BuiltinList(sirString)),
              AnE
            ),
            Apply(
              Apply(
                SIRBuiltins.mkCons,
                Var("a", sirString, AnE),
                Fun(SIRType.BuiltinList(sirString), SIRType.BuiltinList(sirString)),
                AnE
              ),
              Const(Constant.List(DefaultUni.String, List()), SIRType.BuiltinList(sirString), AnE),
              SIRType.BuiltinList(sirString),
              AnE
            ),
            SIRType.BuiltinList(sirString),
            AnE
          ),
          SIR.LetFlags.None,
          AnE
        )

        assert(
          compiled ~=~ expected
        )
    }

    test("compile head function") {
        assert(
          compile { (l: scalus.uplc.builtin.BuiltinList[BigInt]) => l.head }
              ~=~ LamAbs(
                Var("l", sirBuiltinList(sirInt), AnE),
                Apply(SIRBuiltins.headList, Var("l", sirBuiltinList(sirInt), AnE), sirInt, AnE),
                List.empty,
                AnE
              )
        )
    }

    test("compile tail function") {
        assert(
          compile { (l: scalus.uplc.builtin.BuiltinList[BigInt]) => l.tail }
              ~=~ LamAbs(
                Var("l", sirBuiltinList(sirInt), AnE),
                Apply(
                  SIRBuiltins.tailList,
                  Var("l", sirBuiltinList(sirInt), AnE),
                  sirBuiltinList(sirInt),
                  AnE
                ),
                List.empty,
                AnE
              )
        )
    }

    test("compile isEmpty function") {
        assert(
          compile { (l: scalus.uplc.builtin.BuiltinList[BigInt]) => l.isEmpty }
              ~=~ LamAbs(
                Var("l", sirBuiltinList(sirInt), AnE),
                Apply(SIRBuiltins.nullList, Var("l", sirBuiltinList(sirInt), AnE), sirBool, AnE),
                List.empty,
                AnE
              )
        )
    }

    test("compile mkNilData") {
        assert(
          compile(Builtins.mkNilData())
              ~=~
                  (Apply(
                    SIRBuiltins.mkNilData,
                    Const(Constant.Unit, sirVoid, AnE),
                    sirBuiltinList(sirData),
                    AnE
                  ))
        )
    }

    test("compile mkNilPairData") {

        assert(
          compile(Builtins.mkNilPairData())
              ~=~
                  (Apply(
                    SIRBuiltins.mkNilPairData,
                    Const(Constant.Unit, sirVoid, AnE),
                    SIRType.BuiltinList(SIRType.BuiltinPair(sirData, sirData)),
                    AnE
                  ))
        )
    }

    test("compile constrData builtins") {
        val nilData =
            Const(Constant.List(DefaultUni.Data, immutable.Nil), SIRType.BuiltinList(sirData), AnE)
        assert(
          compile(
            Builtins.constrData(
              1,
              scalus.uplc.builtin.BuiltinList(Builtins.iData(2))
            )
          ) ~=~ Apply(
            Apply(SIRBuiltins.constrData, sirConst(1), Fun(sirBuiltinList(sirData), sirData), AnE),
            Apply(
              Apply(
                SIRBuiltins.mkCons,
                Apply(SIRBuiltins.iData, sirConst(2), sirData, AnE),
                Fun(sirBuiltinList(sirData), sirBuiltinList(sirData)),
                AnE
              ),
              nilData,
              sirBuiltinList(sirData),
              AnE
            ),
            sirData,
            AnE
          )
        )
    }

    test("compile listData builtins") {
        val nilData =
            Const(Constant.List(DefaultUni.Data, immutable.Nil), SIRType.BuiltinList(sirData), AnE)
        assert(
          compile(
            Builtins.listData(scalus.uplc.builtin.BuiltinList(Builtins.iData(1)))
          )
              ~=~
                  Apply(
                    SIRBuiltins.listData,
                    Apply(
                      Apply(
                        SIRBuiltins.mkCons,
                        Apply(SIRBuiltins.iData, sirConst(1), sirData, AnE),
                        Fun(sirBuiltinList(sirData), sirBuiltinList(sirData)),
                        AnE
                      ),
                      nilData,
                      sirBuiltinList(sirData),
                      AnE
                    ),
                    sirData,
                    AnE
                  )
        )
    }

    test("compile mkPairData builtins") {

        val compiled =
            compile(
              scalus.uplc.builtin.BuiltinPair(Builtins.bData(hex"deadbeef"), Builtins.iData(1))
            )

        val expected =
            Apply(
              Apply(
                SIRBuiltins.mkPairData,
                Apply(SIRBuiltins.bData, sirConst(hex"deadbeef"), sirData, AnE),
                Fun(sirData, sirPair(sirData, sirData)),
                AnE
              ),
              Apply(SIRBuiltins.iData, sirConst(1), sirData, AnE),
              sirPair(sirData, sirData),
              AnE
            )

        assert(
          compiled ~=~ expected
        )

    }

    test("compile mapData builtins") {

        assert(
          compile(
            Builtins.mapData(
              scalus.uplc.builtin.BuiltinList(
                scalus.uplc.builtin.BuiltinPair(Builtins.bData(hex"deadbeef"), Builtins.iData(1))
              )
            )
          ) ~=~ Apply(
            SIRBuiltins.mapData,
            Apply(
              Apply(
                SIRBuiltins.mkCons,
                Apply(
                  Apply(
                    SIRBuiltins.mkPairData,
                    Apply(SIRBuiltins.bData, Const(deadbeef, sirByteString, AnE), sirData, AnE),
                    Fun(sirData, sirPair(sirData, sirData)),
                    AnE
                  ),
                  Apply(SIRBuiltins.iData, sirConst(1), sirData, AnE),
                  sirPair(sirData, sirData),
                  AnE
                ),
                Fun(
                  sirBuiltinList(sirPair(sirData, sirData)),
                  sirBuiltinList(sirPair(sirData, sirData))
                ),
                AnE
              ),
              Const(
                Constant.List(DefaultUni.Pair(DefaultUni.Data, DefaultUni.Data), immutable.Nil),
                SIRType.BuiltinList(sirPair(sirData, sirData)),
                AnE
              ),
              sirBuiltinList(sirPair(sirData, sirData)),
              AnE
            ),
            sirData,
            AnE
          )
        )
    }

    test("compile unConstrData function") {
        assert(
          compile { (d: Data) => Builtins.unConstrData(d) }
              ~=~ LamAbs(
                Var("d", sirData, AnE),
                Apply(
                  SIRBuiltins.unConstrData,
                  Var("d", sirData, AnE),
                  sirPair(sirInt, sirBuiltinList(sirData)),
                  AnE
                ),
                List.empty,
                AnE
              )
        )
    }

    test("compile unListData function") {
        assert(
          compile { (d: Data) => Builtins.unListData(d) }
              ~=~ LamAbs(
                Var("d", sirData, AnE),
                Apply(SIRBuiltins.unListData, Var("d", sirData, AnE), sirBuiltinList(sirData), AnE),
                List.empty,
                AnE
              )
        )
    }

    test("compile unMapData function") {
        assert(
          compile { (d: Data) => Builtins.unMapData(d) }
              ~=~ LamAbs(
                Var("d", sirData, AnE),
                Apply(
                  SIRBuiltins.unMapData,
                  Var("d", sirData, AnE),
                  sirBuiltinList(sirPair(sirData, sirData)),
                  AnE
                ),
                List.empty,
                AnE
              )
        )
    }

    test("compile unBData function") {
        assert(
          compile { (d: Data) => Builtins.unBData(d) }
              ~=~ LamAbs(
                Var("d", sirData, AnE),
                Apply(SIRBuiltins.unBData, Var("d", sirData, AnE), sirByteString, AnE),
                List.empty,
                AnE
              )
        )
    }

    test("compile unIData function") {
        assert(
          compile { (d: Data) => Builtins.unIData(d) } ~=~
              LamAbs(
                Var("d", sirData, AnE),
                Apply(SIRBuiltins.unIData, Var("d", sirData, AnE), sirInt, AnE),
                List.empty,
                AnE
              )
        )
    }

    test("compile chooseData function") {
        assert(
          compile { (d: Data) => Builtins.chooseData[BigInt](d, 1, 2, 3, 4, 5) }
              ~=~ LamAbs(
                Var("d", sirData, AnE),
                ChooseData $ Var("d", sirData, AnE) $ 1 $ 2 $ 3 $ 4 $ 5,
                List.empty,
                AnE
              )
        )
    }

    test("compile equalsData function") {
        assert(
          compile { (d1: Data, d2: Data) => Builtins.equalsData(d1, d2) }
              ~=~ LamAbs(
                Var("d1", sirData, AnE),
                LamAbs(
                  Var("d2", sirData, AnE),
                  SIRBuiltins.equalsData $ Var("d1", sirData, AnE) $ Var("d2", sirData, AnE),
                  List.empty,
                  AnE
                ),
                List.empty,
                AnE
              )
        )
    }

    test("compile serialiseData builtins") {
        assert(
          compile {
              Builtins.serialiseData
          } ~=~ LamAbs(
            Var("d", sirData, AnE),
            Apply(SIRBuiltins.serialiseData, Var("d", sirData, AnE), sirByteString, AnE),
            List.empty,
            AnE
          )
        )
    }

    test("compile BLS12_381_G1 builtins") {
        val p1Var = Var("p1", SIRType.BLS12_381_G1_Element, AnE)
        val p2Var = Var("p2", SIRType.BLS12_381_G1_Element, AnE)
        val bsVar = Var("bs", SIRType.ByteString, AnE)
        val dstVar = Var("dst", SIRType.ByteString, AnE)

        assert(
          compile(Builtins.bls12_381_G1_add) ~=~ LamAbs(
            Var("p1", SIRType.BLS12_381_G1_Element, AnE),
            LamAbs(
              p2Var,
              Apply(
                Apply(
                  SIRBuiltins.bls12_381_G1_add,
                  p1Var,
                  Fun(SIRType.BLS12_381_G1_Element, SIRType.BLS12_381_G1_Element),
                  AnE
                ),
                p2Var,
                SIRType.BLS12_381_G1_Element,
                AnE
              ),
              List.empty,
              AnE
            ),
            List.empty,
            AnE
          )
        )
        assert(
          compile(Builtins.bls12_381_G1_neg)
              ~=~ LamAbs(
                Var("p", SIRType.BLS12_381_G1_Element, AnE),
                Apply(
                  SIRBuiltins.bls12_381_G1_neg,
                  Var("p", SIRType.BLS12_381_G1_Element, AnE),
                  SIRType.BLS12_381_G1_Element,
                  AnE
                ),
                List.empty,
                AnE
              )
        )
        assert(
          compile(Builtins.bls12_381_G1_scalarMul) ~=~ LamAbs(
            Var("s", SIRType.Integer, AnE),
            LamAbs(
              Var("p", SIRType.BLS12_381_G1_Element, AnE),
              Apply(
                Apply(
                  SIRBuiltins.bls12_381_G1_scalarMul,
                  Var("s", SIRType.Integer, AnE),
                  Fun(SIRType.BLS12_381_G1_Element, SIRType.BLS12_381_G1_Element),
                  AnE
                ),
                Var("p", SIRType.BLS12_381_G1_Element, AnE),
                SIRType.BLS12_381_G1_Element,
                AnE
              ),
              List.empty,
              AnE
            ),
            List.empty,
            AnE
          )
        )

        assert(
          compile(Builtins.bls12_381_G1_equal) ~=~ LamAbs(
            p1Var,
            LamAbs(
              p2Var,
              Apply(
                Apply(
                  SIRBuiltins.bls12_381_G1_equal,
                  p1Var,
                  Fun(SIRType.BLS12_381_G1_Element, SIRType.Boolean),
                  AnE
                ),
                p2Var,
                SIRType.Boolean,
                AnE
              ),
              List.empty,
              AnE
            ),
            List.empty,
            AnE
          )
        )

        assert(
          compile(Builtins.bls12_381_G1_hashToGroup) ~=~ LamAbs(
            bsVar,
            LamAbs(
              dstVar,
              Apply(
                Apply(
                  SIRBuiltins.bls12_381_G1_hashToGroup,
                  bsVar,
                  Fun(SIRType.ByteString, SIRType.BLS12_381_G1_Element),
                  AnE
                ),
                dstVar,
                SIRType.BLS12_381_G1_Element,
                AnE
              ),
              List.empty,
              AnE
            ),
            List.empty,
            AnE
          )
        )
        assert(
          compile(Builtins.bls12_381_G1_compress) ~=~ LamAbs(
            Var("p", SIRType.BLS12_381_G1_Element, AnE),
            Apply(
              SIRBuiltins.bls12_381_G1_compress,
              Var("p", SIRType.BLS12_381_G1_Element, AnE),
              SIRType.ByteString,
              AnE
            ),
            List.empty,
            AnE
          )
        )
        assert(
          compile(Builtins.bls12_381_G1_uncompress) ~=~ LamAbs(
            Var("bs", SIRType.ByteString, AnE),
            Apply(
              SIRBuiltins.bls12_381_G1_uncompress,
              Var("bs", SIRType.ByteString, AnE),
              SIRType.BLS12_381_G1_Element,
              AnE
            ),
            List.empty,
            AnE
          )
        )
    }

    test("compile BLS12_381_G2 builtins") {
        val p1Var = Var("p1", SIRType.BLS12_381_G2_Element, AnE)
        val p2Var = Var("p2", SIRType.BLS12_381_G2_Element, AnE)
        val pVar = Var("p", SIRType.BLS12_381_G2_Element, AnE)
        val sBlsVar = Var("s", SIRType.BLS12_381_G2_Element, AnE)
        val sIntVar = Var("s", SIRType.Integer, AnE)
        val bsVar = Var("bs", SIRType.ByteString, AnE)
        val dstVar = Var("dst", SIRType.ByteString, AnE)
        assert(
          compile(Builtins.bls12_381_G2_add) ~=~ LamAbs(
            p1Var,
            LamAbs(
              p2Var,
              Apply(
                Apply(
                  SIRBuiltins.bls12_381_G2_add,
                  p1Var,
                  Fun(SIRType.BLS12_381_G2_Element, SIRType.BLS12_381_G2_Element),
                  AnE
                ),
                p2Var,
                SIRType.BLS12_381_G2_Element,
                AnE
              ),
              List.empty,
              AnE
            ),
            List.empty,
            AnE
          )
        )
        assert(
          compile(Builtins.bls12_381_G2_neg) ~=~ LamAbs(
            pVar,
            Apply(SIRBuiltins.bls12_381_G2_neg, pVar, SIRType.BLS12_381_G2_Element, AnE),
            List.empty,
            AnE
          )
        )
        assert(
          compile(Builtins.bls12_381_G2_scalarMul) ~=~ LamAbs(
            sIntVar,
            LamAbs(
              pVar,
              Apply(
                Apply(
                  SIRBuiltins.bls12_381_G2_scalarMul,
                  sIntVar,
                  Fun(SIRType.BLS12_381_G2_Element, SIRType.BLS12_381_G2_Element),
                  AnE
                ),
                pVar,
                SIRType.BLS12_381_G2_Element,
                AnE
              ),
              List.empty,
              AnE
            ),
            List.empty,
            AnE
          )
        )
        assert(
          compile(Builtins.bls12_381_G2_equal) ~=~ LamAbs(
            p1Var,
            LamAbs(
              p2Var,
              Apply(
                Apply(
                  SIRBuiltins.bls12_381_G2_equal,
                  p1Var,
                  Fun(SIRType.BLS12_381_G2_Element, SIRType.Boolean),
                  AnE
                ),
                p2Var,
                SIRType.Boolean,
                AnE
              ),
              List.empty,
              AnE
            ),
            List.empty,
            AnE
          )
        )
        assert(
          compile(Builtins.bls12_381_G2_hashToGroup) ~=~ LamAbs(
            bsVar,
            LamAbs(
              dstVar,
              Apply(
                Apply(
                  SIRBuiltins.bls12_381_G2_hashToGroup,
                  bsVar,
                  Fun(SIRType.ByteString, SIRType.BLS12_381_G2_Element),
                  AnE
                ),
                dstVar,
                SIRType.BLS12_381_G2_Element,
                AnE
              ),
              List.empty,
              AnE
            ),
            List.empty,
            AnE
          )
        )
        assert(
          compile(Builtins.bls12_381_G2_compress) ~=~ LamAbs(
            pVar,
            Apply(SIRBuiltins.bls12_381_G2_compress, pVar, SIRType.ByteString, AnE),
            List.empty,
            AnE
          )
        )
        assert(
          compile(Builtins.bls12_381_G2_uncompress) ~=~ LamAbs(
            bsVar,
            Apply(SIRBuiltins.bls12_381_G2_uncompress, bsVar, SIRType.BLS12_381_G2_Element, AnE),
            List.empty,
            AnE
          )
        )
    }

    test("compile BLS12_381 pairing operations builtins") {
        val p1BlsG1Var = Var("p1", SIRType.BLS12_381_G1_Element, AnE)
        val p2BlsG2Var = Var("p2", SIRType.BLS12_381_G2_Element, AnE)
        val p1BlsMlVar = Var("p1", SIRType.BLS12_381_MlResult, AnE)
        val p2BlsMlVar = Var("p2", SIRType.BLS12_381_MlResult, AnE)
        val r1Var = Var("r1", SIRType.BLS12_381_MlResult, AnE)
        val r2Var = Var("r2", SIRType.BLS12_381_MlResult, AnE)

        assert(
          compile(Builtins.bls12_381_millerLoop) ~=~ LamAbs(
            p1BlsG1Var,
            LamAbs(
              p2BlsG2Var,
              Apply(
                Apply(
                  SIRBuiltins.bls12_381_millerLoop,
                  p1BlsG1Var,
                  Fun(SIRType.BLS12_381_G2_Element, SIRType.BLS12_381_MlResult),
                  AnE
                ),
                p2BlsG2Var,
                SIRType.BLS12_381_MlResult,
                AnE
              ),
              List.empty,
              AnE
            ),
            List.empty,
            AnE
          )
        )
        assert(
          compile(Builtins.bls12_381_mulMlResult) ~=~ LamAbs(
            r1Var,
            LamAbs(
              r2Var,
              Apply(
                Apply(
                  SIRBuiltins.bls12_381_mulMlResult,
                  r1Var,
                  Fun(SIRType.BLS12_381_MlResult, SIRType.BLS12_381_MlResult),
                  AnE
                ),
                r2Var,
                SIRType.BLS12_381_MlResult,
                AnE
              ),
              List.empty,
              AnE
            ),
            List.empty,
            AnE
          )
        )
        assert(
          compile(Builtins.bls12_381_finalVerify) ~=~ LamAbs(
            p1BlsMlVar,
            LamAbs(
              p2BlsMlVar,
              Apply(
                Apply(
                  SIRBuiltins.bls12_381_finalVerify,
                  p1BlsMlVar,
                  Fun(SIRType.BLS12_381_MlResult, SIRType.Boolean),
                  AnE
                ),
                p2BlsMlVar,
                SIRType.Boolean,
                AnE
              ),
              List.empty,
              AnE
            ),
            List.empty,
            AnE
          )
        )
    }

    test("compile Keccak_256 builtin") {
        val bsVar = Var("bs", SIRType.ByteString, AnE)
        assert(
          compile(Builtins.keccak_256) ~=~ LamAbs(
            bsVar,
            Apply(SIRBuiltins.keccak_256, bsVar, SIRType.ByteString, AnE),
            List.empty,
            AnE
          )
        )
    }

    test("compile Blake2b_224 builtin") {
        val bsVar = Var("bs", SIRType.ByteString, AnE)
        assert(
          compile(Builtins.blake2b_224) ~=~ LamAbs(
            bsVar,
            Apply(SIRBuiltins.blake2b_224, bsVar, SIRType.ByteString, AnE),
            List.empty,
            AnE
          )
        )
    }

    test("compile BigInt ops") {
        assert(compile(-BigInt(-1)) ~=~ (SubtractInteger $ 0 $ -1))
        assert(compile(BigInt(1) + 2) ~=~ (AddInteger $ 1 $ 2))
        assert(compile(BigInt(1) - 2) ~=~ (SubtractInteger $ 1 $ 2))
        assert(compile(BigInt(1) * 2) ~=~ (MultiplyInteger $ 1 $ 2))
        assert(compile(BigInt(1) / 2) ~=~ (DivideInteger $ 1 $ 2))
        assert(compile(BigInt(1) % 2) ~=~ (RemainderInteger $ 1 $ 2))
        assert(compile(BigInt(1) < 2) ~=~ (LessThanInteger $ 1 $ 2))
        assert(compile(BigInt(1) <= 2) ~=~ (LessThanEqualsInteger $ 1 $ 2))
        assert(compile(BigInt(1) > 2) ~=~ (LessThanInteger $ 2 $ 1))
        assert(compile(BigInt(1) >= 2) ~=~ (LessThanEqualsInteger $ 2 $ 1))
        assert(compile(BigInt(1) == BigInt(2)) ~=~ (EqualsInteger $ 1 $ 2))
        assert(compile(BigInt(1) != BigInt(2)) ~=~ Not(EqualsInteger $ 1 $ 2, AnE))
    }

    test("compile Integer builtins") {
        assert(compile(Builtins.addInteger(1, 2)) ~=~ (AddInteger $ 1 $ 2))
        assert(compile(Builtins.subtractInteger(1, 2)) ~=~ (SubtractInteger $ 1 $ 2))
        assert(compile(Builtins.multiplyInteger(1, 2)) ~=~ (MultiplyInteger $ 1 $ 2))
        assert(compile(Builtins.divideInteger(1, 2)) ~=~ (DivideInteger $ 1 $ 2))
        assert(compile(Builtins.modInteger(1, 2)) ~=~ (ModInteger $ 1 $ 2))
        assert(compile(Builtins.quotientInteger(1, 2)) ~=~ (QuotientInteger $ 1 $ 2))
        assert(compile(Builtins.remainderInteger(1, 2)) ~=~ (RemainderInteger $ 1 $ 2))
        assert(compile(Builtins.lessThanInteger(1, 2)) ~=~ (LessThanInteger $ 1 $ 2))
        assert(compile(Builtins.lessThanEqualsInteger(1, 2)) ~=~ (LessThanEqualsInteger $ 1 $ 2))
        assert(compile(Builtins.equalsInteger(1, 2)) ~=~ (EqualsInteger $ 1 $ 2))
    }

    test("compile ByteStrings builtins") {
        assert(
          compile(
            Builtins.appendByteString(hex"dead", hex"beef")
          ) ~=~ (AppendByteString $ hex"dead" $ hex"beef")
        )

        assert(
          compile(
            Builtins.sliceByteString(1, 2, hex"dead")
          ) ~=~ (SliceByteString $ 1 $ 2 $ hex"dead")
        )

        assert(
          compile(
            Builtins.lengthOfByteString(hex"dead")
          ) ~=~ (LengthOfByteString $ hex"dead")
        )

        assert(
          compile(
            Builtins.indexByteString(hex"dead", 1)
          ) ~=~ (IndexByteString $ hex"dead" $ 1)
        )

        assert(
          compile(
            Builtins.equalsByteString(hex"dead", hex"beef")
          ) ~=~ (EqualsByteString $ hex"dead" $ hex"beef")
        )

        assert(
          compile(
            Builtins.lessThanByteString(hex"dead", hex"beef")
          ) ~=~ (LessThanByteString $ hex"dead" $ hex"beef")
        )

        assert(
          compile(
            Builtins.lessThanEqualsByteString(hex"dead", hex"beef")
          ) ~=~ (LessThanEqualsByteString $ hex"dead" $ hex"beef")
        )
    }

    test("compile Crypto builtins") {
        /*
            // Cryptography and hashes
          case Sha2_256
          case Sha3_256
          case Blake2b_256
          case VerifyEd25519Signature // formerly verifySignature
          case VerifyEcdsaSecp256k1Signature
          case VerifySchnorrSecp256k1Signature
         */
        assert(compile(Builtins.sha2_256(hex"dead")) ~=~ (Sha2_256 $ hex"dead"))
        assert(compile(Builtins.sha3_256(hex"dead")) ~=~ (Sha3_256 $ hex"dead"))
        assert(compile(Builtins.blake2b_256(hex"dead")) ~=~ (Blake2b_256 $ hex"dead"))
        assert(
          compile(
            Builtins.verifyEd25519Signature(
              hex"dead",
              hex"beef",
              hex"cafe"
            )
          ) ~=~ (VerifyEd25519Signature $ hex"dead" $ hex"beef" $ hex"cafe")
        )
        assert(
          compile(
            Builtins.verifyEcdsaSecp256k1Signature(
              hex"dead",
              hex"beef",
              hex"cafe"
            )
          ) ~=~ (VerifyEcdsaSecp256k1Signature $ hex"dead" $ hex"beef" $ hex"cafe")
        )
        assert(
          compile(
            Builtins.verifySchnorrSecp256k1Signature(
              hex"dead",
              hex"beef",
              hex"cafe"
            )
          ) ~=~ (VerifySchnorrSecp256k1Signature $ hex"dead" $ hex"beef" $ hex"cafe")
        )
    }

    test("compile utf8 string interpolator") {
        import scalus.uplc.builtin.ByteString.utf8

        // Test simple ASCII string
        assert(
          compile(utf8"hello") ~=~ Const(
            Constant.ByteString(scalus.uplc.builtin.ByteString.fromString("hello")),
            SIRType.ByteString,
            AnE
          )
        )

        // Test with special characters
        assert(
          compile(utf8"Hello, World!") ~=~ Const(
            Constant.ByteString(scalus.uplc.builtin.ByteString.fromString("Hello, World!")),
            SIRType.ByteString,
            AnE
          )
        )

        // Test with Unicode
        assert(
          compile(utf8"Hello, 世界") ~=~ Const(
            Constant.ByteString(scalus.uplc.builtin.ByteString.fromString("Hello, 世界")),
            SIRType.ByteString,
            AnE
          )
        )

        // Test empty string
        assert(
          compile(utf8"") ~=~ Const(
            Constant.ByteString(scalus.uplc.builtin.ByteString.fromString("")),
            SIRType.ByteString,
            AnE
          )
        )
    }

    test("compile String builtins") {
        assert(compile(Builtins.appendString("dead", "beef")) ~=~ (AppendString $ "dead" $ "beef"))
        assert(compile(Builtins.equalsString("dead", "beef")) ~=~ (EqualsString $ "dead" $ "beef"))
        assert(compile(Builtins.encodeUtf8("dead")) ~=~ (EncodeUtf8 $ "dead"))
        assert(compile(Builtins.decodeUtf8(hex"dead")) ~=~ (DecodeUtf8 $ hex"dead"))
    }

    test("compile IfThenElse/ChooseUnit/Trace builtins") {
        assert(
          compile(
            Builtins.ifThenElse(true, BigInt(1), BigInt(2))
          ) ~=~ (DefaultFun.IfThenElse $ true $ 1 $ 2)
        )
        // TODO: check if that is correct
        assert(compile(Builtins.chooseUnit()(true)) ~=~ (DefaultFun.ChooseUnit $ () $ true))
        assert(compile(Builtins.trace("dead")(BigInt(1))) ~=~ (DefaultFun.Trace $ "dead" $ 1))
    }

    test("compile Pair builtins") {
        val fst = Var("fst", sirData, AnE)
        val snd = Var("snd", sirData, AnE)
        val pv = Var("p", SIRType.BuiltinPair(sirData, sirData), AnE)

        assert(
          compile(Builtins.mkPairData) ~=~ LamAbs(
            fst,
            LamAbs(
              snd,
              Apply(
                Apply(
                  SIRBuiltins.mkPairData,
                  fst,
                  Fun(sirData, SIRType.BuiltinPair(sirData, sirData)),
                  AnE
                ),
                snd,
                SIRType.BuiltinPair(sirData, sirData),
                AnE
              ),
              List.empty,
              AnE
            ),
            List.empty,
            AnE
          )
        )

        assert(
          compile { (p: scalus.uplc.builtin.BuiltinPair[Data, Data]) =>
              scalus.uplc.builtin.BuiltinPair(Builtins.sndPair(p), Builtins.fstPair(p))
          } ~=~
              LamAbs(
                pv,
                (SIRBuiltins.mkPairData $ (SIRBuiltins.sndPair $ pv)) $ (SIRBuiltins.fstPair $ pv),
                List.empty,
                AnE
              )
        )

        assert(
          compile { scalus.uplc.builtin.BuiltinPair(BigInt(1), hex"deadbeef") }
              ~=~ Const(
                Constant.Pair(Constant.Integer(1), deadbeef),
                SIRType.BuiltinPair(sirInt, sirByteString),
                AnE
              )
        )

        assert(
          compile { (p: scalus.uplc.builtin.BuiltinPair[Data, Data]) =>
              scalus.uplc.builtin.BuiltinPair(p.snd, p.fst)
          }
              ~=~ LamAbs(
                pv,
                Apply(
                  Apply(
                    SIRBuiltins.mkPairData,
                    Apply(SIRBuiltins.sndPair, pv, sirData, AnE),
                    Fun(sirData, SIRType.BuiltinPair(sirData, sirData)),
                    AnE
                  ),
                  Apply(SIRBuiltins.fstPair, pv, sirData, AnE),
                  SIRType.BuiltinPair(sirData, sirData),
                  AnE
                ),
                List.empty,
                AnE
              )
        )

    }

    test("compile custom Builtins") {
        val platform = new JVMPlatformSpecific {
            override def sha2_256(bs: ByteString): ByteString = hex"deadbeef"
        }
        object CustomBuiltins extends Builtins(using platform)

        val sir = compile(CustomBuiltins.sha2_256(hex"12"))
        // check that SIRCompiler compiles the custom builtin
        assert(
          sir ~=~ Apply(
            SIRBuiltins.sha2_256,
            Const(Constant.ByteString(hex"12"), sirByteString, AnE),
            sirByteString,
            AnE
          )
        )
    }

    test("compile Boolean &&, ||, ! builtins") {
        val compiled = compile {
            val a = true || (throw new Exception("M"))
            !a && false || true
        }
        assert(
          compiled ~=~
              Let(
                List(Binding("a", sirBool, Or(sirConst(true), Error("M", AnE), AnE))),
                Or(
                  And(Not(Var("a", sirBool, AnE), AnE), sirConst(false), AnE),
                  sirConst(true),
                  AnE
                ),
                SIR.LetFlags.None,
                AnE
              )
        )
        // println(compiled.show)
        val evaled = compiled.toUplc().evaluate
        // println(evaled.show)
        assert(evaled == true.asTerm)
    }

    test("compile Boolean equality") {
        val eq = compile { def check(a: Boolean) = a == false; check }
        val ne = compile { def check(a: Boolean) = a != false; check }
        val aVar = Var("a", sirBool, AnE)

        assert(
          eq ~=~ Let(
            List(
              Binding(
                "check",
                SIRType.Fun(sirBool, sirBool),
                LamAbs(
                  aVar,
                  SIR.IfThenElse(
                    aVar,
                    sirConst(false),
                    SIR.IfThenElse(sirConst(false), sirConst(false), sirConst(true), sirBool, AnE),
                    sirBool,
                    AnE
                  ),
                  List.empty,
                  AnE
                )
              )
            ),
            LamAbs(
              aVar,
              Apply(Var("check", Fun(sirBool, sirBool), AnE), aVar, sirBool, AnE),
              List.empty,
              AnE
            ),
            SIR.LetFlags.Recursivity,
            AnE
          )
        )

        assert(
          ne ~=~ Let(
            List(
              Binding(
                "check",
                SIRType.Fun(sirBool, sirBool),
                LamAbs(
                  aVar,
                  SIR.IfThenElse(
                    aVar,
                    SIR.IfThenElse(sirConst(false), sirConst(false), sirConst(true), sirBool, AnE),
                    sirConst(false),
                    sirBool,
                    AnE
                  ),
                  List.empty,
                  AnE
                )
              )
            ),
            LamAbs(
              aVar,
              Apply(Var("check", Fun(sirBool, sirBool), AnE), aVar, sirBool, AnE),
              List.empty,
              AnE
            ),
            SIR.LetFlags.Recursivity,
            AnE
          )
        )

        val eqterm = eq.toUplc()
        val neterm = ne.toUplc()
        import scalus.uplc.TermDSL.given
        assert((eqterm $ true).evaluate == scalus.uplc.Term.Const(asConstant(false)))
        assert((eqterm $ false).evaluate == scalus.uplc.Term.Const(asConstant(true)))
        assert((neterm $ true).evaluate == scalus.uplc.Term.Const(asConstant(true)))
        assert((neterm $ false).evaluate == scalus.uplc.Term.Const(asConstant(false)))
    }

    test("compile ByteString equality") {
        val eqCompiled = compile { def check(a: ByteString, b: ByteString) = a == b; check }

        val eqExpected = Let(
          List(
            Binding(
              "check",
              SIRType.Fun(sirByteString, Fun(sirByteString, sirBool)),
              LamAbs(
                Var("a", sirByteString, AnE),
                LamAbs(
                  Var("b", sirByteString, AnE),
                  Apply(
                    Apply(
                      SIRBuiltins.equalsByteString,
                      Var("a", sirByteString, AnE),
                      Fun(sirByteString, sirBool),
                      AnE
                    ),
                    Var("b", sirByteString, AnE),
                    sirBool,
                    AnE
                  ),
                  List.empty,
                  AnE
                ),
                List.empty,
                AnE
              )
            )
          ),
          LamAbs(
            Var("a", sirByteString, AnE),
            LamAbs(
              Var("b", sirByteString, AnE),
              Apply(
                Apply(
                  Var("check", Fun(sirByteString, Fun(sirByteString, sirBool)), AnE),
                  Var("a", sirByteString, AnE),
                  Fun(sirByteString, sirBool),
                  AnE
                ),
                Var("b", sirByteString, AnE),
                sirBool,
                AnE
              ),
              List.empty,
              AnE
            ),
            List.empty,
            AnE
          ),
          SIR.LetFlags.Recursivity,
          AnE
        )

        assert(
          eqCompiled ~=~ eqExpected
        )

        val ne = compile {
            def check(a: ByteString, b: ByteString) = a != b; check
        }

        assert(
          ne ~=~ Let(
            List(
              Binding(
                "check",
                SIRType.Fun(sirByteString, Fun(sirByteString, sirBool)),
                LamAbs(
                  Var("a", sirByteString, AnE),
                  LamAbs(
                    Var("b", sirByteString, AnE),
                    Not(
                      Apply(
                        Apply(
                          SIRBuiltins.equalsByteString,
                          Var("a", sirByteString, AnE),
                          Fun(sirByteString, sirBool),
                          AnE
                        ),
                        Var("b", sirByteString, AnE),
                        sirBool,
                        AnE
                      ),
                      AnE
                    ),
                    List.empty,
                    AnE
                  ),
                  List.empty,
                  AnE
                )
              )
            ),
            LamAbs(
              Var("a", sirByteString, AnE),
              LamAbs(
                Var("b", sirByteString, AnE),
                Apply(
                  Apply(
                    Var("check", Fun(sirByteString, Fun(sirByteString, sirBool)), AnE),
                    Var("a", sirByteString, AnE),
                    Fun(sirByteString, sirBool),
                    AnE
                  ),
                  Var("b", sirByteString, AnE),
                  sirBool,
                  AnE
                ),
                List.empty,
                AnE
              ),
              List.empty,
              AnE
            ),
            SIR.LetFlags.Recursivity,
            AnE
          )
        )

        val eqterm = eqCompiled.toUplc()
        val neterm = ne.toUplc()
        import scalus.uplc.TermDSL.given
        assert(
          (eqterm $ ByteString.empty $ ByteString.empty).evaluate == scalus.uplc.Term.Const(
            asConstant(true)
          )
        )
        assert(
          (eqterm $ ByteString.empty $ hex"deadbeef").evaluate == scalus.uplc.Term.Const(
            asConstant(false)
          )
        )
        assert(
          (neterm $ ByteString.empty $ ByteString.empty).evaluate == scalus.uplc.Term.Const(
            asConstant(false)
          )
        )
        assert(
          (neterm $ ByteString.empty $ hex"deadbeef").evaluate == scalus.uplc.Term.Const(
            asConstant(true)
          )
        )

    }

    test("compile String equality") {
        val eq = compile { def check(a: String, b: String) = a == b; check }

        val aVar = Var("a", sirString, AnE)
        val bVar = Var("b", sirString, AnE)

        assert(
          eq ~=~ Let(
            List(
              Binding(
                "check",
                SIRType.Fun(sirString, Fun(sirString, sirBool)),
                LamAbs(
                  aVar,
                  LamAbs(
                    bVar,
                    Apply(
                      Apply(SIRBuiltins.equalsString, aVar, Fun(sirString, sirBool), AnE),
                      bVar,
                      sirBool,
                      AnE
                    ),
                    List.empty,
                    AnE
                  ),
                  List.empty,
                  AnE
                )
              )
            ),
            LamAbs(
              aVar,
              LamAbs(
                bVar,
                Apply(
                  Apply(
                    Var("check", Fun(sirString, Fun(sirString, sirBool)), AnE),
                    aVar,
                    Fun(sirString, sirBool),
                    AnE
                  ),
                  bVar,
                  sirBool,
                  AnE
                ),
                List.empty,
                AnE
              ),
              List.empty,
              AnE
            ),
            SIR.LetFlags.Recursivity,
            AnE
          )
        )

        val ne = compile {
            def check(a: String, b: String) = a != b; check
        }

        val neExpected = Let(
          List(
            Binding(
              "check",
              SIRType.Fun(sirString, Fun(sirString, sirBool)),
              LamAbs(
                aVar,
                LamAbs(
                  bVar,
                  Not(
                    Apply(
                      Apply(SIRBuiltins.equalsString, aVar, Fun(sirString, sirBool), AnE),
                      bVar,
                      sirBool,
                      AnE
                    ),
                    AnE
                  ),
                  List.empty,
                  AnE
                ),
                List.empty,
                AnE
              )
            )
          ),
          LamAbs(
            aVar,
            LamAbs(
              bVar,
              Apply(
                Apply(
                  Var("check", Fun(sirString, Fun(sirString, sirBool)), AnE),
                  aVar,
                  Fun(sirString, sirBool),
                  AnE
                ),
                bVar,
                sirBool,
                AnE
              ),
              List.empty,
              AnE
            ),
            List.empty,
            AnE
          ),
          SIR.LetFlags.Recursivity,
          AnE
        )

        assert(
          ne ~=~ neExpected
        )
        val eqterm = eq.toUplc()
        val neterm = ne.toUplc()
        import scalus.uplc.TermDSL.given
        assert(
          (eqterm $ "" $ "").evaluate == scalus.uplc.Term.Const(
            asConstant(true)
          )
        )
        assert(
          (eqterm $ "" $ "deadbeef").evaluate == scalus.uplc.Term.Const(
            asConstant(false)
          )
        )
        assert(
          (neterm $ "" $ "").evaluate == scalus.uplc.Term.Const(
            asConstant(false)
          )
        )
        assert(
          (neterm $ "" $ "deadbeef").evaluate == scalus.uplc.Term.Const(
            asConstant(true)
          )
        )
    }

    test("compile Data equality") {
        val eq = compile { def check(a: Data, b: Data) = a == b; check }
        val ne = compile { def check(a: Data, b: Data) = a != b; check }
        val a = Var("a", sirData, AnE)
        val b = Var("b", sirData, AnE)
        val check = Var("check", Fun(sirData, Fun(sirData, sirBool)), AnE)
        assert(
          eq ~=~ Let(
            List(
              Binding(
                "check",
                SIRType.Fun(sirData, Fun(sirData, sirBool)),
                LamAbs(
                  a,
                  LamAbs(
                    b,
                    Apply(
                      Apply(SIRBuiltins.equalsData, a, Fun(sirData, sirBool), AnE),
                      b,
                      sirBool,
                      AnE
                    ),
                    List.empty,
                    AnE
                  ),
                  List.empty,
                  AnE
                )
              )
            ),
            LamAbs(
              a,
              LamAbs(
                b,
                Apply(Apply(check, a, Fun(sirData, sirBool), AnE), b, sirBool, AnE),
                List.empty,
                AnE
              ),
              List.empty,
              AnE
            ),
            SIR.LetFlags.None,
            AnE
          )
        )
        assert(
          ne ~=~ Let(
            List(
              Binding(
                "check",
                SIRType.Fun(sirData, Fun(sirData, sirBool)),
                LamAbs(
                  a,
                  LamAbs(
                    b,
                    Not(
                      Apply(
                        Apply(SIRBuiltins.equalsData, a, Fun(sirData, sirBool), AnE),
                        b,
                        sirBool,
                        AnE
                      ),
                      AnE
                    ),
                    List.empty,
                    AnE
                  ),
                  List.empty,
                  AnE
                )
              )
            ),
            LamAbs(
              a,
              LamAbs(
                b,
                Apply(Apply(check, a, Fun(sirData, sirBool), AnE), b, sirBool, AnE),
                List.empty,
                AnE
              ),
              List.empty,
              AnE
            ),
            SIR.LetFlags.None,
            AnE
          )
        )
        val eqterm = eq.toUplc()
        val neterm = ne.toUplc()
        import scalus.uplc.builtin.Data.toData
        import scalus.uplc.TermDSL.given

        assert(
          (eqterm $ 1.toData $ 1.toData).evaluate == scalus.uplc.Term.Const(
            asConstant(true)
          )
        )
        assert(
          (eqterm $ "".toData $ "deadbeef".toData).evaluate == scalus.uplc.Term.Const(
            asConstant(false)
          )
        )
        assert(
          (neterm $ 1.toData $ 1.toData).evaluate == scalus.uplc.Term.Const(
            asConstant(false)
          )
        )
        assert(
          (neterm $ "".toData $ "deadbeef".toData).evaluate == scalus.uplc.Term.Const(
            asConstant(true)
          )
        )
    }

}
