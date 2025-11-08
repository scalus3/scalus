package scalus.uplc.transform
import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.Compiler.compile
import scalus.builtin.Builtins.*
import scalus.uplc.Constant
import scalus.uplc.DefaultUni.asConstant
import scalus.uplc.Term.*
import scalus.uplc.TermDSL.given
import scalus.uplc.Constant.given
import scalus.uplc.DefaultFun
import scalus.uplc.DefaultUni.Bool
import scala.language.implicitConversions

class ForcedBuiltinsExtractorTest extends AnyFunSuite {

    // use simple backend, this test is bound to it
    given Compiler.Options = Compiler.Options.default.copy(
      targetLoweringBackend = Compiler.TargetLoweringBackend.ScottEncodingLowering,
      optimizeUplc = false
    )

    test("extract (force (builtin headList))") {
        val sir = compile(headList(builtin.BuiltinList.empty[Boolean]))
        val uplc = sir.toUplc()
        val (optimized, logs) = ForcedBuiltinsExtractor.extractPass(uplc)
        assert(logs == Seq("Replacing Forced builtin with Var: __builtin_HeadList"))
        assert(
          optimized == (lam("__builtin_HeadList")(
            vr"__builtin_HeadList" $ Constant.List(Bool, Nil)
          ) $ (!Builtin(DefaultFun.HeadList)))
        )
    }

    test("extract (force (force (builtin fstPair)))") {
        val sir = compile(fstPair(builtin.BuiltinPair(true, false)))
        val uplc = sir.toUplc()
        val optimized = ForcedBuiltinsExtractor(uplc)

        val expected = lam("__builtin_FstPair")(
          vr"__builtin_FstPair" $ Constant.Pair(asConstant(true), asConstant(false))
        ) $ (!(!Builtin(DefaultFun.FstPair)))

        assert(
          optimized == (lam("__builtin_FstPair")(
            vr"__builtin_FstPair" $ Constant.Pair(asConstant(true), asConstant(false))
          ) $ (!(!Builtin(DefaultFun.FstPair))))
        )
    }

}
