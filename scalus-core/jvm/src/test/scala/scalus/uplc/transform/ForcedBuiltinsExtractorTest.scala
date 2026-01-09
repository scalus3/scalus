package scalus.uplc.transform
import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.builtin.Builtins.*
import scalus.compiler.sir.TargetLoweringBackend
import scalus.compiler.{compile, Options}
import scalus.uplc.Constant.given
import scalus.uplc.Term.*
import scalus.uplc.eval.Log
import scalus.uplc.{Constant, DefaultFun, Term}

class ForcedBuiltinsExtractorTest extends AnyFunSuite {

    // use simple backend, this test is bound to it
    given Options = Options.default.copy(
      targetLoweringBackend = TargetLoweringBackend.ScottEncodingLowering,
      optimizeUplc = false
    )

    test("extract (force (builtin headList))") {
        val sir = compile(headList(builtin.BuiltinList.empty[Boolean]))
        val uplc = sir.toUplc()
        val logger = Log()
        val optimized =
            ForcedBuiltinsExtractor.apply(uplc, logger = logger.log)
        assert(logger.getLogs(0) == "Replacing Forced builtin with Var: __builtin_HeadList")
        assert(
          optimized == (λ(__builtin_HeadList =>
              __builtin_HeadList $ List.empty[Boolean].asTerm
          ) $ (!Builtin(
            DefaultFun.HeadList
          )))
        )
    }

    test("extract (force (force (builtin fstPair)))") {
        val sir = compile(fstPair(builtin.BuiltinPair(true, false)))
        val uplc = sir.toUplc()
        val optimized = ForcedBuiltinsExtractor(uplc)

        val expected = λ("__builtin_FstPair")(
          vr"__builtin_FstPair" $ (true, false).asTerm
        ) $ (!(!Builtin(DefaultFun.FstPair)))

        assert(
          optimized == (λ("__builtin_FstPair")(
            vr"__builtin_FstPair" $ (true, false).asTerm
          ) $ (!(!Builtin(DefaultFun.FstPair))))
        )
    }

    test("extract multiple single-force builtins (headList + tailList)") {
        val sir = compile {
            val lst = builtin.BuiltinList.empty[Boolean]
            (headList(lst), tailList(lst))
        }
        val uplc = sir.toUplc()
        val logger = Log()
        val optimized = ForcedBuiltinsExtractor.apply(uplc, logger = logger.log)

        // Verify both builtins were extracted
        val logs = logger.getLogs
        assert(logs(0).contains("__builtin_HeadList"))
        assert(logs(1).contains("__builtin_TailList"))
    }

    test("extract multiple double-force builtins (fstPair + sndPair)") {
        val sir = compile {
            val pair = builtin.BuiltinPair(true, false)
            (fstPair(pair), sndPair(pair))
        }
        val uplc = sir.toUplc()
        val logger = Log()
        val optimized = ForcedBuiltinsExtractor.apply(uplc, logger = logger.log)

        // Verify both builtins were extracted
        val logs = logger.getLogs
        assert(logs(0).contains("__builtin_FstPair"))
        assert(logs(1).contains("__builtin_SndPair"))
    }

    test("extract same builtin used multiple times") {
        val sir = compile {
            val lst1 = builtin.BuiltinList.empty[Boolean]
            val lst2 = builtin.BuiltinList.empty[Boolean]
            (headList(lst1), headList(lst2))
        }
        val uplc = sir.toUplc()
        val logger = Log()
        val optimized = ForcedBuiltinsExtractor.apply(uplc, logger = logger.log)

        // Verify both uses were replaced with the same variable
        val logs = logger.getLogs
        assert(logs.count(_.contains("__builtin_HeadList")) == 2) // Two replacements
        assert(logs.size == 2) // But only 2 log entries total (both for the same builtin)
    }

    test("extract builtins in deterministic sorted order") {
        val sir = compile {
            val lst = builtin.BuiltinList.empty[Boolean]
            val pair = builtin.BuiltinPair(true, false)
            // Use builtins in non-alphabetical order: tailList, sndPair, headList, fstPair
            // Expected sorted order: FstPair, HeadList, SndPair, TailList
            (tailList(lst), sndPair(pair), headList(lst), fstPair(pair))
        }
        val uplc = sir.toUplc()
        val logger = Log()
        val optimized = ForcedBuiltinsExtractor.apply(uplc, logger = logger.log)

        optimized match {
            case Apply(
                  Apply(
                    Apply(
                      Apply(
                        LamAbs(name1, LamAbs(name2, LamAbs(name3, LamAbs(name4, body)))),
                        Force(Force(Builtin(DefaultFun.FstPair)))
                      ),
                      Force(Builtin(DefaultFun.HeadList))
                    ),
                    Force(Force(Builtin(DefaultFun.SndPair)))
                  ),
                  Force(Builtin(DefaultFun.TailList))
                ) =>
                // Verify the order of extracted builtins is sorted
                assert(name1 == "__builtin_FstPair")
                assert(name2 == "__builtin_HeadList")
                assert(name3 == "__builtin_SndPair")
                assert(name4 == "__builtin_TailList")
            case _ => fail("Optimized term does not match expected structure")
        }
    }

    test("exceptBuiltins excludes specific builtins from extraction") {
        val sir = compile {
            val lst = builtin.BuiltinList.empty[Boolean]
            (headList(lst), tailList(lst))
        }
        val uplc = sir.toUplc()
        val logger = Log()
        // Exclude HeadList from extraction
        val optimized = ForcedBuiltinsExtractor.apply(
          uplc,
          exceptBuiltins = Set(DefaultFun.HeadList),
          logger = logger.log
        )

        // Verify only TailList was extracted, HeadList was excluded
        val logs = logger.getLogs
        assert(logs.exists(_.contains("__builtin_TailList")))
        assert(!logs.exists(_.contains("__builtin_HeadList")))
        assert(logs.size == 1)
    }

}
