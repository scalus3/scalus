package scalus.uplc.transform
import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.uplc.builtin.Builtins.*
import scalus.compiler.sir.TargetLoweringBackend
import scalus.compiler.{compile, Options}
import scalus.uplc.Term.*
import scalus.uplc.eval.Log
import scalus.uplc.{DefaultFun, Term}

class ForcedBuiltinsExtractorTest extends AnyFunSuite {

    // use simple backend, this test is bound to it
    given Options = Options.default.copy(
      targetLoweringBackend = TargetLoweringBackend.ScottEncodingLowering,
      optimizeUplc = false
    )

    test("single-use builtin is NOT extracted") {
        val sir = compile(headList(scalus.uplc.builtin.BuiltinList.empty[Boolean]))
        val uplc = sir.toUplc()
        val logger = Log()
        val optimized =
            ForcedBuiltinsExtractor.apply(uplc, logger = logger.log)
        // Single-use builtins should not be extracted
        assert(logger.getLogs.isEmpty)
        assert(optimized == uplc)
    }

    test("single-use double-force builtin is NOT extracted") {
        val sir = compile(fstPair(scalus.uplc.builtin.BuiltinPair(true, false)))
        val uplc = sir.toUplc()
        val logger = Log()
        val optimized = ForcedBuiltinsExtractor(uplc)
        // Single-use builtins should not be extracted
        assert(optimized == uplc)
    }

    test("extract same builtin used multiple times") {
        val sir = compile {
            val lst1 = scalus.uplc.builtin.BuiltinList.empty[Boolean]
            val lst2 = scalus.uplc.builtin.BuiltinList.empty[Boolean]
            (headList(lst1), headList(lst2))
        }
        val uplc = sir.toUplc()
        val logger = Log()
        val optimized = ForcedBuiltinsExtractor.apply(uplc, logger = logger.log)

        // Verify both uses were replaced with the same variable
        val logs = logger.getLogs
        assert(logs.count(_.contains("__HeadList")) == 2) // Two replacements
        assert(logs.size == 2) // But only 2 log entries total (both for the same builtin)
    }

    test("extract builtins in deterministic sorted order when used multiple times") {
        val sir = compile {
            val lst = scalus.uplc.builtin.BuiltinList.empty[Boolean]
            val pair = scalus.uplc.builtin.BuiltinPair(true, false)
            // Use each builtin twice to trigger extraction
            (
              tailList(lst),
              sndPair(pair),
              headList(lst),
              fstPair(pair),
              tailList(lst),
              sndPair(pair),
              headList(lst),
              fstPair(pair)
            )
        }
        val uplc = sir.toUplc()
        val logger = Log()
        val optimized = ForcedBuiltinsExtractor.apply(uplc, logger = logger.log)

        optimized match {
            case Apply(
                  Apply(
                    Apply(
                      Apply(
                        LamAbs(
                          name1,
                          LamAbs(name2, LamAbs(name3, LamAbs(name4, body, _), _), _),
                          _
                        ),
                        Force(Force(Builtin(DefaultFun.FstPair, _), _), _),
                        _
                      ),
                      Force(Builtin(DefaultFun.HeadList, _), _),
                      _
                    ),
                    Force(Force(Builtin(DefaultFun.SndPair, _), _), _),
                    _
                  ),
                  Force(Builtin(DefaultFun.TailList, _), _),
                  _
                ) =>
                // Verify the order of extracted builtins is sorted
                assert(name1 == "__FstPair")
                assert(name2 == "__HeadList")
                assert(name3 == "__SndPair")
                assert(name4 == "__TailList")
            case _ => fail("Optimized term does not match expected structure")
        }
    }

    test("exceptBuiltins excludes specific builtins from extraction") {
        val sir = compile {
            val lst1 = scalus.uplc.builtin.BuiltinList.empty[Boolean]
            val lst2 = scalus.uplc.builtin.BuiltinList.empty[Boolean]
            // Use each builtin twice to trigger extraction
            (headList(lst1), tailList(lst1), headList(lst2), tailList(lst2))
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
        assert(logs.exists(_.contains("__TailList")))
        assert(!logs.exists(_.contains("__HeadList")))
        assert(logs.size == 2) // TailList extracted at two call sites
    }

}
