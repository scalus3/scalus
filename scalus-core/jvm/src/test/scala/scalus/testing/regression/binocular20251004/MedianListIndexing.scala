package scalus.testing.regression.binocular20251004

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.compiler.{compile, Options, TargetLoweringBackend}
import scalus.prelude.List

/** Regression test for "Cannot upcast Unit -> Int to Int" error
  *
  * Error occurs when compiling a function that:
  *   1. Pattern matches on a List
  *   2. Uses list indexing operator (!!) in one branch
  *   3. Returns a value from the indexed element
  *
  * Original error from binocular project: scalus.compiler.sir.lowering.LoweringException: Cannot
  * upcast Unit -> Int to Int at BitcoinValidator.scala:212 (getMedianTimePast function)
  */
class MedianListIndexing extends AnyFunSuite {

    given Options = Options(
      targetLoweringBackend = TargetLoweringBackend.SirToUplcV3Lowering,
      generateErrorTraces = true,
      optimizeUplc = false,
      debug = false
    )

    test("compile list indexing in pattern match - minimal reproducer") {
        val sir = compile {
            def getMedian(values: List[BigInt], size: BigInt): BigInt =
                values match
                    case List.Nil => BigInt(0)
                    case _ =>
                        val index = size / 2
                        values !! index

            getMedian(List(BigInt(1), BigInt(2), BigInt(3)), BigInt(3))
        }
//        println(s"sir:\n${sir.showHighlighted}")

        // This should compile to UPLC without "Cannot upcast Unit -> Int to Int" error
        sir.toUplcOptimized().plutusV3
    }

    test("compile list indexing without pattern match - control test") {
        val sir = compile {
            val values = List(BigInt(1), BigInt(2), BigInt(3))
            val index = BigInt(1)
            values !! index
        }

        // This should work fine
        sir.toUplcOptimized().plutusV3
    }

    test("compile pattern match without indexing - control test") {
        val sir = compile {
            def getFirst(values: List[BigInt]): BigInt =
                values match
                    case List.Nil           => BigInt(0)
                    case List.Cons(head, _) => head

            getFirst(List(BigInt(1), BigInt(2), BigInt(3)))
        }

        // This should work fine
        sir.toUplcOptimized().plutusV3
    }

    /*
    test("compile original getMedianTimePast pattern - from binocular") {

        val sir = scalus.compiler.compile {
            def getMedianTimePast(timestamps: List[BigInt], size: BigInt): BigInt =
                val UnixEpoch = BigInt(1231006505)
                timestamps match
                    case List.Nil => UnixEpoch
                    case _ =>
                        val index = size / 2
                        // List is sorted, so just get middle element
                        timestamps !! index

            val testTimestamps = List(
              BigInt(1700000000),
              BigInt(1700000600),
              BigInt(1700001200)
            )
            getMedianTimePast(testTimestamps, BigInt(3))
        }

        // This reproduces the exact error from binocular project
        sir.toUplcOptimized().plutusV3
    }
    
     */
}
