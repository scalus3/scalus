package scalus.verify.uplcblaster

import org.scalatest.{OneInstancePerTest, ParallelTestExecution}
import org.scalatest.funsuite.AnyFunSuite

/** [[LeanProofsSuite]] without Lean: what it allows of the suite it is mixed into. */
class LeanProofsSuiteTest extends AnyFunSuite {

    test("a suite that runs each test in an instance of its own is not a LeanProofsSuite") {
        // Each instance would start a Lean server that afterAll, run in the first one only, does
        // not end, and would keep what its tests no longer state
        val parallel = intercept[IllegalArgumentException](
          new AnyFunSuite with LeanProofsSuite with ParallelTestExecution
        )
        assert(parallel.getMessage.contains("one instance"), parallel.getMessage)
        assertThrows[IllegalArgumentException](
          new AnyFunSuite with LeanProofsSuite with OneInstancePerTest
        )
        // A suite whose tests share one instance is constructed, and starts no Lean
        val shared = new AnyFunSuite with LeanProofsSuite
        assert(shared.testNames.isEmpty)
    }
}
