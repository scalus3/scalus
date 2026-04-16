package scalus.cardano.n2n

import org.scalatest.funsuite.AnyFunSuite

/** Sanity check that the scalus-n2n cross-project scaffolding compiles and runs on every
  * platform the module is cross-built for. Real behaviour lands from Phase 1 (cancellation
  * primitives) onward; delete this suite once the first real tests replace it.
  */
class ScaffoldSuite extends AnyFunSuite {

    test("scalus-n2n module compiles and scalatest runs") {
        assert(1 + 1 == 2)
    }
}
