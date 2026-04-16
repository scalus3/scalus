package scalus.cardano.n2n.it

import org.scalatest.funsuite.AnyFunSuite

/** Sanity check that the scalus-n2n-it project compiles and scalatest can invoke a test in
  * `sbt it`. Real yaci-devkit and preview-relay suites land in Phase 9; delete this once
  * they do.
  */
class ItScaffoldSuite extends AnyFunSuite {

    test("scalus-n2n-it module compiles and scalatest runs") {
        assert(1 + 1 == 2)
    }
}
