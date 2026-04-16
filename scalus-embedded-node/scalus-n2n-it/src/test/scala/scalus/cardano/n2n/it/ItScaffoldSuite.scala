package scalus.cardano.n2n.it

import org.scalatest.funsuite.AnyFunSuite

/** Placeholder suite until Phase 9 lands the real yaci-devkit and preview-relay N2N integration
  * tests. Keeps `sbt it` clean-exiting while earlier phases are in flight.
  */
class ItScaffoldSuite extends AnyFunSuite {

    test("scalus-n2n-it module compiles and scalatest runs") {
        assert(1 + 1 == 2)
    }
}
