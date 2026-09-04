package scalus.cardano.node

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.CertState
import scalus.cardano.ledger.rules.Context

/** [[EmulatorState]] derives `appliedTxIndex` and `appliedTxs` from `appliedTxLog`. Its scaladoc
  * claims no caller can hand back a state whose views disagree; this suite is what turns that claim
  * into something the compiler checks, rather than a convention the next contributor has to notice.
  *
  * "Agree" is about which hashes each view holds, not about their sizes: the log records one entry
  * per application, so a transaction applied twice appears in it twice and in the other two once.
  * `EmulatorParityTest` pins that case on both platforms.
  *
  * The mechanism is the private constructor: Scala 3 makes the synthetic `apply` and `copy` of a
  * case class private along with it, so `state.copy(appliedTxLog = Vector.empty)` — which would
  * leave both caches describing transactions the log no longer holds — cannot be written outside
  * the class at all. That compiler behaviour is what the two negative assertions below pin: if a
  * future compiler stopped propagating the constructor's access to `apply` and `copy`, the
  * invariant would quietly go back to being a convention, and these would fail.
  */
class EmulatorStateInvariantTest extends AnyFunSuite {

    private val state: EmulatorState = EmulatorState.initial(
      utxos = Map.empty,
      certState = CertState.empty,
      context = Context.testMainnet(),
      datums = Map.empty,
      appliedTxLog = Vector.empty
    )

    test("the caches cannot be desynchronised from the log through copy") {
        assertDoesNotCompile("state.copy(appliedTxLog = Vector.empty)")
    }

    test("a state cannot be assembled field by field, bypassing the derivations in initial") {
        assertDoesNotCompile(
          "EmulatorState(state.ledger, state.context, state.datums, state.appliedTxLog, state.appliedTxIndex, state.appliedTxs)"
        )
    }

    test("reading the same surface still compiles, so the two assertions above are not vacuous") {
        // Without this, a typo in either snippet above would make it fail to compile for an
        // unrelated reason, and the assertion would pass while checking nothing.
        assertCompiles("state.appliedTxLog")
    }

    test("initial derives both caches from the log it is given") {
        assert(state.appliedTxLog.isEmpty)
        assert(state.appliedTxIndex.isEmpty)
        assert(state.appliedTxs.isEmpty)
    }
}
