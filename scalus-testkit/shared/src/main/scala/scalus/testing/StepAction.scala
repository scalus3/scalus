package scalus.testing

import scalus.cardano.ledger.Transaction

/** An action taken during scenario exploration.
  *
  * Used to track the path of actions that led to a violation or successful exploration.
  */
sealed trait StepAction

object StepAction {

    /** Submit a transaction to the emulator. */
    case class Submit(tx: Transaction) extends StepAction

    /** Wait (advance slot) by the given number of slots. */
    case class Wait(slots: Long) extends StepAction {
        require(slots > 0, s"Wait slots must be positive, got: $slots")
    }
}
