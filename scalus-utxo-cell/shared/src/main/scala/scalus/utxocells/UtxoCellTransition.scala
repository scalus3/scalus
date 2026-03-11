package scalus.utxocells

import scalus.cardano.onchain.plutus.prelude.*

/** Result of a UtxoCell transition.
  *
  * @param nextState
  *   Some(s) to continue the cell with new state, None to terminate (burn beacon)
  * @param outputs
  *   additional outputs the transition requires (refunds, payments to other parties)
  */
case class UtxoCellTransition[S](
    nextState: Option[S],
    outputs: List[UtxoCellOutput]
)

object UtxoCellTransition {

    /** Convenience: wrap an Option[S] into a UtxoCellTransition with no extra outputs. */
    def fromOption[S](nextState: Option[S]): UtxoCellTransition[S] =
        UtxoCellTransition(nextState, List.Nil)
}
