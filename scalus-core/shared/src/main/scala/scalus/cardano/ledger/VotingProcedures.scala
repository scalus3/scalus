package scalus.cardano.ledger

import io.bullet.borer.*

import scala.collection.immutable.SortedMap

/** Represents voting procedures in the Cardano blockchain governance system.
  *
  * VotingProcedures is a map from voters to maps of governance action IDs to voting procedures.
  * It's used to record votes on governance actions.
  *
  * @param procedures
  *   Map from voters to maps of governance action IDs to voting procedures
  */
case class VotingProcedures(procedures: SortedMap[Voter, SortedMap[GovActionId, VotingProcedure]]) {
    require(procedures.nonEmpty, "VotingProcedures cannot be empty")

    /** Checks if there are any voting procedures.
      *
      * @return
      *   true if there are no voting procedures, false otherwise
      */
    def isEmpty: Boolean = procedures.isEmpty || procedures.forall(_._2.isEmpty)
}

object VotingProcedures {
    given Encoder[VotingProcedures] = (w: Writer, value: VotingProcedures) => {
        w.write(value.procedures)
    }

    given Decoder[VotingProcedures] = (r: Reader) => {
        // Votes exist only from protocol version 9, where the ledger's `decodeMapByKey` fails on a
        // repeated voter, and on a repeated action under one voter.
        val votes: Decoder[SortedMap[GovActionId, VotingProcedure]] =
            Decoder(r => SortedMap.from(LedgerMap.readDistinct[GovActionId, VotingProcedure](r)))
        VotingProcedures(
          SortedMap.from(LedgerMap.readDistinct(r)(using summon[Decoder[Voter]], votes))
        )
    }
}
