package scalus.testing.conformance

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.*
import scalus.utils.Hex

import scala.util.Try

/** The vector decoder reads the gov state and the committee state, spec [CV-6]. */
class GovStateDecoderTest extends AnyFunSuite {

    private val prefix = "Conway.Imp.ConwayImpSpec - Version 10."

    private def states(vectorName: String): List[(LedgerState, Option[LedgerState])] =
        CardanoLedgerVectors.loadVector(vectorName).map { (_, vector) =>
            (
              LedgerState.fromCbor(Hex.hexToBytes(vector.oldLedgerState)),
              vector.newLedgerState.map(hex => LedgerState.fromCbor(Hex.hexToBytes(hex)))
            )
        }

    // Vectors whose stored proposal holds a Word64 above Long.MaxValue, which the Long-based
    // Scalus types cannot hold: withdrawal amounts in a TreasuryWithdrawals
    // (EnactSpec.hs:129-137), for `Coin(value: Long)`, and an arbitrary threshold with a
    // denominator of 10^19 in an UpdateCommittee (GovSpec.hs:807-820), for
    // `UnitInterval(numerator: Long, denominator: Long)`.
    private val aboveLong = List(
      "ENACT.Treasury withdrawals.Withdrawals exceeding maxBound Word64 submitted in a single proposal",
      "GOV.Voting.committee member can not vote on UpdateCommittee action"
    )

    test("spec [CV-6]: every state of every vector decodes") {
        val names = CardanoLedgerVectors.vectorNames()
        assert(names.size > 300)
        val failures = for
            name <- names
            if !aboveLong.exists(name.endsWith)
            (path, vector) <- CardanoLedgerVectors.loadVector(name)
            hex <- vector.oldLedgerState :: vector.newLedgerState.toList
            error <- Try(LedgerState.fromCbor(Hex.hexToBytes(hex))).failed.toOption
        yield s"$name/$path: $error"
        assert(failures.isEmpty, failures.take(5).mkString("\n"))
    }

    test("spec [CV-6]: the proposals keep their order") {
        // GovSpec.hs:703-733 submits InfoAction, NoConfidence, InfoAction, TreasuryWithdrawals
        val cases =
            states(prefix + "GOV.Proposals.Consistency.Proposals are stored in the expected order")
        val proposals =
            cases.flatMap((old, next) => next.toList :+ old).map(_.utxos.govState.proposals)
        val all = proposals.maxBy(_.size)
        def kind(action: GovAction): String = action match
            case GovAction.InfoAction             => "Info"
            case _: GovAction.NoConfidence        => "NoConfidence"
            case _: GovAction.TreasuryWithdrawals => "TreasuryWithdrawals"
            case other                            => other.toString
        assert(
          all.map(p => kind(p.action)) == Vector(
            "Info",
            "NoConfidence",
            "Info",
            "TreasuryWithdrawals"
          )
        )
        // the four proposals come from four txs
        assert(all.map(_.id.transactionId).distinct.size == 4)
        assert(all.forall(p => p.expiresAfter > p.proposedIn))
    }

    test("spec [CV-6]: the committee and its authorizations decode") {
        val cases = states(prefix + "GOVCERT.fails for.resigning a nonexistent CC member hotkey")
        val (old, _) = cases.last
        val committee = old.utxos.govState.committee
        assert(committee.exists(_.members.nonEmpty))
        val authorized = old.certs.vstate.committeeState
        assert(authorized.nonEmpty)
        assert(authorized.keySet.subsetOf(committee.get.members.keySet))
    }
}
