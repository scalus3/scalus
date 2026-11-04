package scalus.testing.conformance

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.Transaction
import scalus.testing.conformance.DugiteLedgerVectors.{Case, Summary}
import scalus.testing.conformance.VectorLedgerState.*

import java.nio.file.{Files, Path, Paths}
import scala.collection.mutable
import scala.util.Try

/** Runs the Dugite LEDGER dumps at PV11 and compares the verdict and the new state with the
  * ledger's, spec [CV-21] to [CV-25a]. The dumps record no failure constructor, so the failure
  * lists are tested by `LedgerFailuresImpTest` (D99).
  */
class DugiteLedgerConformanceTest extends AnyFunSuite {

    /** The cases that Scalus cannot run, by case name, each with its cause (spec [CV-24a], D37) and
      * a part of the difference that the cause gives. Each listed case must still differ, and for
      * that cause, so a fix must remove it.
      */
    private val expectedFailures: Map[String, (String, String)] = {
        // spec section 20 Q9, D44: `Coin(value: Long)` cannot hold a withdrawal of a
        // TreasuryWithdrawals above Long.MaxValue. The Imp scenario is "Withdrawals exceeding
        // maxBound Word64 submitted in a single proposal" (conway Imp/EnactSpec.hs:129-137): the
        // tx of test_5222 proposes it, and the state of the next three holds it. The sample holds
        // the next three only; test_5222 is in the full corpus.
        val bigWithdrawal = "a treasury withdrawal above Long.MaxValue (Q9, D44)"
        val overLong = "Expected Long but got OverLong"
        // D116: the tx proposes a hard fork to 13.0. Scalus decodes the major with the bound 12 of
        // the conway master `decodeProtVer`; conway 1.22.1.0 decodes it with `decCBOR` up to
        // `MaxVersion = 13` (Conway/Governance/Procedures.hs:863, binary Version.hs:63,
        // Decoding/Decoder.hs:427-428), and GOV rejects it with ProposalCantFollow
        // (shelley PParams.hs:304-306). Both reject the tx.
        val major13 = "a hard fork to 13.0 does not decode (D116)"
        val aboveMax = "Protocol version 13 exceeds the maximum expected version of 12"
        Map(
          "test_5222" -> (bigWithdrawal, overLong),
          "test_5223" -> (bigWithdrawal, overLong),
          "test_5224" -> (bigWithdrawal, overLong),
          "test_5225" -> (bigWithdrawal, overLong),
          // D116: a hard fork to 11.2609581943. `ProtocolVersion.minor` is an Int; the ledger's
          // `pvMinor` is a Natural (core BaseTypes.hs:209). GOV rejects it with
          // ProposalCantFollow, since the minor is not the current one plus 1.
          "test_5968" -> (
            "a protocol version minor above Int.MaxValue does not decode (D116)",
            "Expected Int but got Long"
          ),
          "test_5970" -> (major13, aboveMax),
          "test_5974" -> (major13, aboveMax)
        )
    }

    /** Fails on a case that differs and is not listed, or on a listed one that matches. */
    private def check(summary: Summary): Unit = {
        val differing = summary.differing
        val unexpected = differing.filterNot((name, _) => expectedFailures.contains(name))
        val fixed = expectedFailures.keySet.filter(summary.names.contains) -- differing.keySet
        // a listed case must differ for its stated cause, not for a new one
        val otherCause = differing.collect {
            case (name, difference)
                if expectedFailures.get(name).exists((_, part) => !difference.contains(part)) =>
                s"  [$name] $difference"
        }
        if otherCause.nonEmpty then
            fail(s"listed cases differ for another cause:\n${otherCause.mkString("\n")}")
        val report = unexpected.toList
            .sortBy(_._1.stripPrefix("test_").toInt)
            .map((name, problem) => s"  [$name] $problem")
        if report.nonEmpty then fail(s"${report.size} case(s) differ:\n${report.mkString("\n")}")
        if fixed.nonEmpty then fail(s"listed in expectedFailures but match the ledger: $fixed")
    }

    private lazy val sample: Path = {
        val resource = getClass.getClassLoader.getResource("ledger-vectors-pv11.tar.gz")
        // D65: a missing sample fails, a skip would pass with no case
        assert(resource != null, "ledger-vectors-pv11.tar.gz is not on the test class path")
        Paths.get(resource.toURI)
    }

    test("spec [CV-25a]: the vendored sample matches the ledger in every case") {
        val summary = DugiteLedgerVectors.run(sample)
        assert(summary.cases == 932)
        assert(summary.rejected == 93)
        assert(summary.distinct == 932)
        check(summary)
        // The first failure Scalus reports for the 90 rejected cases it runs. Pinned, so that a
        // state decoder that makes every tx fail one rule cannot pass the rejected half
        val firstFailures = summary.rejectedBy.values.groupMapReduce(identity)(_ => 1)(_ + _)
        assert(
          firstFailures == Map(
            "CollectErrors" -> 24,
            "ConflictingCommitteeUpdate" -> 1,
            "ConwayCommitteeHasPreviouslyResigned" -> 2,
            "ConwayCommitteeIsUnknown" -> 3,
            "ConwayDRepAlreadyRegistered" -> 1,
            "ConwayDRepNotRegistered" -> 1,
            "ConwayIncompleteWithdrawals" -> 2,
            "ConwayTxRefScriptsSizeTooBig" -> 1,
            "ConwayWdrlNotDelegatedToDRep" -> 2,
            "ConwayWithdrawalsMissingAccounts" -> 2,
            "DelegateeDRepNotRegisteredDELEG" -> 1,
            "DelegateeStakePoolNotRegisteredDELEG" -> 1,
            "DepositIncorrectDELEG" -> 1,
            "DisallowedVoters" -> 3,
            "ExpirationEpochTooSmall" -> 1,
            "GovActionsDoNotExist" -> 1,
            "InvalidGuardrailsScriptHash" -> 2,
            "InvalidPrevGovActionId" -> 4,
            "MalformedProposal" -> 11,
            "MissingVKeyWitnessesUTXOW" -> 1,
            "ProposalCantFollow" -> 1,
            "ProposalDepositIncorrect" -> 1,
            "ProposalReturnAccountDoesNotExist" -> 2,
            "RefundIncorrectDELEG" -> 1,
            "ScriptIntegrityHashMismatch" -> 1,
            "ScriptWitnessNotValidatingUTXOW" -> 2,
            "StakeKeyNotRegisteredDELEG" -> 1,
            "TreasuryWithdrawalReturnAccountsDoNotExist" -> 2,
            "UnelectedCommitteeVoters" -> 4,
            "UnspendableUTxONoDatumHash" -> 2,
            "ValueNotConservedUTxO" -> 3,
            "VotersDoNotExist" -> 2,
            "ZeroTreasuryWithdrawals" -> 3
          ),
          firstFailures.toList.sorted
        )
    }

    test("spec [CV-25a]: the sample covers state changes and the tx features") {
        val counts = mutable.Map.empty[String, Int].withDefaultValue(0)
        def count(name: String, holds: Boolean): Unit = if holds then counts(name) += 1
        DugiteLedgerVectors.foreachCase(sample) { (c: Case) =>
            Try(Transaction.fromCbor(c.sig)).foreach { tx =>
                val body = tx.body.value
                count("plutus", tx.witnessSet.redeemers.nonEmpty)
                count("certificates", body.certificates.toSeq.nonEmpty)
                count("proposals", body.proposalProcedures.toSeq.nonEmpty)
                count("votes", body.votingProcedures.nonEmpty)
                count("withdrawals", body.withdrawals.nonEmpty)
                count("mint", body.mint.nonEmpty)
                count("treasury value", body.currentTreasuryValue.nonEmpty)
                count("donation", body.donation.nonEmpty)
                count("validity interval", body.ttl.nonEmpty || body.validityStartSlot.nonEmpty)
            }
            // The output state differs from the input one, so the comparison of [CV-24] is live
            c.stOut.foreach { stOut =>
                Try {
                    val before = VectorLedgerState.fromCbor(c.st).ruleState
                    LedgerStateComparison
                        .compare(VectorLedgerState.fromCbor(stOut), before)
                        .nonEmpty
                }.foreach(count("state changes", _))
            }
        }
        // No sampled tx mints or sets a validity interval: a coverage gap of the sample
        assert(
          counts.toMap == Map(
            "plutus" -> 60,
            "certificates" -> 498,
            "proposals" -> 104,
            "votes" -> 112,
            "withdrawals" -> 7,
            "treasury value" -> 2,
            "donation" -> 7,
            // every accepted case that decodes: 839 less the three of Q9
            "state changes" -> 836
          ),
          counts.toList.sorted
        )
    }

    test("spec [CV-21]: the full local corpus matches the ledger in every case") {
        val archive = Option(System.getProperty("scalus.dugite.ledgerRules"))
            .orElse(sys.env.get("SCALUS_DUGITE_LEDGER_RULES"))
            .map(Path.of(_))
            .filter(Files.isRegularFile(_))
        assume(
          archive.isDefined,
          "set -Dscalus.dugite.ledgerRules or SCALUS_DUGITE_LEDGER_RULES to ledger-rules.tar.gz"
        )
        val summary = DugiteLedgerVectors.run(archive.get)
        // spec [CV-22a]
        assert(summary.cases == 7446)
        assert(summary.rejected == 93)
        assert(summary.distinct == 6918)
        assert(expectedFailures.keySet.subsetOf(summary.names))
        check(summary)
    }
}
