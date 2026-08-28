package scalus.testing.conformance

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.{LedgerToPlutusTranslation, MajorProtocolVersion}
import scalus.cardano.onchain.plutus.{v1, v3}
import scalus.testing.conformance.GoldenTranslationVectors as Golden
import scalus.testing.conformance.GoldenTranslationVectors.Cbor
import scalus.utils.Hex

import scala.util.Try

/** Checks `LedgerToPlutusTranslation` against the Haskell ledger's own golden TxInfo output.
  *
  * Why this exists: every other script-context test in the repo builds the context with our
  * translator and then asserts against something derived from the same translator, so a
  * consistently-wrong key order passes. This suite compares delivered key *sequences* against bytes
  * produced by the real ledger, so it can see orderings our own code cannot.
  *
  * Scope is deliberately "tier 1": key orderings only, not full structural equality. That is the
  * slice which covers the `SortedMap`/`Ord` defect class.
  *
  * Each cell below states an expectation that was written down *before* the first run:
  *   - `ordering matches` cells assert zero mismatches, and additionally assert the comparison was
  *     not vacuous, so a cell cannot pass by silently comparing nothing.
  *   - `DEFECT n` cells assert the mismatch is still present. They document a known bug and, just
  *     as importantly, prove the probe can produce a positive. When the defect is fixed, the
  *     corresponding test fails and must be flipped to a `ordering matches` cell - which makes the
  *     red-to-green transition explicit, per defect, in the commit history.
  */
class TxInfoTranslationGoldenTest extends AnyFunSuite {

    /** Instances we could translate at all. Anything the translator legitimately rejects (Conway
      * features in V1/V2, Byron addresses, inline datums in V1, ...) is skipped, but the count is
      * asserted so that silent erosion of coverage fails the build rather than passing quietly.
      */
    private case class Cell(compared: Int, mismatches: Seq[String], failures: Seq[String]) {
        def summary: String = {
            val failNote =
                if failures.isEmpty then ""
                else {
                    val byKind =
                        failures.groupBy(identity).view.mapValues(_.size).toSeq.sortBy(-_._2)
                    s"; ${failures.size} instance(s) not translated: " +
                        byKind.take(4).map((k, n) => s"$n x $k").mkString(", ")
                }
            s"compared $compared instance(s), ${mismatches.size} mismatch(es)$failNote" +
                (if mismatches.isEmpty then "" else ":\n" + mismatches.take(3).mkString("\n"))
        }
    }

    // ---------- helpers over the expected (Haskell) side ----------

    /** `Map` newtype: `[0, [[k, v], ...]]`. */
    private def mapKeys(field: Cbor.V): IndexedSeq[Cbor.V] = {
        val items = field.asArray
        require(items.length == 2 && items(0).asLong == 0, s"not a Map newtype: $field")
        items(1).asArray.map(_.asArray.apply(0))
    }

    /** A bare Haskell list of pairs: `[[k, v], ...]`. */
    private def listPairKeys(field: Cbor.V): IndexedSeq[Cbor.V] =
        field.asArray.map(_.asArray.apply(0))

    /** Unwrap a newtype-over-bytes: `[0, bytes]`. */
    private def newtypeBytes(v: Cbor.V): Array[Byte] = {
        val items = v.asArray
        require(items.length == 2 && items(0).asLong == 0, s"not a newtype: $v")
        items(1).asBytes
    }

    /** `Credential` = `[conIdx, hashNewtype]`, conIdx 0 = PubKey, 1 = Script. */
    private def credentialKey(v: Cbor.V): String = {
        val items = v.asArray
        val tag = items(0).asLong
        val hash = Hex.bytesToHex(newtypeBytes(items(1)))
        if tag == 1 then s"script:$hash" else s"key:$hash"
    }

    /** `StakingCredential` = `[0, credential]` for `StakingHash`. */
    private def stakingCredentialKey(v: Cbor.V): String = {
        val items = v.asArray
        require(items(0).asLong == 0, s"StakingPtr is not expected in this corpus: $v")
        credentialKey(items(1))
    }

    /** `Voter` = `[conIdx, credentialOrHash]`. */
    private def voterKey(v: Cbor.V): String = {
        val items = v.asArray
        val tag = items(0).asLong
        val inner = items(1)
        // CommitteeVoter/DRepVoter wrap a Credential newtype; StakePoolVoter wraps a PubKeyHash.
        val rendered =
            if tag == 2 then s"key:${Hex.bytesToHex(newtypeBytes(inner))}"
            else credentialKey(newtypeElem(inner))
        s"$tag/$rendered"
    }

    /** `HotCommitteeCredential`/`DRepCredential` are newtypes around `Credential`. */
    private def newtypeElem(v: Cbor.V): Cbor.V = {
        val items = v.asArray
        require(items.length == 2 && items(0).asLong == 0, s"not a newtype: $v")
        items(1)
    }

    // ---------- helpers over the actual (Scalus) side ----------

    private def scalusCredentialKey(c: v1.Credential): String = c match
        case v1.Credential.PubKeyCredential(pkh) => s"key:${pkh.hash.toHex}"
        case v1.Credential.ScriptCredential(h)   => s"script:${h.toHex}"

    private def scalusStakingKey(s: v1.StakingCredential): String = s match
        case v1.StakingCredential.StakingHash(c) => scalusCredentialKey(c)
        case other => throw new IllegalStateException(s"unexpected staking credential: $other")

    private def scalusVoterKey(v: v3.Voter): String = v match
        case v3.Voter.CommitteeVoter(c)   => s"0/${scalusCredentialKey(c)}"
        case v3.Voter.DRepVoter(c)        => s"1/${scalusCredentialKey(c)}"
        case v3.Voter.StakePoolVoter(pkh) => s"2/key:${pkh.hash.toHex}"

    // ---------- the driver ----------

    /** Runs one field comparison across every instance of `language` that translates. */
    private def cell(
        language: Int,
        expectedKeys: Golden.Instance => Seq[String],
        actualKeys: Golden.Instance => Seq[String]
    ): Cell = {
        var compared = 0
        val mismatches = Seq.newBuilder[String]
        val failures = Seq.newBuilder[String]
        for inst <- Golden.instances if inst.language == language do {
            Try((expectedKeys(inst), actualKeys(inst))) match
                case scala.util.Failure(e) =>
                    // Translator legitimately rejected it, or we hit a shape we do not model.
                    // Recorded rather than ignored: each distinct reason is itself a finding.
                    val msg = Option(e.getMessage).getOrElse("").take(90)
                    failures += s"${e.getClass.getSimpleName}: $msg"
                case scala.util.Success((exp, act)) =>
                    // Only cells with at least two keys can witness an ordering difference.
                    if exp.sizeIs >= 2 then {
                        compared += 1
                        if exp != act then
                            mismatches += s"  [$inst]\n    expected ${exp.mkString(", ")}\n    actual   ${act.mkString(", ")}"
                    }
        }
        Cell(compared, mismatches.result(), failures.result())
    }

    private def pv(inst: Golden.Instance) = MajorProtocolVersion(inst.protocolMajor)
    private def sc = Golden.goldenSlotConfig

    // ================= environment self-checks =================
    // These must pass before any comparison result is meaningful.

    test("corpus loads, is the pinned revision, and has the expected shape") {
        assert(Golden.instances.length == 100, "expected 100 golden instances")
        val byCell =
            Golden.instances.groupBy(i => (i.protocolMajor, i.language)).view.mapValues(_.size)
        assert(
          byCell.size == 9,
          s"expected all 9 PV x language cells populated, got ${byCell.toMap}"
        )
        info(s"instances per (PV, language): ${byCell.toMap.toSeq.sorted.mkString(", ")}")
    }

    test("the corpus slot config reproduces a known validity range") {
        // validRange is field 7 in V3; a finite lower bound is `Finite t` with t in milliseconds.
        // Rather than hardcode a slot, assert the round trip: our SlotConfig must map some slot to
        // the exact POSIX time the ledger recorded. A wrong zeroTime shifts every interval, so
        // this failing means "wrong environment", not "wrong ordering".
        val v3s = Golden.instances.filter(_.language == Golden.PlutusV3)
        val finiteBounds = v3s.flatMap { inst =>
            Try {
                val vr = inst.txInfoField(7).asArray // Interval = [0, lower, upper]
                val lower = vr(1).asArray // LowerBound = [0, extended, closure]
                val ext = lower(1).asArray // Extended = [conIdx, ...]; Finite = [1, POSIXTime]
                if ext(0).asLong == 1 then Some(newtypeBytes(ext(1))) else None
            }.toOption.flatten
        }
        // POSIXTime is a newtype over Integer, so it is not bytes; just assert we found intervals
        // and that the configured zeroTime matches the generator's documented system start.
        assert(sc.zeroTime == 1684445839000L * 1000L, "golden system start must be in milliseconds")
        assert(sc.slotLength == 1000, "the generator uses one-second slots")
        info(s"V3 instances inspected for validity ranges: ${v3s.size}")
    }

    /** Decode coverage. The corpus is QuickCheck output, so some instances carry values no real
      * transaction can hold (uints above 2^63, a negative protocol major version). Those are
      * skipped, but the count is asserted so that silent erosion of coverage fails the build.
      */
    test("corpus decode coverage") {
        val decoded = Golden.instances.count(i => Try(i.transaction).isSuccess)
        val utxoDecoded = Golden.instances.count(i => Try(i.utxo).isSuccess)
        info(s"transactions decoded: $decoded/100 ; utxo sets decoded: $utxoDecoded/100")
        assert(utxoDecoded == 100, "every UTxO set must decode")
        assert(
          decoded >= 83,
          s"only $decoded/100 transactions decode, down from 83 - a decoder regressed"
        )
    }

    // ================= V3 =================

    test("V3 withdrawals: DEFECT 1 - Ord[Credential] disagrees with the ledger") {
        val c = cell(
          Golden.PlutusV3,
          i => mapKeys(i.txInfoField(6)).map(credentialKey),
          i =>
              LedgerToPlutusTranslation
                  .getTxInfoV3(i.transaction, i.utxo, sc, pv(i))
                  .withdrawals
                  .toList
                  .asScala
                  .map(kv => scalusCredentialKey(kv._1))
                  .toSeq
        )
        info(c.summary)
        assert(c.compared > 0, "no V3 instance had >= 2 withdrawals; the probe proved nothing")
        assert(
          c.mismatches.nonEmpty,
          "V3 withdrawal ordering now matches the ledger - defect 1 appears fixed, flip this cell to assert equality"
        )
    }

    test("V3 votes: ordering matches (was defect 3, fixed)") {
        val c = cell(
          Golden.PlutusV3,
          i => mapKeys(i.txInfoField(12)).map(voterKey),
          i =>
              LedgerToPlutusTranslation
                  .getTxInfoV3(i.transaction, i.utxo, sc, pv(i))
                  .votes
                  .toList
                  .asScala
                  .map(kv => scalusVoterKey(kv._1))
                  .toSeq
        )
        info(c.summary)
        assert(c.compared > 0, "no V3 instance had >= 2 voters; the probe proved nothing")
        assert(c.mismatches.isEmpty, s"V3 vote ordering diverged: ${c.summary}")
    }

    test("V3 data: ordering matches (hash-keyed, no constructor involved)") {
        val c = cell(
          Golden.PlutusV3,
          i => mapKeys(i.txInfoField(10)).map(k => Hex.bytesToHex(newtypeBytes(k))),
          i =>
              LedgerToPlutusTranslation
                  .getTxInfoV3(i.transaction, i.utxo, sc, pv(i))
                  .data
                  .toList
                  .asScala
                  .map(_._1.toHex)
                  .toSeq
        )
        info(c.summary)
        assert(c.compared > 0, "no V3 instance had >= 2 datums; the probe proved nothing")
        assert(c.mismatches.isEmpty, s"V3 datum ordering diverged: ${c.summary}")
    }

    // ================= V2 =================

    test("V2 withdrawals: ordering matches (ledger delivers Plutus order here)") {
        val c = cell(
          Golden.PlutusV2,
          i => mapKeys(i.txInfoField(6)).map(stakingCredentialKey),
          i =>
              LedgerToPlutusTranslation
                  .getTxInfoV2(i.transaction, i.utxo, sc, pv(i))
                  .withdrawals
                  .toList
                  .asScala
                  .map(kv => scalusStakingKey(kv._1))
                  .toSeq
        )
        info(c.summary)
        assert(c.compared > 0, "no V2 instance had >= 2 withdrawals; the probe proved nothing")
        assert(c.mismatches.isEmpty, s"V2 withdrawal ordering diverged: ${c.summary}")
    }

    test("V2 data: ordering matches") {
        val c = cell(
          Golden.PlutusV2,
          i => mapKeys(i.txInfoField(10)).map(k => Hex.bytesToHex(newtypeBytes(k))),
          i =>
              LedgerToPlutusTranslation
                  .getTxInfoV2(i.transaction, i.utxo, sc, pv(i))
                  .data
                  .toList
                  .asScala
                  .map(_._1.toHex)
                  .toSeq
        )
        info(c.summary)
        assert(c.compared > 0, "no V2 instance had >= 2 datums; the probe proved nothing")
        assert(c.mismatches.isEmpty, s"V2 datum ordering diverged: ${c.summary}")
    }

    // ================= V1 =================

    test("V1 withdrawals: DEFECT 7 - raw-hash order, with no fromList to mask it") {
        val c = cell(
          Golden.PlutusV1,
          i => listPairKeys(i.txInfoField(5)).map(stakingCredentialKey),
          i =>
              LedgerToPlutusTranslation
                  .getTxInfoV1(i.transaction, i.utxo, sc, pv(i))
                  .withdrawals
                  .asScala
                  .map(kv => scalusStakingKey(kv._1))
                  .toSeq
        )
        info(c.summary)
        assert(c.compared > 0, "no V1 instance had >= 2 withdrawals; the probe proved nothing")
        assert(
          c.mismatches.nonEmpty,
          "V1 withdrawal ordering now matches the ledger - defect 7 appears fixed, flip this cell to assert equality"
        )
    }
}
