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

    /** Floors for each cell's comparison count, recorded from a known-good run.
      *
      * `cell` catches translator and corpus-parsing failures alike, so without a floor the whole
      * oracle could degrade to a one-sample check - or to nothing - and still report green. These
      * are minimums, not equalities: a corpus bump may legitimately raise them.
      */
    private val minCompared = Map(
      "V3 withdrawals" -> 15,
      "V3 votes" -> 17,
      "V3 data" -> 18,
      "V2 withdrawals" -> 19,
      "V2 data" -> 20,
      "V1 withdrawals" -> 39
    )

    private def assertCoverage(name: String, c: Cell): Unit = {
        val floor = minCompared(name)
        assert(
          c.compared >= floor,
          s"$name compared only ${c.compared} instances, expected at least $floor - the oracle " +
              s"has silently lost coverage. ${c.summary}"
        )
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

    test("the corpus slot config maps a slot to the POSIX time the ledger recorded") {
        // A real check, not a restatement of the constants: take the upper bound the ledger wrote
        // into a V3 validRange, invert it through our SlotConfig, and require the slot to come
        // back a whole number in range. A wrong zeroTime or slotLength shifts every interval and
        // fails here, which is what this test exists to catch.
        val bounds = Golden.instances.filter(_.language == Golden.PlutusV3).flatMap { inst =>
            Try {
                val interval = inst.txInfoField(7).asArray // Interval = [0, lower, upper]
                val upper = interval(2).asArray // UpperBound = [0, extended, closure]
                val ext = upper(1).asArray // Extended: Finite is constructor 1
                if ext(0).asLong == 1 then Some(ext(1).asArray.apply(1).asLong) else None
            }.toOption.flatten
        }
        assert(
          bounds.sizeIs >= 5,
          s"expected several finite upper bounds to check against, found ${bounds.size}"
        )
        for posixMillis <- bounds do {
            val slot = (posixMillis - sc.zeroTime) / sc.slotLength
            assert(
              sc.zeroTime + slot * sc.slotLength == posixMillis,
              s"POSIX time $posixMillis is not on a slot boundary under the corpus SlotConfig " +
                  s"(zeroTime=${sc.zeroTime}, slotLength=${sc.slotLength}) - wrong environment"
            )
            assert(slot >= 0 && slot < 1000000, s"implausible slot $slot from $posixMillis")
        }
        info(s"checked ${bounds.size} finite validity-range bounds against the corpus SlotConfig")
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

    test("V3 withdrawals: ordering matches, in ledger order (was defect 1, fixed)") {
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
        assertCoverage("V3 withdrawals", c)
        assert(c.mismatches.isEmpty, s"V3 withdrawal ordering diverged: ${c.summary}")
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
        assertCoverage("V3 votes", c)
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
        assertCoverage("V3 data", c)
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
        assertCoverage("V2 withdrawals", c)
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
        assertCoverage("V2 data", c)
        assert(c.mismatches.isEmpty, s"V2 datum ordering diverged: ${c.summary}")
    }

    // ================= V1 =================

    test("V1 withdrawals: ordering matches, in Plutus order (was defect 7, fixed)") {
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
        assertCoverage("V1 withdrawals", c)
        assert(c.mismatches.isEmpty, s"V1 withdrawal ordering diverged: ${c.summary}")
    }
}
