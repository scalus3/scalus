# CAPE Submissions Automation Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** One-command generation of Scalus submissions for all 8 UPLC-CAPE scenarios, plus a leaderboard comparator and loss-analysis workflow.

**Architecture:** A scenario registry in `scalus-examples` drives a `GenerateSubmissions` `@main` that writes `.uplc` + `metadata.json` + `README.md` into a local UPLC-CAPE clone; `scripts/cape-submit.sh` runs the upstream `cape` CLI (`verify`/`measure`) and a `CompareWithLeaderboard` report. A shared v3.0.0 `cape-tests.json` loader + ScriptContext builder powers budget-pinned harness tests for every scenario.

**Tech Stack:** Scala 3 / sbt (`sbtn`), ujson (already on classpath), cats-parse via `UplcParser`, upstream `cape` CLI via Nix.

**Spec:** `docs/superpowers/specs/2026-08-24-cape-submissions-automation-design.md`

## Global Constraints

- Mainnet track only; mainnet is PV11 (vanRossem). Compile with `Options.releaseUntagged` (PV11 default in Scalus 1.x). No `_preview` variants.
- Submission dirs: `submissions/<scenario>/Scalus_<version>_nau`, version from `scalus.utils.BuildInfo.version`.
- Local UPLC-CAPE clone: `/Users/nau/projects/lantr/UPLC-CAPE` (origin `IntersectMBO/UPLC-CAPE`, at `276738c` when fixtures were inspected). Rebase to `origin/main` before the E2E task.
- Fixture schema: `3.0.0` — sections `data_structures`/`measurements`/`checks`; `builtin_data` values are UPLC-text Data (`Constr 0 [...]`) **or** cardano-cli detailed JSON objects (`{"constructor":0,"fields":[...]}`); both appear upstream (linear_vesting uses text, htlc uses objects).
- ScriptContext patch semantics MUST match upstream `lib/Cape/ScriptContextBuilder.hs` exactly (documented in Task 2) — list order affects budgets.
- Style: project CLAUDE.md Scala 3 rules; run `sbtn scalafmtAll` before every commit; commit directly to master with conventional-commit messages; NEVER add a Claude co-author trailer.
- `git add` every new file.
- Compile the examples module with `sbtn scalusExamplesJVM/Test/compile`; run cape tests with `sbtn 'scalusExamplesJVM/testOnly scalus.examples.cape.*'`. Validator tests JIT from SIR — if results look stale, run `sbtn scalusExamplesJVM/clean` first.
- Budget pins: plain `ExUnits(memory = ..., steps = ...)` literals as in the existing cape tests. If the CI lts (Scala 3.3) job later disagrees, wrap with `ScalusTest`'s `ScalaCompilerVersion.baseline(pre38, since38)` — do not preemptively.

## File Structure

```
scalus-examples/jvm/src/main/scala/scalus/examples/cape/
  CapeMetadata.scala            # MODIFY: full-schema metadata emitter
  CapeScenarios.scala           # NEW: registry of all 8 scenarios
  GenerateSubmissions.scala     # NEW: @main writing submission dirs
  CompareWithLeaderboard.scala  # NEW: @main ranking metrics.json files
  ecd/EcdBase.scala             # NEW: prescribed naive GCD
  htlc/HtlcValidator.scala      # NEW
  htlc/HtlcContract.scala       # NEW
  linearvesting/LinearVestingValidator.scala  # NEW
  linearvesting/LinearVestingContract.scala   # NEW
  ecd/EcdContract.scala         # NEW
scalus-examples/jvm/src/test/scala/scalus/examples/cape/
  CapeTestSuite.scala           # NEW: v3 loader + ScriptContext builder (shared)
  CapeTestSuiteTest.scala       # NEW: unit tests for the loader/builder
  CapeHarness.scala             # NEW: shared "run suite against program" helper
  factorial/FactorialCapeTest.scala      # MODIFY: use loader
  fibonacci/FibonacciCapeTest.scala      # MODIFY: use loader
  twopartyescrow/TwoPartyEscrowCapeTest.scala  # MODIFY: use loader, drop bespoke parser
  ecd/EcdCapeTest.scala                  # NEW
  htlc/HtlcCapeTest.scala                # NEW
  linearvesting/LinearVestingCapeTest.scala  # NEW
scalus-examples/jvm/src/test/resources/cape/<scenario>/cape-tests.json  # 8 dirs, re-vendored
scripts/cape-submit.sh          # NEW: driver
scalus-examples/jvm/src/main/scala/scalus/examples/cape/CAPE-SUBMISSION.md  # MODIFY: runbook rewrite
docs/internal/CAPE_COMPETITIVE_ANALYSIS.md  # NEW (Task 11/12)
```

---

### Task 1: Re-vendor v3.0.0 fixtures for all 8 scenarios

**Files:**
- Create/overwrite: `scalus-examples/jvm/src/test/resources/cape/{factorial,factorial_naive_recursion,fibonacci,fibonacci_naive_recursion,ecd,htlc,linear_vesting,two_party_escrow}/cape-tests.json`
- Create: `scalus-examples/jvm/src/test/resources/cape/README.md`

**Interfaces:**
- Produces: test resources at `/cape/<scenario>/cape-tests.json`, schema 3.0.0, used by every harness task.

Note the old layout had only 3 fixture dirs (`factorial`, `fibonacci`, `two_party_escrow`) shared by both naive/open variants; the new layout has one dir per CAPE scenario id, so the naive-variant fixtures are separate files.

- [ ] **Step 1: Update the clone and copy fixtures**

```bash
cd /Users/nau/projects/lantr/UPLC-CAPE && git checkout main -q && git pull --ff-only && git log -1 --format='%H'
cd /Users/nau/projects/lantr/scalus
for s in factorial factorial_naive_recursion fibonacci fibonacci_naive_recursion ecd htlc linear_vesting two_party_escrow; do
  mkdir -p scalus-examples/jvm/src/test/resources/cape/$s
  cp /Users/nau/projects/lantr/UPLC-CAPE/scenarios/$s/cape-tests.json scalus-examples/jvm/src/test/resources/cape/$s/
done
```

- [ ] **Step 2: Write provenance README**

`scalus-examples/jvm/src/test/resources/cape/README.md`:

```markdown
# CAPE test fixtures

Vendored from https://github.com/IntersectMBO/UPLC-CAPE `scenarios/<scenario>/cape-tests.json`
at commit <FULL-HASH-FROM-STEP-1> (schema 3.0.0).

To refresh: pull the UPLC-CAPE clone and re-copy each file, then update this hash.
```

- [ ] **Step 3: Verify all 8 files parse and are v3**

```bash
for f in scalus-examples/jvm/src/test/resources/cape/*/cape-tests.json; do python3 -c "import json,sys; d=json.load(open('$f')); assert d['version']=='3.0.0', '$f'" ; done && echo OK
```

Expected: `OK`

- [ ] **Step 4: Commit**

```bash
git add scalus-examples/jvm/src/test/resources/cape
git commit -m "test: vendor UPLC-CAPE v3.0.0 cape-tests.json fixtures for all 8 scenarios"
```

---

### Task 2: Shared CapeTestSuite loader and ScriptContext builder

**Files:**
- Create: `scalus-examples/jvm/src/test/scala/scalus/examples/cape/CapeTestSuite.scala`
- Test: `scalus-examples/jvm/src/test/scala/scalus/examples/cape/CapeTestSuiteTest.scala`

**Interfaces:**
- Produces (used by all harness tasks):
  - `case class CapeCase(name: String, description: String, inputs: List[Data | Term], expectError: Boolean, expectedTerm: Option[Term], isMeasurement: Boolean)` — modeled as below (no union type; see code).
  - `object CapeTestSuite { def load(resourcePath: String): CapeTestSuite }`
  - `class CapeTestSuite { def cases: Seq[CapeCase] }`
- Consumes: fixtures from Task 1; `scalus.uplc.UplcParser` (object member `dataTerm: P[Data]`, class member `term: P[Term]`); `Data.fromJson` (`scalus-core/.../uplc/builtin/DataApi.scala:259`, cardano-cli detailed schema).

**Upstream semantics to reproduce exactly** (from `lib/Cape/ScriptContextBuilder.hs` and `lib/Cape/Tests.hs`, verified 2026-08-24):

1. Baseline `"spending"`: empty inputs/outputs/signatories; `validRange = Interval.always`; `redeemer = ().toData` (i.e. `Constr 0 []`); `scriptInfo = SpendingScript(TxOutRef(TxId(#0000..00 (32 bytes)), 0), Some(().toData))` — note the default datum is `Some(Constr 0 [])`, NOT `None`; `txId = #0000..00`.
2. `add_signature` / `add_input_utxo` / `add_output_utxo` **prepend** (cons) to their lists. Final list order = reverse patch order. Do NOT append.
3. `add_input_utxo`: address is `ScriptCredential(#1111111111111111111111111111111111111111111111111111111111)` when `is_own_input`, else `PubKeyCredential(PubKeyHash(ByteString.empty))` — the **empty** bytestring, not 32 zero bytes. Optional `datum` → inline `OutputDatum.OutputDatum(d)`, absent → `NoOutputDatum`. When `is_own_input` is true, also set the SpendingScript's `TxOutRef` to this input's ref (datum untouched). There is NO auto-synthesized own input.
4. `set_valid_range`: `from_time`/`to_time` both optional integers; present → `Finite(t)` with `isInclusive = true`; absent → `NegInf`/`PosInf` with `isInclusive = true`.
5. `value` spec: `{"lovelace": N}` plus optional `"assets": [{"currency_symbol": ..., "token_name": ..., "quantity": N}]`; `currency_symbol`/`token_name` may be `@refs` to `builtin_data` bytestrings. Build as lovelace value + each asset.
6. `set_script_datum`: replaces the SpendingScript datum with `Some(d)`.
7. `remove_signature`: filter out the given pkh. `remove_output_utxo`: delete by index. `set_redeemer`: replace.
8. `builtin_data` value: JSON string → parse with `UplcParser.dataTerm.parseAll(s.trim)`; JSON object → `Data.fromJson(ujson.write(obj))`.
9. Baselines can be `"spending"` or `"@ref"` to a `script_context` data structure (recursive); a test's input `script_context` applies its own patches on top.
10. `inputs` array may hold multiple entries (ecd has 2): types `uplc` (a UPLC term string like `(con integer 12)`), `builtin_data`, `script_context`. `expected` is `{"type":"error"}` or `{"type":"value","content":"(con ...)"}`.

- [ ] **Step 1: Write failing unit tests**

`CapeTestSuiteTest.scala` (package `scalus.examples.cape`) — tests written against a small inline JSON, not the vendored fixtures:

```scala
package scalus.examples.cape

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.onchain.plutus.v3.*
import scalus.cardano.onchain.plutus.v1.{IntervalBoundType}
import scalus.uplc.builtin.{ByteString, Data}
import scalus.uplc.builtin.ByteString.*
import scalus.uplc.builtin.Data.{toData, fromData}

class CapeTestSuiteTest extends AnyFunSuite {

    private val suiteJson = """{
      "version": "3.0.0",
      "description": "test",
      "data_structures": {
        "pk": {"type": "builtin_data", "value": "B #aaaa"},
        "datum_text": {"type": "builtin_data", "value": "Constr 0 [Constr 1 [], I 1000]"},
        "datum_json": {"type": "builtin_data",
          "value": {"constructor": 0, "fields": [{"constructor": 1, "fields": []}, {"int": 1000}]}}
      },
      "measurements": [
        {"name": "m1", "description": "d", "expected": {"type": "value", "content": "(con unit ())"},
         "inputs": [{"type": "script_context", "script_context": {"baseline": "spending", "patches": [
            {"op": "set_redeemer", "redeemer": "I 1"},
            {"op": "set_script_datum", "datum": "@datum_text"},
            {"op": "add_signature", "pubkey_hash": "@pk"},
            {"op": "add_signature", "pubkey_hash": "#bbbb"},
            {"op": "set_valid_range", "from_time": 11},
            {"op": "add_input_utxo", "utxo_ref": "3333333333333333333333333333333333333333333333333333333333333333:0",
             "value": {"lovelace": 2000000, "assets": [{"currency_symbol": "#dddd", "token_name": "#76657374", "quantity": 1000}]},
             "is_own_input": true, "datum": "@datum_text"},
            {"op": "add_input_utxo", "utxo_ref": "4444444444444444444444444444444444444444444444444444444444444444:1",
             "value": {"lovelace": 5}, "is_own_input": false}
         ]}}]}
      ],
      "checks": [
        {"name": "c1", "description": "d", "expected": {"type": "error"},
         "inputs": [{"type": "builtin_data", "value": "I 42"}]}
      ]
    }"""

    private val suite = CapeTestSuite.fromString(suiteJson)

    test("both Data formats parse to the same value") {
        assert(suite.dataStructure("datum_text") == suite.dataStructure("datum_json"))
    }

    test("measurements and checks are loaded with flags") {
        assert(suite.cases.map(_.name) == Seq("m1", "c1"))
        assert(suite.cases.head.isMeasurement && !suite.cases.last.isMeasurement)
        assert(suite.cases.last.expectError)
    }

    test("script context matches upstream builder semantics") {
        val sc = suite.cases.head.inputs.head.asInstanceOf[CapeInput.Ctx].data.to[ScriptContext]
        // redeemer replaced
        assert(sc.redeemer == Data.I(1))
        // datum set via set_script_datum
        assert(sc.scriptInfo == ScriptInfo.SpendingScript(
          TxOutRef(TxId(hex"3333333333333333333333333333333333333333333333333333333333333333"), 0),
          Option.Some(suite.dataStructure("datum_text"))))
        // signatures PREPENDED: last-added first
        assert(fromData[List[PubKeyHash]](sc.txInfo.signatories.toData).toList
            .map(_.hash) == scala.List(hex"bbbb", hex"aaaa"))
        // inputs PREPENDED; non-own input first, own second
        val ins = sc.txInfo.inputs.toList
        assert(ins.size == 2)
        assert(ins.head.resolved.address.credential ==
            Credential.PubKeyCredential(PubKeyHash(ByteString.empty)))
        assert(ins(1).resolved.address.credential == Credential.ScriptCredential(
          hex"1111111111111111111111111111111111111111111111111111111111"))
        // own input value carries the asset
        assert(ins(1).resolved.value.quantityOf(hex"dddd", hex"76657374") == BigInt(1000))
        // valid range: [11, +inf), both inclusive
        assert(sc.txInfo.validRange.from.boundType == IntervalBoundType.Finite(BigInt(11)))
        assert(sc.txInfo.validRange.from.isInclusive)
        assert(sc.txInfo.validRange.to.boundType == IntervalBoundType.PosInf)
    }

    test("spending baseline defaults match upstream") {
        val minimal = """{"version":"3.0.0","description":"","data_structures":{},
          "measurements":[{"name":"m","description":"","expected":{"type":"error"},
            "inputs":[{"type":"script_context","script_context":{"baseline":"spending","patches":[]}}]}],
          "checks":[]}"""
        val sc = CapeTestSuite.fromString(minimal)
            .cases.head.inputs.head.asInstanceOf[CapeInput.Ctx].data.to[ScriptContext]
        assert(sc.redeemer == ().toData)
        assert(sc.scriptInfo == ScriptInfo.SpendingScript(
          TxOutRef(TxId(hex"0000000000000000000000000000000000000000000000000000000000000000"), 0),
          Option.Some(().toData)))
        assert(sc.txInfo.inputs.isEmpty && sc.txInfo.outputs.isEmpty && sc.txInfo.signatories.isEmpty)
    }
}
```

(Adjust prelude-`List`-to-Scala conversions to what compiles — `toList` or a fold — but keep the asserted ORDER and VALUES exactly.)

- [ ] **Step 2: Run to verify failure**

Run: `sbtn 'scalusExamplesJVM/testOnly scalus.examples.cape.CapeTestSuiteTest'`
Expected: compile FAILURE — `CapeTestSuite` not found.

- [ ] **Step 3: Implement `CapeTestSuite.scala`**

Package `scalus.examples.cape`, test tree. Skeleton (fill in per the semantics list above):

```scala
package scalus.examples.cape

import scalus.cardano.onchain.plutus.v2.OutputDatum
import scalus.cardano.onchain.plutus.v3.*
import scalus.cardano.onchain.plutus.v1.{IntervalBound, IntervalBoundType}
import scalus.cardano.onchain.plutus.prelude.{List as SList, Option as SOption}
import scalus.uplc.{Term, UplcParser}
import scalus.uplc.builtin.{ByteString, Data}
import scalus.uplc.builtin.Data.toData

/** One resolved input of a CAPE test case. */
enum CapeInput {
    case Uplc(term: Term)        // "(con integer 12)"
    case Dat(data: Data)         // builtin_data
    case Ctx(data: Data)         // built ScriptContext, as Data
}

case class CapeCase(
    name: String,
    description: String,
    inputs: Seq[CapeInput],
    expectError: Boolean,
    expectedTerm: scala.Option[Term], // parsed "content" when type=value
    isMeasurement: Boolean
)

class CapeTestSuite(json: ujson.Value) {
    private val ds: Map[String, ujson.Value] = json("data_structures").obj.toMap
    private val termParser = UplcParser()

    def dataStructure(name: String): Data = parseDataValue(ds(name)("value"))

    /** builtin_data value: UPLC-text string or detailed-schema JSON object. */
    def parseDataValue(v: ujson.Value): Data = v match
        case ujson.Str(s) => UplcParser.dataTerm.parseAll(s.trim) match
            case Right(d)  => d
            case Left(err) => throw RuntimeException(s"Bad Data '$s': $err")
        case obj: ujson.Obj => Data.fromJson(ujson.write(obj))
        case other          => throw RuntimeException(s"Unexpected builtin_data: $other")

    private def resolveData(v: ujson.Value): Data = v match
        case ujson.Str(s) if s.startsWith("@") =>
            val r = ds(s.drop(1)); require(r("type").str == "builtin_data"); parseDataValue(r("value"))
        case other => parseDataValue(other)

    private def resolveBytes(v: ujson.Value): ByteString = resolveData(v) match
        case Data.B(bs) => bs
        case d          => throw RuntimeException(s"Expected bytes, got $d")

    private def parseTerm(s: String): Term = termParser.term.parseAll(s.trim) match
        case Right(t)  => t
        case Left(err) => throw RuntimeException(s"Bad UPLC term '$s': $err")

    val cases: Seq[CapeCase] =
        def mk(v: ujson.Value, meas: Boolean) = CapeCase(
          name = v("name").str,
          description = v("description").str,
          inputs = v("inputs").arr.toSeq.map(parseInput),
          expectError = v("expected")("type").str == "error",
          expectedTerm = v("expected").obj.get("content").map(c => parseTerm(c.str)),
          isMeasurement = meas
        )
        json("measurements").arr.toSeq.map(mk(_, true))
            ++ json.obj.get("checks").map(_.arr.toSeq).getOrElse(Nil).map(mk(_, false))

    private def parseInput(in: ujson.Value): CapeInput = in("type").str match
        case "uplc"           => CapeInput.Uplc(parseTerm(in("value").str))
        case "builtin_data"   => CapeInput.Dat(resolveData(in("value")))
        case "script_context" => CapeInput.Ctx(buildContext(in("script_context")).toData)

    // ---- ScriptContext builder: MUST mirror upstream ScriptContextBuilder.hs ----

    private val scriptHash = ByteString.fromHex("1111111111111111111111111111111111111111111111111111111111")
    private val zeroTxId = TxId(ByteString.fromHex("0" * 64))

    private case class Builder(
        redeemer: Data = ().toData,
        signatories: scala.List[PubKeyHash] = Nil, // stored in FINAL order (already reversed)
        validRange: Interval = Interval.always,
        inputs: scala.List[TxInInfo] = Nil,
        outputs: scala.List[TxOut] = Nil,
        scriptDatum: scala.Option[Data] = scala.Some(().toData),
        ownRef: TxOutRef = TxOutRef(zeroTxId, 0)
    )

    private def buildContext(scJson: ujson.Value): ScriptContext = {
        val b = builderOf(scJson)
        ScriptContext(
          txInfo = TxInfo(
            inputs = SList.from(b.inputs),
            outputs = SList.from(b.outputs),
            validRange = b.validRange,
            signatories = SList.from(b.signatories),
            id = zeroTxId
          ),
          redeemer = b.redeemer,
          scriptInfo = ScriptInfo.SpendingScript(
            b.ownRef,
            b.scriptDatum.fold(SOption.None)(SOption.Some(_))
          )
        )
    }

    private def builderOf(scJson: ujson.Value): Builder = {
        val base = scJson("baseline") match
            case ujson.Str("spending")             => Builder()
            case ujson.Str(s) if s.startsWith("@") =>
                builderOf(ds(s.drop(1))("script_context"))
            case other => throw RuntimeException(s"Unknown baseline: $other")
        scJson("patches").arr.foldLeft(base)(applyPatch)
    }

    private def applyPatch(b: Builder, p: ujson.Value): Builder = p("op").str match
        case "set_redeemer"  => b.copy(redeemer = resolveData(p("redeemer")))
        case "add_signature" => // PREPEND (upstream cons)
            b.copy(signatories = PubKeyHash(resolveBytes(p("pubkey_hash"))) :: b.signatories)
        case "remove_signature" =>
            val pkh = resolveBytes(p("pubkey_hash"))
            b.copy(signatories = b.signatories.filterNot(_.hash == pkh))
        case "set_valid_range" =>
            def bound(key: String, inf: IntervalBoundType) =
                p.obj.get(key).map(t => IntervalBound(IntervalBoundType.Finite(BigInt(t.num.toLong)), true))
                    .getOrElse(IntervalBound(inf, true))
            b.copy(validRange = Interval(
              bound("from_time", IntervalBoundType.NegInf),
              bound("to_time", IntervalBoundType.PosInf)))
        case "add_input_utxo" =>
            val Array(h, ix) = p("utxo_ref").str.split(':')
            val ref = TxOutRef(TxId(ByteString.fromHex(h)), BigInt(ix.toInt))
            val own = p("is_own_input").bool
            val addr =
                if own then Address(Credential.ScriptCredential(scriptHash), SOption.None)
                else Address(Credential.PubKeyCredential(PubKeyHash(ByteString.empty)), SOption.None)
            val txIn = TxInInfo(ref, TxOut(addr, parseValue(p("value")), datumOf(p), SOption.None))
            val b2 = b.copy(inputs = txIn :: b.inputs) // PREPEND
            if own then b2.copy(ownRef = ref) else b2
        case "add_output_utxo" =>
            val addr = p("address")("type").str match
                case "script" => Address(
                  Credential.ScriptCredential(ByteString.fromHex(p("address")("script_hash").str)), SOption.None)
                case "pubkey" => Address(
                  Credential.PubKeyCredential(PubKeyHash(resolveBytes(p("address")("pubkey_hash")))), SOption.None)
            b.copy(outputs = TxOut(addr, parseValue(p("value")), datumOf(p), SOption.None) :: b.outputs) // PREPEND
        case "remove_output_utxo" =>
            val i = p("index").num.toInt
            b.copy(outputs = b.outputs.patch(i, Nil, 1))
        case "set_script_datum" => b.copy(scriptDatum = scala.Some(resolveData(p("datum"))))
        case other              => throw RuntimeException(s"Unknown patch op: $other")

    private def datumOf(p: ujson.Value): OutputDatum =
        p.obj.get("datum").map(d => OutputDatum.OutputDatum(resolveData(d))).getOrElse(OutputDatum.NoOutputDatum)

    private def parseValue(v: ujson.Value): Value = {
        val base = Value.lovelace(BigInt(v("lovelace").num.toLong))
        v.obj.get("assets").map(_.arr.toSeq).getOrElse(Nil).foldLeft(base) { (acc, a) =>
            acc + Value(resolveBytes(a("currency_symbol")), resolveBytes(a("token_name")),
              BigInt(a("quantity").num.toLong))
        }
    }
}

object CapeTestSuite {
    def fromString(s: String): CapeTestSuite = new CapeTestSuite(ujson.read(s))
    def load(resourcePath: String): CapeTestSuite = {
        val stream = getClass.getResourceAsStream(resourcePath)
        assert(stream != null, s"$resourcePath not found in test resources")
        new CapeTestSuite(ujson.read(stream))
    }
}
```

Adapt small API mismatches (e.g. `SList.from`, `Value.apply(cs, tn, amount)`, `Interval.always`, `TxOut` field names) to what actually compiles — the SEMANTICS in the comments are the contract. Also add the shared runner in the same file or `CapeHarness.scala`:

```scala
/** Applies a program to a case's inputs and asserts the expected outcome. Returns the budget on success. */
object CapeHarness {
    import scalus.*
    import scalus.uplc.Program
    import scalus.uplc.Term.asTerm
    import scalus.uplc.eval.Result

    def run(program: Program, c: CapeCase): scala.Option[scalus.uplc.eval.ExBudget] = {
        val applied = c.inputs.foldLeft(program) { (p, in) =>
            in match
                case CapeInput.Uplc(t) => p $ t
                case CapeInput.Dat(d)  => p $ d
                case CapeInput.Ctx(d)  => p $ d
        }
        val result = applied.evaluateDebug
        if c.expectError then
            assert(result.isFailure, s"${c.name}: expected error but succeeded")
            scala.None
        else
            result match
                case Result.Success(term, budget, _, _) =>
                    c.expectedTerm.foreach(exp => assert(term == exp, s"${c.name}: expected $exp, got $term"))
                    scala.Some(budget)
                case Result.Failure(err, _, _, logs) =>
                    org.scalatest.Assertions.fail(s"${c.name}: expected success, got $err; logs: ${logs.mkString(", ")}")
    }
}
```

(`applied.evaluateDebug` — use `applied.term.evaluateDebug` if `Program` lacks the method, matching the existing factorial harness.)

- [ ] **Step 4: Run tests to verify they pass**

Run: `sbtn 'scalusExamplesJVM/testOnly scalus.examples.cape.CapeTestSuiteTest'`
Expected: PASS (4 tests).

- [ ] **Step 5: Format and commit**

```bash
sbtn scalafmtAll
git add scalus-examples/jvm/src/test/scala/scalus/examples/cape/CapeTestSuite.scala \
        scalus-examples/jvm/src/test/scala/scalus/examples/cape/CapeTestSuiteTest.scala
git commit -m "test: shared CAPE v3 cape-tests.json loader and upstream-exact ScriptContext builder"
```

---

### Task 3: Migrate factorial and fibonacci harnesses to the loader

**Files:**
- Modify: `scalus-examples/jvm/src/test/scala/scalus/examples/cape/factorial/FactorialCapeTest.scala`
- Modify: `scalus-examples/jvm/src/test/scala/scalus/examples/cape/fibonacci/FibonacciCapeTest.scala`

**Interfaces:**
- Consumes: `CapeTestSuite.load`, `CapeHarness.run`, `CapeCase` (Task 2); fixtures (Task 1).
- Produces: green budget-pinned suites for 4 synthetic scenarios.

The naive and open variants now have SEPARATE fixture files: `/cape/factorial_naive_recursion/cape-tests.json` runs against `baseProgram`, `/cape/factorial/cape-tests.json` against `openProgram` (same split for fibonacci).

- [ ] **Step 1: Rewrite `FactorialCapeTest`** to iterate `CapeTestSuite.load("/cape/factorial_naive_recursion/cape-tests.json").cases` against `FactorialContract.baseProgram` and `.../cape/factorial/...` against `openProgram`, using `CapeHarness.run`. Keep the script-size tests and the existing `expectedBaseBudgets`/`expectedOpenBudgets`/fee maps and their assertion pattern (`map.get(name).foreach(assert...)`) — print `s"$name: $budget"` on success so new pins can be captured.
- [ ] **Step 2: Run** `sbtn 'scalusExamplesJVM/testOnly scalus.examples.cape.factorial.*'`. If budget pins mismatch (input sets may differ from v1 fixtures), update the pin literals to the printed values. Expected: PASS.
- [ ] **Step 3: Same rewrite for `FibonacciCapeTest`**, run, re-pin as needed.
- [ ] **Step 4: Format and commit**

```bash
sbtn scalafmtAll
git add -u && git commit -m "test: migrate factorial/fibonacci CAPE harnesses to v3 fixtures via shared loader"
```

---

### Task 4: Migrate two_party_escrow harness; make the validator pass v3 checks

**Files:**
- Modify: `scalus-examples/jvm/src/test/scala/scalus/examples/cape/twopartyescrow/TwoPartyEscrowCapeTest.scala`
- Possibly modify: `scalus-examples/jvm/src/main/scala/scalus/examples/cape/twopartyescrow/TwoPartyEscrowValidator.scala`

**Interfaces:**
- Consumes: Task 2 loader; `/cape/two_party_escrow/cape-tests.json` (10 measurements + 37 checks).
- Produces: green suite; final script size + per-measurement budget pins.

- [ ] **Step 1: Rewrite the harness**: delete `CapeDataParser`, `ScriptContextBuilder`, and all patch/JSON helpers (lines 155-434); iterate `CapeTestSuite.load("/cape/two_party_escrow/cape-tests.json").cases` with `CapeHarness.run(program, c)`; keep the script-size test and the budget/fee pin maps (pins only for measurement cases).
- [ ] **Step 2: Run** `sbtn scalusExamplesJVM/clean` then `sbtn 'scalusExamplesJVM/testOnly scalus.examples.cape.twopartyescrow.*'`.
- [ ] **Step 3: Triage failures.** The v3 suite is much stricter (37 checks) and the context defaults changed (default datum `Some(Constr 0 [])`, no auto own-input, empty-bytes pubkey addresses, reversed list order). For each failing check, read its `description` in the fixture and fix `TwoPartyEscrowValidator` accordingly — the CAPE spec is `/Users/nau/projects/lantr/UPLC-CAPE/scenarios/two_party_escrow/two_party_escrow.md`. Do NOT weaken a check to pass; the fixture is the contract. Iterate until all 47 cases pass.
- [ ] **Step 4: Re-pin** budgets/fees/size from printed values; rerun; expected: PASS.
- [ ] **Step 5: Format and commit**

```bash
sbtn scalafmtAll
git add -u && git commit -m "feat: two_party_escrow CAPE validator passes v3 fixture suite (10 measurements + 37 checks)"
```

---

### Task 5: ecd scenario (fixed-mode naive GCD)

**Files:**
- Create: `scalus-examples/jvm/src/main/scala/scalus/examples/cape/ecd/EcdBase.scala`
- Create: `scalus-examples/jvm/src/main/scala/scalus/examples/cape/ecd/EcdContract.scala`
- Test: `scalus-examples/jvm/src/test/scala/scalus/examples/cape/ecd/EcdCapeTest.scala`

**Interfaces:**
- Produces: `EcdContract.program: Program` (2-argument: applied as `program $ a $ b`), used by the registry (Task 8).

Prescribed algorithm (fixed mode — direct translation only; compiler-automatic optimization is allowed):

```haskell
ecd a b | b == 0 = abs a | otherwise = ecd b (a `mod` b)
```

- [ ] **Step 1: Write the failing test**

```scala
package scalus.examples.cape.ecd

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.{CardanoInfo, ExUnits}
import scalus.examples.cape.{CapeHarness, CapeTestSuite}
import scalus.testing.kit.ScalusTest

class EcdCapeTest extends AnyFunSuite with ScalusTest {
    private given CardanoInfo = CardanoInfo.mainnet
    private val program = EcdContract.program
    private val suite = CapeTestSuite.load("/cape/ecd/cape-tests.json")

    test(s"Script size: ${program.cborByteString.length} bytes") {
        assert(program.cborByteString.length > 0) // pin exact size after first run
    }

    private val expectedBudgets: Map[String, ExUnits] = Map.empty // pin after first run

    for c <- suite.cases do
        test(s"CAPE: ${c.name}") {
            CapeHarness.run(program, c).foreach { budget =>
                val actual = ExUnits(memory = budget.memory, steps = budget.steps)
                info(s"${c.name}: $actual")
                expectedBudgets.get(c.name).foreach(exp => assert(actual == exp))
            }
        }
}
```

- [ ] **Step 2: Run to verify failure** — `sbtn 'scalusExamplesJVM/testOnly scalus.examples.cape.ecd.*'` — expected: compile FAILURE (`EcdContract` missing).

- [ ] **Step 3: Implement**

`EcdBase.scala`:

```scala
package scalus.examples.cape.ecd

import scalus.compiler.Compile
import scalus.*

/** CAPE `ecd` scenario: prescribed naive recursive Euclidean algorithm.
  *
  * Direct translation of the spec (fixed mode):
  * {{{ ecd a b | b == 0 = abs a | otherwise = ecd b (a `mod` b) }}}
  */
@Compile
object EcdBase {
    def ecd(a: BigInt, b: BigInt): BigInt =
        if b == BigInt(0) then (if a < 0 then -a else a)
        else ecd(b, a % b)
}
```

Note: Plutus `modInteger` (`%` here) follows the divisor's sign, same as the Haskell `mod` in the spec — the negative-input test cases (`ecd(-12, 8) = 4`, `ecd(12, -8) = 4`) verify this; do not "fix" to `remainder`.

`EcdContract.scala`:

```scala
package scalus.examples.cape.ecd

import scalus.compiler.Options
import scalus.uplc.{PlutusV3, Program}

object EcdContract {
    private given Options = Options.releaseUntagged
    lazy val program: Program = PlutusV3.compile(EcdBase.ecd).program
}
```

- [ ] **Step 4: Run; all 14 cases must pass; capture printed budgets and pin them + the exact script size; rerun green.**
- [ ] **Step 5: Compare the summed steps against Plinth's 18.7M (preview leader).** Record the number for Task 11's table — do not block on beating it here.
- [ ] **Step 6: Format and commit**

```bash
sbtn scalafmtAll
git add scalus-examples/jvm/src/main/scala/scalus/examples/cape/ecd \
        scalus-examples/jvm/src/test/scala/scalus/examples/cape/ecd
git commit -m "feat: CAPE ecd scenario (prescribed naive GCD) with budget-pinned harness"
```

---

### Task 6: htlc scenario

**Files:**
- Create: `scalus-examples/jvm/src/main/scala/scalus/examples/cape/htlc/HtlcValidator.scala`
- Create: `scalus-examples/jvm/src/main/scala/scalus/examples/cape/htlc/HtlcContract.scala`
- Test: `scalus-examples/jvm/src/test/scala/scalus/examples/cape/htlc/HtlcCapeTest.scala`

**Interfaces:**
- Produces: `HtlcContract.compiled` / `HtlcContract.program: Program` (`Data -> Unit`, one ScriptContext argument), used by Task 8.

Spec (`scenarios/htlc/htlc.md`, category **open** — implementation freedom, tests are the contract):

- Datum `Constr 0 [payer: Address, recipient: Address, hash: ByteString(32), timeout: Integer]`.
- Redeemer `Constr 0 [preimage: ByteString]` = Claim; `Constr 1 []` = Refund. Raw integers / bytestrings / lists / `Claim` without a field must FAIL.
- Claim: recipient signature; `sha2_256(preimage) == hash`; upper bound of validRange finite and (inclusive `t` ⇒ `t`, exclusive `t` ⇒ `t-1`) strictly `< timeout`; exactly one input from the script address.
- Refund: payer signature; lower bound finite and (inclusive `t` ⇒ `t`, exclusive ⇒ `t+1`) strictly `> timeout`; exactly one script input.

- [ ] **Step 1: Write the failing test** — same shape as `EcdCapeTest`, but load `/cape/htlc/cape-tests.json` and use `HtlcContract.program` (single ScriptContext input, 4 measurements + 21 checks; empty pin maps to start).
- [ ] **Step 2: Run to verify compile failure.**
- [ ] **Step 3: Implement the validator**

```scala
package scalus.examples.cape.htlc

import scalus.compiler.Compile
import scalus.cardano.onchain.plutus.prelude.*
import scalus.cardano.onchain.plutus.v1.{IntervalBound, IntervalBoundType}
import scalus.cardano.onchain.plutus.v3.*
import scalus.uplc.builtin.Builtins.sha2_256
import scalus.uplc.builtin.{ByteString, Data}
import scalus.uplc.builtin.Data.{toData, FromData, ToData}
import scalus.*

case class HtlcDatum(
    payer: Address,
    recipient: Address,
    hash: ByteString,
    timeout: BigInt
) derives FromData, ToData

enum HtlcRedeemer derives FromData, ToData:
    case Claim(preimage: ByteString)
    case Refund

/** UPLC-CAPE HTLC validator: claim with SHA-256 preimage before timeout, refund after. */
@Compile
object HtlcValidator {

    inline def validate(scData: Data): Unit = {
        val sc = scData.to[ScriptContext]
        sc.scriptInfo match
            case ScriptInfo.SpendingScript(txOutRef, datum) =>
                spend(datum, sc.redeemer, sc.txInfo, txOutRef)
            case _ => fail("Spending only")
    }

    inline def spend(datum: Option[Data], redeemer: Data, txInfo: TxInfo, txOutRef: TxOutRef): Unit = {
        val d = datum.getOrFail("No datum").to[HtlcDatum]
        requireSingleScriptInput(txInfo.inputs, txOutRef)
        redeemer.to[HtlcRedeemer] match
            case HtlcRedeemer.Claim(preimage) =>
                requireSignedBy(txInfo.signatories, pkhOf(d.recipient), "Recipient must sign")
                require(sha2_256(preimage) == d.hash, "Preimage mismatch")
                require(finiteUpperBound(txInfo.validRange) < d.timeout, "Too late to claim")
            case HtlcRedeemer.Refund =>
                requireSignedBy(txInfo.signatories, pkhOf(d.payer), "Payer must sign")
                require(finiteLowerBound(txInfo.validRange) > d.timeout, "Too early to refund")
    }

    def pkhOf(address: Address): PubKeyHash = address.credential match
        case Credential.PubKeyCredential(pkh) => pkh
        case _                                => fail("Expected pubkey address")

    /** Effective upper bound: inclusive t => t, exclusive t => t - 1; infinite fails. */
    def finiteUpperBound(range: Interval): BigInt = range.to.boundType match
        case IntervalBoundType.Finite(t) => if range.to.isInclusive then t else t - 1
        case _                           => fail("Upper bound must be finite")

    /** Effective lower bound: inclusive t => t, exclusive t => t + 1; infinite fails. */
    def finiteLowerBound(range: Interval): BigInt = range.from.boundType match
        case IntervalBoundType.Finite(t) => if range.from.isInclusive then t else t + 1
        case _                           => fail("Lower bound must be finite")

    /** Exactly one input spends from the script's own address (double-satisfaction guard). */
    def requireSingleScriptInput(inputs: List[TxInInfo], txOutRef: TxOutRef): Unit = {
        val ownCred = findOwnInput(inputs, txOutRef).resolved.address.credential
        def count(ins: List[TxInInfo], acc: BigInt): BigInt = ins match
            case List.Nil => acc
            case List.Cons(h, t) =>
                count(t, if h.resolved.address.credential.toData == ownCred.toData then acc + 1 else acc)
        require(count(inputs, 0) == BigInt(1), "Multiple script inputs")
    }

    def findOwnInput(inputs: List[TxInInfo], txOutRef: TxOutRef): TxInInfo = inputs match
        case List.Cons(h, t) => if h.outRef.toData == txOutRef.toData then h else findOwnInput(t, txOutRef)
        case List.Nil        => fail("Own input not found")

    def requireSignedBy(signatories: List[PubKeyHash], party: PubKeyHash, msg: String): Unit =
        signatories match
            case List.Nil            => fail(msg)
            case List.Cons(h, tail)  => if h.toData == party.toData then () else requireSignedBy(tail, party, msg)
}
```

`HtlcContract.scala`:

```scala
package scalus.examples.cape.htlc

import scalus.compiler.Options
import scalus.uplc.{PlutusV3, Program}

object HtlcContract {
    private given Options = Options.releaseUntagged
    lazy val compiled = PlutusV3.compile(HtlcValidator.validate)
    lazy val program: Program = compiled.program
}
```

Reuse helper style from `TwoPartyEscrowValidator` (same file layout, `derives FromData, ToData`). Note the derived `FromData` for `HtlcRedeemer` must FAIL on `I 0`, bytestrings, lists, and `Constr 0 []` (missing preimage field) — the `redeemer_*` checks verify this; if the derived decoder is lenient, decode manually via `unConstrData` and reject unexpected shapes.

- [ ] **Step 4: Run** (after `sbtn scalusExamplesJVM/clean` if stale); iterate until all 25 cases pass; pin budgets + script size; rerun green.
- [ ] **Step 5: Format and commit**

```bash
sbtn scalafmtAll
git add scalus-examples/jvm/src/main/scala/scalus/examples/cape/htlc \
        scalus-examples/jvm/src/test/scala/scalus/examples/cape/htlc
git commit -m "feat: CAPE htlc scenario validator with budget-pinned harness"
```

---

### Task 7: linear_vesting scenario

**Files:**
- Create: `scalus-examples/jvm/src/main/scala/scalus/examples/cape/linearvesting/LinearVestingValidator.scala`
- Create: `scalus-examples/jvm/src/main/scala/scalus/examples/cape/linearvesting/LinearVestingContract.scala`
- Test: `scalus-examples/jvm/src/test/scala/scalus/examples/cape/linearvesting/LinearVestingCapeTest.scala`

**Interfaces:**
- Produces: `LinearVestingContract.program: Program`, used by Task 8.

Spec (`scenarios/linear_vesting/linear_vesting.md`, category **open**):

- Datum `Constr 0 [beneficiary: Address, asset: Constr 0 [cs: ByteString, tn: ByteString], totalVestingQty, vestingPeriodStart, vestingPeriodEnd, firstUnlockPossibleAfter, totalInstallments]` (all remaining fields Integer).
- Redeemer `Constr 0 []` = PartialUnlock, `Constr 1 []` = FullUnlock; raw integers must fail.
- PartialUnlock: beneficiary signature; effective lower bound finite and `> firstUnlockPossibleAfter`; exactly one script input; continuing output at the script address must hold `newRemaining` of the asset with `0 < newRemaining < oldRemaining` and `newRemaining == expectedRemaining`, and its datum must equal the input datum exactly. Where, with `currentTime` = effective lower bound:
  - `divCeil(x, y) = 1 + ((x - 1) / y)` (integer division)
  - `timeBetween = divCeil(vestingPeriodEnd - vestingPeriodStart, totalInstallments)`
  - `futureInstallments = divCeil(vestingPeriodEnd - currentTime, timeBetween)`
  - `expectedRemaining = divCeil(futureInstallments * totalVestingQty, totalInstallments)`
- FullUnlock: beneficiary signature; effective lower bound finite and `> vestingPeriodEnd`.

- [ ] **Step 1: Write the failing test** — same shape as `HtlcCapeTest`, loading `/cape/linear_vesting/cape-tests.json` (6 measurements + 23 checks).
- [ ] **Step 2: Run to verify compile failure.**
- [ ] **Step 3: Implement**

```scala
package scalus.examples.cape.linearvesting

import scalus.compiler.Compile
import scalus.cardano.onchain.plutus.prelude.*
import scalus.cardano.onchain.plutus.v1.{IntervalBound, IntervalBoundType}
import scalus.cardano.onchain.plutus.v2.OutputDatum
import scalus.cardano.onchain.plutus.v3.*
import scalus.uplc.builtin.{ByteString, Data}
import scalus.uplc.builtin.Data.{toData, FromData, ToData}
import scalus.*

case class VestingAsset(currencySymbol: ByteString, tokenName: ByteString) derives FromData, ToData

case class VestingDatum(
    beneficiary: Address,
    asset: VestingAsset,
    totalVestingQty: BigInt,
    vestingPeriodStart: BigInt,
    vestingPeriodEnd: BigInt,
    firstUnlockPossibleAfter: BigInt,
    totalInstallments: BigInt
) derives FromData, ToData

enum VestingRedeemer derives FromData, ToData:
    case PartialUnlock
    case FullUnlock

/** UPLC-CAPE linear vesting validator: schedule-driven partial unlocks, full unlock after end. */
@Compile
object LinearVestingValidator {

    inline def validate(scData: Data): Unit = {
        val sc = scData.to[ScriptContext]
        sc.scriptInfo match
            case ScriptInfo.SpendingScript(txOutRef, datum) =>
                spend(datum, sc.redeemer, sc.txInfo, txOutRef)
            case _ => fail("Spending only")
    }

    inline def spend(datum: Option[Data], redeemer: Data, txInfo: TxInfo, txOutRef: TxOutRef): Unit = {
        val datumData = datum.getOrFail("No datum")
        val d = datumData.to[VestingDatum]
        val beneficiaryPkh = d.beneficiary.credential match
            case Credential.PubKeyCredential(pkh) => pkh
            case _                                => fail("Expected pubkey beneficiary")
        requireSignedBy(txInfo.signatories, beneficiaryPkh, "Beneficiary must sign")
        val currentTime = finiteLowerBound(txInfo.validRange)
        redeemer.to[VestingRedeemer] match
            case VestingRedeemer.FullUnlock =>
                require(currentTime > d.vestingPeriodEnd, "Vesting period not over")
            case VestingRedeemer.PartialUnlock =>
                require(currentTime > d.firstUnlockPossibleAfter, "Too early to unlock")
                val ownInput = findSingleScriptInput(txInfo.inputs, txOutRef)
                val ownCred = ownInput.resolved.address.credential
                val oldRemaining =
                    ownInput.resolved.value.quantityOf(d.asset.currencySymbol, d.asset.tokenName)
                val continuing = findSingleOutputByCredential(txInfo.outputs, ownCred)
                val newRemaining =
                    continuing.value.quantityOf(d.asset.currencySymbol, d.asset.tokenName)
                require(newRemaining > BigInt(0), "Nothing left: use FullUnlock")
                require(newRemaining < oldRemaining, "Must withdraw something")
                val timeBetween = divCeil(d.vestingPeriodEnd - d.vestingPeriodStart, d.totalInstallments)
                val futureInstallments = divCeil(d.vestingPeriodEnd - currentTime, timeBetween)
                val expectedRemaining =
                    divCeil(futureInstallments * d.totalVestingQty, d.totalInstallments)
                require(newRemaining == expectedRemaining, "Wrong remaining quantity")
                continuing.datum match
                    case OutputDatum.OutputDatum(outDatum) =>
                        require(outDatum == datumData, "Datum must be preserved")
                    case _ => fail("Continuing output must carry the datum")
    }

    def divCeil(x: BigInt, y: BigInt): BigInt = 1 + ((x - 1) / y)

    def finiteLowerBound(range: Interval): BigInt = range.from.boundType match
        case IntervalBoundType.Finite(t) => if range.from.isInclusive then t else t + 1
        case _                           => fail("Lower bound must be finite")

    /** The unique own input; fails when the script address is spent more than once. */
    def findSingleScriptInput(inputs: List[TxInInfo], txOutRef: TxOutRef): TxInInfo = {
        def own(ins: List[TxInInfo]): TxInInfo = ins match
            case List.Cons(h, t) => if h.outRef.toData == txOutRef.toData then h else own(t)
            case List.Nil        => fail("Own input not found")
        val ownInput = own(inputs)
        val cred = ownInput.resolved.address.credential.toData
        def count(ins: List[TxInInfo], acc: BigInt): BigInt = ins match
            case List.Nil        => acc
            case List.Cons(h, t) => count(t, if h.resolved.address.credential.toData == cred then acc + 1 else acc)
        require(count(inputs, 0) == BigInt(1), "Multiple script inputs")
        ownInput
    }

    def findSingleOutputByCredential(outputs: List[TxOut], cred: Credential): TxOut =
        outputs.filter(_.address.credential.toData == cred.toData) match
            case List.Cons(out, List.Nil) => out
            case _                        => fail("Expected exactly one continuing output")

    def requireSignedBy(signatories: List[PubKeyHash], party: PubKeyHash, msg: String): Unit =
        signatories match
            case List.Nil           => fail(msg)
            case List.Cons(h, tail) => if h.toData == party.toData then () else requireSignedBy(tail, party, msg)
}
```

`LinearVestingContract.scala` mirrors `HtlcContract` (given `Options.releaseUntagged`; `lazy val program = PlutusV3.compile(LinearVestingValidator.validate).program`).

FullUnlock intentionally checks only signature + time (per spec) — resist adding output checks; the fixtures define correctness. If a check case fails, read its fixture `description` and adjust (e.g. whether FullUnlock also needs the single-input guard — follow the fixtures).

- [ ] **Step 4: Run; iterate to 29/29 green; pin budgets + script size; rerun green.**
- [ ] **Step 5: Format and commit**

```bash
sbtn scalafmtAll
git add scalus-examples/jvm/src/main/scala/scalus/examples/cape/linearvesting \
        scalus-examples/jvm/src/test/scala/scalus/examples/cape/linearvesting
git commit -m "feat: CAPE linear_vesting scenario validator with budget-pinned harness"
```

---

### Task 8: Scenario registry, full-schema metadata, GenerateSubmissions

**Files:**
- Modify: `scalus-examples/jvm/src/main/scala/scalus/examples/cape/CapeMetadata.scala`
- Create: `scalus-examples/jvm/src/main/scala/scalus/examples/cape/CapeScenarios.scala`
- Create: `scalus-examples/jvm/src/main/scala/scalus/examples/cape/GenerateSubmissions.scala`
- Modify: `scalus-examples/jvm/src/main/scala/scalus/examples/cape/{factorial/FactorialContract.scala,fibonacci/FibonacciContract.scala,twopartyescrow/TwoPartyEscrowContract.scala}` — delete the per-scenario `@main` methods (keep the `lazy val` programs and blueprints).
- Test: `scalus-examples/jvm/src/test/scala/scalus/examples/cape/CapeMetadataTest.scala`

**Interfaces:**
- Consumes: `FactorialContract.{baseProgram,openProgram}`, `FibonacciContract.{baseProgram,openProgram}`, `EcdContract.program`, `HtlcContract.program`, `LinearVestingContract.program`, `TwoPartyEscrowContract.compiled.program`.
- Produces:
  - `case class CapeScenario(name: String, program: () => Program, implementationNotes: String, readmeApproach: String, sourceSubdir: String)`
  - `CapeScenarios.all: List[CapeScenario]` (8 entries)
  - `@main def GenerateSubmissions(args: String*): Unit` — `runMain scalus.examples.cape.GenerateSubmissions <cape-repo-dir> [<version>]`
  - `CapeMetadata.render(version, compilerCommit, date, sourceCommit, notes): String`

- [ ] **Step 1: Write the failing metadata test**

```scala
package scalus.examples.cape

import org.scalatest.funsuite.AnyFunSuite

class CapeMetadataTest extends AnyFunSuite {
    private val hash = "a" * 40
    private val json = ujson.read(CapeMetadata.render(
      version = "1.1.0", compilerCommit = hash,
      date = "2026-08-24T00:00:00Z", sourceCommit = hash,
      notes = "Some \"quoted\" notes"))

    test("schema-required fields are present and well-formed") {
        assert(json("compiler")("name").str == "Scalus")
        assert(json("compiler")("version").str == "1.1.0")
        assert(json("compiler")("commit_hash").str.matches("^[a-f0-9]{40}$"))
        assert(json("compilation_config")("target").str == "uplc")
        assert(json("submission")("date").str == "2026-08-24T00:00:00Z")
        assert(json("submission")("source_available").bool)
        assert(json("submission")("source_repository").str.startsWith("https://github.com/"))
        assert(json("submission")("source_commit_hash").str.matches("^[a-f0-9]{40}$"))
        assert(json("submission")("implementation_notes").str.contains("\"quoted\""))
        assert(json("contributors").arr.nonEmpty)
    }

    test("registry covers all 8 scenarios with unique names") {
        val names = CapeScenarios.all.map(_.name)
        assert(names.sorted == scala.List("ecd", "factorial", "factorial_naive_recursion",
          "fibonacci", "fibonacci_naive_recursion", "htlc", "linear_vesting", "two_party_escrow"))
    }
}
```

- [ ] **Step 2: Run to verify failure** (`CapeMetadata.render` and `CapeScenarios` missing).
- [ ] **Step 3: Implement**

`CapeMetadata.scala` — replace the string template with ujson (escaping-safe). Schema: `submissions/TEMPLATE/metadata.schema.json` — `additionalProperties: false` everywhere; required: `compiler{name,version}`, `compilation_config{target}`, `submission{date,source_available,implementation_notes}`:

```scala
package scalus.examples.cape

object CapeMetadata {
    val SourceRepository = "https://github.com/nau/scalus"

    def render(version: String, compilerCommit: String, date: String,
               sourceCommit: String, notes: String): String =
        ujson.write(ujson.Obj(
          "compiler" -> ujson.Obj(
            "name" -> "Scalus", "version" -> version, "commit_hash" -> compilerCommit),
          "compilation_config" -> ujson.Obj(
            "optimization_level" -> "release", "target" -> "uplc",
            "flags" -> ujson.Arr("Options.release")),
          "contributors" -> ujson.Arr(ujson.Obj(
            "name" -> "Alexander Nemish", "organization" -> "Lantr", "contact" -> "@nau")),
          "submission" -> ujson.Obj(
            "date" -> date, "source_available" -> true,
            "source_repository" -> SourceRepository,
            "source_commit_hash" -> sourceCommit,
            "implementation_notes" -> notes)
        ), indent = 2) + "\n"
}
```

(Verify `git remote get-url origin` — if the canonical public repo differs from `https://github.com/nau/scalus`, use that.)

`CapeScenarios.scala`:

```scala
package scalus.examples.cape

import scalus.examples.cape.ecd.EcdContract
import scalus.examples.cape.factorial.FactorialContract
import scalus.examples.cape.fibonacci.FibonacciContract
import scalus.examples.cape.htlc.HtlcContract
import scalus.examples.cape.linearvesting.LinearVestingContract
import scalus.examples.cape.twopartyescrow.TwoPartyEscrowContract
import scalus.uplc.Program

case class CapeScenario(
    name: String,
    program: () => Program,
    implementationNotes: String,
    readmeApproach: String,
    sourceSubdir: String
)

object CapeScenarios {
    private val compiledNote =
        "Compiled from Scala 3 with the Scalus compiler plugin, Options.release " +
            "(PV11/vanRossem target: flexible case, batch-6 builtins), no traces."

    val all: List[CapeScenario] = List(
      CapeScenario("factorial", () => FactorialContract.openProgram,
        "Hand-crafted UPLC with self-application recursion and CaseConstrApply.",
        "Hand-written UPLC term optimized by the Scalus UPLC pipeline.", "factorial"),
      CapeScenario("factorial_naive_recursion", () => FactorialContract.baseProgram,
        compiledNote, "Direct @Compile of the prescribed naive recursion.", "factorial"),
      CapeScenario("fibonacci", () => FibonacciContract.openProgram,
        "Hand-crafted UPLC with self-application recursion and CaseConstrApply.",
        "Hand-written UPLC term optimized by the Scalus UPLC pipeline.", "fibonacci"),
      CapeScenario("fibonacci_naive_recursion", () => FibonacciContract.baseProgram,
        compiledNote, "Direct @Compile of the prescribed naive recursion.", "fibonacci"),
      CapeScenario("ecd", () => EcdContract.program,
        compiledNote + " Direct translation of the prescribed Euclidean algorithm; " +
            "the only compiler-automatic transforms are recursion encoding and inlining.",
        "Direct @Compile of the prescribed naive recursive GCD.", "ecd"),
      CapeScenario("htlc", () => HtlcContract.program,
        compiledNote, "Scala 3 validator: SHA-256 preimage claim before timeout, payer refund after.", "htlc"),
      CapeScenario("linear_vesting", () => LinearVestingContract.program,
        compiledNote, "Scala 3 validator implementing the ceiling-division vesting schedule.", "linearvesting"),
      CapeScenario("two_party_escrow", () => TwoPartyEscrowContract.compiled.program,
        compiledNote, "Scala 3 validator: Deposited -> Accepted | Refunded state machine.", "twopartyescrow")
    )
}
```

`GenerateSubmissions.scala`:

```scala
package scalus.examples.cape

import scalus.utils.BuildInfo

import java.nio.file.{Files, Path}
import java.time.Instant
import scala.sys.process.*

/** Writes Scalus_<version>_nau submission dirs for all scenarios into a UPLC-CAPE clone.
  *
  * Usage: runMain scalus.examples.cape.GenerateSubmissions <cape-repo-dir> [<version>]
  */
@main def GenerateSubmissions(args: String*): Unit = {
    val capeRepo = Path.of(args.headOption.getOrElse(sys.error("usage: GenerateSubmissions <cape-repo-dir> [version]")))
    require(Files.isDirectory(capeRepo.resolve("submissions")), s"$capeRepo is not a UPLC-CAPE checkout")
    val version = args.lift(1).getOrElse(BuildInfo.version)
    val commit = "git rev-parse HEAD".!!.trim
    require(commit.matches("^[a-f0-9]{40}$"), s"bad git commit: $commit")
    val date = Instant.now().toString

    for s <- CapeScenarios.all do {
        val dir = capeRepo.resolve("submissions").resolve(s.name).resolve(s"Scalus_${version}_nau")
        Files.createDirectories(dir)
        val program = s.program()
        Files.writeString(dir.resolve(s"${s.name}.uplc"), program.show)
        Files.writeString(dir.resolve("metadata.json"),
          CapeMetadata.render(version, commit, date, commit, s.implementationNotes))
        Files.writeString(dir.resolve("README.md"), readme(s, version, commit))
        println(f"${s.name}%-28s ${program.cborByteString.length}%6d bytes -> $dir")
    }
}

private def readme(s: CapeScenario, version: String, commit: String): String =
    s"""# Scalus $version — ${s.name}
       |
       |${s.readmeApproach}
       |
       |## Source
       |
       |${CapeMetadata.SourceRepository}/tree/$commit/scalus-examples/jvm/src/main/scala/scalus/examples/cape/${s.sourceSubdir}/
       |
       |## Build
       |
       |In the Scalus repository at commit `$commit`:
       |
       |```
       |sbtn "scalusExamplesJVM/runMain scalus.examples.cape.GenerateSubmissions <path-to-UPLC-CAPE>"
       |```
       |
       |Compiled with `Options.release` (all optimizations, no traces), targeting protocol
       |version 11 (vanRossem), Plutus V3, plutus-core 1.1.0.
       |""".stripMargin
```

Compare the README section headings against `submissions/TEMPLATE/benchmark-README-template.md` in the clone and match its required sections.

- [ ] **Step 4: Delete the three old `@main`s** (`compileFactorial`, `compileFibonacci`, `compileTwoPartyEscrow`) and their now-unused imports; keep programs/blueprints.
- [ ] **Step 5: Run the tests** — `sbtn 'scalusExamplesJVM/testOnly scalus.examples.cape.CapeMetadataTest'` — expected PASS; then `sbtn scalusExamplesJVM/Test/compile` for the whole module.
- [ ] **Step 6: Smoke-run the generator**

```bash
sbtn "scalusExamplesJVM/runMain scalus.examples.cape.GenerateSubmissions /Users/nau/projects/lantr/UPLC-CAPE"
ls /Users/nau/projects/lantr/UPLC-CAPE/submissions/*/Scalus_*_nau/
```

Expected: 8 dirs, each with `<scenario>.uplc`, `metadata.json`, `README.md`.

- [ ] **Step 7: Format and commit**

```bash
sbtn scalafmtAll
git add -A scalus-examples/jvm/src/main/scala/scalus/examples/cape \
        scalus-examples/jvm/src/test/scala/scalus/examples/cape/CapeMetadataTest.scala
git commit -m "feat: CAPE scenario registry and one-command GenerateSubmissions generator"
```

---

### Task 9: CompareWithLeaderboard

**Files:**
- Create: `scalus-examples/jvm/src/main/scala/scalus/examples/cape/CompareWithLeaderboard.scala`

**Interfaces:**
- Consumes: `submissions/*/*/metrics.json` files in a UPLC-CAPE clone (schema: top-level `evaluations` array with `cpu_units`, `memory_units`, `included_in_aggregates`, `execution_result`).
- Produces: `@main def CompareWithLeaderboard(args: String*)` — `runMain ... CompareWithLeaderboard <cape-repo-dir> [--strict]`; with `--strict`, exits 1 when Scalus is not first somewhere.

- [ ] **Step 1: Implement**

```scala
package scalus.examples.cape

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

/** Ranks every submission per scenario by summed CPU over aggregate-included evaluations. */
@main def CompareWithLeaderboard(args: String*): Unit = {
    val capeRepo = Path.of(args.headOption.getOrElse(sys.error("usage: CompareWithLeaderboard <cape-repo-dir> [--strict]")))
    val strict = args.contains("--strict")
    val subs = capeRepo.resolve("submissions")
    var scalusBehind = false

    val scenarios = Files.list(subs).iterator.asScala.toSeq
        .filter(p => Files.isDirectory(p) && p.getFileName.toString != "TEMPLATE").sortBy(_.getFileName.toString)

    for scenario <- scenarios do {
        val rows = Files.list(scenario).iterator.asScala.toSeq
            .map(_.resolve("metrics.json")).filter(Files.exists(_))
            .map { mf =>
                val m = ujson.read(Files.readString(mf))
                val evs = m("evaluations").arr.filter(_("included_in_aggregates").bool)
                val cpu = evs.map(_("cpu_units").num.toLong).sum
                val mem = evs.map(_("memory_units").num.toLong).sum
                (mf.getParent.getFileName.toString, cpu, mem)
            }
            .sortBy(_._2)
        println(s"== ${scenario.getFileName}")
        for ((name, cpu, mem), i) <- rows.zipWithIndex do
            println(f"  ${i + 1}%2d. $name%-45s cpu=$cpu%,15d mem=$mem%,12d")
        rows.headOption.foreach { case (leader, leaderCpu, _) =>
            rows.find(_._1.startsWith("Scalus_")) match
                case Some((us, cpu, _)) if !leader.startsWith("Scalus_") =>
                    scalusBehind = true
                    val pct = (cpu - leaderCpu) * 100.0 / leaderCpu
                    println(f"  -> Scalus is BEHIND $leader by $pct%.1f%% CPU")
                case Some(_) => println("  -> Scalus leads")
                case None =>
                    scalusBehind = true
                    println("  -> NO Scalus submission")
        }
    }
    if strict && scalusBehind then sys.exit(1)
}
```

Note: rank the **best** Scalus row (there may be several versions); `rows.find` on a cpu-sorted list already picks the best one.

- [ ] **Step 2: Smoke-run against the clone's existing submissions**

```bash
sbtn "scalusExamplesJVM/runMain scalus.examples.cape.CompareWithLeaderboard /Users/nau/projects/lantr/UPLC-CAPE"
```

Expected: 8 scenario blocks; ecd shows "NO Scalus submission"; fibonacci shows "Scalus leads" (matches the design-doc baseline table).

- [ ] **Step 3: Format and commit**

```bash
sbtn scalafmtAll
git add scalus-examples/jvm/src/main/scala/scalus/examples/cape/CompareWithLeaderboard.scala
git commit -m "feat: CAPE leaderboard comparator with --strict gate"
```

---

### Task 10: Driver script and runbook rewrite

**Files:**
- Create: `scripts/cape-submit.sh` (chmod +x)
- Modify: `scalus-examples/jvm/src/main/scala/scalus/examples/cape/CAPE-SUBMISSION.md`

**Interfaces:**
- Consumes: `GenerateSubmissions`, `CompareWithLeaderboard` (@mains), upstream `scripts/cape.sh` (nix) in the clone.

- [ ] **Step 1: Write `scripts/cape-submit.sh`**

```bash
#!/usr/bin/env bash
# Generate, verify, and measure Scalus UPLC-CAPE submissions, then rank them.
# Usage: scripts/cape-submit.sh <path-to-UPLC-CAPE-clone> [version]
set -euo pipefail

CAPE_DIR=${1:?usage: cape-submit.sh <uplc-cape-dir> [version]}
VERSION=${2:-}

cd "$(dirname "$0")/.."
echo "==> Generating submissions"
sbtn "scalusExamplesJVM/runMain scalus.examples.cape.GenerateSubmissions $CAPE_DIR $VERSION"

cd "$CAPE_DIR"
shopt -s nullglob
DIRS=(submissions/*/Scalus_*_nau)
[[ ${#DIRS[@]} -eq 8 ]] || { echo "expected 8 submission dirs, found ${#DIRS[@]}"; exit 1; }

for d in "${DIRS[@]}"; do
  echo "==> verify $d"
  ./scripts/cape.sh submission verify "$d"
  echo "==> measure $d"
  ./scripts/cape.sh submission measure "$d"
done

cd - >/dev/null
echo "==> Leaderboard"
sbtn "scalusExamplesJVM/runMain scalus.examples.cape.CompareWithLeaderboard $CAPE_DIR"

echo "Done. Review $CAPE_DIR, then commit and open the PR manually."
```

If the generated dirs are for a specific version, tighten the glob to `Scalus_${VERSION}_nau` when `VERSION` is set. Check `./scripts/cape.sh --help` in the clone for the exact verify/measure invocation and adjust flags if they differ.

- [ ] **Step 2: Rewrite `CAPE-SUBMISSION.md`** — replace steps 2-8 of the old runbook with: prerequisites (Nix, UPLC-CAPE clone rebased on origin/main), `scripts/cape-submit.sh ../UPLC-CAPE`, review the output + leaderboard, then commit in the clone and `gh pr create`. Keep the "Adding a New Scenario" section, updated to: vendor the fixture, add `<Name>Base/Validator` + `<Name>Contract`, harness via `CapeTestSuite`/`CapeHarness`, add a `CapeScenarios.all` entry.
- [ ] **Step 3: Run** `chmod +x scripts/cape-submit.sh` and `bash -n scripts/cape-submit.sh` (syntax check). Expected: no output, exit 0.
- [ ] **Step 4: Format and commit**

```bash
git add scripts/cape-submit.sh scalus-examples/jvm/src/main/scala/scalus/examples/cape/CAPE-SUBMISSION.md
git commit -m "feat: one-command CAPE submission driver script and runbook rewrite"
```

---

### Task 11: End-to-end run and standings snapshot

**Files:**
- Create: `docs/internal/CAPE_COMPETITIVE_ANALYSIS.md`

- [ ] **Step 1: Rebase the clone on origin/main** (`git -C /Users/nau/projects/lantr/UPLC-CAPE pull --ff-only`), re-vendor fixtures if any changed (repeat Task 1 steps 1-3 if so — rerun affected harnesses).
- [ ] **Step 2: Full test pass**: `sbtn 'scalusExamplesJVM/testOnly scalus.examples.cape.*'` — all green.
- [ ] **Step 3: Run the driver**: `scripts/cape-submit.sh /Users/nau/projects/lantr/UPLC-CAPE`. All 8 verify + measure steps must pass. If `cape submission verify` rejects anything (uplc parse, schema, test failure), fix the generator/validators and rerun — our harness and upstream must agree; disagreement means a builder-semantics bug (compare against `lib/Cape/ScriptContextBuilder.hs`).
- [ ] **Step 4: Record standings** in `docs/internal/CAPE_COMPETITIVE_ANALYSIS.md`: date, Scalus version + commit, the comparator's full output, and a per-scenario table: our summed CPU/mem/size vs the leader's, delta %, verdict (lead / behind / new). 
- [ ] **Step 5: Commit** (`git add docs/internal/CAPE_COMPETITIVE_ANALYSIS.md && git commit -m "docs: CAPE standings snapshot at 1.1.0"`). Then, in the clone, commit the submission dirs on a branch for the eventual PR — but do NOT open the PR yet (that happens after Task 12 tuning).

---

### Task 12: Loss analysis and tuning loop

**Files:**
- Modify: `docs/internal/CAPE_COMPETITIVE_ANALYSIS.md` (findings per scenario)
- Modify: `docs/internal/CODEGEN_IMPROVEMENT_PLAN.md` (evidence lines only)
- Possibly modify: scenario sources under `scalus-examples/.../cape/` (tuning within scenario rules)

For each scenario where Task 11 shows Scalus behind (expected candidates per the design doc: `factorial` open −1.3%, `factorial_naive_recursion` −17%, `ecd` unknown, and whatever remains of the validator gaps at 1.1.0):

- [ ] **Step 1: Profile our program per measurement case.** In a scratch test or the harness, use the profiling CEK: `applied.term.evaluateProfile` (see `scalus-core/.../uplc/Term.scala:403`) and dump `ProfilingData.byFunction` (per-builtin costs) plus total machine steps.
- [ ] **Step 2: Profile the winner.** Load the leader's `.uplc` from the clone with `UplcParser().parseProgram(Files.readString(...))`, apply the same inputs, evaluate on our CEK, and capture its builtin counts/budget the same way.
- [ ] **Step 3: Diff and classify** the gap into one of: (a) algorithm choice (open mode only — rewrite our implementation), (b) representation/boundary strategy, (c) missing optimizer pass (match against CODEGEN plan tasks T3-T15), (d) recursion/encoding overhead. Record the diff table and classification in `CAPE_COMPETITIVE_ANALYSIS.md`.
- [ ] **Step 4: Apply in-scope fixes.** Open scenarios: source/algorithm changes are fair game (e.g. a leaner hand-crafted UPLC for `factorial` open — the gap to Plutarch is 480K CPU total; inspect their `.uplc` for the trick). Fixed scenarios (`ecd`, `factorial_naive_recursion`, `fibonacci_naive_recursion`): only compiler options may change; anything else goes to (c)/(d) as compiler work. After each fix: rerun the harness, re-pin budgets, rerun the driver's measure step, update standings.
- [ ] **Step 5: File compiler gaps.** For each class-(c)/(d) finding, append an evidence line to the matching task in `docs/internal/CODEGEN_IMPROVEMENT_PLAN.md` (or add a new task following its format) citing the scenario, the builtin/step diff, and the winner's technique.
- [ ] **Step 6: Final sweep and submission.** `sbtn quick` green; commit all changes; regenerate + re-verify + re-measure via `scripts/cape-submit.sh`; update the standings table; then in the clone commit `submissions/` and open the PR: `gh pr create --title "Add Scalus <version> submissions (all 8 scenarios)" --body "..."`. Report final standings.

---

## Self-Review Notes

- Spec coverage: deliverable 1 → Tasks 8-10; deliverable 2 → Tasks 5-7 (+1-2 infra); deliverable 3 → Tasks 8, 11; deliverable 4 → Tasks 9, 11, 12; deliverable 5 → Task 10. Fixture/loader work (spec §6) → Tasks 1-4.
- The builder semantics list in Task 2 was verified line-by-line against upstream `ScriptContextBuilder.hs`/`Tests.hs` on 2026-08-24; if `cape submission verify` disagrees with our harness in Task 11, upstream is the authority.
- Budget pins are captured-then-pinned by design (values depend on the compiler build); this is the established repo pattern, not a placeholder.
- API names (`IntervalBound.isInclusive`, `IntervalBoundType.Finite`, `Value.quantityOf`, `UplcParser.dataTerm`, `Data.fromJson`, `Term.evaluateProfile`) were verified against `master` on 2026-08-24; small signature drift is expected to be fixed inline by the implementer without changing semantics.
