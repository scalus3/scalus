# JS Conformance Evidence Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Turn the plutus-conformance result the JavaScript build already produces into a published, citable claim, and add the transaction-level differential test the corpus does not cover.

**Architecture:** Three of the four tasks are documentation and test-scoping, not new capability: `PlutusConformanceJsTest` already runs the whole corpus under `ci-js` with exact budget assertions. Task 1 discovers whether the three currently-skipped cases are a JVM-only problem. Task 4 adds a differential test against the evaluator lucid-evolution ships by default, at the layer the corpus does not reach.

**Tech Stack:** Scala 3.3.8, ScalaTest, Scala.js, vitest, `@lucid-evolution/uplc` (Rust→wasm), cbor-x.

**Spec:** `docs/superpowers/specs/2026-08-30-ts-emulator-provider-parity-design.md` (§1.5, §7.1)

## Global Constraints

- Run sbt inside the nix devshell, or tests fail for environment reasons:
  `nix develop .#ci --accept-flake-config --command bash -c "sbt --batch -Dsbt.supershell=false -Dsbt.log.noformat=true '<task>'"`.
- In a git worktree, `plutus-conformance` must be a symlink to the same nix-store path as the primary checkout, or the conformance tests fail to compile.
- Scala 3 style: braces for top-level and multi-line function bodies, indentation syntax for `if`/`match`, `then` in `if`.
- Never run two sbt commands concurrently.
- Corpus facts as of 2026-08-30: 999 `.uplc` evaluation cases, 999 `.uplc.budget.expected` files, 3 entries in `ignoredCases`.

---

### Task 1: Scope the blst skips to the platforms that have the bug

The three skipped cases are ignored for `supranational/blst#232`, a **Java** binding bug. The Scala.js build does not use blst — BLS12-381 goes through `@noble` in `scalus.uplc.builtin.platform`. The shared ignore list therefore overstates the problem on the platform we are pitching. This task finds out the truth and encodes it.

**Files:**
- Modify: `scalus-core/shared/src/test/scala/scalus/uplc/eval/PlutusConformanceTest.scala:53-61`
- Modify: `scalus-core/jvm/src/test/scala/scalus/uplc/eval/PlutusConformanceJvmTest.scala`
- Modify: `scalus-core/native/src/test/scala/scalus/uplc/eval/PlutusConformanceNativeTest.scala`

**Interfaces:**
- Produces: `PlutusConformanceTest.blstLargeDstCases: Map[String, String]` — the three case paths and their reason, available to any platform subclass that needs them.
- Produces: `PlutusConformanceTest.ignoredCases` now defaults to `Map.empty`; platform subclasses opt in.

- [ ] **Step 1: Move the skip list out of the shared class**

In `PlutusConformanceTest.scala`, replace the `ignoredCases` definition (currently lines 53-61) with:

```scala
    /** Conformance cases this platform deliberately skips, each with the reason. Keys are corpus
      * case paths relative to `uplc/evaluation`, without the `.uplc` suffix.
      *
      * Empty by default. A skip is a statement about one platform's dependencies, so it belongs in
      * that platform's subclass — keeping it here made the JS build claim a JVM defect as its own.
      */
    protected def ignoredCases: Map[String, String] = Map.empty

    /** The three BLS12-381 cases that fail wherever blst is used with a DST longer than 255 bytes
      * (supranational/blst#232). Platform subclasses that link blst mix this in.
      */
    protected val blstLargeDstCases: Map[String, String] = Map(
      "builtin/semantics/bls12_381-cardano-crypto-tests/signature/large-dst/large-dst" ->
          "blst binding bug for DST longer than 255 bytes (supranational/blst#232)",
      "builtin/semantics/bls12_381_G1_hashToGroup/hash-dst-len-255/hash-dst-len-255" ->
          "blst binding bug for DST longer than 255 bytes (supranational/blst#232)",
      "builtin/semantics/bls12_381_G2_hashToGroup/hash-dst-len-255/hash-dst-len-255" ->
          "blst binding bug for DST longer than 255 bytes (supranational/blst#232)"
    )
```

- [ ] **Step 2: Opt the JVM platform back in**

Replace the body of `PlutusConformanceJvmTest.scala`:

```scala
package scalus
package uplc
package eval

/** JVM-specific Plutus Conformance tests.
  *
  * BLS12-381 goes through the blst Java binding, which has a bug for DSTs longer than 255 bytes.
  */
class PlutusConformanceJvmTest extends PlutusConformanceTest {
    override protected def ignoredCases: Map[String, String] = blstLargeDstCases
}
```

- [ ] **Step 3: Opt the Native platform back in, provisionally**

Native reaches blst through FFI rather than the Java binding, so it may or may not be affected. Start from the safe assumption and correct it in step 5.

```scala
package scalus.uplc.eval

/** Native-specific Plutus Conformance tests.
  *
  * BLS12-381 is implemented using the blst library via FFI.
  */
class PlutusConformanceNativeTest extends PlutusConformanceTest {
    override protected def ignoredCases: Map[String, String] = blstLargeDstCases
}
```

- [ ] **Step 4: Run the JS suite and record what happens**

Run:

```bash
nix develop .#ci --accept-flake-config --command bash -c \
  "sbt --batch -Dsbt.supershell=false -Dsbt.log.noformat=true \
   'scalusJS/testOnly scalus.uplc.eval.PlutusConformanceJsTest'"
```

Two possible outcomes, both fine — record which one you got, because Task 2 and Task 3 quote the number:

- **All 999 pass.** The JS build is fully conformant and `PlutusConformanceJsTest` needs no override. This is the outcome worth having.
- **The three BLS cases fail.** `@noble` has the same DST limitation. Add the override to `PlutusConformanceJsTest.scala` exactly as in step 2, and change its scaladoc to name `@noble` rather than blst. Do **not** paper over a different failure: if some *other* case fails, stop and report it — that is a real regression, not a skip.

Grep the output for `*** FAILED` rather than `[error]`: ScalaTest failures print as `[info]`.

- [ ] **Step 5: Run the JVM and Native suites**

```bash
nix develop .#ci --accept-flake-config --command bash -c \
  "sbt --batch -Dsbt.supershell=false -Dsbt.log.noformat=true \
   'scalusJVM/testOnly scalus.uplc.eval.PlutusConformanceJvmTest'"
```

Expected: PASS, with three ignored. Then the same for `scalusNative/testOnly scalus.uplc.eval.PlutusConformanceNativeTest`; if Native passes all 999, delete its override and the `ignoredCases` stale-entry test will keep it honest.

- [ ] **Step 6: Commit**

```bash
git add scalus-core/shared/src/test/scala/scalus/uplc/eval/PlutusConformanceTest.scala \
        scalus-core/jvm/src/test/scala/scalus/uplc/eval/PlutusConformanceJvmTest.scala \
        scalus-core/native/src/test/scala/scalus/uplc/eval/PlutusConformanceNativeTest.scala \
        scalus-core/js/src/test/scala/scalus/uplc/eval/PlutusConformanceJsTest.scala
git commit -m "test: scope the blst conformance skips to the platforms that link blst"
```

---

### Task 2: Pin the published conformance numbers with a test

The README will state a count. A count in prose rots the first time the corpus is bumped. This test makes the corpus bump fail loudly, in the file whose author is about to update the number anyway.

**Files:**
- Modify: `scalus-core/js/src/test/scala/scalus/uplc/eval/PlutusConformanceJsTest.scala`

**Interfaces:**
- Consumes: `PlutusConformanceTest.ignoredCases` from Task 1.
- Consumes: `PlutusConformanceTest.discoveredCases` — needs its visibility widened from `private` to `protected` in this task.

- [ ] **Step 1: Write the failing test**

In `PlutusConformanceJsTest.scala`:

```scala
package scalus.uplc.eval

/** JS-specific Plutus Conformance tests.
  *
  * BLS12-381 goes through @noble, not blst.
  */
class PlutusConformanceJsTest extends PlutusConformanceTest {

    /** The numbers published in `scalus-cardano-ledger/js/src/main/npm/README.md` and on
      * scalus.org. If the corpus moves, this fails — update both the constant and the README in the
      * same commit, so the published claim can never quietly drift from what CI proves.
      */
    test("published conformance counts are still accurate") {
        assert(
          discoveredCases.size == 999,
          s"corpus has ${discoveredCases.size} evaluation cases, README says 999"
        )
        assert(
          ignoredCases.isEmpty,
          s"JS now skips ${ignoredCases.size} cases; README claims none: ${ignoredCases.keys}"
        )
    }
}
```

If Task 1 step 4 found that JS *does* need the three skips, write `ignoredCases.size == 3` instead and keep the message.

- [ ] **Step 2: Run it to verify it fails**

Run:

```bash
nix develop .#ci --accept-flake-config --command bash -c \
  "sbt --batch -Dsbt.supershell=false -Dsbt.log.noformat=true \
   'scalusJS/testOnly scalus.uplc.eval.PlutusConformanceJsTest -- -z published'"
```

Expected: FAIL to compile, `discoveredCases` is `private` in `PlutusConformanceTest`.

- [ ] **Step 3: Widen the visibility**

In `PlutusConformanceTest.scala`, change:

```scala
    private val discoveredCases: List[String] = discoveredCasesInline
```

to:

```scala
    protected val discoveredCases: List[String] = discoveredCasesInline
```

- [ ] **Step 4: Run it to verify it passes**

Same command as step 2. Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add scalus-core/js/src/test/scala/scalus/uplc/eval/PlutusConformanceJsTest.scala \
        scalus-core/shared/src/test/scala/scalus/uplc/eval/PlutusConformanceTest.scala
git commit -m "test: pin the conformance counts the npm README publishes"
```

---

### Task 3: Publish the claim where a maintainer will see it

**Files:**
- Modify: `scalus-cardano-ledger/js/src/main/npm/README.md` (after the intro paragraph, before `## Installation`)
- Modify: `scalus-site/content/testing/js-emulator.mdx` (new section before `## See Also`)

**Interfaces:**
- Consumes: the count confirmed in Task 1 step 4 and pinned in Task 2.

- [ ] **Step 1: Add the section to the npm README**

Insert after the two intro paragraphs. Use the number Task 1 actually produced — the text below assumes 999/999; if JS needed the three skips, write `996 of 999` and keep the final sentence.

```markdown
## Conformance

The JavaScript build is checked against the
[Plutus conformance test suite](https://github.com/IntersectMBO/plutus/tree/master/plutus-conformance)
on every CI run, not only the JVM build. It passes **999 of 999** UPLC evaluation cases, and each
case asserts two things: that the resulting term is α-equivalent to the expected one, and that the
**execution budget matches the reference exactly** — the same CPU and memory numbers a Cardano node
would charge, under Plutus's reference variant-E builtin cost model and CEK machine costs.

Budget equality is the part that matters for a transaction builder: an evaluator that agrees on
success but disagrees on cost still produces transactions a node rejects.
```

- [ ] **Step 2: Add the same claim to the site**

In `scalus-site/content/testing/js-emulator.mdx`, insert before `## See Also`:

```markdown
## Conformance

Scalus's JavaScript build runs the Plutus conformance suite in CI: 999 of 999 UPLC evaluation
cases, asserting both the resulting term and the exact CPU and memory budget against the reference
implementation. The evaluator that prices your transaction here is the same code the emulator runs
in phase 2, so `evaluateTx` and `submitTx` cannot disagree about cost.
```

- [ ] **Step 3: Verify the numbers against a real run**

Do not trust the plan. Run the JS suite once more and read the ScalaTest summary line:

```bash
nix develop .#ci --accept-flake-config --command bash -c \
  "sbt --batch -Dsbt.supershell=false -Dsbt.log.noformat=true \
   'scalusJS/testOnly scalus.uplc.eval.PlutusConformanceJsTest'" 2>&1 | tail -20
```

Expected: a `Tests: succeeded N, failed 0` line. `N` includes the two bookkeeping tests, so the case count is `N - 2`. Make the README match.

- [ ] **Step 4: Commit**

```bash
git add scalus-cardano-ledger/js/src/main/npm/README.md scalus-site/content/testing/js-emulator.mdx
git commit -m "docs: publish the JS conformance result"
```

---

### Task 4: Differential test against the evaluator lucid-evolution ships

The corpus exercises the CEK machine on bare UPLC terms. It never touches script-context construction, redeemer indexing, per-language cost-model selection or the protocol-version switch — the layer an evaluator adapter actually drives, and the only layer where two conformant implementations can still disagree. This test covers it against `@lucid-evolution/uplc`, which is what lucid-evolution uses today.

**Files:**
- Create: `scalus-cardano-ledger/js/src/main/npm/__tests__/differential.test.ts`
- Modify: `scalus-cardano-ledger/js/src/main/npm/package.json` (add the devDependency)

**Interfaces:**
- Consumes: `evalPlutusScripts(txBytes, utxoBytes, slotConfig, costModels)` from `../scalus`, and `testTxCborHex` / `testUtxoCborHex` / `costModels` / `hexToBytes` from `./fixtures`.
- Consumes: `eval_phase_two_raw(txBytes, inputsCbor[], outputsCbor[], costModelsCbor, budgetNum, budgetDen, zeroTime, zeroSlot, slotLength): Uint8Array[]` from `@lucid-evolution/uplc`.

- [ ] **Step 1: Add the devDependency**

In `scalus-cardano-ledger/js/src/main/npm/package.json`, add to `devDependencies`:

```json
    "@lucid-evolution/uplc": "^0.2.22",
```

Then run `npm install` in that directory.

- [ ] **Step 2: Write the failing test**

Create `differential.test.ts`. `eval_phase_two_raw` wants inputs and outputs as two parallel arrays of CBOR items, while our fixture is one CBOR map, so the test splits it with `cbor-x` (already a devDependency).

```ts
// __tests__/differential.test.ts
// Scalus and the Rust/wasm evaluator lucid-evolution ships must agree on every redeemer's budget.

import { describe, test, expect } from "vitest";
import { Decoder, Encoder } from "cbor-x";
import { evalPlutusScripts, SlotConfig } from "../scalus";
import { eval_phase_two_raw } from "@lucid-evolution/uplc";
import { hexToBytes, costModels, testTxCborHex, testUtxoCborHex } from "./fixtures";

const decoder = new Decoder({ mapsAsObjects: false });
const encoder = new Encoder();

/** Split a CBOR map of input -> output into the two parallel arrays the wasm evaluator wants. */
function splitUtxoMap(utxoMapCbor: Uint8Array): { inputs: Uint8Array[]; outputs: Uint8Array[] } {
  const decoded = decoder.decode(utxoMapCbor) as Map<unknown, unknown>;
  const inputs: Uint8Array[] = [];
  const outputs: Uint8Array[] = [];
  for (const [input, output] of decoded) {
    inputs.push(new Uint8Array(encoder.encode(input)));
    outputs.push(new Uint8Array(encoder.encode(output)));
  }
  return { inputs, outputs };
}

/** The cost models as the wasm evaluator wants them: a CBOR map from language id to cost array. */
function costModelsCbor(): Uint8Array {
  const map = new Map<number, number[]>([
    [0, costModels.PlutusV1],
    [1, costModels.PlutusV2],
    [2, costModels.PlutusV3],
  ]);
  return new Uint8Array(encoder.encode(map));
}

describe("Scalus vs @lucid-evolution/uplc", () => {
  test("agree on every redeemer budget", () => {
    const txBytes = hexToBytes(testTxCborHex);
    const utxoBytes = hexToBytes(testUtxoCborHex);
    const slot = SlotConfig.mainnet;

    const scalus = evalPlutusScripts(txBytes, utxoBytes, slot, [
      costModels.PlutusV1,
      costModels.PlutusV2,
      costModels.PlutusV3,
    ]);

    const { inputs, outputs } = splitUtxoMap(utxoBytes);
    const wasmRedeemers = eval_phase_two_raw(
      txBytes,
      inputs,
      outputs,
      costModelsCbor(),
      10_000_000_000n,
      14_000_000n,
      BigInt(slot.zeroTime),
      BigInt(slot.zeroSlot),
      slot.slotLength,
    );

    expect(scalus.length).toBe(wasmRedeemers.length);

    // Each wasm redeemer is CBOR [tag, index, data, [mem, steps]].
    const wasmBudgets = wasmRedeemers
      .map((r) => decoder.decode(r) as [number, number, unknown, [bigint, bigint]])
      .map(([tag, index, , [mem, steps]]) => ({ tag, index, mem: BigInt(mem), steps: BigInt(steps) }))
      .sort((a, b) => a.tag - b.tag || a.index - b.index);

    const scalusBudgets = scalus
      .map((r) => ({ tag: r.tag, index: r.index, mem: r.budget.memory, steps: r.budget.steps }))
      .sort((a, b) => a.index - b.index);

    for (let i = 0; i < scalusBudgets.length; i++) {
      expect(scalusBudgets[i].mem, `redeemer ${i} memory`).toBe(wasmBudgets[i].mem);
      expect(scalusBudgets[i].steps, `redeemer ${i} steps`).toBe(wasmBudgets[i].steps);
    }
  });
});
```

- [ ] **Step 3: Run it**

```bash
cd scalus-cardano-ledger/js/src/main/npm && npx vitest run __tests__/differential.test.ts
```

Three outcomes, and the third is the valuable one:

- **PASS.** Record the numbers; this is the sentence the PRs cite.
- **Shape mismatch** — the redeemer CBOR layout or the cost-model encoding is not what the wasm evaluator wants. Fix the test; this is not a finding about either evaluator.
- **Budgets differ.** Stop and investigate before writing any upstream PR. A disagreement here is in script-context construction, redeemer indexing or cost-model selection, and it is exactly what §7.1 of the spec exists to find. Report it with both budgets and the transaction.

- [ ] **Step 4: Extend the corpus**

One fixture transaction proves very little. Add at least one V1 and one V2 script transaction alongside the existing V3 fixture, and a transaction with two redeemers, so redeemer indexing is actually exercised. Take them from `scalus-cardano-ledger/jvm/src/test/resources` if suitable fixtures exist there; otherwise build them with `TxBuilder` in a Scala test and dump the CBOR hex into `fixtures.ts`.

- [ ] **Step 5: Commit**

```bash
git add scalus-cardano-ledger/js/src/main/npm/__tests__/differential.test.ts \
        scalus-cardano-ledger/js/src/main/npm/__tests__/fixtures.ts \
        scalus-cardano-ledger/js/src/main/npm/package.json \
        scalus-cardano-ledger/js/src/main/npm/package-lock.json
git commit -m "test: differential budgets against @lucid-evolution/uplc"
```

---

## Self-Review Notes

- **Spec coverage:** §1.5 item 4 → Tasks 1-3. §7.1 bullet 1 → Task 3. §7.1 bullet 2 → Task 1. §7.1 bullet 3 → Task 4. §9 risk "differential testing may find our bugs" → Task 4 step 3, third outcome.
- **Discovery task:** Task 1 step 4 is genuinely a discovery, not a placeholder — both outcomes are specified, and the number it produces is consumed by Tasks 2 and 3.
- **Type consistency:** `blstLargeDstCases` and `ignoredCases` are used with the same signature in Tasks 1 and 2; `discoveredCases` visibility is widened in Task 2 step 3, which is the only task that reads it.
