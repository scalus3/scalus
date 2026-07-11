# F2: Structural Flat Encoding for `con value` (BuiltinValue) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: superpowers:subagent-driven-development or superpowers:executing-plans. Steps use checkbox (`- [ ]`).

**Goal:** Encode a `BuiltinValue` (`con value`) constant in the flat wire format Plutus uses, instead of as a `Flat[Data]` CBOR-style blob (audit finding F2).

**Architecture:** `DefaultUni.builtinValueFlat` (`DefaultUni.scala:106-119`) currently serializes a `BuiltinValue` by round-tripping through `BuiltinValue.toData`/`fromData` and flat-encoding the resulting `Data`. Plutus (`PlutusCore/Value.hs:209`, `Flat Value`) encodes structurally: `Flat.encode v` where `v :: Map K (Map K Quantity)` decodes as a cons-list of `(currency: ByteString, [(tokenName: ByteString, quantity: Integer)])`, sorted by key, with decode-side validation (`K` ≤ 32 bytes via `k`; `Quantity` in signed-128-bit range). Scalus's `iterableFlat` (`package.scala`) is byte-identical to Haskell flat's `encodeListWith`/`decodeListWith` (1-bit-then-elem, 0-terminate), `pairFlat` matches `(,) <$> decode <*> decode`, and `Flat[ByteString]`/`Flat[BigInt]` are the same instances already verified against Plutus for `con bytestring`/`con integer`. `BuiltinValue.InnerType = SortedMap[ByteString, SortedMap[ByteString, BigInt]]` is exactly Plutus's `NestedMap`, and `ByteString`'s `Ord` uses `lessThanByteString` (lexicographic unsigned) — matching Haskell's `Ord ByteString`, so the sorted iteration order matches. The fix replaces the Data round-trip with `Flat[List[(ByteString, List[(ByteString, BigInt)])]]` over the value's sorted entries.

**Tech Stack:** Scala 3, sbt, ScalaTest, Scalus flat serialization.

## Global Constraints

- Wire format target (per `Flat Value` in Plutus 1.63.0.0 / master): cons-list of `(cs: ByteString, cons-list of (tn: ByteString, q: Integer))`, entries in ascending `ByteString` key order (currency then token). Use the existing `listFlat`, `pairFlat`, `Flat[ByteString]`, `Flat[BigInt]` givens — do NOT hand-roll new primitives.
- `DefaultUni.scala` is a **shared file copied into the plugin** (`scalus-plugin/src/main/shared/...`), and `BuiltinValueApi` has two versions: the real `SortedMap`-based one (`scalus-core/.../BuiltinValueApi.scala`) and a plugin **stub** (`scalus-plugin/.../BuiltinValueApi.scala`, `InnerType = Unit`, methods throw `UnsupportedOperationException`). Put the entry-list conversion in `BuiltinValueApi` (both versions), mirroring `toData`/`fromData`: real implements it, stub throws. Flat-encoding a `BuiltinValue` never happens in the plugin (same as `toData` today).
- Decode must rebuild via a normalizing/validating path matching Plutus `buildValueWith`: drop zero quantities and empty inner maps (invariants the type guarantees). Key-length (≤ 32 bytes) and signed-128-bit quantity validation mirror Plutus's decode `fail`s — include them (throw a flat `DecoderState`-style error) so hostile input is rejected the same way.
- This changes the flat/CBOR bytes (and script hash) of any serialized program containing a `con value` constant. Such constants only arise from V4/CIP-153 `Value` constant folding, so churn is expected to be near-zero — but any pinned bytes/hash in tests must be re-measured. The old bytes were Plutus-incompatible, so this is a correction.
- `docs/internal/UPLC_CORRECTNESS_AUDIT.md` and `PatternMatchAuditTest.scala` stay untracked. Commit to `master`, `scalafmtAll` before commit, `git pull --rebase` before push, no `Co-Authored-By` trailer.

---

### Task 1: RED tests

**Files:**
- Create: `scalus-core/jvm/src/test/scala/scalus/uplc/BuiltinValueFlatTest.scala`

**Interfaces:**
- Consumes: `DefaultUni.BuiltinValue` flat instance (via the existing `Flat`/`DefaultUni` machinery), `BuiltinValue` construction (e.g. `insertCoin` or a test builder), `EncoderState`/`DecoderState`, `listFlat`/`pairFlat`/`Flat[ByteString]`/`Flat[BigInt]`.

- [ ] **Step 1: Write the failing tests**
  - **Structural-format test (the crux of F2):** build a small value `v = { cs -> { tn -> 5 } }`; assert the flat bytes of `v` equal the flat bytes of the equivalent `List((cs, List((tn, BigInt(5)))))` encoded with `listFlat`/`pairFlat`. Pre-fix, `v` encodes as the `Data` blob → unequal → RED; post-fix equal.
  - **Round-trip test:** several values (empty; single token; multi-currency multi-token; negative quantities) survive `encode` → `decode` unchanged (structural `SortedMap` equality).
  - **(Optional) golden-bytes test:** hand-compute the expected flat bytes for a one-entry value from the Plutus format and assert equality — documents the wire format and guards the list/pair/bytestring/integer composition.
- [ ] **Step 2:** Run `sbt "scalusJVM/testOnly scalus.uplc.BuiltinValueFlatTest"`; verify RED (structural-format test fails: Data blob ≠ list encoding). `git add` the test.

### Task 2: Fix (GREEN)

**Files:**
- Modify: `scalus-core/shared/src/main/scala/scalus/uplc/builtin/BuiltinValueApi.scala` (add `toEntryList`/`fromEntryList`)
- Modify: `scalus-plugin/src/main/scala/scalus/uplc/builtin/BuiltinValueApi.scala` (stub: throw)
- Modify: `scalus-core/shared/src/main/scala/scalus/uplc/DefaultUni.scala` (`builtinValueFlat`)

- [ ] **Step 1:** In the real `BuiltinValueApi`, add:
  ```scala
  def toEntryList(value: BuiltinValue): List[(ByteString, List[(ByteString, BigInt)])] =
      value.inner.iterator.map { case (cs, tokens) => (cs, tokens.iterator.toList) }.toList
  def fromEntryList(entries: List[(ByteString, List[(ByteString, BigInt)])]): BuiltinValue =
      unsafeFromInner(SortedMap.from(entries.iterator.flatMap { case (cs, tokens) =>
          val inner = SortedMap.from(tokens.filter(_._2 != 0))   // drop zero quantities
          if inner.isEmpty then None else Some(cs -> inner)      // drop empty inner maps
      }))
  ```
  (`import ByteString.given` is already present for the `SortedMap` ordering. Add key-length validation — `require(cs.size <= 32)` and per-token — matching Plutus `k`/`quantity`; throw a decode error on violation.)
- [ ] **Step 2:** In the plugin stub `BuiltinValueApi`, add `toEntryList`/`fromEntryList` throwing `UnsupportedOperationException` (mirror `toData`/`fromData`).
- [ ] **Step 3:** Rewrite `DefaultUni.builtinValueFlat` to encode/decode `builtin.BuiltinValue.toEntryList(a)` / `fromEntryList(...)` via `summon[Flat[List[(ByteString, List[(ByteString, BigInt)])]]]` (needs imports: `Flat[ByteString]` from `ByteStringFlatInstance`, and `listFlat`/`pairFlat`/`Flat[BigInt]` from the flat package). Drop the `Flat[builtin.Data]` dependency.
- [ ] **Step 4:** Run the test suite; verify GREEN (structural-format, round-trip, golden all pass).

### Task 3: Regression

- [ ] `sbt "scalusJVM/Test/clean; scalusJVM/test"` — pass except the 2 intentional `PatternMatchAuditTest` failures. Watch for any pinned flat/CBOR bytes or script hashes of `con value`-containing programs; re-measure if they shift (expected near-zero).
- [ ] `sbt "scalusCardanoLedgerJVM/test; scalusPlugin/test"` and `sbt scalafmtAll; sbt quick`. Confirm the plugin still compiles (the shared `DefaultUni.scala` copy uses the stub `BuiltinValueApi`, which throws — never invoked at plugin time).

### Task 4: Docs + commit

- [ ] Mark F2 **FIXED** in the untracked audit doc (section + `F1/F2` executive-summary row — note only F2 is fixed; F1 stays open per the earlier decision).
- [ ] Commit + push (BuiltinValueApi ×2, DefaultUni, test, plan).

---

## Notes

- The list/pair/ByteString/Integer flat instances are already verified against Plutus for UPLC constants (audit "Verified clean" section: `UPLC Flat[Data] = CBOR-bytestring matches Plutus`, DefaultUni tags match, Constant type-tag scheme matches), so composing them per the `Flat Value` shape gives Plutus-compatible bytes without needing a live Plutus round-trip — though a golden-bytes test is the cheapest way to lock the format.
- F1 (`MultiIndexArray` tag 101) remains deliberately open (Plutus hasn't standardized it; see the earlier investigation). This plan addresses F2 only.
