# Backlog

Tasks for Night Shift (`~/projects/lantr/night-shift`). The runner reads this file from local
`master` and takes `## Ready` entries in order. Approve an entry by moving it to `## Ready` in a
commit on `master`. Format: Night Shift spec, section 5.2.

Rules for every entry:

- Each bug fix needs a repo test that fails on the parent commit.
- Example and compiler work: re-pin ExUnits on both compiler generations (3.3.x and 3.8+), and run
  `scalusExamplesJVM/clean` before example tests.
- Plugin work: edit the scalus-core copies of shared files, never `scalus-plugin/src/main/shared/**`.

## Ready

### spec-status-sweep: Fix out-of-date Status lines in specs
- size: S
- gate: none
- source: docs/superpowers/specs/2026-10-28-onchain-imports-design.md:3, docs/superpowers/specs/2026-11-02-emulator-ledger-rules-design.md:3, docs/superpowers/specs/2026-08-31-t5-let-chain-regrouping-design.md:7, docs/superpowers/specs/2026-08-05-t1-static-argument-transformation-design.md:4, docs/superpowers/specs/2026-08-30-ts-emulator-provider-parity-design.md:4, docs/superpowers/specs/2026-06-28-blueprint-caching-and-naming-design.md:4, docs/superpowers/specs/2026-07-03-treasury-donation-txbuilder-design.md:4, docs/superpowers/specs/2026-07-31-uplc-source-view-design.md:4, docs/superpowers/specs/2026-10-28-explicit-nulls-migration-design.md:3, scalus-site/content/get-started/for-cardano-developers.mdx:116, scalus-site/content/get-started/for-scala-developers.mdx:128

Example: the onchain-imports spec says "Not merged", but 1aba6f19d is on master.

Done when: each listed spec's Status line names what landed, with its commit. The two "Evaluating
Scripts (In progress)" links point to `smart-contracts/evaluating-script`.

Risk: none (docs only).

### triage-core-small: Five small core bugs F-66, F-75, F-72, F-77, F-79
- size: S
- gate: none
- source: docs/internal/test-refactor/bug-triage.md:66-82, docs/internal/test-refactor/findings.md:1587, docs/internal/test-refactor/findings.md:1838, docs/internal/test-refactor/findings.md:1755, docs/internal/test-refactor/findings.md:1899, docs/internal/test-refactor/findings.md:1972, scalus-core/shared/src/main/scala/scalus/cardano/onchain/plutus/prelude/List.scala:309

Done when: one red-to-green test per defect, as `findings.md` prescribes.

Risk: F-66 changes prelude `List.show` on chain; F-75 is a JVM builtin.

### triage-examples: Example validator bugs F-85, F-86, F-87
- size: S
- gate: none
- source: docs/internal/test-refactor/findings.md:2142-2164

F-85: vault token stripping. F-86: negative oracle rate. F-87: Groth16 input range.

Done when: a negative test for each defect, and each validator fixed.

Risk: example budget re-pins.

### triage-emulator-testkit: Emulator and testkit bugs F-60, F-69, F-70, F-71
- size: S
- gate: none
- source: docs/internal/test-refactor/bug-triage.md:61

Done when: each finding is checked against 1.5.0 first (the ledger-rules work changed
`SubmitError.rule` and `checkTransaction`), and each one still open is fixed with a test.

Risk: testkit API.

### nofib-primes-queens: Port the primes and queens benchmarks
- size: S
- gate: none
- source: docs/internal/CODEGEN_IMPROVEMENT_PLAN.md:577

T11b. Neither benchmark exists in the repo.

Done when: primes and queens exist in both representations, with pinned budgets.

Risk: none (test-only).

### plugin-audit-lowrisk: Compiler plugin cleanup from the audit
- size: M
- gate: plan
- source: docs/internal/COMPILER_PLUGIN_AUDIT.md:127-143, scalus-plugin/src/main/scala/scalus/plugin/Plugin.scala:594

Items: M4, L1 to L4, L9, L14, R1, R2, and section 1 steps 2 and 3. Still present: `QQQ` at
`Plugin.scala:594`, typos, 126 `println` calls.

Done when: dead code and typos are gone; errors carry positions; a phase-boundary `NonFatal` goes to
`report.error`; symbol caches are in place; SIR output is byte-identical (md5 of the
`PipelineDeterminismChild` output before and after).

Risk: compiler plugin; no output change expected.

## Proposed

### versioned-llms-skills: Skills fetch the llms file for the project's own Scalus version
- size: S
- gate: plan
- source: docs/superpowers/specs/2026-10-22-versioned-api-docs-design.md:247, scalus-skills/skills/contract/SKILL.md:8, scalus-skills/skills/contract-test/SKILL.md:8

Done when: skills read the version from `build.sbt` or `project.scala`, fetch
`/llms/<v>/llms-api.txt`, and fall back to the root file; the skills version is bumped
(CONTRIBUTING "Agent artifacts").

Risk: skills release; the g8 half is in another repo.

### interop-forwarders-aliases: Enum static forwarders and named operator aliases
- size: M
- gate: plan
- source: docs/superpowers/specs/2026-07-11-cross-language-interop-style-guide-design.md:101-113, ~/.claude/projects/-Users-nau-projects-lantr-scalus/memory/interop-guide-pilot-emulator.md

Done when: static forwarders exist for each listed enum's cases; `Value` (and `Coin`, `MultiAsset`)
has named aliases such as `plus` and `negate`; a Java test calls them.

Risk: additive public API; MiMa additive; check `checkDtsUpToDate`.

### t18-let-sinking: LetSinking pass, flag-gated
- size: M
- gate: plan
- source: docs/superpowers/specs/2026-09-08-t18-let-sinking-design.md:8, docs/internal/CODEGEN_IMPROVEMENT_PLAN.md:903

Done when: the pass exists behind a flag (default off), with the spec tests and a measurement table.

Risk: none at the default.

### triage-offchain: Off-chain bugs F-35/38, F-36/57/76, F-37, F-39, F-54, F-55, F-61, F-73, F-74
- size: M
- gate: plan
- source: docs/internal/test-refactor/bug-triage.md:44-61

Done when: one red-to-green test per defect.

Risk: public behaviour; F-55 is JS.

### triage-compiler: Compiler bugs F-40, F-41, F-42/43, F-44, F-45
- size: M
- gate: plan
- source: docs/internal/test-refactor/bug-triage.md:63-72

F-41 is the ignored `ZeroArgMethodLoweringTest:36`. F-42/43 is `SIR.~=~`.

Done when: the ignored test runs again, and each fix has a test.

Risk: compiler.

### eq-lowering-peepholes: Cheaper equality lowering
- size: M
- gate: plan
- source: docs/internal/ORDER_UPLCCONSTR_REPR.md:145, docs/internal/SORTEDMAP_LOOKUP_COST_FINDINGS.md:91

`sum === Nullary` becomes a recursive `_sumEq` helper; generic `===` on Data-repr BigInt and
ByteString uses `equalsData`.

Done when: one `case` for literal-constructor comparisons; `unIData`/`unBData` plus a narrow
equality builtin; archived benchmarks re-run.

Risk: script hashes, re-pins.

### t17-spine-ops: Intrinsic dispatch for spine ops on packed lists
- size: M
- gate: plan
- source: docs/internal/CODEGEN_IMPROVEMENT_PLAN.md:843-871

`head`, `tail`, `at`, `drop` and `isEmpty`. The plan names the fix site in `IntrinsicResolver.scala:71`.

Done when: `elemRepr`/`listRepr` get `PackedSumDataList` cases; the spine ops are in
`PackedListViewMethods`; MembershipToken is green.

Risk: many re-pins; 2 to 13% wins expected.

### t19-trace-aware-pe: Partial evaluator folds closed terms whose untaken branches trace
- size: M
- gate: plan
- source: docs/internal/CODEGEN_IMPROVEMENT_PLAN.md:951-990

The decision table in the plan is the spec. Fix site: `PartialEvaluator.scala:48`.

Done when: the table and its listed tests pass; CEK results and logs are equal before and after.

Risk: release output changes; the plan calls it "future work".

### bloxbean-interop-delegate: Deprecate the orphaned bloxbean script-context translator
- size: M
- gate: plan
- source: ~/.claude/projects/-Users-nau-projects-lantr-scalus/memory/sortedmap-ledger-key-order-bug.md:130

`bloxbean-cardano-client-lib/.../Interop.scala:707` and after have no callers.

Done when: `getTxInfoV*`, `getScriptContextV*` and the related functions are deprecated and delegate
to `LedgerToPlutusTranslation`.

Risk: public API (deprecation).

### interop-surface-test: Build the planned InteropSurfaceTest
- size: M
- gate: plan
- source: docs/superpowers/specs/2026-07-11-cross-language-interop-style-guide-design.md:183-199, AGENTS.md:216

Done when: the 4 mechanical checks run, with today's violations pinned as an allow-list.

Risk: none (test-only).

### dev-lifecycle-twin: Dev-lifecycle design sections 3 and 4, additive part
- size: M
- gate: plan
- source: docs/superpowers/specs/2026-08-16-dev-lifecycle-options-design.md:4

Done when: `debugScript`/`debugScriptEntry` exist; `EmulatorBase.registerDebugScripts` has a Java
overload; the docs cover them.

Risk: adding a trait member may trip MiMa.

### explicit-nulls-core: Next module of the explicit-nulls migration
- size: L
- gate: plan
- source: docs/superpowers/specs/2026-10-28-explicit-nulls-migration-design.md

Step 2 of the spec.

Done when: scalus-core shared, jvm, js and native compile with the flag, main and test.

Risk: big diff; erasure means no MiMa impact.

### size-dead-args-constpool: Two script-size passes
- size: M
- gate: plan
- source: ~/.claude/projects/-Users-nau-projects-lantr-scalus/memory/uplc-size-compression-findings.md:44-59

Dead-argument elimination and constant pooling. On escrow: 87 B, 8.1%.

Done when: both passes exist behind flags, with a short design note first.

Risk: script hashes when enabled.

### codegen-peepholes: T3 fixpoint driver, T13 FloatDelay/ForceCaseDelay, T14 case-of-case
- size: M
- gate: plan
- source: docs/internal/CODEGEN_IMPROVEMENT_PLAN.md:358, docs/internal/CODEGEN_IMPROVEMENT_PLAN.md:663, docs/internal/CODEGEN_IMPROVEMENT_PLAN.md:677, docs/internal/BOOLEAN_OPTIMIZER_FINDINGS.md:67

Done when: each pass is flag-gated, with a design note first and the corpus measured.

Risk: compiler.

### deprecations-1.6: Remove the 1.4.1 deprecations
- size: S
- gate: plan
- source: CONTRIBUTING.md:152, docs/superpowers/specs/2026-10-28-explicit-nulls-migration-design.md:58

`SIRHashCodeInRec`, the `EmulatorBase` factories at `:617`, `:639`, `:752`, and
`StakeDistribution`/`RewardsCalculation`.

Decision needed: is the next release from master 1.6.0, or can a 1.5.x patch come from it?

Risk: MiMa filters.

### default-flips: Turn on T5 and T18 by default
- size: M
- gate: plan
- source: docs/internal/CODEGEN_IMPROVEMENT_PLAN.md:402, docs/superpowers/specs/2026-09-08-t18-let-sinking-design.md, docs/superpowers/specs/2026-09-10-cse-cce-placement-and-refactoring-design.md:376

One re-pin; possibly also `cceEnabled`.

Decision needed: flip the defaults? Every release script hash changes.

Risk: script hashes.

### cse-value-form-siblings: Share value forms across sibling branches
- size: S
- gate: plan
- source: docs/design/cse-measurements.md:374, ~/.claude/projects/-Users-nau-projects-lantr-scalus/memory/uplc-size-compression-findings.md:63

Decision needed: reverse the region rule for value forms only? It fixes traced builds that are 15%
larger (HelloCardano 268 to 395 B).

Risk: script hashes.

### abbreviate-error-traces: Wire AbbreviateErrorTraces
- size: M
- gate: plan
- source: ~/.claude/projects/-Users-nau-projects-lantr-scalus/memory/uplc-size-compression-findings.md:47

No caller today. Related: F-44.

Decision needed: the blueprint schema for the abbreviation map.

Risk: blueprint format.

### t9-expect: Opt-in deep datum validation
- size: M
- gate: plan
- source: docs/internal/CODEGEN_IMPROVEMENT_PLAN.md:536, ~/.claude/projects/-Users-nau-projects-lantr-scalus/memory/datum-shape-validation-research.md

Decision needed: the API name and shape; no spec exists yet.

Risk: public API.

### dev-lifecycle-presets: Dev-lifecycle design sections 1 and 2
- size: S
- gate: plan
- source: docs/superpowers/specs/2026-08-16-dev-lifecycle-options-design.md

Decision needed: redefine `Options.debug` and the `compile` default? The design targeted 1.1.0.

Risk: user-visible output change.

### examples-open: Remaining example defects
- size: M
- gate: plan
- source: docs/internal/test-refactor/findings.md:2176, docs/internal/EXAMPLES_REVIEW.md:179, docs/internal/EXAMPLES_REVIEW.md:355

F-89; the AMM LP binding; Betting double satisfaction. The review doc may be stale.

Decision needed: deadlines, key commitment, tagged payouts.

Risk: example budgets.

### jit-fewer-larger-methods: Shorten JIT warm-up on large scripts (research)
- size: L
- gate: plan
- source: docs/superpowers/specs/2026-10-30-staged-jit-publication-design.md:265, ~/.claude/projects/-Users-nau-projects-lantr-scalus/memory/vm-bench-harness-state.md

Output: a design for merging methods, and a prototype measurement on `stablecoin_*`. [P2-1] gates
Post 2.

### repr-measurements: Arrays vs lists, and derived FromData field access (research)
- size: M
- gate: plan
- source: docs/internal/CODEGEN_IMPROVEMENT_PLAN.md:593, docs/internal/CODEGEN_IMPROVEMENT_PLAN.md:795

Output: a report on `BuiltinArray` vs `List` (T12), and whether derived `FromData` still decodes
every field without `dropList` at PV11 (T16).

### t7-phase2: Keep Value ops native between calls (research)
- size: M
- gate: plan
- source: docs/internal/CODEGEN_IMPROVEMENT_PLAN.md:482, docs/superpowers/specs/2026-08-18-t7-value-builtins-lowering-design.md:175

Output: a spec for a `BuiltinValueBacked` representation.
