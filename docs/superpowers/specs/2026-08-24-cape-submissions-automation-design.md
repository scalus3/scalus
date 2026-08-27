# CAPE submissions automation and full scenario coverage – design

Date: 2026-08-24
Status: approved for planning
Scope owner: nau

## 1. Goal

Make Scalus submissions to [UPLC-CAPE](https://github.com/IntersectMBO/UPLC-CAPE)
a one-command, repeatable process; cover all 8 CAPE scenarios; and know, per
scenario, whether Scalus is best – and if not, why, with findings routed into
`docs/internal/CODEGEN_IMPROVEMENT_PLAN.md`.

Decisions made during brainstorming:

- **Automation level:** one-command generator + driver script. PR creation and
  review stay manual (semi-automated).
- **Track:** mainnet only. Mainnet is PV 11 (vanRossem) as of epoch 651
  (verified via Koios 2026-08-24), so submissions target `Options.release`
  (PV11 default in Scalus 1.x). No `_preview` variants, no PV knob.
- **Fix scope:** tune benchmark sources within scenario rules and analyze
  losses; compiler/optimizer changes are follow-up work filed as evidence in
  `CODEGEN_IMPROVEMENT_PLAN.md`, not part of this effort.
- **Identity:** submission dirs `Scalus_<version>_nau`, version = latest
  release (1.1.0 at design time).

## 2. Current state (verified 2026-08-24)

Upstream (`../UPLC-CAPE`, origin/main at `276738c`, 2026-07-10):

- 8 scenarios: `factorial`, `factorial_naive_recursion`, `fibonacci`,
  `fibonacci_naive_recursion`, `ecd`, `htlc`, `linear_vesting`,
  `two_party_escrow`.
- `cape-tests.json` schema is `3.0.0`: sections `data_structures` /
  `measurements` / `checks`; `builtin_data` values use UPLC-text Data syntax
  (`Constr 0 [...]`, `I 0`, `B #hex`) per closed issue #148.
- Latest Scalus submissions are 0.18.2 (by Unisay); none for `ecd`; none from
  Scalus 1.x.

Scalus repo:

- Harnesses exist only for factorial, fibonacci, two_party_escrow
  (`scalus-examples/jvm/src/{main,test}/scala/scalus/examples/cape/`).
- Vendored fixtures are stale schema `1.0.0` with the old custom Data
  notation; `TwoPartyEscrowCapeTest` carries a bespoke parser for it.
- `UplcParser.dataTerm` (`scalus-core/.../uplc/UplcParser.scala:384`) already
  parses the new Data syntax – no custom parser needed.
- `Term.show` output is always name-sanitized (`TermSanitizer.sanitizeNames`
  via `given Pretty[Term]`, `Term.scala:606`) – no extra alpha-rename pass
  needed for plutus-core textual-parser compatibility.
- Nix 2.34.6 and the upstream `cape` CLI (`scripts/cape.sh`, nix-backed) work
  locally.

Leaderboard baseline (summed CPU over each scenario's aggregated tests;
`_preview` rows are the real bar since mainnet is now PV11):

| Scenario | Leader today | Best Scalus (0.18.2) | Delta |
|---|---|---|---|
| fibonacci (open) | Scalus preview 14.6M | same | win |
| fibonacci_naive (fixed) | Scalus preview 115.4G | same | win |
| factorial (open) | Plutarch 37.0M | 37.5M | -1.3% |
| factorial_naive (fixed) | Plinth preview 28.0M | 32.8M | -17% |
| ecd (fixed) | Plinth preview 18.7M | none | missing |
| htlc | Plinth preview 60.5M | 83.2M | -37% |
| linear_vesting | Plinth preview 140.5M | 224.7M | -60% |
| two_party_escrow | Plinth preview 168.1M | 231.9M | -38% |

The validator-scenario losses predate Scalus 1.x (T1 SAT, T2 self-application
recursion, T7 Value builtins); re-measurement at 1.1.0 comes first, analysis
second.

## 3. Deliverables

1. **Generator**: `scalus.examples.cape.GenerateSubmissions` (`@main`) +
   `scripts/cape-submit.sh`.
2. **Three new scenario implementations** with budget-pinned harness tests:
   `ecd`, `htlc`, `linear_vesting`.
3. **Refreshed submissions for all 8 scenarios** at Scalus 1.1.0, mainnet
   track, handle `nau`.
4. **Leaderboard comparator** `CompareWithLeaderboard` + a loss-analysis
   workflow; findings in `docs/internal/CAPE_COMPETITIVE_ANALYSIS.md` and as
   evidence lines in `CODEGEN_IMPROVEMENT_PLAN.md`.
5. **Runbook rewrite**: `CAPE-SUBMISSION.md` becomes "run the command, review,
   open PR".

## 4. Generator design

### 4.1 Scenario registry

`CapeScenario` case class in `scalus-examples/jvm/src/main/scala/scalus/examples/cape/`:

- `name` (CAPE scenario id), `mode` (fixed/open), `program: Program`
  (compiled with `Options.release`), `implementationNotes: String`, README
  template parameters.
- One registry value lists all 8 entries. Existing per-scenario `@main`s
  (`compileFactorial`, ...) become thin wrappers or are removed in favor of
  the single entry point.

### 4.2 Emitters

- **UPLC**: `program.show` into
  `$UPLC_CAPE/submissions/<scenario>/Scalus_<ver>_nau/<scenario>.uplc`.
- **metadata.json**: jsoniter case classes mirroring
  `submissions/TEMPLATE/metadata.schema.json`; injected at run time: Scalus
  version, 40-char git commit hash (`git rev-parse HEAD`), ISO-8601 date,
  contributor block (name from git config, handle `nau`), per-scenario
  `implementation_notes` from the registry.
- **README.md**: rendered from one template with per-scenario sections
  (approach, source links into the Scalus repo, build instructions pointing
  at the runbook).

### 4.3 Driver script

`scripts/cape-submit.sh <path-to-UPLC-CAPE-clone> [--version <v>]`:

1. Runs the `@main` via sbtn to generate all submission dirs.
2. Runs `cape submission verify` per dir (authoritative schema + correctness
   check, via the clone's `scripts/cape.sh` under Nix).
3. Runs `cape submission measure` per dir (writes `metrics.json`).
4. Runs `CompareWithLeaderboard` and prints per-scenario rank + delta to the
   leader.
5. Stops there – the user reviews and opens the PR.

## 5. New scenarios

- **ecd** (fixed mode, 14 tests): `@Compile` direct translation of the
  prescribed algorithm: `ecd(a, b) = if b == 0 then abs(a) else
  ecd(b, a mod b)`. Compiler-automatic optimizations (T2 self-application,
  SAT) are allowed by the scenario rules ("beyond what the compiler does
  automatically"). Target: beat Plinth preview 18.7M CPU.
- **htlc** (4 script-context tests): implement exactly to
  `scenarios/htlc/htlc.md`; reuse our existing HTLC example where it matches
  the spec's datum/redeemer/parameter shapes.
- **linear_vesting** (6 script-context tests): implement per
  `scenarios/linear_vesting/linear_vesting.md`.

Each scenario follows the existing layout: sources under
`main/.../cape/<scenario>/` (`<Name>Base.scala` / `<Name>Open.scala` where the
mode allows / registry entry), harness under `test/.../cape/<scenario>/`.

## 6. Shared cape-tests harness

New test-tree module (shared by all 8 harnesses):

- **`CapeTestSuite` loader**: parses schema-3.0.0 `cape-tests.json` with
  jsoniter; resolves `@name` references into `data_structures`; parses all
  `builtin_data` values with `UplcParser.dataTerm`.
- **Script-context interpreter**: builds a PV11 `ScriptContext` from the
  `baseline` (e.g. `spending`) plus `patches` ops (`set_redeemer`,
  `set_script_datum`, `add_signature`, `set_valid_range`, `add_input_utxo`,
  `add_output_utxo`, ...). Extracted from the current `TwoPartyEscrowCapeTest`
  machinery and generalized; the bespoke v1-notation Data parser is deleted.
- **Fixture refresh**: re-vendor all 8 `cape-tests.json` at 3.0.0 from the
  updated clone; document the upstream commit they were copied from.

## 7. Competitive-analysis loop

- **`CompareWithLeaderboard`** (`@main`): scans
  `submissions/*/*/metrics.json` in the clone; per scenario ranks submissions
  by summed CPU (mem and script size reported alongside); prints our position
  and delta to the leader; exits non-zero when Scalus is not first – usable
  as a release-time gate.
- **Loss workflow**, per losing test case:
  1. Profile our program with the profiling CEK (`Term.evaluateProfile`,
     per-builtin breakdown).
  2. Load the winner's `.uplc` (textual, parseable by `UplcParser`) and
     evaluate it on our counting CEK for their builtin/step profile.
  3. Diff the profiles; classify the gap (representation, recursion encoding,
     pass gap, algorithm choice).
- **Routing**: source-level fixes within scenario rules land here (open mode:
  algorithm/implementation changes; fixed mode: compiler options only).
  Compiler-level gaps are appended as evidence to the matching
  `CODEGEN_IMPROVEMENT_PLAN.md` task (or a new task). Narrative findings go
  to `docs/internal/CAPE_COMPETITIVE_ANALYSIS.md`.

## 8. Testing and acceptance

- Each scenario harness asserts correctness against its `cape-tests.json` and
  pins exact ExUnits via the `ScalaCompilerVersion.baseline` pattern; runs in
  normal `jvm/test`.
- Metadata: in-repo unit test checks required fields and value shapes; the
  authoritative schema validation is `cape submission verify` in the driver.
- End-to-end acceptance: `cape submission verify` and `measure` green for all
  8 generated dirs; comparator output reviewed; PR opened manually.
- `sbtn quick` green; new files `git add`ed.

## 9. Risks and notes

- **Upstream is promoting preview to mainnet** (branch
  `yura/post-vanrossem-promote-casing` in flight). Rankings may reshuffle
  when that lands; the comparator reads whatever `metrics.json` are present,
  so it stays correct. Rebase the clone before submitting.
- **Fixed-mode compliance** is human-reviewed upstream; ecd README must state
  how the source matches the prescribed algorithm.
- **htlc / linear_vesting specs** must be implemented to the CAPE spec, not
  to our existing examples; spec drift here is the main correctness risk and
  is covered by the fixture-driven harness tests.
- **Budget pins** on new scenarios may need dual baselines (pre-3.8 /
  since-3.8 Scala compiler generations), same as existing suites.
