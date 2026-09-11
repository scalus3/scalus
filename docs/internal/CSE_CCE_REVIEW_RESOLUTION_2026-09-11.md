# CSE/CCE review resolution

The [review](CSE_CCE_REVIEW_2026-09-11.md) was checked against `d315bb9b0`; the [verification](CSE_CCE_REVIEW_VERIFICATION_2026-09-11.md) records corrections to its diagnoses and proposed fixes. Implementation used focused red/green regressions, separate logical commits, and independent Astra reviews. No validator source algorithms changed in this fix pass.

## Findings addressed

| Finding | Resolution | Commits |
|---|---|---|
| F1 | Bound folding through retained constants by symbolic input size, with the existing small-value allowance. The 1,049-byte reproduction remains 1,049 bytes instead of growing to 3,100. | `8d3c8680e` |
| F2 | Defer function-body traversal until its argument environment is known; finish deferred bodies when retained. Review found and tested returned-lambda and impure-argument corners. Release sizing caches after each pass. | `e56625af1`, `366bebe5a`, `aae330641`, `8edb41729` |
| F3 | Stop CCE decomposition beneath Delay while preserving optimization inside its body and whole-delay leaves. | `d8a5b34da` |
| F4 | Reuse De Bruijn round trips for alpha keys, preserving free identities and original extraction terms. Keep structural CCE keys separate. | `2929435cc` |
| F5 | Return unavailable optional sizes for unsupported Flat constants, including composite BLS values. Skip unpriceable candidates; retain multiply used unpriceable constants in Inliner. | `2840ee223`, `64a6e3ff8`, `8d3c8680e` |
| F6 | Keep fee pricing for values; prefer sharing repeated computations such as opaque calls. Rank by the actual size/binding estimate and document a lexicographic termination argument. | `638946636`, `9386d2768` |
| F7 | Cache sizes/hashes/keys per pass, reuse unchanged subtrees, and canonicalize when hashes match. No batched extraction. | `26f2c9f66` |
| F8 | Correct pipeline, pricing, tracing and optimization-default documentation; retain historical measurements as historical and add final measurements separately. | `759cba78c`, `ed6595ad9`, final measurements |
| F9 | Retain requested Knights source improvements; label external budgets historical. Fresh comparison output uses actual measured budgets, not expected pins. | `964a00207`, `eb58ac51a` |
| F10 | Measure encoded output independently of pricing/log text. A constant-positive pricing mutation fails: 22 bytes exceeds 20. | `f13aca404` |
| F11 | Share fractional fee conversion and reference-bit pricing; explicitly charge CCE's Apply/Var/LamAbs overhead. | `b1b24eb7a` |
| F12 | Keep the simple estimator; correct index-zero documentation and test Flat parity/boundary differences. | `2840ee223` |
| F13 | Use exact wide geometric-series arithmetic and check final Coin range. `fee(460801, 15)` returns 49,197,199. | `4084d9600` |
| F14 | Cache constant encoding by identity within each pass. Regression counts encoder reads across fresh term wrappers without production instrumentation. | `2840ee223`, `64a6e3ff8` |
| F15 | Reuse ledger fee utilities in the leaderboard while preserving CAPE's explicit price snapshot and aggregate rounding. | `63e479ae2` |
| F16 | Replace affected new Scaladoc punctuation during the owning fixes. | `64a6e3ff8`, `8d3c8680e` |

## Additional review items

- Zero CSE iterations now disable post-CCE CSE too (`962758b9d`).
- CCE log numbers use locale-independent string interpolation; tests no longer parse them.
- Helper names beginning with `__` no longer establish totality. Forced-builtin exceptions that could never reach the template threshold were removed (`64a6e3ff8`).
- The corpus script accepts `--scala-version`; an explicit `--root` still takes precedence (`a03f2aaa1`).
- Deprecated forwarders and `termSize` remain for binary compatibility. Inspection of tag `v1.1.0` confirms these emitted methods already existed. `termSize` also has test callers. Removing them requires a binary-breaking release; adding broad MiMa exemptions solely to delete them is unjustified.
- The default Nix shell uses JDK 25; the `ci` shell selects 21. Measurements here used JDK 25.

## Validation and measured limits

Focused optimizer validation passed 255 tests, plus the final four computation-sharing regressions. Both Scala versions passed all 46 controlled corpus comparisons against normal current compilation. `nix develop --command sbtn quick` passed on Scala 3.3.8: 5,273 tests succeeded across the affected JVM suites, with no failures. `mima` passed for all five configured compatibility targets (core JVM/JS, ledger JVM/JS, and Bloxbean integration). No new compatibility exemptions were added.

Escrow/HTLC measured expectations are committed in `19044c0a3`; Knights reporting and expectations are in `eb58ac51a`. The remaining example expectations are in `5be26b0ba`. All 306 tests across the 20 affected example suites passed on both Scala versions, combining the initial runs with focused reruns of corrected expectations. Core expectations are in `c3c2ab638`, with all 425 tests across seven targeted suites passing on Scala 3.3.8 and all affected cases passing on 3.8.4. The field-decoding size expectation is in `5f2f89ff6`. Existing compiler-version branches were retained; no validator algorithms or semantic assertions changed.

See [final measurements](../design/cse-measurements.md) for all 23 blueprint deltas, controlled traced/release totals, both compiler versions, Knights/Escrow/HTLC budgets, fees, and scaling probes.

The results remain heuristic: some KnightsData and ordinary HTLC execution budgets grow. Worst-case CSE scaling remains superlinear because each extraction recollects the tree. The local folding guard does not minimize total binding-aware fees. These limitations are explicit; none relax semantic placement safety.
