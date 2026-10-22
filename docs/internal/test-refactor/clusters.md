# P2 clusters

`[P2-1]`: 606 test files, 144,156 lines, 42 clusters. Produced by `scripts/clusters.py`;
`scripts/clusters.py --files` lists the files of each cluster.

The spec asks for about 35 clusters of at most about 3k lines. The two numbers disagree: 144k
lines in 3k clusters is 48 clusters. The split keeps a package with its main code together, so
most clusters have 2.5k–4.5k lines. `uplc.transform` (6.5k) stays whole because `[P2-3]` names it
as the pilot.

Rules: a file goes to the first rule in `clusters.py` that matches its path. The example
contracts share no main code, so they are chunked by package, in name order, into clusters of
at most 4.5k lines.

| Cluster | Files | Lines |
|---|---|---|
| `uplc.transform` (pilot) | 19 | 6502 |
| `testing.regression` | 41 | 5946 |
| `ledger.rules.scripts` | 7 | 4766 |
| `verify` | 15 | 4758 |
| `prelude` | 18 | 4475 |
| `ledger.domain` | 35 | 4240 |
| `examples.3` | 15 | 4224 |
| `uplc` | 27 | 4152 |
| `compiler.plugin` | 25 | 4147 |
| `txbuilder` | 11 | 4107 |
| `compiler.lowering.intrinsics` | 21 | 4036 |
| `examples.2` | 7 | 4002 |
| `testing` | 17 | 3998 |
| `compiler.sir` | 24 | 3980 |
| `examples.7` | 10 | 3867 |
| `benchmarks-bloxbean-sbt` | 23 | 3837 |
| `examples.4` | 6 | 3777 |
| `node.emulator` | 8 | 3718 |
| `examples.5` | 7 | 3533 |
| `ledger.rules.value` | 12 | 3344 |
| `txbuilder.core` | 6 | 3277 |
| `uplc.eval.budget` | 13 | 3271 |
| `ledger.api` | 9 | 3214 |
| `prelude.list` | 1 | 3210 |
| `uplc.eval` | 20 | 3173 |
| `examples.1` | 19 | 3153 |
| `crypto` | 22 | 3131 |
| `compiler.lowering` | 11 | 3127 |
| `js-facade` | 18 | 2936 |
| `ledger.rules.witnesses-certs` | 16 | 2818 |
| `testing.conformance` | 12 | 2802 |
| `examples.6` | 6 | 2802 |
| `patterns` | 8 | 2799 |
| `utxocells` | 8 | 2686 |
| `examples.8` | 4 | 2467 |
| `address-wallet-blueprint` | 9 | 2320 |
| `uplc.builtin` | 15 | 2288 |
| `node.query-stream` | 15 | 2143 |
| `testing.integration` | 8 | 2122 |
| `compiler.pattern-matching` | 10 | 1960 |
| `uplc.jit` | 16 | 1770 |
| `ledger.state` | 12 | 1278 |
