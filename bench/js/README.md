# Replaying recorded evaluator requests

The JavaScript evaluator is measured on real requests: the arguments Lucid passed to Aiken's
`eval_phase_two_raw` while Midgard's test suites ran on 2026-09-23. Each request is one JSON file
(optionally gzipped) holding the transaction, its resolved inputs and outputs, the cost models, the
budget and the slot configuration, plus Aiken's result.

A sample of 10 requests per suite is bundled with the JVM benchmark in
`bench/src/main/resources/midgard/`: `deep-deposit`, `mint-authorization` and
`value-conservation`. Wherever a script takes a directory, it also takes one of these names. The
full recordings (2,010, 13,116 and 39,875 requests) are not in the repository; record your own as
below.

## Run

```bash
sbtn scalusCardanoLedgerJS/prepareNpmPackage        # builds scalus-cardano-ledger/js/src/main/npm/scalus.js
S=scalus-cardano-ledger/js/src/main/npm/scalus.js

# TxEvaluator on every request: median of 3 passes, optional shared ScriptCache
node bench/js/replay.mjs $S deep-deposit --cache 64

# the same requests on the JVM (MidgardReplayBenchmark.evaluateFromCbor is the same work)
sbtn "bench/Jmh/run -i 5 -wi 3 -f 1 -t 1 -p dir=deep-deposit .*MidgardReplayBenchmark.evaluateFromCbor"

# several engines, each checked against the Aiken result recorded with the request
node bench/js/compare-evaluators.mjs --dump deep-deposit \
     --engine aiken=<lucid-evolution>/packages/uplc/dist/speed/node/uplc_tx.js \
     --engine scalus=$S --engine txeval=$S
```

An engine whose name starts with `aiken` is loaded as Lucid's `uplc` package; one starting with
`txeval` uses `TxEvaluator`, one evaluator per parameter set; any other name calls
`Scalus.evalPlutusScripts` per request.

To check that two builds, or the JVM and JS, agree, write the budgets and diff them:

```bash
node bench/js/replay.mjs $S deep-deposit --results /tmp/js.txt
sbtn "bench/Jmh/run -i 1 -wi 1 -f 1 -jvmArgsAppend -Dscalus.bench.results=/tmp/jvm -p dir=deep-deposit .*MidgardReplayBenchmark.evaluate$"
diff /tmp/js.txt /tmp/jvm.deep-deposit.txt
```

## Record

1. Install the project whose tests you want to record, for example Midgard.
2. Patch its Lucid build: `bench/js/record-lucid-requests.sh node_modules/@lucid-evolution/lucid/dist`
3. Run its tests with `EVAL_DUMP_DIR=/path/to/empty/dir`, using Lucid's Aiken evaluator.

Every evaluation, successful or failed, becomes one `<pid>-<n>.json` file.
