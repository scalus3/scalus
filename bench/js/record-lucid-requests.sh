#!/usr/bin/env bash
# Patches a built @lucid-evolution/lucid dist so that, with EVAL_DUMP_DIR set, every call Lucid makes
# to Aiken's `eval_phase_two_raw` is written to that directory as one JSON request, in the format the
# replay benchmarks read. Behaviour is unchanged otherwise. Run your test suite afterwards.
# Usage: bench/js/record-lucid-requests.sh <node_modules/@lucid-evolution/lucid/dist>
set -eu
cd "$1"
for f in index.js index.cjs; do
    [ -f "$f" ] || continue
    grep -q __recordUplc "$f" && continue
    perl -0pi -e 's/\b(\w+)\.eval_phase_two_raw\(/__recordUplc($1).eval_phase_two_raw(/g or die "no eval_phase_two_raw call in $ARGV\n"' "$f"
    cat >> "$f" <<'JS'

function __recordUplc(uplc) {
  const dir = process.env.EVAL_DUMP_DIR;
  if (!dir) return uplc;
  const fs = process.getBuiltinModule("node:fs");
  const hex = (b) => Buffer.from(b).toString("hex");
  return { eval_phase_two_raw: (...args) => {
    const [tx, inputs, outputs, costModels, maxSteps, maxMemory, zeroTime, zeroSlot, slotLength] = args;
    const rec = { tx: hex(tx), inputs: inputs.map(hex), outputs: outputs.map(hex), costModels: hex(costModels),
      maxSteps: String(maxSteps), maxMemory: String(maxMemory), zeroTime: String(zeroTime),
      zeroSlot: String(zeroSlot), slotLength };
    const t0 = performance.now();
    try {
      const result = uplc.eval_phase_two_raw(...args);
      rec.redeemers = result.map(hex);
      return result;
    } catch (e) {
      rec.error = String(e?.message ?? e).slice(0, 2000);
      throw e;
    } finally {
      rec.ms = performance.now() - t0;
      __recordUplc.n = (__recordUplc.n ?? 0) + 1;
      fs.writeFileSync(`${dir}/${process.pid}-${String(__recordUplc.n).padStart(5, "0")}.json`, JSON.stringify(rec));
    }
  } };
}
JS
    echo "patched $f"
done
