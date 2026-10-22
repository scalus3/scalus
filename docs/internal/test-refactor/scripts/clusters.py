#!/usr/bin/env python3
"""Splits the test sources into P2 clusters ([P2-1]) and prints one row per cluster.

Each test file goes to the first rule whose regex matches its path. The example contracts
have no shared main code, so they are chunked by package into clusters of about 4.5k lines.
Fails if a file matches no rule.

Usage: clusters.py [--files]   (run from the repository root)
"""
import collections
import re
import subprocess
import sys

T = r"/src/test/scala/"
RULES = [
    ("uplc.transform", T + r"scalus/uplc/transform/"),
    ("uplc.jit", T + r"scalus/uplc/jit/"),
    ("uplc.builtin", T + r"scalus/uplc/builtin/"),
    ("uplc.eval.budget", T + r"scalus/uplc/eval/(Sort|SortedMapFromList|ValueBuiltins|EqualsDataVs|ExprSize|DatumEquality|UplcConstrSort|BuiltinCostModel|CostingInteger|CekBudget)"),
    ("js-facade", r"scalus-cardano-ledger/js/src/test/scala/scalus/uplc/eval/|scalus-core/js/src/test/scala/scalus/cardano/ledger/|scalus-ts-exporter/"),
    ("uplc.eval", T + r"scalus/uplc/eval/"),
    ("uplc", T + r"scalus/uplc/|" + T + r"scalus/serialization/"),
    ("crypto", T + r"scalus/crypto/|" + T + r"scalus/cardano/onchain/plutus/crypto/"),
    ("compiler.pattern-matching", T + r"scalus/compiler/[^/]*(Pattern|Match|CaseOnDataConstr)[^/]*\.scala$"),
    ("compiler.plugin", T + r"scalus/compiler/[^/]+\.scala$|" + T + r"scalus/compiler/(plugin|test)/|scalus-plugin/"),
    ("compiler.sir", T + r"scalus/compiler/sir/(linking|transform)/|" + T + r"scalus/compiler/sir/[^/]+\.scala$"),
    ("compiler.lowering.intrinsics", T + r"scalus/compiler/sir/lowering/(simple/|[^/]*(Eq|Intrinsic|Data|Value|Order|Constants|ZeroArg|BuiltinArray|NativeList))"),
    ("compiler.lowering", T + r"scalus/compiler/sir/lowering/"),
    ("prelude.list", T + r"scalus/cardano/onchain/plutus/prelude/ListTest"),
    ("prelude", T + r"scalus/cardano/onchain/plutus/prelude/|" + T + r"scalus/[^/]+\.scala$"),
    ("ledger.api", T + r"scalus/ledger/api/"),
    ("ledger.rules.scripts", T + r"scalus/cardano/ledger/rules/(PlutusScripts|MissingOrExtraScript|ExactSetOfRedeemers|MissingRequiredDatums|NativeScripts|TxRefScripts|ExUnitsTooBig)"),
    ("ledger.rules.value", T + r"scalus/cardano/ledger/rules/(FeesOk|ValueNotConserved|Outputs|TransactionSize|Treasury|TooManyCollateral|EmptyInputs|AllInputs|InputsAndReference|OutsideValidity|Metadata)"),
    ("ledger.rules.witnesses-certs", T + r"scalus/cardano/ledger/rules/"),
    ("ledger.state", r"scalus-cardano-ledger/(jvm|shared)/src/test/scala/scalus/cardano/ledger/(utils/|[^/]+\.scala$)"),
    ("ledger.domain", T + r"scalus/cardano/ledger/"),
    ("address-wallet-blueprint", T + r"scalus/cardano/(address|wallet|blueprint)/"),
    ("txbuilder.core", T + r"scalus/cardano/txbuilder/(TxBuilder|JsSigner)"),
    ("txbuilder", T + r"scalus/cardano/txbuilder/"),
    ("node.emulator", T + r"scalus/cardano/node/[^/]*(Emulator|SubmitError|DifferentialFixture)"),
    ("node.query-stream", T + r"scalus/cardano/node/"),
    ("testing.regression", T + r"scalus/testing/regression/"),
    ("testing.conformance", T + r"scalus/testing/conformance/"),
    ("testing.integration", T + r"scalus/testing/integration/|scalus-cardano-ledger-it/"),
    ("testing", T + r"scalus/testing/|" + T + r"scalus/utils/|" + T + r"eval/"),
    ("patterns", T + r"scalus/patterns/|scalus-design-patterns/"),
    ("utxocells", r"scalus-utxo-cell/"),
    ("verify", r"scalus-verification/|" + T + r"scalus/verify/"),
    ("benchmarks-bloxbean-sbt", T + r"scalus/benchmarks/|bloxbean-cardano-client-lib/|scalus-sbt-plugin/"),
]
EXAMPLES = T + r"scalus/examples/"
EXAMPLES_CHUNK = 4500


def lines(path):
    with open(path, "rb") as f:
        return sum(1 for _ in f)


def main():
    files = [
        f
        for f in subprocess.run(["git", "ls-files"], capture_output=True, text=True, check=True).stdout.split()
        if re.search(T + r".*\.scala$", f)
    ]
    clusters = collections.defaultdict(list)
    examples = collections.defaultdict(list)
    unmatched = []
    for f in files:
        if re.search(EXAMPLES, f):
            pkg = f.split(EXAMPLES)[1].split("/")[0]
            examples[pkg if not pkg.endswith(".scala") else "(root)"].append(f)
            continue
        for name, rx in RULES:
            if re.search(rx, f):
                clusters[name].append(f)
                break
        else:
            unmatched.append(f)
    if unmatched:
        sys.exit("no rule matches:\n" + "\n".join(unmatched))

    # Chunk the example packages in name order.
    chunk, size, n = [], 0, 1
    for pkg in sorted(examples):
        pkg_lines = sum(lines(f) for f in examples[pkg])
        if chunk and size + pkg_lines > EXAMPLES_CHUNK:
            clusters[f"examples.{n}"] = chunk
            chunk, size, n = [], 0, n + 1
        chunk += examples[pkg]
        size += pkg_lines
    clusters[f"examples.{n}"] = chunk

    total = 0
    for name, fs in clusters.items():
        n_lines = sum(lines(f) for f in fs)
        total += n_lines
        print(f"{name}\t{len(fs)}\t{n_lines}")
        if "--files" in sys.argv:
            for f in sorted(fs):
                print(f"\t{f}")
    print(f"TOTAL\t{len(files)}\t{total}\t{len(clusters)} clusters", file=sys.stderr)


if __name__ == "__main__":
    main()
