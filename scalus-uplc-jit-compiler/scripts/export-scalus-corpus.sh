#!/usr/bin/env bash
# Exports the Scalus part of the JIT corpus (spec 2026-10-19-uplc-jit-simplification-design.md,
# [MS-2], [MS-3]): fully applied PlutusV3 validators of scalus-examples, with the datum, redeemer
# and script context of an existing example test.
#
# 1. Runs each example test with SCALUS_DUMP=flat, so PlutusScriptEvaluator writes every applied
#    script it evaluates to <dump>/<package>/<hash>-<lang>-<tag>-<index>.flat (the last evaluation
#    of a script wins).
# 2. ScalusCorpusSelect keeps the PlutusV3 scripts that the CEK machine (variant E, PV11) evaluates
#    successfully, the largest one per package, and writes them to src/test/resources/corpus/scalus/.
#
# Usage, from the repository root: scalus-uplc-jit-compiler/scripts/export-scalus-corpus.sh
set -euo pipefail

dump=${DUMP_DIR:-/tmp/scalus-corpus-dump}
out=scalus-uplc-jit-compiler/src/test/resources/corpus/scalus
rm -rf "$dump"
mkdir -p "$dump" "$out"

tests=(
  scalus.examples.amm.AmmTest
  scalus.examples.anonymousdata.AnonymousDataTest
  scalus.examples.atomictransactions.AtomicTransactionsTest
  scalus.examples.auction.AuctionValidatorTest
  scalus.examples.betting.BettingTransactionTest
  scalus.examples.crowdfunding.CrowdfundingEmulatorTest
  scalus.examples.decentralizedidentity.DecentralizedIdentityTest
  scalus.examples.editablenft.EditableNftValidatorTest
  scalus.examples.escrow.EscrowTest
  scalus.examples.htlc.HtlcTest
  scalus.examples.linkedlist.LinkedListTest
  scalus.examples.lottery.LotteryValidatorTest
  scalus.examples.midgard.DaBondPoolEmulatorTest
  scalus.examples.paymentsplitter.PaymentSplitterTxBuilderTest
  scalus.examples.pricebet.PricebetValidatorTest
  scalus.examples.setbench.SetBenchEmulatorTest
  scalus.examples.storage.StorageTest
  scalus.examples.upgradeableproxy.UpgradeableProxyTest
  scalus.examples.vault.VaultTransactionTest
  scalus.examples.vesting.VestingTransactionTest
  scalus.examples.simplewallet.SimpleWalletTest
)

cmds=("project scalusExamplesJVM")
for t in "${tests[@]}"; do
  pkg=$(echo "$t" | awk -F. '{print $3}')
  cmds+=("set Test / envVars := Map(\"SCALUS_DUMP\" -> \"flat\", \"SCALUS_DUMP_DIR\" -> \"$dump/$pkg\")")
  cmds+=("testOnly $t")
done
# a failing example test stops the command chain of sbt, but its passing tests still dumped
# their scripts: select from what is there
sbt -Dsbt.supershell=false -Dsbt.log.noformat=true "${cmds[@]}" || true
sbt -Dsbt.supershell=false -Dsbt.log.noformat=true \
  "scalusUplcJitCompiler/Test/runMain scalus.uplc.jit.ScalusCorpusSelect $dump $out"
ls -l "$out"
