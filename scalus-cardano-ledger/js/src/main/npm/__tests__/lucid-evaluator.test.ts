// __tests__/lucid-evaluator.test.ts
//
// That the two evaluators agree on a budget is `differential.test.ts`'s claim, and it makes it
// script by script on a real mainnet transaction by calling both directly.
//
// This one covers the layer above: that `scalusEvaluator` actually plugs into Lucid, and that a
// transaction built through it is the same transaction. Lucid builds one script-spending
// transaction twice, once with each evaluator, and the two must come out byte-identical - which
// they only can if the execution units matched, since the fee is derived from them.
//
// So: `differential.test.ts` says the numbers are right, this says the wiring is.

import { Lucid, Data } from "@lucid-evolution/lucid";
import { Emulator, generateEmulatorAccount } from "@lucid-evolution/provider";
import { validatorToAddress } from "@lucid-evolution/utils";
import { beforeAll, describe, expect, test } from "vitest";
import type { LucidEvolution, Script, UTxO } from "@lucid-evolution/lucid";
import { scalusEvaluator } from "./lucid-evaluator";
import { successScriptHex } from "./fixtures";

/** An always-succeeds validator: it runs, so it has a real budget, and it never rejects. */
const validator: Script = { type: "PlutusV3", script: successScriptHex };

const account = generateEmulatorAccount({ lovelace: 100_000_000n });

let lucid: LucidEvolution;
let scriptUtxo: UTxO;

beforeAll(async () => {
  const emulator = new Emulator([account]);
  lucid = await Lucid(emulator, "Custom");
  lucid.selectWallet.fromSeed(account.seedPhrase);

  // Lock funds at the validator's address, so there is something a script must approve spending.
  const scriptAddress = validatorToAddress("Custom", validator);
  const lock = await lucid
    .newTx()
    .pay.ToContract(scriptAddress, { kind: "inline", value: Data.void() }, { lovelace: 10_000_000n })
    .complete();
  const signed = await lock.sign.withWallet().complete();
  await signed.submit();
  emulator.awaitBlock(1);

  [scriptUtxo] = await lucid.utxosAt(scriptAddress);
});

/** The spend that runs the script. Built fresh each time so neither build sees the other's state. */
const buildSpend = (evaluator?: typeof scalusEvaluator) =>
  lucid
    .newTx()
    .collectFrom([scriptUtxo], Data.void())
    .attach.SpendingValidator(validator)
    .complete(evaluator ? { evaluator } : undefined);

describe("Scalus as lucid-evolution's evaluator", () => {
  test("produces the same transaction as the bundled Aiken evaluator", async () => {
    const withAiken = await buildSpend();
    const withScalus = await buildSpend(scalusEvaluator);

    // Identical bytes means identical execution units: the fee is computed from them, so any
    // disagreement on cost would move the fee and change the body.
    expect(withScalus.toCBOR()).toBe(withAiken.toCBOR());
  });

  test("the redeemer it reports is the one Lucid asked about", async () => {
    const built = await buildSpend(scalusEvaluator);
    const redeemers = built.toTransaction().witness_set().redeemers();

    expect(redeemers).toBeDefined();
    // One script spent, so one redeemer, and its budget has to be non-zero for the script to
    // have actually run rather than been skipped.
    const units = redeemers!.as_arr_legacy_redeemer()!.get(0).ex_units();
    expect(units.steps()).toBeGreaterThan(0n);
    expect(units.mem()).toBeGreaterThan(0n);
  });
});
