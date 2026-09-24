// __tests__/lucid-evaluator-errors.test.ts
//
// What a Lucid user sees when a script fails under the Scalus evaluator. Lucid reduces an
// evaluator's error to its message, so the message alone has to say which redeemer failed and
// what the script traced.

import { Lucid, Data } from "@lucid-evolution/lucid";
import { Emulator, generateEmulatorAccount } from "@lucid-evolution/provider";
import { validatorToAddress } from "@lucid-evolution/utils";
import { beforeAll, describe, expect, test } from "vitest";
import type { LucidEvolution, Script, UTxO } from "@lucid-evolution/lucid";
import { scalusEvaluator } from "./lucid-evaluator";
import { tracingFailScriptHex } from "./fixtures";

const validator: Script = { type: "PlutusV3", script: tracingFailScriptHex };
const account = generateEmulatorAccount({ lovelace: 100_000_000n });

let lucid: LucidEvolution;
let scriptUtxo: UTxO;

beforeAll(async () => {
  const emulator = new Emulator([account]);
  lucid = await Lucid(emulator, "Custom");
  lucid.selectWallet.fromSeed(account.seedPhrase);
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

const rejection = () =>
  lucid
    .newTx()
    .collectFrom([scriptUtxo], Data.void())
    .attach.SpendingValidator(validator)
    .complete({ evaluator: scalusEvaluator })
    .then(() => undefined, (error: unknown) => error as Error);

describe("a failing script through Lucid", () => {
  test("names the redeemer and carries the trace in the message", async () => {
    // spec [MSG-1] [MSG-5]
    const error = await rejection();
    expect(error).toBeDefined();
    expect(error!.message).toContain("Spend[0] failed: ");
    expect(error!.message).toMatch(/logs:\nboom/);
  });
});
