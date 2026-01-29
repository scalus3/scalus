import { describe, it, expect } from "vitest";
import { Emulator, SlotConfig } from "../scalus.js";
import { Decoder } from "cbor-x";

// need to decode utxos to then check them for correctness
const decoder = new Decoder({ mapsAsObjects: false });

function hexToBytes(hex: string): Uint8Array {
  const bytes = new Uint8Array(hex.length / 2);
  for (let i = 0; i < hex.length; i += 2) {
    bytes[i / 2] = parseInt(hex.substring(i, i + 2), 16);
  }
  return bytes;
}

describe("Emulator", () => {
  const aliceAddress =
    "addr_test1vryvgass5dsrf2kxl3vgfz76uhp83kv5lagzcp29tcana6q4a064h";

  const bobAddress =
    "addr_test1vpqy3lufef8c3en9nrnzp2svwy5vy9zangvp46dy4qw23cc48qaad";

  // Alice has 2 UTxOs with 100 ADA and 10 ADA, Bob has no UTxOs
  const initialUtxosCborHex =
    "a282582000000000000000000000000000000000000000000000000000000000000000000082581d60c8c47610a36034aac6fc58848bdae5c278d994ff502c05455e3b3ee81a05f5e10082582000000000000000000000000000000000000000000000000000000000000000000182581d60c8c47610a36034aac6fc58848bdae5c278d994ff502c05455e3b3ee81a00989680";

  // Simple payment of 25 ADA from Alice to Bob
  const txCborHex =
    "84a300d90102818258200000000000000000000000000000000000000000000000000000000000000000000182a200581d604048ff89ca4f88e66598e620aa0c7128c2145d9a181ae9a4a81ca8e3011a017d784082581d60c8c47610a36034aac6fc58848bdae5c278d994ff502c05455e3b3ee81a0475e167021a00028759a100d90102818258206ea31d27d585439ea8fd9cd8e6664ed83e605c06aec24d32dfaba488e49287d958405c08a74aabe9d1d05bde60a0c00468762da45ade5c9fb6854ca007ef80bef8bfaeca178b36e09dfabaa12a582a9f2e4d0807c90c234fd702b07340bf7f6cfb05f5f6";

  function getLovelaceFromUtxo(utxoCbor: Uint8Array): bigint {
    const decoded = decoder.decode(utxoCbor) as Map<unknown, unknown>;
    for (const [, output] of decoded) {
      let value: unknown;
      if (output instanceof Map) {
        value = output.get(1);
      } else if (Array.isArray(output)) {
        value = output[1];
      } else {
        continue;
      }
      if (typeof value === "number" || typeof value === "bigint") {
        return BigInt(value);
      } else if (Array.isArray(value)) {
        return BigInt(value[0] as number);
      }
    }
    return BigInt(0);
  }

  it("should submit transaction and update UTxO state", () => {
    const emulator = new Emulator(
      hexToBytes(initialUtxosCborHex),
      SlotConfig.preview
    );

    const initialAliceUtxos = emulator.getUtxosForAddress(aliceAddress);
    expect(initialAliceUtxos.length).toBe(2);

    const initialBobUtxos = emulator.getUtxosForAddress(bobAddress);
    expect(initialBobUtxos.length).toBe(0);

    const result = emulator.submitTx(hexToBytes(txCborHex));

    expect(result.isSuccess).toBe(true);
    expect(result.txHash).toBeDefined();

    // After transaction:
    // - Alice should have 2 UTxOs: change from tx (~75 ADA) + untouched (10 ADA)
    // - Bob should have 1 UTxO: 25 ADA
    const finalAliceUtxos = emulator.getUtxosForAddress(aliceAddress);
    expect(finalAliceUtxos.length).toBe(2);

    const finalBobUtxos = emulator.getUtxosForAddress(bobAddress);
    expect(finalBobUtxos.length).toBe(1);

    // Bob should have exactly 25 ADA (25_000_000 lovelace)
    const bobLovelace = getLovelaceFromUtxo(finalBobUtxos[0]);
    expect(bobLovelace).toBe(BigInt(25_000_000));

    // Alice's total should be ~85 ADA (110 - 25 - fee)
    const aliceTotal = finalAliceUtxos.reduce(
      (acc, utxo) => acc + getLovelaceFromUtxo(utxo),
      BigInt(0)
    );
    // Alice started with 110 ADA, sent 25 ADA to Bob, paid a fee
    // Should have roughly 85 ADA minus fee (around 84.8 ADA)
    expect(aliceTotal).toBeGreaterThan(BigInt(84_000_000));
    expect(aliceTotal).toBeLessThan(BigInt(85_000_000));
  });

  it("should execute withdraw zero trick with staking script", () => {
    // Alice starts with 5k ADA
    const withdrawZeroInitialUtxosCborHex =
      "a1825820000000000000000000000000000000000000000000000000000000000000000000a200581d61c8c47610a36034aac6fc58848bdae5c278d994ff502c05455e3b3ee8011b000000012a05f200";

    // Transaction CBOR that:
    // 1. Registers a script-based stake address (with 2 ADA deposit)
    // 2. Withdraws 0 ADA from it (triggering the always-ok staking script)
    const withdrawZeroTxCborHex =
      "84a700d9010281825820000000000000000000000000000000000000000000000000000000000000000000018182581d61c8c47610a36034aac6fc58848bdae5c278d994ff502c05455e3b3ee81b0000000129e4ca38021a0002a34804d901028183078201581c186e32faa80a26810392fda6d559c7ed4721a65ce1c9d4ef3e1c87b41a001e848005a1581df1186e32faa80a26810392fda6d559c7ed4721a65ce1c9d4ef3e1c87b4000b58209e57f1863700476178b78bd28fe0c19e1d62fac2690cec07a752aff11ab7edec0dd9010281825820000000000000000000000000000000000000000000000000000000000000000000a300d90102818258206ea31d27d585439ea8fd9cd8e6664ed83e605c06aec24d32dfaba488e49287d95840cdcc746217bd14727aa6be4dc4a37aaebdf3925e806bbc7d8b04cdc9095d95c1fd992b1665b5d66a69dd5052ff0c9286378c444c45d78b15a050f5c0f54a330605a282020082d87980821901f419fa6482030082d87980821901f419fa6407d901028146450101002499f5f6";

    const emulator = new Emulator(
      hexToBytes(withdrawZeroInitialUtxosCborHex),
      SlotConfig.mainnet
    );

    const result = emulator.submitTx(hexToBytes(withdrawZeroTxCborHex));

    expect(result.isSuccess).toBe(true);
    expect(result.txHash).toBeDefined();

    const finalUtxos = emulator.getAllUtxos();
    expect(finalUtxos.length).toBe(1);

    const changeLovelace = getLovelaceFromUtxo(finalUtxos[0]);
    expect(changeLovelace).toBeGreaterThan(BigInt(4_997_000_000));
    expect(changeLovelace).toBeLessThan(BigInt(4_998_000_000));
  });
});
