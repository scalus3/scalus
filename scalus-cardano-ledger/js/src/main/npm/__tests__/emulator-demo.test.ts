import { describe, it, expect } from "vitest";
import {
  MeshTxBuilder,
  MeshWallet,
} from "@meshsdk/core";
import { DEFAULT_PROTOCOL_PARAMETERS } from "@meshsdk/common";
import { Emulator, SlotConfig } from "../scalus.js";
import { Decoder } from "cbor-x";

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

function hexToBytes(hex: string): Uint8Array {
  const bytes = new Uint8Array(hex.length / 2);
  for (let i = 0; i < hex.length; i += 2) {
    bytes[i / 2] = parseInt(hex.substring(i, i + 2), 16);
  }
  return bytes;
}

const cborDecoder = new Decoder({ mapsAsObjects: false });

/** Decode a CBOR UTxO entry returned by the emulator. */
function decodeUtxoRef(utxoCbor: Uint8Array): {
  txHash: string;
  txIndex: number;
  lovelace: bigint;
} {
  const decoded = cborDecoder.decode(utxoCbor) as Map<unknown, unknown>;
  const [[txIn, txOut]] = decoded.entries();
  const txHash = Buffer.from(txIn[0] as Uint8Array).toString("hex");
  const txIndex = txIn[1] as number;
  // TxOut is a CBOR map (key 1 = value) or could be decoded as object by cbor-x
  let lovelace: bigint;
  if (txOut instanceof Map) {
    const value = txOut.get(1);
    lovelace = typeof value === "bigint" ? value : BigInt(value as number);
  } else {
    // cbor-x may decode small integer-keyed maps as arrays
    const value = (txOut as any)[1] ?? (txOut as any)["1"];
    lovelace = typeof value === "bigint" ? value : BigInt(value as number);
  }
  return { txHash, txIndex, lovelace };
}

const MNEMONIC =
  "test test test test test test test test test test test test test test test test test test test test test test test sauce".split(
    " "
  );

const wallet = new MeshWallet({
  networkId: 0,
  key: { type: "mnemonic", words: MNEMONIC },
});

const ALICE_ADDRESS =
  "addr_test1qryvgass5dsrf2kxl3vgfz76uhp83kv5lagzcp29tcana68ca5aqa6swlq6llfamln09tal7n5kvt4275ckwedpt4v7q48uhex";
const BOB_ADDRESS =
  "addr_test1qpqy3lufef8c3en9nrnzp2svwy5vy9zangvp46dy4qw23clgfxhn3pqv243d6wptud7fuaj5tjqer7wc7m036gx0emsqaqa8te";

const TEN_K_ADA = "10000000000";

describe("", () => {

  it("Demo 1: Simple ADA transfer succeeds", async () => {

    // 1. Bootstrap emulator with Alice holding 10 000 ADA
    const emulator = Emulator.withAddresses(
      [ALICE_ADDRESS],
      SlotConfig.preview,
      BigInt(10_000_000_000)
    );

    // 2. Read Alice's UTxO from the emulator
    const aliceUtxos = emulator.getUtxosForAddress(ALICE_ADDRESS);
    expect(aliceUtxos.length).toBe(1);
    const { txHash, txIndex } = decodeUtxoRef(aliceUtxos[0]);

    // 3. Build tx offline: Alice → 25 ADA → Bob
    const txBuilder = new MeshTxBuilder({ params: DEFAULT_PROTOCOL_PARAMETERS });
    const unsignedTx = await txBuilder
      .txIn(txHash, txIndex, [{ unit: "lovelace", quantity: TEN_K_ADA }], ALICE_ADDRESS, 0)
      .txOut(BOB_ADDRESS, [{ unit: "lovelace", quantity: "25000000" }])
      .changeAddress(ALICE_ADDRESS)
      .complete();

    // 4. Sign with Alice's key
    const signedTx = await wallet.signTx(unsignedTx);

    // 5. Submit to Scalus emulator (full Cardano ledger validation)
    const result = emulator.submitTx(hexToBytes(signedTx));

    expect(result.isSuccess).toBe(true);
    expect(result.txHash).toBeDefined();

    // 6. Verify UTxO state
    const finalBobUtxos = emulator.getUtxosForAddress(BOB_ADDRESS);
    expect(finalBobUtxos.length).toBe(1);
    console.log("Bob utxos:")
    console.log(decodeUtxoRef(finalBobUtxos[0]))
    const finalAliceUtxos = emulator.getUtxosForAddress(ALICE_ADDRESS);
    expect(finalAliceUtxos.length).toBe(1); // change
    console.log("Alice utxos:")
    console.log(decodeUtxoRef(finalAliceUtxos[0]))
  });

  // -------------------------------------------------------------------------
  // Demo 2: Double spend is rejected
  // -------------------------------------------------------------------------

  it("Demo 2: Double spend is rejected", async () => {
    const emulator = Emulator.withAddresses(
      [ALICE_ADDRESS],
      SlotConfig.preview,
      BigInt(10_000_000_000)
    );

    const { txHash, txIndex } = decodeUtxoRef(
      emulator.getUtxosForAddress(ALICE_ADDRESS)[0]
    );

    const txBuilder = new MeshTxBuilder({ params: DEFAULT_PROTOCOL_PARAMETERS });
    const unsignedTx = await txBuilder
      .txIn(txHash, txIndex, [{ unit: "lovelace", quantity: TEN_K_ADA }], ALICE_ADDRESS, 0)
      .txOut(BOB_ADDRESS, [{ unit: "lovelace", quantity: "25000000" }])
      .changeAddress(ALICE_ADDRESS)
      .complete();

    const signedTx = await wallet.signTx(unsignedTx);
    const txBytes = hexToBytes(signedTx);

    // First submit succeeds
    const result1 = emulator.submitTx(txBytes);
    expect(result1.isSuccess).toBe(true);

    // Second submit of the exact same tx — inputs already consumed
    const result2 = emulator.submitTx(txBytes);
    console.log("Demo 2 – double spend error:", result2.error);

    expect(result2.isSuccess).toBe(false);
    expect(result2.error).toBeDefined();
    console.log(result2.error)
  });

  it("Demo 3: Failing Plutus script returns trace logs", () => {
    // Pre-built tx generated by EmulatorDemoCborGenerator.scala:
    // Step 1: Alice locks 5 ADA at a script address with an inline datum
    // Step 2: A tx tries to spend that script UTxO using an always-failing PlutusV3 script
    // The script traces "Hello Scalus JS Emulator Demo" then throws.
    const failingTxCborHex =
      "84a500d9010282825820bc1bd5565ca23560436ca0d25a4022902eaa9ecce69a2e389d3af0d2263ac6a300825820bc1bd5565ca23560436ca0d25a4022902eaa9ecce69a2e389d3af0d2263ac6a3010182a200581d61c8c47610a36034aac6fc58848bdae5c278d994ff502c05455e3b3ee8011a004c4b4082581d61c8c47610a36034aac6fc58848bdae5c278d994ff502c05455e3b3ee81b0000000253a30fae021a0019ff7d0b5820d1ad7db6369abb5f4f93aaf3971fa5aab551e11fc451401c88a6332de4bf37d30dd9010281825820bc1bd5565ca23560436ca0d25a4022902eaa9ecce69a2e389d3af0d2263ac6a301a300d90102818258206ea31d27d585439ea8fd9cd8e6664ed83e605c06aec24d32dfaba488e49287d958403ef69f3e9cd4761f6eefb4512508fd8fd4dff52d6e674b33c5c5153c19a9ad6a3ece59dc54649849f473fc27b6f646b9b0c8de44f91e0e9a12459e6170d48d0305a182000082d87980821a00d59f801b00000002540be40007d90102815840583e010100232533573892010b5468726f77696e672e2e2e001633573892011d48656c6c6f205363616c7573204a5320456d756c61746f722044656d6f004981f5f6";

    // UTxO set after the lock tx: script UTxO (with inline datum) + Alice's change UTxO
    const failingUtxoCborHex =
      "a2825820bc1bd5565ca23560436ca0d25a4022902eaa9ecce69a2e389d3af0d2263ac6a300a300581d71cc85cb2deff7206d3d2658a3077d5abec0aa38a2e7ee7b76909dbe3b011a004c4b40028201d81843d87980825820bc1bd5565ca23560436ca0d25a4022902eaa9ecce69a2e389d3af0d2263ac6a30182581d61c8c47610a36034aac6fc58848bdae5c278d994ff502c05455e3b3ee81b0000000253bd0f2b";

    // Bootstrap emulator with the pre-built UTxO set
    const emulator = new Emulator(hexToBytes(failingUtxoCborHex), SlotConfig.mainnet);

    // Submit the tx that tries to spend the script UTxO — the script runs and fails
    const result = emulator.submitTx(hexToBytes(failingTxCborHex));

    console.log("Demo 3 – script error:", result.error);
    console.log("Demo 3 – script logs:", result.logs);

    expect(result.isSuccess).toBe(false);
    expect(result.logs).toBeDefined();
    expect(result.logs!.join(" ")).toContain("Hello Scalus JS Emulator Demo");
    console.log(result)
  });

  // -------------------------------------------------------------------------
  // Demo 4: Transaction outside validity interval is rejected
  // -------------------------------------------------------------------------

  it("Demo 4: Transaction outside validity interval is rejected", async () => {
    const emulator = Emulator.withAddresses(
      [ALICE_ADDRESS],
      SlotConfig.preview,
      BigInt(10_000_000_000)
    );

    const { txHash, txIndex } = decodeUtxoRef(
      emulator.getUtxosForAddress(ALICE_ADDRESS)[0]
    );

    // Build tx valid only in slot range [100, 200]
    const txBuilder = new MeshTxBuilder({ params: DEFAULT_PROTOCOL_PARAMETERS });
    const unsignedTx = await txBuilder
      .txIn(txHash, txIndex, [{ unit: "lovelace", quantity: TEN_K_ADA }], ALICE_ADDRESS, 0)
      .txOut(BOB_ADDRESS, [{ unit: "lovelace", quantity: "25000000" }])
      .invalidBefore(100)
      .invalidHereafter(200)
      .changeAddress(ALICE_ADDRESS)
      .complete();

    const signedTx = await wallet.signTx(unsignedTx);

    // Advance the emulator past the validity window
    emulator.setSlot(500);

    const result = emulator.submitTx(hexToBytes(signedTx));
    console.log("Demo 4 – validity error:", result.error);

    expect(result.isSuccess).toBe(false);
    expect(result.error).toBeDefined();
  });
});
