import {describe, it, expect} from "vitest";
import {Emulator, SlotConfig} from "../scalus.js";
import {Decoder} from "cbor-x";

// need to decode utxos to then check them for correctness
const decoder = new Decoder({mapsAsObjects: false});

function extractLovelaceFromObject(utxo: unknown): bigint | undefined {
    if (typeof utxo !== "object" || utxo === null) return undefined;
    const lovelace = (utxo as { lovelace?: unknown }).lovelace;
    if (typeof lovelace === "bigint" || typeof lovelace === "number") {
        return BigInt(lovelace);
    }
    return undefined;
}

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

    function getLovelaceFromUtxo(utxo: unknown): bigint {
        const objectValue = extractLovelaceFromObject(utxo);
        if (objectValue !== undefined) return objectValue;
        const decoded = decoder.decode(utxo) as Map<unknown, unknown>;
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
            "a182582000000000000000000000000000000000000000000000000000000000000000000082581d61c8c47610a36034aac6fc58848bdae5c278d994ff502c05455e3b3ee81b000000012a05f200";

        // Transaction 1: Register a script-based stake address (with 2 ADA deposit)
        const registerTxCborHex =
            "84a600d9010281825820000000000000000000000000000000000000000000000000000000000000000000018182581d61c8c47610a36034aac6fc58848bdae5c278d994ff502c05455e3b3ee81b0000000129e4d299021a00029ae704d901028183078201581c186e32faa80a26810392fda6d559c7ed4721a65ce1c9d4ef3e1c87b41a001e84800b58203f860d65ff8d3c971dd10e075df60cc0221e9c5e7612d15563d446aa39f05efc0dd9010281825820000000000000000000000000000000000000000000000000000000000000000000a300d90102818258206ea31d27d585439ea8fd9cd8e6664ed83e605c06aec24d32dfaba488e49287d9584071088231449aeedbdd6e4b1fcc1c695d43e401f78c9c60e78eac3b3cb12719fd6d8c736df0ff29d3d631ae5580f3a2ae6f4a8099dc80cd938affac5a685c5d0b05a182020082d87980821901f419fa6407d901028146450101002499f5f6";

        // Transaction 2: Withdraw 0 ADA from the stake address (the "withdraw zero trick")
        const withdrawZeroTxCborHex =
            "84a600d9010281825820d2519d4e68eee3731a77c9a63dc7cfcfa5afec2fcb9710c58653713972cef84900018182581d61c8c47610a36034aac6fc58848bdae5c278d994ff502c05455e3b3ee81b0000000129e2396a021a0002992f05a1581df1186e32faa80a26810392fda6d559c7ed4721a65ce1c9d4ef3e1c87b4000b58203915d1a691174fddd60c82e48d3e9eee08ecb12fc510bc2ee8256de2a0e44a0a0dd9010281825820d2519d4e68eee3731a77c9a63dc7cfcfa5afec2fcb9710c58653713972cef84900a300d90102818258206ea31d27d585439ea8fd9cd8e6664ed83e605c06aec24d32dfaba488e49287d95840ec4631fe298e7867847d6f570b4a5d2e72ff407b8dcba8658148e3075e775dacb4f2c8fd9a67ee7b0912a86766b4b4a240ce8cfb5377323d682d52be3a7e750305a182030082d87980821901f419fa6407d901028146450101002499f5f6";

        const emulator = new Emulator(
            hexToBytes(withdrawZeroInitialUtxosCborHex),
            SlotConfig.mainnet
        );

        // Step 1: Register the stake address
        const registerResult = emulator.submitTx(hexToBytes(registerTxCborHex));
        expect(registerResult.isSuccess).toBe(true);
        expect(registerResult.txHash).toBeDefined();

        // Step 2: Withdraw 0 ADA (triggering the always-ok staking script)
        const withdrawResult = emulator.submitTx(
            hexToBytes(withdrawZeroTxCborHex)
        );
        expect(withdrawResult.isSuccess).toBe(true);
        expect(withdrawResult.txHash).toBeDefined();

        const finalUtxos = emulator.getAllUtxos();
        expect(finalUtxos.length).toBe(1);

        const changeLovelace = getLovelaceFromUtxo(finalUtxos[0]);
        // Alice started with 5k ADA, paid 2 ADA deposit + two tx fees
        expect(changeLovelace).toBeGreaterThan(BigInt(4_995_000_000));
        expect(changeLovelace).toBeLessThan(BigInt(4_998_000_000));
    });
});
