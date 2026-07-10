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

    it("should expose pre-registered stake reward via getStakeReward", () => {
        const scriptHashHex = "186e32faa80a26810392fda6d559c7ed4721a65ce1c9d4ef3e1c87b4";
        const preRegInitialUtxosCborHex =
            "a182582000000000000000000000000000000000000000000000000000000000000000000082581d61c8c47610a36034aac6fc58848bdae5c278d994ff502c05455e3b3ee81b000000012a05f200";

        const emulator = new Emulator(
            hexToBytes(preRegInitialUtxosCborHex),
            SlotConfig.mainnet,
            { [scriptHashHex]: "42000000" }
        );

        expect(emulator.getStakeReward(scriptHashHex)).toBe(BigInt(42_000_000));
        expect(emulator.getStakeReward("0".repeat(56))).toBeNull();
    });

    it("should execute withdraw zero trick with pre-registered stake credential", () => {
        // alwaysOk PlutusV3 script hash
        const scriptHashHex = "186e32faa80a26810392fda6d559c7ed4721a65ce1c9d4ef3e1c87b4";

        // Alice starts with 5k ADA
        const preRegInitialUtxosCborHex =
            "a182582000000000000000000000000000000000000000000000000000000000000000000082581d61c8c47610a36034aac6fc58848bdae5c278d994ff502c05455e3b3ee81b000000012a05f200";

        // Withdraw 0 ADA directly, without a pre-registration tx
        const preRegWithdrawTxCborHex =
            "84a600d9010281825820000000000000000000000000000000000000000000000000000000000000000000018182581d61c8c47610a36034aac6fc58848bdae5c278d994ff502c05455e3b3ee81b000000012a0358d1021a0002992f05a1581df1186e32faa80a26810392fda6d559c7ed4721a65ce1c9d4ef3e1c87b4000b5820287025a07dedd8bf2c746203011b09c4841fb62d219da4944652ba95b5e76f040dd9010281825820000000000000000000000000000000000000000000000000000000000000000000a300d90102818258206ea31d27d585439ea8fd9cd8e6664ed83e605c06aec24d32dfaba488e49287d9584016198d6e2c1dd6886b5f059e92bdf7a5a898498133c85d198327e44723de03a6a16071d7907f2724cb73a6540b4dd15780fe0f277f341fa50c3ee7b250e73b0a05a182030082d87980821901f419fa6407d901028146450101002499f5f6";

        const emulator = new Emulator(
            hexToBytes(preRegInitialUtxosCborHex),
            SlotConfig.mainnet,
            { [scriptHashHex]: "0" }
        );

        const result = emulator.submitTx(hexToBytes(preRegWithdrawTxCborHex));
        expect(result.isSuccess).toBe(true);
        expect(result.txHash).toBeDefined();
    });

    it("should execute withdraw zero trick with staking script", () => {
        // Alice starts with 5k ADA
        const withdrawZeroInitialUtxosCborHex =
            "a182582000000000000000000000000000000000000000000000000000000000000000000082581d61c8c47610a36034aac6fc58848bdae5c278d994ff502c05455e3b3ee81b000000012a05f200";

        // Transaction 1: Register a script-based stake address (with 2 ADA deposit)
        const registerTxCborHex =
            "84a600d9010281825820000000000000000000000000000000000000000000000000000000000000000000018182581d61c8c47610a36034aac6fc58848bdae5c278d994ff502c05455e3b3ee81b0000000129e4d299021a00029ae704d901028183078201581c186e32faa80a26810392fda6d559c7ed4721a65ce1c9d4ef3e1c87b41a001e84800b5820c8c7e0b860635aab3c57a4e90cc56d2ee8043baf0e0256058af5e197a5e391ea0dd9010281825820000000000000000000000000000000000000000000000000000000000000000000a300d90102818258206ea31d27d585439ea8fd9cd8e6664ed83e605c06aec24d32dfaba488e49287d9584016515bf5f7b9ec86344001ff5a9224088f273afcd64f1799e5da210d15f600585e0dbd3619a02a1f9454bf77ae0bc1bfb8ceeab58a9a7145e6a2f3bbc418a40105a182020082d87980821901f419fa6407d901028146450101002499f5f6";

        // Transaction 2: Withdraw 0 ADA from the stake address (the "withdraw zero trick")
        const withdrawZeroTxCborHex =
            "84a600d9010281825820f215edd7658290871ad3bc3e2771d87d3c2f74b8b4ccca4adb628ea61f293a0b00018182581d61c8c47610a36034aac6fc58848bdae5c278d994ff502c05455e3b3ee81b0000000129e2396a021a0002992f05a1581df1186e32faa80a26810392fda6d559c7ed4721a65ce1c9d4ef3e1c87b4000b5820287025a07dedd8bf2c746203011b09c4841fb62d219da4944652ba95b5e76f040dd9010281825820f215edd7658290871ad3bc3e2771d87d3c2f74b8b4ccca4adb628ea61f293a0b00a300d90102818258206ea31d27d585439ea8fd9cd8e6664ed83e605c06aec24d32dfaba488e49287d9584065a92a36b0776cf0865138709f1e9bb6ba2e661c4ae465fe542513724230ee8f113107df0019ea8af1fc9bf92824cc9dc8ec00e42df9af46cd22e4408ae6a40705a182030082d87980821901f419fa6407d901028146450101002499f5f6";

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

    it("should track the current slot via getSlot/setSlot/tick", () => {
        const emulator = new Emulator(
            hexToBytes(initialUtxosCborHex),
            SlotConfig.preprod
        );

        expect(Number(emulator.getSlot())).toBe(0);

        emulator.setSlot(1_000_000);
        expect(Number(emulator.getSlot())).toBe(1_000_000);

        emulator.tick(5);
        expect(Number(emulator.getSlot())).toBe(1_000_005);

        // epochOf(slot) lets clients derive the current epoch for e.g. pool retirement
        const epoch = Number(SlotConfig.preprod.epochOf(emulator.getSlot()));
        expect(epoch).toBe(4 + Math.floor((1_000_005 - 86_400) / 432_000));
    });
});
