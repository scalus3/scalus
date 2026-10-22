// The emulator as a chain, through a Lucid provider: spec [TE-10].
//
// With `blockProduction: "manual"` a submitted transaction waits in the mempool, as on a real
// network, so code that reads the ledger right after `submit` sees the old state. Lucid's
// `awaitTx` is what makes the outputs appear: the provider's `awaitTx` forges the block.
import { beforeAll, describe, expect, test } from "vitest";
import { Lucid, type LucidEvolution } from "@lucid-evolution/lucid";
import { CardanoInfo, Emulator, Utxo, Value } from "../scalus.js";
import { EmulatorProvider } from "./lucid-provider.js";

const SEED =
    "abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon art";

const BOB = "addr_test1vzpwq95z3xyum8vqndgdd9mdnmafh3djcxnc6jemlgdmswcve6tkw";

describe("a Manual emulator behind a Lucid provider", () => {
    let emulator: Emulator;
    let lucid: LucidEvolution;

    beforeAll(async () => {
        emulator = Emulator.create(CardanoInfo.preview(), { blockProduction: "manual" });
        lucid = await Lucid(new EmulatorProvider(emulator), "Preview");
        lucid.selectWallet.fromSeed(SEED);
        const alice = await lucid.wallet().address();
        emulator.addUtxo(new Utxo("00".repeat(32), 0, alice, Value.ada(1000n)));
    });

    test("the outputs appear after awaitTx, not after submit", async () => {
        // spec [TE-5], [TE-10], [SF-7], [VW-1]
        const tx = await lucid.newTx().pay.ToAddress(BOB, { lovelace: 25_000_000n }).complete();
        const signed = await tx.sign.withWallet().complete();
        const txHash = await signed.submit();

        expect(emulator.getTransactionStatus(txHash)).toBe("Pending");
        expect(await lucid.utxosAt(BOB)).toEqual([]);
        expect(emulator.getTip().height).toBe(0);

        expect(await lucid.awaitTx(txHash)).toBe(true);

        const bobUtxos = await lucid.utxosAt(BOB);
        expect(bobUtxos.map((u) => [u.txHash, u.assets["lovelace"]])).toEqual([
            [txHash, 25_000_000n],
        ]);
        expect(emulator.getTip().height).toBe(1);
        const status = emulator.getTxStatus(txHash);
        expect(status.status).toBe("Confirmed");
        expect(status.depth).toBe(1);
    });
});
