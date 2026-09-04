// Both packages export `UTxO`/`Utxo`, `Value` and `Asset`. A module that imports both must stay
// readable: the point of the emulator's JS surface is that an adapter author writes one file that
// speaks both vocabularies, so the names have to be separable without ceremony.
import { describe, expect, test } from "vitest";
import * as Scalus from "../scalus.js";
import { MeshTxBuilder, type Asset as MeshAsset, type UTxO as MeshUTxO } from "@meshsdk/core";

describe("scalus and @meshsdk/core coexist in one module", () => {
    test("a namespace import keeps both vocabularies usable side by side", () => {
        const v = Scalus.Value.ada(1n);
        expect(v.coin).toBe(1_000_000n);
        expect(typeof MeshTxBuilder).toBe("function");
    });

    test("the colliding names can be aliased apart and converted between", () => {
        // scalus's Asset is a class with a bigint quantity; mesh's is a plain object with a
        // decimal string. Both are called `Asset`, so a real adapter aliases one of them.
        const scalusAsset = new Scalus.Asset("aa".repeat(28), "1234", 7n);
        const meshAsset: MeshAsset = {
            unit: scalusAsset.unit,
            quantity: scalusAsset.quantity.toString(),
        };
        expect(meshAsset.unit).toBe("aa".repeat(28) + "1234");
        expect(meshAsset.quantity).toBe("7");

        // `unit` is the whole reason this conversion is one line: without it the adapter would
        // concatenate policy id and asset name itself, at every call site.
        expect(scalusAsset.unit).toBe(scalusAsset.policyId + scalusAsset.assetName);

        const utxo = new Scalus.Utxo(
            "00".repeat(32),
            0,
            "addr_test1vzpwq95z3xyum8vqndgdd9mdnmafh3djcxnc6jemlgdmswcve6tkw",
            new Scalus.Value(5_000_000n, [scalusAsset]),
        );
        const meshUtxo: MeshUTxO = {
            input: { txHash: utxo.txHash, outputIndex: utxo.outputIndex },
            output: {
                address: utxo.address,
                amount: [
                    { unit: "lovelace", quantity: utxo.value.coin.toString() },
                    ...utxo.value.assets.map((a) => ({
                        unit: a.unit,
                        quantity: a.quantity.toString(),
                    })),
                ],
            },
        };
        expect(meshUtxo.output.amount).toEqual([
            { unit: "lovelace", quantity: "5000000" },
            { unit: "aa".repeat(28) + "1234", quantity: "7" },
        ]);
    });

    test("named imports of the scalus types do not shadow the mesh ones", async () => {
        // The import that a reader would worry about: both packages' names in one statement.
        const { Utxo, Value } = await import("../scalus.js");
        const { MeshTxBuilder: Builder } = await import("@meshsdk/core");
        const u = new Utxo(
            "11".repeat(32),
            3,
            "addr_test1vzpwq95z3xyum8vqndgdd9mdnmafh3djcxnc6jemlgdmswcve6tkw",
            Value.ada(2n),
        );
        expect(u.outputIndex).toBe(3);
        expect(new Builder({}).txOut(u.address, [])).toBeDefined();
    });
});
