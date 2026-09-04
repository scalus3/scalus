// The emulator as a MeshJS provider backend.
//
// This is the claim a PR to MeshJS would make, so it is tested here first: an adapter that turns
// the emulator into mesh's `IFetcher` + `ISubmitter` + `IEvaluator` needs no CBOR codec, no
// protocol-parameter table and no evaluation plumbing of its own. Everything below the
// `EmulatorProvider` class is a real transaction: built by `MeshTxBuilder`, signed by a
// `MeshWallet`, submitted to the emulator, and read back out of the ledger it changed.
import { beforeAll, describe, expect, test } from "vitest";
import {
    type Action,
    type Asset as MeshAsset,
    type IEvaluator,
    type IFetcher,
    type ISubmitter,
    MeshTxBuilder,
    MeshWallet,
    type Protocol,
    type RedeemerTagType,
    type UTxO as MeshUTxO,
    castProtocol,
} from "@meshsdk/core";
import { Asset, CardanoInfo, Emulator, Utxo, Value } from "../scalus.js";
import { hexToBytes, scriptTxCborHex, scriptUtxoCborHex } from "./fixtures.js";

function bytesToHex(bytes: Uint8Array): string {
    return Array.from(bytes, (b) => b.toString(16).padStart(2, "0")).join("");
}

/** `"Spend"` and friends, as mesh spells them. */
const MESH_TAG: Record<string, RedeemerTagType> = {
    Spend: "SPEND",
    Mint: "MINT",
    Cert: "CERT",
    Reward: "REWARD",
    Voting: "VOTE",
    Proposing: "PROPOSE",
};

function toMeshUtxo(utxo: Utxo): MeshUTxO {
    const amount: MeshAsset[] = [
        { unit: "lovelace", quantity: utxo.value.coin.toString() },
        ...utxo.value.assets.map((a) => ({ unit: a.unit, quantity: a.quantity.toString() })),
    ];
    return {
        input: { txHash: utxo.txHash, outputIndex: utxo.outputIndex },
        output: {
            address: utxo.address,
            amount,
            dataHash: utxo.datumHash,
            plutusData: utxo.inlineDatum && bytesToHex(utxo.inlineDatum),
            scriptRef: utxo.scriptRef && bytesToHex(utxo.scriptRef),
        },
    };
}

function fromMeshUtxo(utxo: MeshUTxO): Utxo {
    const lovelace = utxo.output.amount.find((a) => a.unit === "lovelace");
    const assets = utxo.output.amount
        .filter((a) => a.unit !== "lovelace")
        .map((a) => new Asset(a.unit.slice(0, 56), a.unit.slice(56), BigInt(a.quantity)));
    const value = new Value(BigInt(lovelace?.quantity ?? "0"), assets);
    const { dataHash, plutusData, scriptRef } = utxo.output;
    if (dataHash && plutusData) {
        // Ambiguous, and not ours to guess: a hash-datum output whose datum has since been
        // resolved, and an inline-datum output whose hash a Blockfrost-shaped fetcher filled in as
        // well, arrive here identically. The two are different ledger outputs - different
        // serialised size, different min-ada, a different script context - so an adapter that
        // picked one would be right half the time and silently wrong the other half.
        throw new Error("a UTxO carrying both a datum hash and a datum is ambiguous here");
    }
    const base = new Utxo(utxo.input.txHash, utxo.input.outputIndex, utxo.output.address, value);
    // Neither the datum nor the reference script may be dropped here. An inline datum is how a
    // script UTxO carries its state, so an input that reaches `evaluateTx` without it builds a
    // script context that is missing it - a wrong budget, or a phase-2 failure that reads like a
    // validator bug. Mesh carries both as the ledger's own CBOR, so this really is a rename.
    const withDatum = plutusData
        ? base.withInlineDatum(hexToBytes(plutusData))
        : dataHash
          ? base.withDatumHash(dataHash)
          : base;
    return scriptRef ? withDatum.withScriptRef(hexToBytes(scriptRef)) : withDatum;
}

function notNeeded(method: string): never {
    throw new Error(`${method} is not part of what a transaction build asks a provider for`);
}

/**
 * The whole adapter. Every method below is a field rename: no CBOR is decoded here, no protocol
 * parameter is transcribed from a table, and no cost model is hard-coded.
 */
class EmulatorProvider implements IFetcher, ISubmitter, IEvaluator {
    constructor(readonly emulator: Emulator) {}

    async fetchProtocolParameters(epoch = 0): Promise<Protocol> {
        const p = this.emulator.getProtocolParameters();
        // `castProtocol` fills in the parameters a transaction build never reads (block sizes,
        // decentralisation, min pool cost) from mesh's own defaults.
        return castProtocol({
            epoch,
            minFeeA: p.txFeePerByte,
            minFeeB: p.txFeeFixed,
            maxTxSize: p.maxTxSize,
            maxValSize: p.maxValueSize,
            keyDeposit: p.stakeAddressDeposit.toString(),
            poolDeposit: p.stakePoolDeposit.toString(),
            coinsPerUtxoSize: Number(p.utxoCostPerByte),
            priceMem: p.priceMemory,
            priceStep: p.priceSteps,
            maxTxExMem: p.maxTxExecutionMemory.toString(),
            maxTxExSteps: p.maxTxExecutionSteps.toString(),
            collateralPercent: p.collateralPercentage,
            maxCollateralInputs: p.maxCollateralInputs,
            minFeeRefScriptCostPerByte: p.minFeeRefScriptCostPerByte,
        });
    }

    async fetchAddressUTxOs(address: string, asset?: string): Promise<MeshUTxO[]> {
        const filter = asset === undefined ? { address } : { address, unit: asset };
        return this.emulator.getUtxos(filter).map(toMeshUtxo);
    }

    async fetchUTxOs(hash: string, index?: number): Promise<MeshUTxO[]> {
        const utxos = this.emulator.getUtxos({ txHash: hash });
        const matching = index === undefined ? utxos : utxos.filter((u) => u.outputIndex === index);
        return matching.map(toMeshUtxo);
    }

    async submitTx(txHex: string): Promise<string> {
        const result = this.emulator.submitTx(hexToBytes(txHex));
        if (!result.isSuccess) {
            throw new Error(`${result.errorRule}: ${result.error} ${result.logs.join(" ")}`);
        }
        return result.txHash!;
    }

    async evaluateTx(
        txHex: string,
        additionalUtxos: MeshUTxO[] = [],
    ): Promise<Omit<Action, "data">[]> {
        const budgets = this.emulator.evaluateTx(
            hexToBytes(txHex),
            additionalUtxos.map(fromMeshUtxo),
        );
        return budgets.map((r) => ({
            tag: MESH_TAG[r.tag]!,
            index: r.index,
            budget: { mem: Number(r.budget.memory), steps: Number(r.budget.steps) },
        }));
    }

    async fetchCostModels(): Promise<number[][]> {
        const { PlutusV1, PlutusV2, PlutusV3 } = this.emulator.getProtocolParameters().costModels;
        return [PlutusV1, PlutusV2, PlutusV3];
    }

    // The rest of IFetcher is chain-explorer surface, not build surface. Left unimplemented on
    // purpose: what a transaction build actually asks for is everything above.
    async fetchAccountInfo(): Promise<never> {
        return notNeeded("fetchAccountInfo");
    }
    async fetchAddressTxs(): Promise<never> {
        return notNeeded("fetchAddressTxs");
    }
    async fetchAssetAddresses(): Promise<never> {
        return notNeeded("fetchAssetAddresses");
    }
    async fetchAssetMetadata(): Promise<never> {
        return notNeeded("fetchAssetMetadata");
    }
    async fetchBlockInfo(): Promise<never> {
        return notNeeded("fetchBlockInfo");
    }
    async fetchCollectionAssets(): Promise<never> {
        return notNeeded("fetchCollectionAssets");
    }
    async fetchTxInfo(): Promise<never> {
        return notNeeded("fetchTxInfo");
    }
    async fetchGovernanceProposal(): Promise<never> {
        return notNeeded("fetchGovernanceProposal");
    }
    async get(): Promise<never> {
        return notNeeded("get");
    }
}

const MNEMONIC =
    "abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon art".split(
        " ",
    );

const BOB = "addr_test1vzpwq95z3xyum8vqndgdd9mdnmafh3djcxnc6jemlgdmswcve6tkw";

describe("Emulator as a MeshJS provider backend", () => {
    let emulator: Emulator;
    let provider: EmulatorProvider;
    let wallet: MeshWallet;
    let alice: string;

    beforeAll(async () => {
        emulator = Emulator.create(CardanoInfo.preview());
        provider = new EmulatorProvider(emulator);
        wallet = new MeshWallet({
            networkId: 0,
            fetcher: provider,
            submitter: provider,
            key: { type: "mnemonic", words: MNEMONIC },
        });
        await wallet.init();
        alice = await wallet.getChangeAddress();
        // Seed the wallet directly: `addUtxo` skips validation, so no genesis transaction is
        // needed just to have money.
        emulator.addUtxo(new Utxo("00".repeat(32), 0, alice, Value.ada(1000n)));
    });

    test("serves protocol parameters, UTxOs and cost models without hand-rolled codecs", async () => {
        const params = await provider.fetchProtocolParameters();
        expect(params.minFeeA).toBe(emulator.getProtocolParameters().txFeePerByte);
        expect(params.minFeeA).toBeGreaterThan(0);
        expect(params.coinsPerUtxoSize).toBeGreaterThan(0);

        const costModels = await provider.fetchCostModels();
        expect(costModels[2]!.length).toBeGreaterThan(200); // PlutusV3, van Rossem

        const utxos = await provider.fetchAddressUTxOs(alice);
        expect(utxos.length).toBe(1);
        expect(utxos[0]!.output.amount).toEqual([{ unit: "lovelace", quantity: "1000000000" }]);
        expect(utxos[0]!.output.address).toBe(alice);

        // The Blockfrost rendering is a separate, ready-made path for adapters that already
        // parse that shape - and its snake_case keys carry the same numbers, with the JSON
        // types Blockfrost's own schema names. The `typeof` checks are the point: these fields
        // shipped as JSON strings until ujson's implicit `Long -> Str` conversion was found,
        // and a value assertion alone cannot see that.
        const json = JSON.parse(emulator.getProtocolParameters().toBlockfrostJson());
        expect(typeof json.min_fee_a).toBe("number");
        expect(json.min_fee_a).toBe(params.minFeeA);
        expect(typeof json.max_tx_size).toBe("number");
        expect(json.max_tx_size).toBe(params.maxTxSize);
        // and the lovelace-scale fields stay strings, because they can exceed 2^53
        expect(typeof json.key_deposit).toBe("string");
    });

    test("builds, signs and submits a transaction that changes the emulator's ledger", async () => {
        const before = emulator.getUtxos({ address: alice });
        expect(before.length).toBe(1);
        expect(emulator.getUtxos({ address: BOB }).length).toBe(0);

        const params = emulator.getProtocolParameters();
        const unsigned = await new MeshTxBuilder({
            fetcher: provider,
            submitter: provider,
            evaluator: provider,
        })
            // The emulator's own cost models, so the builder prices scripts exactly as the
            // ledger it is about to submit to will.
            .setNetwork([params.costModels.PlutusV1, params.costModels.PlutusV2, params.costModels.PlutusV3])
            .txOut(BOB, [{ unit: "lovelace", quantity: "25000000" }])
            .changeAddress(alice)
            .selectUtxosFrom(await provider.fetchAddressUTxOs(alice))
            .complete();

        const signed = await wallet.signTx(unsigned);
        const txHash = await provider.submitTx(signed);

        // The ledger moved, and the emulator - not the builder - is the thing that says so.
        expect(emulator.hasTx(txHash)).toBe(true);
        expect(emulator.getTransactionStatus(txHash)).toBe("Confirmed");

        const bobUtxos = emulator.getUtxos({ address: BOB });
        expect(bobUtxos.length).toBe(1);
        expect(bobUtxos[0]!.value.coin).toBe(25_000_000n);
        expect(bobUtxos[0]!.txHash).toBe(txHash);

        const aliceUtxos = emulator.getUtxos({ address: alice });
        expect(aliceUtxos.length).toBe(1);
        const change = aliceUtxos[0]!.value.coin;
        const fee = 1_000_000_000n - 25_000_000n - change;
        // The fee has to clear the ledger's own minimum for a transaction of this size, computed
        // from the parameters the adapter served. A builder that fell back to its own defaults, or
        // an emulator serving the wrong network's parameters, misses this bound.
        const minFee =
            BigInt(params.txFeeFixed) + BigInt(params.txFeePerByte) * BigInt(signed.length / 2);
        expect(fee).toBeGreaterThanOrEqual(minFee);
        expect(fee).toBeLessThan(minFee * 2n);

        // The transaction the emulator kept is the one that was submitted.
        expect(bytesToHex(emulator.getTransaction(txHash)!)).toBe(signed);
    });

    test("the round trip through mesh's UTxO type keeps the datum and the reference script", () => {
        const plain = Utxo.fromCbor(hexToBytes(scriptUtxoCborHex));
        // The fixture is a bare `[address, coin]` output, so a round trip over it alone cannot
        // tell a converter that carries the datum and script fields from one that drops them.
        // These are the shapes that can. `d8184482034100` is a one-byte PlutusV3 script as a
        // ledger `script_ref`, and `182a` is CBOR for the integer 42.
        const inline = plain.withInlineDatum(hexToBytes("182a"));
        for (const utxo of [
            plain,
            inline,
            plain.withDatumHash("ab".repeat(32)),
            plain.withScriptRef(hexToBytes("d8184482034100")),
            inline.withScriptRef(hexToBytes("d8184482034100")),
        ]) {
            expect(fromMeshUtxo(toMeshUtxo(utxo)).toObject()).toEqual(utxo.toObject());
        }

        // And the case no adapter can resolve: both a datum hash and a datum.
        const both = toMeshUtxo(inline);
        both.output.dataHash = "ab".repeat(32);
        expect(() => fromMeshUtxo(both)).toThrow(/ambiguous/);
    });

    test("IEvaluator returns real budgets for UTxOs the emulator does not hold", async () => {
        // `evaluateTx`'s second parameter is exactly mesh's `additionalUtxos`: inputs of a
        // transaction that is not on the ledger yet.
        const scriptUtxo = toMeshUtxo(Utxo.fromCbor(hexToBytes(scriptUtxoCborHex)));
        const budgets = await provider.evaluateTx(scriptTxCborHex, [scriptUtxo]);
        // One withdrawal guarded by a PlutusV3 validator, so exactly one Reward redeemer.
        // The budget is pinned, and the fixture's validator reads its ScriptContext (see
        // fixtures.ts), so these numbers move if the script context, the redeemer or the
        // cost models handed to the evaluator are wrong. `DifferentialFixtureGenTest`
        // pins the same pair on the Scala side.
        expect(budgets).toEqual([
            { tag: "REWARD", index: 0, budget: { mem: 32_318, steps: 8_754_898 } },
        ]);
    });

    test("a transaction the ledger rejects surfaces the condition that rejected it", async () => {
        // Build and submit a valid transaction, then submit the very same bytes a second time.
        // Its input is spent by then, so the ledger rejects it and names the condition.
        const unsigned = await new MeshTxBuilder({ fetcher: provider, submitter: provider })
            .txOut(BOB, [{ unit: "lovelace", quantity: "5000000" }])
            .changeAddress(alice)
            .selectUtxosFrom(await provider.fetchAddressUTxOs(alice))
            .complete();
        const signed = await wallet.signTx(unsigned);
        await provider.submitTx(signed);
        // The adapter above throws `${errorRule}: ${error}`, so this matches on `errorRule`.
        // `UtxoNotAvailable` is all of it: `BadInputsUTxO` and `BadAllInputsUTxO` both report that
        // one name by design, because they are one condition, and `error` is prose that names
        // neither rule. So there is nothing finer than the condition to assert on here.
        await expect(provider.submitTx(signed)).rejects.toThrow(/UtxoNotAvailable/);
    });
});
