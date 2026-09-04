// The emulator as a lucid-evolution `Provider`.
//
// Same claim as the MeshJS test, against the other SDK's interface: the adapter is field renaming,
// and the transaction below is really built by lucid, really signed by a lucid wallet, and really
// applied by the emulator's ledger rules.
import { beforeAll, describe, expect, test } from "vitest";
import {
    type Credential,
    Data,
    type Delegation,
    type EvalRedeemer,
    Lucid,
    type LucidEvolution,
    type OutRef,
    type ProtocolParameters,
    type Provider,
    type RedeemerTag,
    type Script,
    type UTxO as LucidUTxO,
    validatorToAddress,
} from "@lucid-evolution/lucid";
import { Asset, CardanoInfo, Emulator, Utxo, Value } from "../scalus.js";
import {
    contextSensitiveScriptHex,
    hexToBytes,
    scriptTxCborHex,
    scriptUtxoCborHex,
} from "./fixtures.js";

function bytesToHex(bytes: Uint8Array): string {
    return Array.from(bytes, (b) => b.toString(16).padStart(2, "0")).join("");
}

/** `"Spend"` and friends, as lucid spells them. */
const LUCID_TAG: Record<string, RedeemerTag> = {
    Spend: "spend",
    Mint: "mint",
    Cert: "publish",
    Reward: "withdraw",
    Voting: "vote",
    Proposing: "propose",
};

function toLucidUtxo(utxo: Utxo): LucidUTxO {
    const assets: Record<string, bigint> = { lovelace: utxo.value.coin };
    for (const a of utxo.value.assets) assets[a.unit] = a.quantity;
    if (utxo.scriptRef !== undefined) {
        // The one shape the two sides do not share. The emulator hands back the ledger's own
        // CBOR-encoded `script_ref`; lucid wants the bare script hex plus its language. Unwrapping
        // that is the adapter's job, and no test here produces one, so this fails loudly rather
        // than handing lucid a script it would mis-hash.
        throw new Error("reference scripts are outside what this adapter translates");
    }
    return {
        txHash: utxo.txHash,
        outputIndex: utxo.outputIndex,
        address: utxo.address,
        assets,
        datumHash: utxo.datumHash ?? null,
        datum: utxo.inlineDatum === undefined ? null : bytesToHex(utxo.inlineDatum),
        scriptRef: null,
    };
}

function fromLucidUtxo(utxo: LucidUTxO): Utxo {
    const assets = Object.entries(utxo.assets)
        .filter(([unit]) => unit !== "lovelace")
        .map(([unit, qty]) => new Asset(unit.slice(0, 56), unit.slice(56), qty));
    const value = new Value(utxo.assets["lovelace"] ?? 0n, assets);
    if (utxo.scriptRef) {
        // Symmetric with `toLucidUtxo`, which refuses the same shape going the other way. Dropping
        // it instead would hand `evaluateTx` an input whose reference script has vanished.
        throw new Error("reference scripts are outside what this adapter translates");
    }
    if (utxo.datumHash && utxo.datum) {
        // Ambiguous, and not ours to guess: a hash-datum output whose datum lucid has since
        // resolved, and an inline-datum output whose hash a Blockfrost-shaped fetcher filled in
        // as well, arrive here identically. The two produce different ledger outputs - different
        // serialised size, different min-ada, a different script context - so an adapter that
        // picked one would be right half the time and silently wrong the other half.
        throw new Error("a UTxO carrying both a datum hash and a datum is ambiguous here");
    }
    const base = new Utxo(utxo.txHash, utxo.outputIndex, utxo.address, value);
    // An inline datum is how a script UTxO carries its state, so neither of these may be dropped:
    // an input that reaches `evaluateTx` without its datum builds a script context that is missing
    // it, which shows up as a wrong budget or a phase-2 failure that reads like a validator bug.
    if (utxo.datum) return base.withInlineDatum(hexToBytes(utxo.datum));
    if (utxo.datumHash) return base.withDatumHash(utxo.datumHash);
    return base;
}

/**
 * The whole adapter. `getUtxos*` are the emulator's own filters, `getProtocolParameters` is a
 * rename, and `evaluateTx` is the emulator evaluating against the state it already holds.
 */
class EmulatorProvider implements Provider {
    constructor(readonly emulator: Emulator) {}

    async getProtocolParameters(): Promise<ProtocolParameters> {
        const p = this.emulator.getProtocolParameters();
        return {
            protocolMajorVersion: p.protocolMajorVersion,
            minFeeA: p.txFeePerByte,
            minFeeB: p.txFeeFixed,
            maxTxSize: p.maxTxSize,
            maxValSize: p.maxValueSize,
            keyDeposit: p.stakeAddressDeposit,
            poolDeposit: p.stakePoolDeposit,
            drepDeposit: p.dRepDeposit,
            govActionDeposit: p.govActionDeposit,
            priceMem: p.priceMemory,
            priceStep: p.priceSteps,
            maxTxExMem: p.maxTxExecutionMemory,
            maxTxExSteps: p.maxTxExecutionSteps,
            coinsPerUtxoByte: p.utxoCostPerByte,
            collateralPercentage: p.collateralPercentage,
            maxCollateralInputs: p.maxCollateralInputs,
            minFeeRefScriptCostPerByte: p.minFeeRefScriptCostPerByte,
            costModels: p.costModels,
        };
    }

    /** `UtxoFilter.paymentCredential` is the query a wallet makes; the address form is the same
     * filter with a different field. Note the emulator matches a credential by hash alone, so a
     * key and a script credential with the same hash would not be told apart. */
    private filterFor(addressOrCredential: string | Credential) {
        return typeof addressOrCredential === "string"
            ? { address: addressOrCredential }
            : { paymentCredential: addressOrCredential.hash };
    }

    async getUtxos(addressOrCredential: string | Credential): Promise<LucidUTxO[]> {
        return this.emulator.getUtxos(this.filterFor(addressOrCredential)).map(toLucidUtxo);
    }

    async getUtxosWithUnit(
        addressOrCredential: string | Credential,
        unit: string,
    ): Promise<LucidUTxO[]> {
        return this.emulator
            .getUtxos({ ...this.filterFor(addressOrCredential), unit })
            .map(toLucidUtxo);
    }

    async getUtxoByUnit(unit: string): Promise<LucidUTxO> {
        const found = this.emulator.getUtxos({ unit, limit: 2 });
        if (found.length !== 1) throw new Error(`${found.length} UTxOs hold ${unit}, expected 1`);
        return toLucidUtxo(found[0]!);
    }

    async getUtxosByOutRef(outRefs: OutRef[]): Promise<LucidUTxO[]> {
        return this.emulator.getUtxos({ outRefs }).map(toLucidUtxo);
    }

    async getDelegation(rewardAddress: string): Promise<Delegation> {
        const d = this.emulator.getDelegation(rewardAddress);
        return { poolId: d.poolId ?? null, rewards: d.rewards };
    }

    async getDatum(datumHash: string): Promise<string> {
        const datum = this.emulator.getDatum(datumHash);
        if (datum === undefined) throw new Error(`no datum for hash ${datumHash}`);
        return bytesToHex(datum);
    }

    async awaitTx(txHash: string): Promise<boolean> {
        return this.emulator.hasTx(txHash);
    }

    async submitTx(txHex: string): Promise<string> {
        const result = this.emulator.submitTx(hexToBytes(txHex));
        if (!result.isSuccess) {
            throw new Error(`${result.errorRule}: ${result.error} ${result.logs.join(" ")}`);
        }
        return result.txHash!;
    }

    async evaluateTx(txHex: string, additionalUTxOs: LucidUTxO[] = []): Promise<EvalRedeemer[]> {
        return this.emulator
            .evaluateTx(hexToBytes(txHex), additionalUTxOs.map(fromLucidUtxo))
            .map((r) => ({
                redeemer_tag: LUCID_TAG[r.tag]!,
                redeemer_index: r.index,
                ex_units: { mem: Number(r.budget.memory), steps: Number(r.budget.steps) },
            }));
    }
}

const SEED =
    "abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon art";

const BOB = "addr_test1vzpwq95z3xyum8vqndgdd9mdnmafh3djcxnc6jemlgdmswcve6tkw";

describe("Emulator as a lucid-evolution Provider", () => {
    let emulator: Emulator;
    let provider: EmulatorProvider;
    let lucid: LucidEvolution;
    let alice: string;

    beforeAll(async () => {
        emulator = Emulator.create(CardanoInfo.preview());
        provider = new EmulatorProvider(emulator);
        // Lucid reads the protocol parameters through the provider as it starts up, so this call
        // already exercises `getProtocolParameters`.
        lucid = await Lucid(provider, "Preview");
        lucid.selectWallet.fromSeed(SEED);
        alice = await lucid.wallet().address();
        emulator.addUtxo(new Utxo("00".repeat(32), 0, alice, Value.ada(1000n)));
    });

    test("lucid reads the emulator's parameters, UTxOs and delegation state", async () => {
        const params = await provider.getProtocolParameters();
        expect(params.minFeeA).toBe(emulator.getProtocolParameters().txFeePerByte);
        expect(params.coinsPerUtxoByte).toBeGreaterThan(0n);
        expect(params.costModels.PlutusV3.length).toBeGreaterThan(200);

        // Through lucid's own accessors, not the adapter's, so the wiring is what is under test.
        const utxos = await lucid.utxosAt(alice);
        expect(utxos.length).toBe(1);
        expect(utxos[0]!.assets["lovelace"]).toBe(1_000_000_000n);

        const byRef = await lucid.utxosByOutRef([
            { txHash: "00".repeat(32), outputIndex: 0 },
            { txHash: "ff".repeat(32), outputIndex: 7 },
        ]);
        expect(byRef.length).toBe(1);
        expect(byRef[0]!.txHash).toBe("00".repeat(32));

        // An unregistered credential: the emulator answers, it does not throw.
        const rewardAddress = await lucid.wallet().rewardAddress();
        const delegation = await lucid.delegationAt(rewardAddress!);
        expect(delegation.poolId).toBe(null);
        expect(delegation.rewards).toBe(0n);
    });

    test("builds, signs and submits a transaction that changes the emulator's ledger", async () => {
        expect(emulator.getUtxos({ address: BOB }).length).toBe(0);

        const tx = await lucid.newTx().pay.ToAddress(BOB, { lovelace: 25_000_000n }).complete();
        const signed = await tx.sign.withWallet().complete();
        const txHash = await signed.submit();

        expect(emulator.hasTx(txHash)).toBe(true);
        expect(await provider.awaitTx(txHash)).toBe(true);

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
        const p = emulator.getProtocolParameters();
        const txBytes = BigInt(signed.toCBOR().length / 2);
        const minFee = BigInt(p.txFeeFixed) + BigInt(p.txFeePerByte) * txBytes;
        expect(fee).toBeGreaterThanOrEqual(minFee);
        expect(fee).toBeLessThan(minFee * 2n);

        // Lucid's own view of the ledger is the emulator's, one query later.
        const afterViaLucid = await lucid.utxosAt(alice);
        expect(afterViaLucid.length).toBe(1);
        expect(afterViaLucid[0]!.assets["lovelace"]).toBe(change);
    });

    test("the round trip through lucid's UTxO type keeps the datum, or refuses the UTxO", () => {
        const plain = Utxo.fromCbor(hexToBytes(scriptUtxoCborHex));
        // The fixture is a bare `[address, coin]` output, so a round trip over it alone cannot
        // tell a converter that carries the datum fields from one that drops them. These are the
        // shapes that can: an inline datum is how a script UTxO carries its state, and it is
        // exactly what an adapter handed a script UTxO through `additionalUTxOs` must not lose.
        const inline = plain.withInlineDatum(hexToBytes("182a")); // CBOR for the integer 42
        const hashed = plain.withDatumHash("ab".repeat(32));
        for (const utxo of [plain, inline, hashed]) {
            expect(fromLucidUtxo(toLucidUtxo(utxo)).toObject()).toEqual(utxo.toObject());
        }

        // The one shape the two sides do not share, refused in both directions rather than
        // dropped in one. `d8184482034100` is a one-byte PlutusV3 script as a ledger `script_ref`.
        const withScript = plain.withScriptRef(hexToBytes("d8184482034100"));
        expect(() => toLucidUtxo(withScript)).toThrow(/reference scripts/);
        expect(() =>
            fromLucidUtxo({
                ...toLucidUtxo(plain),
                scriptRef: { type: "PlutusV3", script: "00" },
            }),
        ).toThrow(/reference scripts/);

        // And the case no adapter can resolve: both a datum hash and a datum.
        expect(() =>
            fromLucidUtxo({ ...toLucidUtxo(hashed), datum: "182a" }),
        ).toThrow(/ambiguous/);
    });

    test("evaluateTx returns real budgets for UTxOs the emulator does not hold", async () => {
        // Lucid's `additionalUTxOs` is the emulator's second `evaluateTx` parameter.
        const scriptUtxo = toLucidUtxo(Utxo.fromCbor(hexToBytes(scriptUtxoCborHex)));
        const redeemers = await provider.evaluateTx(scriptTxCborHex, [scriptUtxo]);
        // One withdrawal guarded by a PlutusV3 validator, so exactly one `withdraw` redeemer.
        // The budget is pinned, and the fixture's validator reads its ScriptContext (see
        // fixtures.ts), so these numbers move if the script context, the redeemer or the
        // cost models handed to the evaluator are wrong. `DifferentialFixtureGenTest`
        // pins the same pair on the Scala side.
        expect(redeemers).toEqual([
            {
                redeemer_tag: "withdraw",
                redeemer_index: 0,
                ex_units: { mem: 32_318, steps: 8_754_898 },
            },
        ]);
    });

    test("a transaction the ledger rejects surfaces the condition that rejected it", async () => {
        const ghost: LucidUTxO = {
            txHash: "ff".repeat(32),
            outputIndex: 0,
            address: alice,
            assets: { lovelace: 1_000_000_000n },
            datumHash: null,
            datum: null,
            scriptRef: null,
        };
        const tx = await lucid
            .newTx()
            .collectFrom([ghost])
            .pay.ToAddress(BOB, { lovelace: 25_000_000n })
            .complete();
        const signed = await tx.sign.withWallet().complete();
        // The adapter above throws `${errorRule}: ${error}`, so this matches on `errorRule`.
        // `UtxoNotAvailable` is all of it: `BadInputsUTxO` and `BadAllInputsUTxO` both report that
        // one name by design, because they are one condition, and `error` is prose that names
        // neither rule. So there is nothing finer than the condition to assert on here.
        await expect(signed.submit()).rejects.toThrow(/UtxoNotAvailable/);
    });
});

// The payment transactions above never run a script, so nothing they do depends on the cost models
// or the script prices the adapter serves: scrambling `costModels` so PlutusV3 is priced with V1's
// model, and zeroing `priceMemory`/`priceSteps`, leaves every one of them green. This is the
// transaction that notices.
//
// Spending a script UTxO makes lucid use all three. It asks `evaluateTx` for the redeemer's budget,
// prices that budget with `priceMem`/`priceStep` into the fee, and hashes the cost model for the
// script's own language into the transaction's `script_data_hash`. The emulator then recomputes
// both from its real parameters when the bytes arrive: a wrong cost model is an integrity-hash
// mismatch, and a zero script price is a fee below the minimum. Either one rejects the transaction.
describe("Emulator as a lucid-evolution Provider: spending a Plutus script UTxO", () => {
    // The fixture validator, as an SDK takes it. The same script `scriptTxCborHex` withdraws with:
    // a PlutusV3 script is handed nothing but its `ScriptContext`, so one script is both a
    // withdrawal validator and a spending validator.
    const validator: Script = { type: "PlutusV3", script: contextSensitiveScriptHex };

    let emulator: Emulator;
    let provider: EmulatorProvider;
    let lucid: LucidEvolution;
    let alice: string;
    let scriptAddress: string;

    beforeAll(async () => {
        emulator = Emulator.create(CardanoInfo.preview());
        provider = new EmulatorProvider(emulator);
        lucid = await Lucid(provider, "Preview");
        lucid.selectWallet.fromSeed(SEED);
        alice = await lucid.wallet().address();
        // Lucid derives the address from the script bytes; the emulator's ledger derives the hash
        // it checks the witness against from the same bytes. Seeding the UTxO at lucid's address
        // and having the ledger accept the spend is the two agreeing.
        scriptAddress = validatorToAddress("Preview", validator);
        // Two wallet UTxOs: one to balance the transaction, one for collateral.
        emulator.addUtxo(new Utxo("00".repeat(32), 0, alice, Value.ada(1000n)));
        emulator.addUtxo(new Utxo("00".repeat(32), 1, alice, Value.ada(1000n)));
        emulator.addUtxo(
            new Utxo("11".repeat(32), 0, scriptAddress, Value.ada(50n))
                // `182a` is CBOR for the integer 42. A datum is how a script UTxO carries state,
                // and an inline one is what a V3 spending script sees in its `ScriptContext`.
                .withInlineDatum(hexToBytes("182a")),
        );
    });

    test("builds a script spend lucid prices with the emulator's cost models, and submits it", async () => {
        const scriptUtxos = await lucid.utxosAt(scriptAddress);
        expect(scriptUtxos.length).toBe(1);
        expect(scriptUtxos[0]!.datum).toBe("182a");

        const tx = await lucid
            .newTx()
            // `Data.to(5n)` is CBOR for the integer 5. The validator reads it, so the budget below
            // is a function of this number.
            .collectFrom(scriptUtxos, Data.to(5n))
            .attach.SpendingValidator(validator)
            .complete();
        const signed = await tx.sign.withWallet().complete();

        // What lucid asked for, and what it built the fee and the script-data hash from.
        const budgets = await provider.evaluateTx(signed.toCBOR());
        expect(budgets.length).toBe(1);
        expect(budgets[0]!.redeemer_tag).toBe("spend");
        expect(budgets[0]!.ex_units.mem).toBeGreaterThan(0);
        expect(budgets[0]!.ex_units.steps).toBeGreaterThan(0);

        // Phase 2 runs here: the emulator resolves the input, builds the script context, executes
        // the validator and charges it. A cost model or a script price the adapter got wrong
        // stops the transaction at this line, not at an assertion after it.
        const txHash = await signed.submit();
        expect(emulator.hasTx(txHash)).toBe(true);
        expect(emulator.getUtxos({ address: scriptAddress }).length).toBe(0);

        // The 50 ada the script held are Alice's now, less the fee.
        const total = emulator
            .getUtxos({ address: alice })
            .reduce((sum, u) => sum + u.value.coin, 0n);
        expect(total).toBeGreaterThan(2_000_000_000n);
        expect(total).toBeLessThan(2_050_000_000n);
    });
});
