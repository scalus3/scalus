// __tests__/lucid-provider.ts
//
// The emulator as a lucid-evolution `Provider`. The adapter is field renaming: `getUtxos*` are the
// emulator's own filters, `getProtocolParameters` is a rename, `evaluateTx` evaluates against the
// state the emulator holds, and `awaitTx` is the emulator's own, which forges blocks for a pending
// transaction under `blockProduction: "manual"`. `provider-lucid.test.ts` and
// `emulator-chain.test.ts` drive it.

import type {
    Credential,
    Delegation,
    EvalRedeemer,
    OutRef,
    ProtocolParameters,
    Provider,
    RedeemerTag,
    UTxO as LucidUTxO,
} from "@lucid-evolution/lucid";
import { Asset, Emulator, Utxo, Value } from "../scalus.js";
import { hexToBytes } from "./fixtures.js";

export function bytesToHex(bytes: Uint8Array): string {
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

export function toLucidUtxo(utxo: Utxo): LucidUTxO {
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

export function fromLucidUtxo(utxo: LucidUTxO): Utxo {
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
export class EmulatorProvider implements Provider {
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

    /** Keep the key/script discriminator when querying by credential. */
    private filterFor(addressOrCredential: string | Credential) {
        return typeof addressOrCredential === "string"
            ? { address: addressOrCredential }
            : {
                  paymentCredential: addressOrCredential.hash,
                  paymentCredentialType: addressOrCredential.type === "Key" ? "key" as const : "script" as const,
              };
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
        return this.emulator.awaitTx(txHash);
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
