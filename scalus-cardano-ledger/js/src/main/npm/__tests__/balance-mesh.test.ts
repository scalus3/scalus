// __tests__/balance-mesh.test.ts
// `balancer.balanceTx` against transactions MeshJS really builds.
//
// `balance.test.ts` covers the script path with a fixture. This covers the other half of the
// claim: that a transaction produced by another SDK's serializer survives the round trip, and that
// the fee we compute is one a ledger accepts. The transactions below are built by `MeshTxBuilder`,
// signed by a `MeshWallet`, and submitted to the emulator, which runs the real rules.
import { beforeAll, describe, expect, test } from "vitest";
import { MeshTxBuilder, MeshWallet } from "@meshsdk/core";
import { Decoder } from "cbor-x";
import { balancer, CardanoInfo, Emulator, Utxo, Value } from "../scalus.js";

// Cardano bodies are CBOR maps, so they must come back as `Map`, not objects.
const cbor = new Decoder({ mapsAsObjects: false });

const MNEMONIC = ("abandon ".repeat(23) + "art").split(" ");
const BOB = "addr_test1vzpwq95z3xyum8vqndgdd9mdnmafh3djcxnc6jemlgdmswcve6tkw";
const CHANGE = 1; // MeshTxBuilder appends the change output last

const info = CardanoInfo.preview();

const feeOf = (bytes: Uint8Array): bigint => {
    const [body] = cbor.decode(bytes) as [Map<number, unknown>];
    return BigInt(body.get(2) as number | bigint);
};

/**
 * The transaction with its fee set to zero, leaving the inputs worth more than the outputs. The
 * balancer's job is exactly that surplus: it moves it into the change output and prices the
 * transaction properly.
 *
 * Balancing a transaction that already carries Mesh's fee proves nothing. The balancer never
 * lowers a fee below the one it is given, so it hands the same number back without computing
 * anything. Every fee assertion below therefore starts from this.
 *
 * The rewrite goes through `@cardano-sdk/core`, the serializer that produced the transaction.
 * Re-encoding with a general CBOR library instead changes bytes it was not asked to change: Mesh
 * writes its auxiliary data as a tag-259 map, and a round trip that loses the tag leaves the
 * body's auxiliary data hash pointing at something else, which the ledger rejects with
 * `InvalidAuxiliaryDataHash`.
 */
async function withZeroFee(bytes: Uint8Array): Promise<Uint8Array> {
    const { Serialization } = await import("@cardano-sdk/core");
    const hex = Buffer.from(bytes).toString("hex");
    const tx = Serialization.Transaction.fromCbor(Serialization.TxCBOR(hex));
    const body = tx.body();
    body.setFee(0n);
    tx.setBody(body);
    return Uint8Array.from(Buffer.from(tx.toCbor().toString(), "hex"));
}

describe("balanceTx on transactions MeshJS builds", () => {
    let wallet: MeshWallet;
    let alice: string;

    beforeAll(async () => {
        wallet = new MeshWallet({
            networkId: 0,
            key: { type: "mnemonic", words: MNEMONIC },
        });
        await wallet.init();
        alice = await wallet.getChangeAddress();
    });

    /** A ledger holding one 1000 ada UTxO for Alice, and that UTxO as Mesh sees it. */
    function seeded() {
        const emulator = Emulator.create(info);
        emulator.addUtxo(new Utxo("11".repeat(32), 0, alice, Value.ada(1000n)));
        const utxos = emulator.getUtxos({ address: alice });
        const meshUtxos = utxos.map((u) => ({
            input: { txHash: u.txHash, outputIndex: u.outputIndex },
            output: {
                address: u.address,
                amount: [{ unit: "lovelace", quantity: u.value.coin.toString() }],
            },
        }));
        return { emulator, utxos, meshUtxos };
    }

    const build = async (meshUtxos: ReturnType<typeof seeded>["meshUtxos"]) =>
        await new MeshTxBuilder({})
            .txOut(BOB, [{ unit: "lovelace", quantity: "25000000" }])
            .changeAddress(alice)
            .selectUtxosFrom(meshUtxos)
            .complete();

    test("leaves a transaction it has nothing to change byte-identical", async () => {
        const { utxos, meshUtxos } = seeded();
        const tx = Buffer.from(await build(meshUtxos), "hex");

        const balanced = balancer.balanceTx(
            tx, utxos, info.slotConfig, info.protocolParams, CHANGE, [],
        );

        // Mesh's transaction is already at the fixpoint, so the interesting claim is that we give
        // it back untouched: Scalus decoded another serializer's CBOR, including its legacy array
        // outputs, and re-encoded it without disturbing a byte.
        expect(Buffer.compare(tx, Buffer.from(balanced))).toBe(0);
    });

    test("a transaction with no fee comes back at the ledger's own minimum", async () => {
        const { emulator, utxos, meshUtxos } = seeded();
        const tx = Buffer.from(await build(meshUtxos), "hex");
        const draft = await withZeroFee(tx);
        expect(feeOf(draft)).toBe(0n);

        const balanced = balancer.balanceTx(
            draft, utxos, info.slotConfig, info.protocolParams, CHANGE, [],
        );
        const fee = feeOf(balanced);
        expect(fee).toBeGreaterThan(0n);
        // At most what Mesh charged: Mesh carries a byte or two of slack over the true minimum.
        expect(fee).toBeLessThanOrEqual(feeOf(tx));

        // The ledger is the judge of whether the fee is big enough. `FeesOk` rejects a transaction
        // whose fee is below the minimum for its size and execution units, so a pass here is the
        // real assertion and the arithmetic above is only a bound.
        const signed = await wallet.signTx(Buffer.from(balanced).toString("hex"), true);
        const result = emulator.submitTx(Uint8Array.from(Buffer.from(signed, "hex")));
        expect(result.isSuccess, `${result.errorRule}: ${result.error}`).toBe(true);
        expect(emulator.getUtxos({ address: BOB })[0]!.value.coin).toBe(25_000_000n);
    });

    test("each extra signer adds one witness worth of fee", async () => {
        const { utxos, meshUtxos } = seeded();
        const draft = await withZeroFee(Buffer.from(await build(meshUtxos), "hex"));
        const fee = (signers: string[]) =>
            feeOf(balancer.balanceTx(
                draft, utxos, info.slotConfig, info.protocolParams, CHANGE, signers,
            ));

        const none = fee([]);
        const one = fee(["aa".repeat(28)]);
        const two = fee(["aa".repeat(28), "bb".repeat(28)]);

        expect(one).toBeGreaterThan(none);
        expect(two - one).toBe(one - none); // each costs the same
        // A vkey witness is 32 bytes of key, 64 of signature and a little CBOR, so about 101.
        const bytes = Number(one - none) / info.protocolParams.txFeePerByte;
        expect(bytes).toBeGreaterThan(90);
        expect(bytes).toBeLessThan(115);

        // A hash given twice is one signature, not two.
        expect(fee(["aa".repeat(28), "aa".repeat(28)])).toBe(one);
    });

    test("a plain record of numbers works as well as the ProtocolParams handle", async () => {
        const { utxos, meshUtxos } = seeded();
        const draft = await withZeroFee(Buffer.from(await build(meshUtxos), "hex"));
        const viaHandle = feeOf(balancer.balanceTx(
            draft, utxos, info.slotConfig, info.protocolParams, CHANGE, [],
        ));

        // What an SDK actually holds is plain numbers and plain arrays, not our handle's bigints
        // and CostModels object. This is the path MeshJS takes, so it is the one worth pinning.
        const p = info.protocolParams;
        const record = {
            txFeePerByte: p.txFeePerByte,
            txFeeFixed: p.txFeeFixed,
            maxTxSize: p.maxTxSize,
            maxValueSize: p.maxValueSize,
            stakeAddressDeposit: Number(p.stakeAddressDeposit),
            stakePoolDeposit: Number(p.stakePoolDeposit),
            dRepDeposit: Number(p.dRepDeposit),
            govActionDeposit: Number(p.govActionDeposit),
            utxoCostPerByte: Number(p.utxoCostPerByte),
            priceMemory: p.priceMemory,
            priceSteps: p.priceSteps,
            maxTxExecutionMemory: Number(p.maxTxExecutionMemory),
            maxTxExecutionSteps: Number(p.maxTxExecutionSteps),
            collateralPercentage: p.collateralPercentage,
            maxCollateralInputs: p.maxCollateralInputs,
            minFeeRefScriptCostPerByte: p.minFeeRefScriptCostPerByte,
            protocolMajorVersion: p.protocolMajorVersion,
            costModels: {
                PlutusV1: [...p.costModels.PlutusV1],
                PlutusV2: [...p.costModels.PlutusV2],
                PlutusV3: [...p.costModels.PlutusV3],
            },
        };
        expect(feeOf(balancer.balanceTx(
            draft, utxos, info.slotConfig, record, CHANGE, [],
        ))).toBe(viaHandle);
    });

    test("a change index that is not an output is a TypeError", async () => {
        const { utxos, meshUtxos } = seeded();
        const tx = Buffer.from(await build(meshUtxos), "hex");
        expect(() =>
            balancer.balanceTx(tx, utxos, info.slotConfig, info.protocolParams, 99, []),
        ).toThrow(TypeError);
        expect(() =>
            balancer.balanceTx(tx, utxos, info.slotConfig, info.protocolParams, 99, []),
        ).toThrow(/changeOutputIndex/);
    });
});
