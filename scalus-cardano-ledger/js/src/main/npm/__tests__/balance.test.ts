// __tests__/balance.test.ts
// `balancer.balanceTx` against a transaction that actually runs a Plutus script.
//
// The interesting part of balancing is the part a plain payment cannot reach: the evaluated
// execution units have to land in the redeemers, the script data hash has to be recomputed over
// them, and the fee has to pay for them. So the fixture here is `scriptTxCborHex`, a real
// withdrawal-validator transaction, and every assertion below is about one of those three.
import { describe, expect, test } from "vitest";
import { Decoder, Encoder } from "cbor-x";

// Cardano redeemers are a CBOR map keyed by `[tag, index]`, which cannot be a JS property name, so
// maps must come back as `Map` rather than objects.
const cbor = new Decoder({ mapsAsObjects: false });
// `tagUint8Array: false` writes a plain CBOR byte string; cbor-x otherwise emits tag 64,
// which is a typed array, and the ledger decoder rejects it.
const cborOut = new Encoder({ mapsAsObjects: false, tagUint8Array: false });
const decode = (b: Uint8Array) => cbor.decode(b);
import { balancer, CardanoInfo, evaluator } from "../scalus.js";
import { hexToBytes, scriptTxCborHex, scriptUtxoPairHex } from "./fixtures.js";

const info = CardanoInfo.mainnet();
const CHANGE = 0; // the fixture has a single output
const utxos = [scriptUtxoPairHex];

/** tx = [body, witnessSet, isValid, auxData]; body is a map and so is the witness set. */
function parts(bytes: Uint8Array) {
    const [body, witnessSet] = decode(bytes) as [Map<number, unknown>, Map<number, unknown>];
    // Redeemers are witness-set key 5. The Conway form is a map from [tag, index] to [data, units].
    const redeemers = witnessSet.get(5) as Map<unknown, unknown> | undefined;
    const units = redeemers
        ? [...redeemers.values()].map((v) => (v as [unknown, [number, number]])[1])
        : [];
    return {
        fee: body.get(2) as number | bigint,
        scriptDataHash: Buffer.from(body.get(11) as Uint8Array).toString("hex"),
        outputCoin: (body.get(1) as [unknown, number | bigint][])[CHANGE]![1],
        units,
    };
}

/** The fixture as a draft: execution units at zero, and a script data hash that is now wrong. */
function asDraft(hex: string): Uint8Array {
    const bytes = hexToBytes(hex);
    const tx = decode(bytes) as unknown[];
    const body = tx[0] as Map<number, unknown>;
    const witnessSet = tx[1] as Map<number, unknown>;
    const redeemers = witnessSet.get(5) as Map<unknown, unknown>;
    for (const [k, v] of redeemers) redeemers.set(k, [(v as unknown[])[0], [0, 0]]);
    body.set(11, new Uint8Array(32)); // an all-zero hash, matching nothing
    // cbor-x re-encodes the whole transaction; balanceTx only needs it to decode, not to be
    // byte-identical to the fixture.
    return new Uint8Array(cborOut.encode(tx));
}

describe("balanceTx on a transaction that runs a Plutus script", () => {
    test("installs the evaluated execution units in the redeemers", () => {
        // Balance the draft, not the fixture. The fixture already declares the units the evaluator
        // computes, so balancing it proves nothing: an implementation that copied its input
        // through would pass. Starting from zero, only an implementation that evaluates and writes
        // can arrive at the right numbers.
        const balanced = balancer.balanceTx(
            asDraft(scriptTxCborHex), utxos, info.slotConfig, info.protocolParams, CHANGE, [],
        );
        const evaluated = evaluator.evaluateTx(
            scriptTxCborHex, utxos, info.slotConfig, info.protocolParams.costModels,
            info.protocolParams.protocolMajorVersion,
        );
        expect(evaluated.length).toBe(1); // the withdrawal validator

        const { units } = parts(balanced);
        expect(units.length).toBe(1);
        expect(units[0]![0]).toBe(Number(evaluated[0]!.budget.memory));
        expect(units[0]![1]).toBe(Number(evaluated[0]!.budget.steps));
        expect(units[0]![0]).toBeGreaterThan(0);
    });

    test("recomputes the script data hash rather than carrying the one it was given", () => {
        const draft = asDraft(scriptTxCborHex);
        const before = parts(draft);
        expect(before.units).toEqual([[0, 0]]);
        expect(before.scriptDataHash).toBe("00".repeat(32));

        const balanced = balancer.balanceTx(
            draft, utxos, info.slotConfig, info.protocolParams, CHANGE, [],
        );
        const after = parts(balanced);

        // The units are restored, so the correct hash over them is the one the fixture carries.
        // Landing on it from an all-zero input is only possible by recomputing: a balancer that
        // copied the hash through would keep the zeros and a node would answer
        // `PPViewHashesDontMatch`.
        expect(after.units[0]![0]).toBeGreaterThan(0);
        expect(after.scriptDataHash).toBe(parts(hexToBytes(scriptTxCborHex)).scriptDataHash);
        expect(after.scriptDataHash).not.toBe(before.scriptDataHash);
    });

    test("the fee pays for the execution units, not only the bytes", () => {
        const balanced = balancer.balanceTx(
            scriptTxCborHex, utxos, info.slotConfig, info.protocolParams, CHANGE, [],
        );
        const { fee, units } = parts(balanced);

        const p = info.protocolParams;
        const executionFee =
            units[0]![0] * p.priceMemory + units[0]![1] * p.priceSteps;
        expect(executionFee).toBeGreaterThan(0);

        // The fee has to cover the bytes and the execution units together. Checking it exceeds the
        // execution part alone is what fails if the units are priced at zero.
        expect(Number(fee)).toBeGreaterThan(executionFee);
        expect(Number(fee)).toBeGreaterThan(Number(p.txFeeFixed));
    });

    test("is a fixpoint: balancing an already balanced transaction changes nothing", () => {
        const once = balancer.balanceTx(
            scriptTxCborHex, utxos, info.slotConfig, info.protocolParams, CHANGE, [],
        );
        const twice = balancer.balanceTx(
            once, utxos, info.slotConfig, info.protocolParams, CHANGE, [],
        );
        expect(Buffer.compare(Buffer.from(once), Buffer.from(twice))).toBe(0);
    });

    test("fails while balancing when the scripts exceed the protocol maximum", () => {
        // The evaluator is built with initialBudget = maxTxExecutionUnits, as TxBuilder builds it,
        // so the transaction's scripts are limited together exactly as a node limits them. Squeeze
        // the maximum below what this script needs and balancing must stop, rather than converge on
        // a transaction the chain would reject.
        const p = info.protocolParams;
        const squeezed = { ...p.toObject(), maxTxExecutionMemory: 1, maxTxExecutionSteps: 1 };
        let thrown: unknown = null;
        try {
            balancer.balanceTx(
                scriptTxCborHex, utxos, info.slotConfig, squeezed, CHANGE, [],
            );
        } catch (e) {
            thrown = e;
        }
        expect(thrown).toBeInstanceOf(Error);
        expect((thrown as { code?: string }).code).toBe("OUT_OF_BUDGET");

        // ...and the same transaction balances fine at the real maximum, so the failure is the
        // budget and not something else about the squeezed record.
        expect(() =>
            balancer.balanceTx(scriptTxCborHex, utxos, info.slotConfig, p.toObject(), CHANGE, []),
        ).not.toThrow();
    });

    test("extra signers raise the fee, and the change output absorbs it", () => {
        const plain = balancer.balanceTx(
            scriptTxCborHex, utxos, info.slotConfig, info.protocolParams, CHANGE, [],
        );
        const withSigner = balancer.balanceTx(
            scriptTxCborHex, utxos, info.slotConfig, info.protocolParams, CHANGE,
            ["aa".repeat(28)],
        );
        const a = parts(plain);
        const b = parts(withSigner);
        expect(Number(b.fee)).toBeGreaterThan(Number(a.fee));
        // Whatever the fee took, the change output gave up.
        expect(Number(a.outputCoin) - Number(b.outputCoin)).toBe(
            Number(b.fee) - Number(a.fee),
        );
    });
});
