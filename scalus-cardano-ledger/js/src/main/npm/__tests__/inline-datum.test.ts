// __tests__/inline-datum.test.ts
// An inline datum keeps the CBOR it arrived in (spec [SC-13a]). d879811a0000000a is Constr 0 [10]
// with the integer in 5 bytes: valid, but not minimal. It used to come back as d8799f0aff, so its
// hash changed on the way through.
import { describe, expect, test } from "vitest";
import { CardanoInfo, Emulator, Utxo, Value } from "../scalus.js";

const ALICE = "addr_test1vzpwq95z3xyum8vqndgdd9mdnmafh3djcxnc6jemlgdmswcve6tkw";
const PROBE = "d879811a0000000a";
const CANONICAL = "d8799f0aff";

const toHex = (bytes: Uint8Array): string => Buffer.from(bytes).toString("hex");
const fromHex = (hex: string): Uint8Array => new Uint8Array(Buffer.from(hex, "hex"));

const plain = (): Utxo => new Utxo("00".repeat(32), 0, ALICE, Value.ada(5n));

/** `[input, output]` CBOR holding the probe datum inline, built without withInlineDatum's help. */
function probeUtxoCbor(): Uint8Array {
    const canonical = toHex(plain().withInlineDatum(fromHex(CANONICAL)).toCbor());
    // tag 24 wraps a byte string: 0x45 holds 5 bytes, 0x48 holds 8
    expect(canonical).toContain("d81845" + CANONICAL);
    return fromHex(canonical.replace("d81845" + CANONICAL, "d81848" + PROBE));
}

describe("inline datum bytes", () => {
    test("Utxo.fromCbor keeps them, for inlineDatum and toCbor", () => {
        const cbor = probeUtxoCbor();
        const utxo = Utxo.fromCbor(cbor);
        expect(toHex(utxo.inlineDatum!)).toBe(PROBE);
        expect(toHex(utxo.toCbor())).toBe(toHex(cbor));
    });

    test("withInlineDatum keeps them", () => {
        const utxo = plain().withInlineDatum(fromHex(PROBE));
        expect(toHex(utxo.inlineDatum!)).toBe(PROBE);
        expect(toHex(utxo.toCbor())).toBe(toHex(probeUtxoCbor()));
    });

    test("addUtxo keeps them", () => {
        const emulator = Emulator.create(CardanoInfo.preview());
        emulator.addUtxo(Utxo.fromCbor(probeUtxoCbor()));
        const datums = emulator.getUtxos().map((u) => toHex(u.inlineDatum!));
        expect(datums).toEqual([PROBE]);
    });
});
