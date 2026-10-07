// __tests__/script-ref.test.ts
// Utxo.script is the inverse of withScriptRef (spec [SC-11]): double-CBOR hex for Plutus, CBOR hex
// for Native.
import { describe, expect, test } from "vitest";
import { type PlainScript, Utxo, Value } from "../scalus.js";
import { failScriptHex, successScriptHex } from "./fixtures";

const ALICE = "addr_test1vzpwq95z3xyum8vqndgdd9mdnmafh3djcxnc6jemlgdmswcve6tkw";

// failScriptHex is a 1.0.0 program in single CBOR: wrap it once more
const V1_PROGRAM = "46" + failScriptHex;
const V3_PROGRAM = successScriptHex;
// a native script requiring the signature of key hash ab..ab
const SIGNATURE = "8200581c" + "ab".repeat(28);

const plain = (): Utxo => new Utxo("00".repeat(32), 0, ALICE, Value.ada(5n));

describe("Utxo.script", () => {
    const cases: PlainScript[] = [
        { type: "Native", script: SIGNATURE },
        { type: "PlutusV1", script: V1_PROGRAM },
        { type: "PlutusV2", script: V1_PROGRAM },
        { type: "PlutusV3", script: V3_PROGRAM },
    ];
    for (const script of cases) {
        test(`round-trips a ${script.type} script`, () => {
            const utxo = plain().withScriptRef(script);
            expect(utxo.script).toEqual(script);
            expect(utxo.toObject().script).toEqual(script);
        });
    }

    test("returns single-CBOR Plutus input as double CBOR", () => {
        const single = V3_PROGRAM.slice(2);
        expect(plain().withScriptRef({ type: "PlutusV3", script: single }).script).toEqual({
            type: "PlutusV3",
            script: V3_PROGRAM,
        });
    });

    test("is undefined without a reference script", () => {
        expect(plain().script).toBeUndefined();
    });
});
