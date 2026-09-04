// __tests__/object-api-guards.test.ts
// Two places where the object API used to answer a malformed request instead of refusing it.
//
// Both are the same failure mode: a query API that returns a plausible wrong answer is worse than
// one that throws, because nothing downstream can tell the answer was wrong. TypeScript callers
// were already protected by the declared types; these guards are what gives untyped JavaScript
// callers the same result.
import { describe, expect, test } from "vitest";
import { CardanoInfo, Emulator, type UtxoFilter, Utxo, Value } from "../scalus.js";

const ALICE = "addr_test1vzpwq95z3xyum8vqndgdd9mdnmafh3djcxnc6jemlgdmswcve6tkw";
const BOB = "addr_test1vryvgass5dsrf2kxl3vgfz76uhp83kv5lagzcp29tcana6q4a064h";

function seeded(): Emulator {
    const emulator = Emulator.create(CardanoInfo.preview());
    emulator.addUtxo(new Utxo("00".repeat(32), 0, ALICE, Value.ada(10n)));
    emulator.addUtxo(new Utxo("00".repeat(32), 1, ALICE, Value.ada(20n)));
    emulator.addUtxo(new Utxo("00".repeat(32), 2, BOB, Value.ada(30n)));
    return emulator;
}

describe("getUtxos rejects a filter field it does not know", () => {
    test("an unknown field throws instead of widening the query to the whole ledger", () => {
        const emulator = seeded();
        // Every unknown field used to be ignored, and a filter with no *recognised* field matches
        // everything - so this returned all three UTxOs. A wallet asking for one address's funds
        // with a misspelt key would have been handed everyone's.
        expect(() => emulator.getUtxos({ adress: ALICE } as unknown as UtxoFilter)).toThrow(
            /unknown UtxoFilter field/,
        );
        // Including when a known field is present too: the answer would be right by accident here,
        // and wrong as soon as the misspelt field was the narrowing one.
        expect(() =>
            emulator.getUtxos({ address: ALICE, minLovelaceAmount: 1n } as unknown as UtxoFilter),
        ).toThrow(/minLovelaceAmount/);
    });

    test("the fields it does know still work, and an empty filter still matches everything", () => {
        const emulator = seeded();
        expect(emulator.getUtxos({}).length).toBe(3);
        expect(emulator.getUtxos({ address: ALICE }).length).toBe(2);
        expect(emulator.getUtxos({ address: ALICE, minLovelace: 15_000_000n }).length).toBe(1);
        expect(emulator.getUtxos({ txHash: "00".repeat(32), limit: 2 }).length).toBe(2);
    });
});

describe("the surface says what it wanted when it is handed the wrong thing", () => {
    test("addUtxo names the type it needs instead of leaking a Scala one", () => {
        const emulator = seeded();
        // The shape an untyped caller writes by hand. It carries none of the parsed ledger pair
        // `addUtxo` reads, so it used to fail a frame later with `undefined (of class
        // java.lang.Void)` - a message with nothing in it a JavaScript caller can act on.
        const plain = { txHash: "00".repeat(32), outputIndex: 3, address: ALICE, value: 1n };
        expect(() => emulator.addUtxo(plain as unknown as Utxo)).toThrow(/expects a Utxo/);
    });

    test("a reward address that is not one says so, rather than naming a Base58 decoder", () => {
        const emulator = seeded();
        expect(() => emulator.getStakeReward("nope")).toThrow(/reward address/);
        expect(() => emulator.getDelegation("nope")).toThrow(/reward address/);
        // A well-formed address of the wrong kind is a different mistake, and already said so.
        expect(() => emulator.getDelegation(ALICE)).toThrow(/reward address/);
    });

    test("a payment address that is not one says so too", () => {
        // At the constructor, not at the getter: the argument is wrong where it is passed, and a
        // handle that cannot represent a UTxO should never come into existence.
        expect(() => new Utxo("00".repeat(32), 0, "not-bech32", Value.ada(1n))).toThrow(
            /not a Cardano address/,
        );
    });
});

describe("Utxo.fromCbor rejects a map that is not exactly one UTxO", () => {
    test("a whole UTxO set throws instead of yielding one arbitrary entry", () => {
        const emulator = seeded();
        // `getUtxosCbor()` is the whole ledger in one CBOR map - the easiest value to pass here by
        // mistake, since it has the same shape. Only the empty map used to be refused; a bigger one
        // kept whichever entry the decoded map iterated first and discarded the rest in silence.
        expect(() => Utxo.fromCbor(emulator.getUtxosCbor())).toThrow(/exactly one/);
        expect(() => Utxo.fromCbor(new Uint8Array([0xa0]))).toThrow(/exactly one/);
    });

    test("the one-entry map toCbor writes still round-trips", () => {
        const utxo = new Utxo("00".repeat(32), 0, ALICE, Value.ada(10n));
        expect(Utxo.fromCbor(utxo.toCbor()).toObject()).toEqual(utxo.toObject());
    });
});
