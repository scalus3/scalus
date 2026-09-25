# The JavaScript API: decisions and why

The npm package `scalus` serves SDK adapters (Lucid Evolution, MeshJS, Evolution SDK, Blaze) and
Midgard. They apply parameters to scripts, evaluate scripts, and evaluate transactions. This file
records the decisions behind that surface. The API itself is documented in `scalus.d.ts`, which is
generated from the Scaladoc, and in the package README.

## Surface

| Export | Role |
|---|---|
| `uplc.decodeToFlat`, `uplc.applyArgs` | composable: any script form to flat bytes; apply CBOR `Data` arguments |
| `cbor.wrapBytes` / `unwrapBytes`, `bytesToHex` / `hexToBytes` | one CBOR byte-string layer at a time; hex |
| `uplc.applyParamsToScript` | facade: Lucid's and Mesh's contract, any script form in, double-CBOR hex out |
| `evaluator.evaluateScript(script, args, options)` | one script under an `EvaluationOptions` record |
| `evaluator.evaluateTx(tx, utxos, slotConfig, costModels, protocolMajorVersion)` | every script of a transaction |
| `Emulator.evaluateTx(tx, utxos?)` | the same, with the emulator's own ledger state |
| `scriptHash`, `dataHash` | a script's hash whatever its wrapping; a datum's hash over its CBOR as given |
| `Scalus.*`, top-level `evaluateScript`, `evaluateScriptProfile`, `applyDataArgToScript`, `evalPlutusScripts` | deprecated, kept working |

## Decisions

**CBOR at the boundary, hex or bytes.** Every input that is a ledger or Plutus value is CBOR, as hex
or `Uint8Array`. The SDKs already hold CBOR; a JSON Data schema would be a second encoding to keep
in step. The primitives return bytes, the facade returns hex, because that is what each caller
passes on.

**Composable primitives plus one facade.** `applyParamsToScript` is exactly
`bytesToHex(cbor.wrapBytes(cbor.wrapBytes(uplc.applyArgs(uplc.decodeToFlat(s), params))))`. An
adapter that needs another envelope composes the primitives instead of asking for a new flag.
The output envelope never depends on the input envelope.

**Script envelopes are read, not vetted.** `decodeToFlat` strips every CBOR byte-string layer, as
borer's `hasBytes` peek finds them: a flat program never starts with a byte-string header. Whether
a chain would accept the program is `PlutusScript.isWellFormed`'s question. For the same reason
`Utxo.withScriptRef` stores a Plutus program as given.

**Explicit evaluation options.** `EvaluationOptions` is a plain record: language, protocol version,
cost model, optional `maxBudget`. There is no silent default protocol version: the PV10 to PV11
default change in 1.x changed every budget without an error. `maxBudget` exists because Midgard
evaluates untrusted scripts. A cost parameter the model does not reach is `Long.MaxValue`, as in
Plutus.

**Two error channels.** A script that fails is a result (`isSuccess: false`, `error.code`, the
budget and traces it earned); a transaction script that fails throws
`PlutusScriptEvaluationError`, because a transaction has no partial success. Input that cannot be
read throws `TypeError`, naming the argument (`utxos[1]`, `params[0]`). Branch on `code`, not on
`message`.

**Runtime checks only where a wrong value would be silent.** The shipped bundle does not check
`asInstanceOf`, so records are read through `js.Dynamic`, and integers are checked to be safe or to
fit 64 bits: a `number` past 2^53 or a `bigint` past 2^63 would otherwise mis-cost silently.

**`evaluateTx`, not `evalPlutusScripts`.** The name matches `Emulator.evaluateTx` and the evaluator
interfaces of Lucid, Mesh and Blaze. It takes cost models by language name and a required protocol
version. `evalPlutusScripts` (positional cost models, optional version) is deprecated, not widened:
union-typed parameters would give every caller worse types and keep the silent version default.

**One UTxO is `[input, output]`.** The ledger encodes a UTxO *set* as a CBOR map; a single UTxO has
no ledger type, and CIP-30's `transaction_unspent_output`, `[input, output]`, is what CML, CST,
cardano-client-lib and wallets use. So `Utxo.toCbor()` writes the pair (1.2 wrote a one-entry map,
which nothing else reads), `Utxo.fromCbor` reads both, and both `evaluateTx`s take pairs or `Utxo`
handles directly, with no CBOR round trip. `Emulator.getUtxosCbor()` stays a map: it is a set.

**Namespace methods work detached.** `const { evaluateTx } = evaluator` must work, as `Math.max`
does. Scala.js emits object members as `this`-based prototype methods, so every
`@JSExportTopLevel` object calls `bindExports(this)` first. The rejected alternatives are listed on
`bindExports` in `scalus.utils.scalajs.internal`.

**One bundle.** One package, one `scalus.js` ES module. Subpath entries were measured to save 0 to
7%: the size is the Plutus VM, which every entry needs.

## Open

- Configure-once classes (`PlutusVM`, a transaction evaluator) would skip about 0.2 ms of machine
  setup per `evaluateScript` call. They would sit beside `evaluator`, not replace it.
- `evaluator` does not profile. Profiling needs the compiler output behind a script, so it is done
  on the JVM; the deprecated `evaluateScriptProfile` keeps working meanwhile.
