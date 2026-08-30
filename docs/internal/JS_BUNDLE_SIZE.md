# scalus.js bundle size: attribution and reduction plan

Branch `worktree-js-bundle-size`. Numbers measured 2026-08-30 on this branch's base
(origin/master), inside `nix develop .#ci`.

| build | linker `main.js` | `scalus.js` minified | gzip |
|---|---:|---:|---:|
| baseline | 7,786,217 | 3,109,903 | 704,461 |
| + lever 1, timezone database | 7,381,599 | 2,969,683 | 666,979 |
| + lever 2, scribe | 7,145,269 | 2,880,733 | 643,158 |
| + lever 3, upickle | 6,396,895 | **2,585,053** | **592,165** |
| total | −1,389,322 (−17.8%) | **−524,850 (−16.9%)** | **−112,296 (−15.9%)** |

All three levers are on this branch and pass `scalafmtCheckAll`; MiMa for `scalusJS`,
`scalusCardanoLedgerJS`, `scalusJVM` and `scalusCardanoLedgerJVM`; the full `scalusJS/test`,
`scalusCardanoLedgerJS/test`, `scalusJVM/test` and `scalusCardanoLedgerJVM/test` suites;
`prepareNpmPackage` and `runNpmTests` (vitest plus the `scalus.d.ts` consumer typecheck); the
project's own `quick` alias, which compiles every JVM project including examples, testkit and
the plugin tests; and `scalusNative/Test/compile`, since `scalus-core` also cross-compiles to
Native and three of the four files touched there are in `shared/`.

`scalus.d.ts` is byte-identical to before, and `grep -cE 'upickle|ujson|scribe|threeten'` over
`scalus.js` is 0.

## Method

All of it is wrapped by `scripts/js-bundle-size.sh`:

| subcommand | what it answers |
|---|---|
| `measure [label]` | what the bundle weighs now. The only real measurement. |
| `packages` | size per package and library, through the source map. Ranks; does not measure. |
| `modules` | size per package, summed over per-class modules. Real file sizes, fastLink scale. |
| `attribute` | the raw source-map-explorer JSON, for ad-hoc queries. |
| `graph` | relink one ES module per class, for reachability questions. |

Prefer `modules` over `packages` when the two disagree, for the reason in the caveat below.

### 1. Attribution needs no rebuild

`fullLinkJS` already writes `main.js` next to `main.js.map`, so
`scripts/js-bundle-size.sh attribute` just runs:

```bash
npx source-map-explorer \
  scalus-cardano-ledger/js/target/scala-3.3.8/scalus-cardano-ledger-opt/main.js \
  scalus-cardano-ledger/js/target/scala-3.3.8/scalus-cardano-ledger-opt/main.js.map \
  --json --no-border-checks > sizes.json
```

Aggregate `results[0].files` by path prefix. Library sources appear as
`raw.githubusercontent.com/...` URLs, so third-party weight is attributed too.

**Caveat, learned the hard way: the table is a ranking, not a measurement.** The map marks
positions, so every byte between one mapping and the next is credited to the earlier one, and a
single stray mapping absorbs everything after it. `scala-java-time` was attributed 1,021,082 B
where removing it saved 404,618 B, and it is *still* credited with 821,328 B in a build whose
linker output contains zero occurrences of `threeten`. `--only-mapped` does not fix this: those
bytes really are mapped there. Always confirm a row by grepping the linker output for a marker
of the library, by cross-checking `js-bundle-size.sh modules`, and ultimately with a re-link
delta.

### 2. A re-link is the ground truth

`scripts/js-bundle-size.sh measure <label>` runs `scalusCardanoLedgerJS/prepareNpmPackage` in
the nix shell and prints the linker, minified and gzip sizes. One run is ~5 min from warm
state. Every claimed saving in this document is a delta between two such runs.

### 3. "Why is this reachable?" – the module graph

Do not chase roots by guessing and re-linking; two of my guesses cost a link each and bought
29 B and 503 B. Link once with one ES module per class
(`scripts/js-bundle-size.sh graph`), then every root question is a graph query:

```bash
sbt 'set LocalProject("scalusCardanoLedgerJS") / scalaJSLinkerConfig ~= (_.withModuleSplitStyle(
      org.scalajs.linker.interface.ModuleSplitStyle.SmallModulesFor(
        List("scalus","upickle","ujson","upack","io","scribe","org","com","cats"))))' \
    scalusCardanoLedgerJS/fastLinkJS
```

That writes 2,660 modules into `…/scalus-cardano-ledger-fastopt/`, of which 1,330 are
reachable from `main.js`. Parse each file's `from "./X.js"` imports into a graph, BFS from
`main.js`, and you get both the shortest root path to any module and the full set of importers
of a package. `fastLinkJS` is enough – the reachability analysis is the same as `fullLinkJS`
and it is much quicker. Module split style is a diagnostic here only; shipping it is worse
(measured 2026-08-29).

Note the byte counts from this graph are `fastLinkJS` sizes, roughly 2–3x the optimised
figures. Use them for ratios, not absolutes.

## Where the bytes are

Attribution of the 7.81 MB baseline linker output, ranked:

| bytes | share | what |
|---:|---:|---|
| 1,076,508 | 13.8% | `[unmapped]` – class metadata (`$TypeData`) and the linker prelude. Scales with reachable class count, so it shrinks with every cut below. |
| 1,021,082 | 13.1% | `scala-java-time`, of which `tzdb_java.scala` is 820,180 B (the IANA timezone database) |
| 1,169,009 | 15.0% | Scala stdlib (`Vector` 98 K, `Tuple2` 80 K, `HashMap` 56 K, `HashSet` 51 K, `RedBlackTree` 39 K) |
| ~1,030,000 | 13.2% | cost-model machinery: `PlutusParams` 282,742 + `CostModel` 242,231 + `BuiltinCostModel` 204,396 + upickle 301,494 |
| 980,588 | 12.6% | `scalus.cardano.ledger` domain + borer codecs |
| 607,127 | 7.8% | `scalus.uplc.eval` (`Cek` 112 K) – the machine itself |
| 467,168 | 6.0% | Scala.js javalib (`regex/PatternCompiler` 31 K, `Formatter` 26 K, `BigDecimal` 24 K, `BigInteger` 20 K) |
| 347,754 | 4.5% | `scalus.cardano.onchain` `Contexts` (v3 150 K, v1 101 K) |
| 209,819 | 2.7% | borer |
| 195,956 | 2.5% | `RuntimeLong` (Scala.js `Long` emulation) |
| 141,251 | 1.8% | `scalus.uplc.builtin` |
| 123,474 | 1.6% | scribe, reached from `PlutusScriptEvaluator` |
| 52,667 | 0.7% | paiges |
| 52,064 | 0.7% | `scalus.cardano.node` (Emulator, `JEmulator`, `BlockchainProvider`) |

Confirmed already dead-code-eliminated: `cats-parse` (the UPLC text parser),
`scalus.cardano.txbuilder`, `BlockfrostProvider`, `ProfileFormatter.toHtml`.
`scalus.compiler.sir` survives at only 7,228 B. Protocol-parameter JSON is **not** embedded:
`Macros.inlineProtocolParams` parses it at compile time and emits a `ProtocolParams` literal,
and `__tests__/bundle-size.test.ts` guards that.

## The rule that produced lever 1

**Every public member of a `@JSExportTopLevel` class extending `js.Object` is an export root.**
`@TsIgnore` only hides a member from `scalus.d.ts`; it does not stop the linker from
retaining it and everything it reaches.

`SlotConfig` had:

```scala
@TsIgnore def slotToInstant(slot: Double): Instant = Instant.ofEpochMilli(slotToTime(slot).toLong)
@TsIgnore def instantToSlot(instant: Instant): Double = timeToSlot(instant.toEpochMilli.toDouble)
```

Two methods absent from the public `.d.ts`, unusable from JS (an `Instant` surfaces as an
opaque Scala.js object), kept `java.time` alive: `Instant.toString` reaches
`DateTimeFormatterBuilder`, which reaches the zone rules and the whole tzdb.

**Fix, applied:** move them into `object SlotConfig` as extension methods. Scala 3 finds
extensions in the companion without an import, so `SlotConfigTest`, `TxBuilder` and
`BlockfrostProvider` call sites are unchanged; they are no longer class members, so they are
no longer export roots and the linker drops them.

Measured: **−404,618 B linker, −140,220 B minified (−4.5%), −37,482 B gzip (−5.3%)**;
`threeten` occurrences in `main.js` go from many to zero. (`America/New_York` still appears
once, from javalib `TimeZone`/`Locale` residue, not from `scala-java-time`. Do not re-hunt it.)

Worth auditing the other exported classes for the same pattern: `Emulator`/`JEmulator` and the
`JScalus` value classes.

## Lever 2 – scribe, one importer (applied)

The graph says scribe's 84 reachable modules (401,865 B fastLink) have exactly **one**
importer: `scalus.cardano.ledger.-Plutus-Script-Evaluator$`. `PlutusScriptEvaluator` creates a
`scribe.Logger()` and makes ~15 `log.debug`/`info`/`warn` calls; nothing else on the JS side
touches scribe (`CancelToken` and `BlockfrostProvider` do, but both are already eliminated).

**Fix, applied:** `scalus.cardano.ledger.internal.Logger`, a `private[scalus]` facade with a
per-platform `LoggerPlatform` (JVM → scribe unchanged; JS → `console` behind a level that
defaults to `Warn`). Messages stay by-name, so a disabled level never builds the string – which
matters here because the debug messages serialise datums and script contexts.

Measured: **−236,330 B linker, −88,950 B minified (−3.0%), −23,821 B gzip (−3.6%)**, and zero
`scribe` occurrences left in `scalus.js`.

Note the name: `scalus.uplc.eval.Log` already exists (the VM's trace-log collector) and
`import scalus.uplc.eval.*` is in scope in `PlutusScriptEvaluator`, so the facade cannot be
called `Log`.

## Lever 3 – upickle, three independent root groups (applied)

167 reachable modules, 742,854 B of fastLink output. A cut simulation over the module graph
confirmed these are the **only** roots, and that they have to go together: cutting A+B saves
43,791 B, A+C saves 74,745 B, B+C saves nothing at all, and A+B+C removes 167 of 167 modules.
Partial work here buys nothing, which is worth knowing before starting.

Measured for all three together: **−748,212 B linker, −295,650 B minified (−10.3%),
−50,999 B gzip (−7.9%)**. That is well past the 90–130 KB minified the byte attribution
predicted, because dropping upickle also drops `java.math.BigDecimal` (ujson's number parsing)
and a large share of the class-metadata block.

### Group B – four eager `val`s, not a design problem

`ProtocolParams.scala:97` (`blockfrostParamsReadWriter`) and `:270` (`cardanoCliParamsReadWriter`),
`Types.scala:732` (`CostModels.cardanoCliReadWriter`) and `CostModel.scala:10` (`longReadWriter`)
were plain `val`s, so their bodies ran in the enclosing **module constructor** and linked
whenever anything touched the object at all. `CardanoInfo.mainnet` alone dragged in both
`ProtocolParams` codecs, and `cardanoCliParamsReadWriter` ends in `macroRW`, whose 30-field
derivation is what pulled `CostModels`, `DRepVotingThresholds`, `ExUnits`, `PoolVotingThresholds`,
`ProtocolVersion`, `UnitInterval`, `NonNegativeInterval` and `ExUnitPrices` along with it.

Scala 3 `given` aliases are already lazy; these four explicit `val`s were the anomaly.

**Fix, applied:** `val` → `lazy val`, four lines. The only MiMa consequence is that the
`CostModel` package object loses its static initializer, which needs one filter.

### Group A – the reference cost models

`BuiltinCostModel.vanRossemReferenceD/E = fromJsonString(inlineResource("builtinCostModel{D,E}.json"))`
embeds each resource's pretty-printed **text** and parses it at runtime with upickle.

They are read at exactly one place: `MachineParams.fromCostModels` substitutes the van Rossem
costs for the fourteen builtins that a pre-PV11 cost model has no entries for. Two facts make
this much smaller than it looks: only **14 of the ~100 fields** are ever read, and **D and E
carry identical values for all fourteen**.

**Fix, applied:** `VanRossemNewBuiltinCosts`, one literal set of fourteen costing functions
generated from the JSON, used by `fromCostModels`. `vanRossemReferenceD/E` and
`fromJsonString` stay exactly as they are, public and JSON-backed, and are simply no longer
called, so DCE drops them from `scalus.js` and nothing breaks on the JVM. No MiMa impact.
`VanRossemNewBuiltinCostsTest` compares all fourteen against both vendored models, so a
resource change fails the build instead of silently mispricing a builtin.

### Group C – `Data`'s JSON codec

`DataApi.scala` implemented `Data.toJson`/`fromJson` as an upickle `readwriter[ujson.Value].bimap`.
`Data.fromJson` backs the **exported** `applyDataArgToScript`, so it could not just be deleted.

**Fix, applied:** a jsoniter codec, since the build already depends on jsoniter. All four
surfaces had to move together (the `toJson`/`toJsonIndented` extensions and the
`fromJson`/`toJson` object methods); leaving one on upickle keeps the whole group rooted.
`given DataReadWriter` is still defined for source compatibility and is simply uncalled.

**The encoding cannot be derived from `Data` directly**, which is worth recording so nobody
tries. The format is not internally consistent: `int`, `bytes`, `list` and `map` are single-key
wrapper objects, but `constructor` is a flat two-key object. jsoniter writes an ADT either with
a discriminator field (`{"type":"I","value":42}`) or, with `withDiscriminatorFieldName(None)`,
as a single-key wrapper (`{"I":{"value":42}}`). `withAdtLeafClassNameMapper` fixes the names and
`withInlineOneValueClasses(true)` would unwrap the single-argument leaves to give exactly
`{"int":42}`, but `Constr` has two constructor arguments and must be flat, and no combination of
options expresses that. jsoniter-scala-core also ships no AST by design, so there is no
`ujson.Value` equivalent to lean on.

So the codec follows jsoniter's usual answer for a bespoke format: a `DataJson` DTO that mirrors
the wire shape and *can* be derived, plus a mapping to and from `Data`. `transientNone` is on by
default, so unset fields are omitted, which is what produces the single-key objects;
`transientEmpty` has to be turned **off**, or an empty `fields` would vanish and
`{"constructor":1,"fields":[]}` would come out as `{"constructor":1}`. Field declaration order is
write order, so `constructor` precedes `fields`. The digit limit is declarative here
(`CodecMakerConfig.withBigIntDigitsLimit`), unlike on `ReaderConfig` where it does not exist.

Measured against the hand-written streaming codec it replaced: **+5,999 B minified (+0.23%),
+1,623 B gzip**, for 57 fewer lines and no behaviour change at all, `DataJsonTest` passing
unmodified. The runtime cost is one intermediate tree per parse and per write, which does not
matter on the paths that use it. Kept on that trade.

`DataJsonTest` pins the encoding, since it is public API and reaches npm through
`applyDataArgToScript`. Three behaviour differences from the upickle reader, all deliberate:

- **Large integers now round-trip.** The old reader took `int` through `ujson`'s `.num`, a
  `Double`, so anything past 2^53 came back wrong. The jsoniter codec reads a `BigInt`.
- **An integer may have up to 65536 digits.** jsoniter's default limit is 308, which is too
  small for this domain and would reject values this codec's own writer emits: `expModInteger`
  exists to work on RSA-sized moduli and a 2048-bit one already has 617 digits. A bound is kept
  rather than removed because `fromJson` parses input the caller does not control. Note the
  limit is an argument to `JsonReader.readBigInt`, not a `ReaderConfig` setting; there is no
  `ReaderConfig.withBigIntDigitsLimit` in jsoniter 2.40.1.
- **Stricter and more lenient in one place each.** `{"int":1.5}` is now rejected rather than
  silently truncated to `1`, and `{"constructor":0}` with no `fields` decodes as a constructor
  with no fields rather than throwing.

### On converging the build's two JSON libraries

jsoniter contributes **zero** bytes to `scalus.js` today: its only users, `Blueprint` and
`PlutusDataSchema`, are fully eliminated, so it is JVM tooling. Its whole runtime is ~81 KB of
fastLink output against upickle's 742 KB.

"ujson only" was considered and rejected: ujson pulls 20 of the 70 reachable `upickle.core`
modules regardless, so it has a floor of ~240 KB where removing upickle outright reaches zero,
and it needs the same hand-written codecs. Porting the remaining upickle users
(`blockfrost/models.scala` with its 46 derivations and 105 `@key`s, `PlutusParams`, `Timelock`,
`DynJson`) buys **no** JS bytes, since all of it is already eliminated, so schedule it as
maintenance rather than as size work. Note `-Xmax-inlines:100` is still required: the
`derives ReadWriter` declarations still exist and are still expanded by the compiler, they are
merely unreachable at link time.

## Guardrail, deferred

`__tests__/bundle-size.test.ts` asserts `scalus.js < 3.5 MB` and that no Blockfrost JSON is
embedded. It reads the **committed** `scalus.js`, and `ci-js` runs `runNpmTests` without
`prepareNpmPackage`, so the test only ever sees the artifact from the last release. That is why
this branch leaves both the committed bundle and the limit untouched: tightening the number
against a stale artifact proves nothing. When the bundle is next regenerated and committed at
release, drop the limit to just above the new size and add two assertions that would have
caught both regressions cheaply:

```ts
expect(bundle).not.toContain("threeten");  // scala-java-time and the tzdb
expect(bundle).not.toContain("scribe");
```

## Lever 4 – small independent cuts

- `ExUnits.showJson` uses `String.format("%.6f", …)`, which links `java.util.Formatter` (26 K)
  and its `BigDecimal` path. It is reachable because `PlutusScriptEvaluationException` builds
  its message with it. Hand-rolled 6-decimal formatting removes that.
- `org.typelevel.paiges` (52,667 B attributed, 151,864 B fastLink) is reached from `Term`, but
  not through `Term.toString`, which is hand-written. The callers are the CEK machine's error
  messages: `Cek.scala:628` (`term.show`) and `:949`, `:957`, `:1258`, `:1393`, `:1545`, `:1568`
  (`term.pretty.render(n).take(m)`). Machine errors are always reachable, so paiges always is.
  Dropping it means rendering terms in machine errors with the plain `toString` instead, which
  is a readability regression on the JVM for about 1.7% of the bundle. Low priority.
- `PlutusScriptEvaluator`'s `prices` default parameter reached `CardanoInfo.mainnet`
  (`ExUnitPrices` literals would avoid it). Measured on its own: **−503 B**. Not worth a
  standalone change; fold it into group B if that work happens.

## Rejected / already falsified – do not re-test

- **Entry-point splitting** (measured 2026-08-29): eval-only entry −4.2%, emulator −0.1%, and
  publishing raw linker output for subpaths costs 1.08 MB gz vs 0.70 MB today. The payload is
  the VM cone, not the emulator.
- **`ESVersion.ES2021`**: tried June 2026, 3.5 KB larger.
- **`withMinify(true)`**: linker output byte-identical; esbuild already does the renaming.
- **Closure Compiler**: deprecated in Scala.js 1.21, incompatible with ESM output.
- **`@noble/*` inlining** (~200 KB): deliberate, keeps `scalus.js` loadable from a plain
  `<script type="module">`.
- **Replacing `CardanoInfo.mainnet` references** to drop an embedded JSON: there is no embedded
  JSON – the macro already inlines a literal. Measured −29 B.
- **`T | Null` instead of `Option[T]` in the `DataJson` DTO**, to avoid a `Some` per field.
  jsoniter recognises unions as nullable (`isNullable` handles `OrType`) and `withTransientNull`
  skips them, but the macro cannot see through a union to a collection's element type: a
  `List[DataJson] | Null` field fails with "Cannot get 1st type argument". Only the scalar fields
  can be unions, and that hybrid measured **−391 B minified, −32 B gzip**, which does not pay for
  a DTO carrying two different optionality conventions. Note also that this build does not set
  `-Yexplicit-nulls`, so `BigInt | Null` is not compiler-checked here anyway.
