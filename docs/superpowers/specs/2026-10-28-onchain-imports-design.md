# Simpler on-chain imports – design

Status: option A implemented on branch `worktree-onchain-reexports`. Not merged.

## Goal

A validator should need only:

```scala
import scalus.cardano.onchain.plutus.v3.*
import scalus.cardano.onchain.plutus.prelude.*
```

Stretch goal: only `import scalus.cardano.onchain.plutus.v3.*`.

## What validators import today

Measured over the 77 files in `scalus-examples` that import anything from `v3` (41 of them
use `import ...plutus.v3.*`):

| Import | Files | Why it is needed |
|---|---|---|
| `scalus.compiler.Compile` | 40 | not under `v3` |
| `scalus.uplc.builtin.{ByteString, Data, FromData, ToData}` (all forms) | ~70 | not under `v3` |
| `scalus.cardano.onchain.plutus.v2.OutputDatum` | 21 | `v3` exports `v2.TxOut` but forgot `OutputDatum` |
| `scalus.uplc.builtin.Data.toData` | 29 | extension method in `object Data` |
| `scalus.uplc.builtin.ByteString.{hex, utf8, *}` | ~19 | `StringContext` extensions in `object ByteString` |
| `scalus.uplc.builtin.Data.{FromData, ToData}` | 12 | redundant aliases of `builtin.{FromData, ToData}` |
| `scalus.cardano.onchain.plutus.v1.PubKeyHash` etc. | 8+ | redundant: `v3` already exports them |
| `scalus.cardano.onchain.plutus.v1.Value.getLovelace` | 5 | redundant: extension in the `Value` companion |

`v3/Contexts.scala` already re-exports 23 `v1` names and `v2.TxOut`. The gap is small.

## State of the art

- **Plutus** `PlutusLedgerApi.V3` re-exports `V2.OutputDatum`, `BuiltinData`, `ToData`,
  `FromData`, `BuiltinByteString`. It does **not** re-export `PlutusTx.Prelude`. Plutus
  validators use two imports: the ledger API and the prelude.
  (Checked in a local clone of `IntersectMBO/plutus`, `PlutusLedgerApi/V3.hs`.)

Not verified against source (from memory):

- **Plutarch**: `Plutarch.Prelude` plus `Plutarch.LedgerApi.V3`. Two imports.
- **OpShin**: one import, `from opshin.prelude import *`. Python has no `List`/`Option` to
  shadow.
- **Aiken**: explicit `use cardano/transaction.{...}`, builtins are always in scope.
- **Scala**: `cats.syntax.all.*` is an opt-in umbrella object built from inheritance.

The two-import shape matches Plutus and Plutarch, the APIs Scalus mirrors.

## Scala 3 facts that decide the design

Facts 1–4 and 6 were tested with scala-cli on Scala 3.3.7 and 3.9.0. Fact 5 was tested in
the prototype on Scala 3.3.8 and 3.9.0, and the examples pass on 3.3.9. The snippets below are minimal; the errors are verbatim.

1. **A package cannot be wildcard-exported.** Each prelude member must be listed by name.

   ```scala
   package lib2.prelude { case class Foo(x: Int); def helper = 1 }
   package lib2.v3 { export lib2.prelude.* }
   // Implementation restriction: package lib2.prelude is not a valid prefix
   // for a wildcard export, as it is a package
   ```

2. **Exported types and objects are not ambiguous** with their origin. This compiles:

   ```scala
   package lib.v1 { case class PKH(h: Int); enum Cred { case A(x: Int); case B } }
   package lib.v3 { export lib.v1.PKH; export lib.v1.Cred }
   package user { import lib.v1.*; import lib.v3.*
     object U { val p: PKH = PKH(1); def m(c: Cred) = c match { case Cred.A(x) => x; case Cred.B => 0 } } }
   ```

3. **Exported methods are ambiguous** with their origin, under two wildcard imports. This
   holds for an inherited extension, a direct extension and a plain method. A shared parent
   trait (same symbol, two prefixes) is also ambiguous.

   ```scala
   package lib4.bs {
     trait Api { extension (sc: StringContext) def hex(args: Any*): Int = 1 }
     object BS extends Api { extension (x: Int) inline def toD: Int = x; def plain(x: Int) = x }
   }
   package lib4.v3 { export lib4.bs.BS.{hex, toD, plain} }
   package user4 { import lib4.bs.BS.*; import lib4.v3.*
     object U { val a = hex"00"; val b = 1.toD; val c = plain(1) } }
   // Reference to hex is ambiguous.   (same for toD and plain)
   ```

   A named import (`import ByteString.hex`) wins over the wildcard, so it does not break.
   The prototype hit this in `scalus-core` `TutorialTest.scala`: 5 errors on `hex"..."`.
   14 files in examples, docs and skills use `import scalus.uplc.builtin.ByteString.*`.

4. **Exporting prelude from `v3` silently changes off-chain code.** A wildcard import has
   higher precedence than `scala.*` and `Predef.*`:

   ```scala
   package lib3.v3 { export lib3.prelude.List }
   package user3 { import lib3.v3.*
     object U { val s = List(1, 2) } }   // resolves to lib3.prelude.List, not scala.List
   ```

   The real prelude has `List.apply(args*)` and `Option.apply`. So `List(1, 2)` in off-chain
   code still compiles, but builds an on-chain `List`. The error, if any, appears far away.
   In this repo, 84 files use `import ...plutus.v3.*`. 18 of them do not import prelude
   (tests, transaction builders), and 8 of those use unqualified `List(`, `Option(`, `Some(`,
   `None` or `require(`. The larger population is user off-chain code.
5. **The plugin sees through exported types and objects.** A prototype validator used
   `@Compile`, `derives FromData, ToData`, `OutputDatum` and `Credential` patterns only
   through `v3`. It compiled to UPLC, and its helpers returned the same result on the JVM and
   on-chain. `scalus.importstest.V3ExportsTest` in `scalus-core` keeps this check, so a
   removed export fails `scalusJVM/test`, not only the examples build. The plugin has no export-specific code: a method call on an exported object
   resolves to the original symbol.
6. **Inline forwarders compile.** Exported `inline def require(inline ...)`, the overloaded
   pair and an inline extension compile in scala-cli. Not tested through the plugin, which
   accepts only a literal message in `fail`. An umbrella must test that first.
7. **Prelude givens need no export.** All of them sit in companion objects (implicit scope).
8. **On Scala 3.3, a type nested in an exported object finds its companion givens twice.**
   With `export v1.Credential`, the type `Credential.PubKeyCredential` (written through `v3`)
   finds `given Eq[Credential]` both through the forwarder `v3.Credential` and through the
   companion `v1.Credential`:

   ```
   Ambiguous given instances: both scalus.cardano.onchain.plutus.v3.Credential.given_Eq_Credential
   and scalus.cardano.onchain.plutus.v1.Credential.given_Eq_Credential match type ...
   ```

   This hit `PaymentSplitterValidator` (an `AssocMap[Credential.PubKeyCredential, BigInt]`) once
   its `v1.Credential` import pointed at `v3`. A `type Credential = v1.Credential` plus
   `val Credential = v1.Credential` pair does not have the problem. That is why the pair was
   in `v3` all along. `DCert`, `IntervalBoundType` and `StakingCredential` were older
   `export`s with the same bug; they are now pairs too. `V3ExportsTest` covers all five.

   The trigger is a nested type written out through the export path, not enums as such
   (`need` below takes a `given Eq` from the companion):

   | Use through an exported object | Scala 3.3.7 | Scala 3.9.0 |
   |---|---|---|
   | Enum type: `val c: Cred = Cred.Pk(1); need(c)` | OK | OK |
   | Enum case, type inferred: `val p = Cred.Pk(1); need(p)` | OK | OK |
   | Enum case type written out: `val p: Cred.Pk = Cred.Pk(1); need(p)` | ambiguous | OK |
   | Case class: `val x: Box = Box(1); need(x)` | OK | OK |
   | Class nested in the companion: `val i: Box.Inner = Box.Inner(1); need(i)` | ambiguous | OK |

   So `export` is safe for a case class without nested types (`Value`, `Address`, `TxOut`).
   An object with nested types needs the pair while Scalus supports Scala 3.3. Scala 3.9.0 has
   no such ambiguity, so the pairs can become `export`s once 3.3 support ends.

## Options

### A. Close the gaps in `v3` (recommended)

Add to the export list in `v3/Contexts.scala`. A file may import a name and export it too;
this compiles without ambiguity on 3.3.7 and 3.9.0.

```scala
type OutputDatum = scalus.cardano.onchain.plutus.v2.OutputDatum // a type/val pair (fact 8)
val OutputDatum = scalus.cardano.onchain.plutus.v2.OutputDatum
export scalus.uplc.builtin.{ByteString, Data, FromData, ToData}
export scalus.compiler.Compile
```

Keep the `type Credential`/`val Credential` pair. Replacing it with `export` breaks a case type
such as `Credential.PubKeyCredential` as an `AssocMap` key (fact 8).

- Source compatible: only types and objects, so no ambiguity (fact 2). Verified: all of
  `scalus-examples` (98 main and 82 test files) and the `scalus-core` tests compile unchanged.
- Binary compatible: adds forwarders to `Contexts$package`. Verified: `mima-jvm` and
  `mima-js` pass.
- Result: a validator needs the two target imports, plus `Data.toData` and
  `ByteString.{hex, utf8}` when it uses them.

### B. Opt-in umbrella, for one import

A new object, for example `scalus.cardano.onchain.plutus.v3.onchain` (name open), that
exports everything in `v3`, every prelude member by name, and `hex`, `utf8`, `toData`.

- Gives the one-import form, as an opt-in. Off-chain code keeps `v3.*` unchanged.
- Cost: a file that imports the umbrella must not also wildcard-import `prelude.*`,
  `ByteString.*` or `Data.*` (fact 3). Mixed files fail with "ambiguous".
- Cost: the export list drifts as prelude grows. It needs a test that every public
  top-level prelude member is in the umbrella.
- Cost: a third path for the same names in docs, IDE completion and error messages.

### C. Re-export prelude from `v3` itself

Rejected. It silently changes off-chain code that imports `v3.*` (fact 4). It breaks files
that also wildcard-import `prelude.*` or `ByteString.*` (fact 3).

## Drawbacks of any export

- **Drift.** Enumerated exports go stale. `OutputDatum` is the proof. Guard: a test that
  every public type in `v1`/`v2` that `v3` uses is reachable from `v3`.
- **IDE.** Go-to-definition can land on the export line first.
- **Error messages** show the alias path, for example `scalus.cardano.onchain.plutus.v3.Data`.

## Plan

1. Land option A: the new exports in `v3/Contexts.scala`. Done on
   the branch. Verified: `scalusJVM/test`, `scalusExamplesJVM/clean` + `test` (1 canceled:
   needs a local Lean build), `ci-js` (includes `scalus.d.ts` and `mima-js`), `mima-jvm`,
   `scalafmtAll`, and the examples compile on Scala 3.9.0.
   Open: give `v2/package.scala` the same builtin and `Compile` exports, for consistency.
2. Clean the examples, docs (`scalus-site/content`) and skills (`scalus-skills/skills`) down to
   the two imports. Done: 40 example files (132 import lines), 6 doc pages, and an "Imports" rule
   in the `contract` skill (plugin 1.6.0). Deploy Site builds the site from the dispatched
   branch, so a deploy before the release shows these snippets to 1.4.1 users.
   The g8 templates pin a released Scalus (1.4.0), which lacks the new exports. Change them in
   the release step that bumps their Scalus version (CONTRIBUTING, "When you release"):
   - `hello.g8/HelloCardano.scala`: remove `import scalus.compiler.Compile` and
     `import scalus.uplc.builtin.Data`. Change the same snippet in
     `scalus-site/content/get-started/index.mdx`, which shows the generated file.
   - `validator.g8/$name;format="Camel"$.scala`: remove the same two lines and
     `import scalus.cardano.onchain.plutus.v1.PolicyId`.
3. Add the drift test for `v3` exports.
4. Option B only if the owner wants one import.
