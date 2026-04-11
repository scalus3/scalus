# Scalus Tag: Onchain Identification Marker

**Date:** 2026-04-11
**Status:** Approved

## Goal

Add an optional UPLC marker to every compiled Scalus contract so offchain
tooling (explorers, indexers, the Scalus CLI) can identify a script as
"compiled by Scalus" by inspecting its bytes. The marker is a bare
recognizability stamp — no version information, no onchain consumers.

## Tag shape

Wrap the program's top-level term in a pure, inert application:

```
[(lam _scalusTag body) (con string "S")]
```

Concretely: `Term.Apply(Term.LamAbs("_scalusTag", body), Term.Const(Constant.String("S")))`.

### Why this shape

- Works on Plutus V1, V2, and V3 (all support `Apply`/`LamAbs`/`Const`).
- At runtime the lambda is applied to the constant and reduced to `body`,
  preserving semantics.
- The outer shape `Apply(LamAbs(_, _), Const(String("S")))` is distinctive
  enough for tooling to pattern-match against.

### Rejected alternative: `(case (constr 0 []) [body])`

V3-only, ~3 bytes cheaper, but the user explicitly chose the lambda form for
uniformity across Plutus versions.

## Size cost

Measured from the flat encoder (`Term.scala:322-345`, `Constant.scala:214-234`):

- `Apply` tag: 4 bits
- `LamAbs` tag: 4 bits
- `Const` tag: 4 bits
- `String` type-list encoding: 6 bits (cons + 4-bit tag + nil)
- `"S"` value (byte-aligned ByteString blocks): padding + 1 length byte + 1
  data byte + 1 terminator = 24 bits + padding

Total: ~46 bits ≈ **6 bytes** of flat overhead. CBOR adds at most 1 extra
byte for the length header. This is negligible next to typical script sizes.

## Optimizer interaction (critical)

`Inliner.go()` at `scalus-core/shared/src/main/scala/scalus/uplc/transform/Inliner.scala:276`
eliminates `(λx. body) arg` when `x` is unused and `arg` is pure. A
`Const(String)` is pure, so a tag injected **before** the optimizer runs
would be deleted.

**Therefore:** the tag must be injected **after** `compileInlineWithOptions`
and all optimizer passes have produced the final term. The natural site is
the `makeProgram` boundary in `Compiled.scala`, which already receives the
optimized term.

## `compiler.Options` API

File: `scalus-core/shared/src/main/scala/scalus/compiler/compiler.scala`.

```scala
case class Options(
    /* existing fields unchanged */,
    addScalusTag: Boolean = false
) {
    def withScalusTag(enable: Boolean = true): Options =
        copy(addScalusTag = enable)
}

object Options {
    val default: Options = Options()                    // untagged
    val debug:   Options = Options(/* existing */)      // untagged
    val release: Options = Options(/* existing */, addScalusTag = true)  // tagged
    val vanRossem: Options = Options(/* existing */)    // untagged
}
```

**Rationale for defaults:**

- Field default `false` so plain `Options()` stays untagged — no surprise
  script-hash changes for projects that construct `Options` directly.
- `Options.debug` untagged so debug builds stay byte-identical to the
  untagged path during iteration.
- `Options.release` tagged because release artifacts are the ones that want
  identification in the wild.
- A fluent builder `withScalusTag(enable)` lets users derive variants from
  any preset.

## Injection sites

Three call sites in `scalus-core/shared/src/main/scala/scalus/uplc/Compiled.scala`:

- `PlutusV1.makeProgram`
- `PlutusV2.makeProgram`
- `PlutusV3.makeProgram`

All three use identical code, applied to the already-optimized `term`:

```scala
val finalTerm =
    if options.addScalusTag then ScalusTag.wrap(term) else term
Program(version, finalTerm)
```

`Options` is already in scope at each `makeProgram` call via
`CompiledPlutus.program`.

## `ScalusTag` helper

New file: `scalus-core/shared/src/main/scala/scalus/uplc/ScalusTag.scala`.

```scala
package scalus.uplc

object ScalusTag {
    val marker: Constant = Constant.String("S")

    def wrap(term: Term): Term =
        Term.Apply(Term.LamAbs("_scalusTag", term), Term.Const(marker))

    def isTagged(term: Term): Boolean = term match
        case Term.Apply(
              Term.LamAbs(_, _, _),
              Term.Const(Constant.String("S"), _),
              _
            ) =>
            true
        case _ => false
}
```

Centralizes the marker so the injection site, tests, and any future tooling
all agree on the shape.

## Tests

New file: `scalus-core/shared/src/test/scala/scalus/uplc/ScalusTagTest.scala`.

1. **Default is untagged.** For a trivial validator compiled with
   `Options.default`, the outermost term does not match
   `ScalusTag.isTagged`. Verified on V1, V2, V3.
2. **`Options.release` is tagged.** Same validator, `Options.release`, outer
   term matches `ScalusTag.isTagged`. V1, V2, V3.
3. **`withScalusTag(true)` threads through.** A custom `Options` built via
   `.withScalusTag(true)` produces a tagged program.
4. **Semantics preserved.** Tagged and untagged programs evaluate to the
   same CEK result for the same inputs, all three versions.
5. **Size bound.** Tagged program's flat size is at most untagged size + 8
   bytes (measured ~6, generous upper bound).
6. **Optimizer would eat the tag.** Running the optimizer pipeline directly
   on `ScalusTag.wrap(body)` (bypassing `makeProgram`) produces a term where
   `isTagged` is false. Guards the invariant that injection must stay
   post-optimization.

## Documentation

Scaladoc on `Options.addScalusTag` stating:

- Purpose: offchain identification of Scalus-compiled scripts.
- Cost: ~6 bytes of flat/CBOR overhead.
- **Warning:** enabling the tag changes the script hash. Do not toggle for
  an already-deployed contract.

## Out of scope

- Version string embedding. Bare marker only (per user choice).
- CLI `scalus identify` subcommand — can be a later tooling task.
- Retro-tagging already-compiled programs.
- `case/constr` form — rejected for uniformity.
