# Lean types for Scala types

How a case class or an enum of a contract becomes a Lean type with its `Data` encoding, written
into the project's Lean workspace. It is the first part of a larger step: a workspace that stays,
holds the contract's types, statements and programs as Lean modules, and takes proofs written by
hand ([overview §6.4](../verification-overview.md#64-lean-manual)).

Status: the types are built. `LeanTypes` is in `scalus.verify.lean`; the linear vesting example
has its types in `scalus-examples/jvm/src/test/lean/LinearVesting/LinearVesting/Generated/Types.lean`.
"What comes next" is proposed.

## Why

A check of `blaster-uplc` says which `Data` a variable ranges over with a predicate,
`Data → Bool` ([`DataEncoding.leanPredicate`](uplc-blaster.md)). The predicate of a script context
is a term of some thousand characters, in which no field has a name. It serves a check that is
written, run and thrown away. It does not serve a person:

- a statement over it cannot be read: `fs5_6` is the withdrawals;
- a proof by hand has nothing to do induction on, or to take a field of;
- the Universal Annotation Language states properties over typed values, as
  `∀ (p : VestingParams) (ctx : ScriptContext), …`, and converts them where the program is called.

A Lean type says the same as the predicate, with the names the Scala source has.

## What is written

For every type asked for, and every type its values are made of, each once:

```lean
structure Config where
  beneficiary : PubKeyHash
  startTimestamp : Integer
  duration : Integer
  initialAmount : Integer
  deriving DecidableEq, Repr

def Config.toData (x : Config) : Data :=
  Data.Constr 0 [(PubKeyHash.toData x.beneficiary), (Data.I x.startTimestamp), (Data.I x.duration), (Data.I x.initialAmount)]

def Config.fromData : Data → Option Config
  | Data.Constr t fs => if t == 0 then (match fs with | [d0, d1, d2, d3] => … | _ => none) else none
  | _ => none

def Config.wellFormed : Config → Bool
  | _ => true

def Config.isEncoding (d : Data) : Bool :=
  match Config.fromData d with | some x => Config.wellFormed x | none => false
```

| Scala | Lean |
|---|---|
| a case class | a `structure` of its fields |
| a case class encoded as its one field (`PubKeyHash`) | a `structure` of that field, whose `Data` is the field's |
| an enum, a sealed trait | an `inductive`, a constructor for each, in the order of their tags |
| `BigInt`, `ByteString`, `Boolean`, `Data` | `Integer`, `ByteString`, `Bool`, `Data` |
| a list | `List` of the items' type |
| a map (`SortedMap`, `AssocMap`, a list of pairs) | `List` of pairs |

The shapes come from `DataLayout`, which reads the `Data` representation the compiler gives a type
with the verifier's options. So the Lean encoding is the one the predicate of a check says, and the
one a compiled program reads. `DataLayout.shape` gives one level of a type by the types of its
parts; the layout of a type is now derived through it, so the two cannot differ.

The four functions of a type:

- **`toData`** is the encoding of a value. A statement over a typed variable passes `toData x`
  to the program.
- **`fromData`** reads a `Data` by its shape: the tags, the number of fields, lists and maps where
  they are to be.
- **`wellFormed`** is what a value has to meet besides its shape. Today that is one thing: the keys
  of a `SortedMap` of byte strings or integers ascend strictly. A Lean list of pairs does not say
  that, so a statement over a typed variable takes `wellFormed x` as a premise where the type has
  such a part (`Codec.wellFormed` is none where it has not).
- **`isEncoding`** is the predicate of a check, by another road: a `Data` is read, and what is
  read is well formed.

No function is passed as an argument. A list is encoded by a walk of its own, `List_TxOut.toData`,
a recursive definition, and an `Option` is taken apart by `match`, not by `bind`. Blaster gives the
solver a function argument as a lambda with quantified axioms, on which Z3 did not finish a check
of a script context ([mapping a variable](uplc-blaster.md#mapping-a-variable)); the definitions
are to be usable in a statement Blaster is asked.

### Names

A type has its Scala name without the package, and its type arguments after it:
`Option_Integer`. Where two types of a module would have one name, or the name is one the
declarations are written with (`Data`, `List`, `Option`), the later one has 64 bits of the hash of
its key after it. A field or a constructor that is a Lean keyword is written `«from»`.

The functions of a type are in its namespace, where Lean sees a constructor's name before a
type's. Plutus names some constructors as their types, `OutputDatum.OutputDatum` and `DRep.DRep`;
inside `def OutputDatum.fromData : Data → Option OutputDatum` the result type would be taken for
the constructor. A constructor named as its type, as the type of one of its type's fields, or as a
type the declarations are written with, has a `'` after its name: `OutputDatum.OutputDatum'`.

A walk is named by what it walks: `List_TxInInfo`, `SortedMap_ByteString_Integer`,
`Map_Credential_Integer`.

### What stays `Data`

- **A string** is the byte string of its UTF-8, typed `ByteString`. That it is UTF-8 is not said,
  as the predicate of a check does not say it either. `Codec.exact` is false for a type with one.
- **A type that contains itself**, as a tree. Its Lean type is `Data`, `toData` is the identity,
  and `wellFormed` is the recursive predicate of its encoding, which is exact. Nothing is lost but
  the names of its parts. A Lean `inductive` for it needs `mutual` blocks of types and of their
  walks, and Lean has to see every one as structural; no type of the ledger contains itself, so
  it waits.
- **A part whose encoding is not told** (`DataEncoding.Unknown`: a function, a type the compiler
  gives no `Data` form) is any `Data`, and nothing is said of it. `LeanTypes.untyped` lists them,
  and the module's header says them.

## Where it is written

A workspace's package has a directory for the modules Scalus writes, `<Package>/Generated`. The
types are `<Package>.Generated.Types`. `LeanWorkspace.generated(workspace, "Types")` gives the
module's name and file, and `LeanWorkspace.write` writes a text where the file differs.

- A module there is written again whenever the Scala sources change. It is nobody's to edit.
- It is committed. A proof by hand imports it, and a checkout without Scala built is to be able to
  run `lake build`.
- **It is no part of what a kept result rests on.** `LeanWorkspace.environment` hashes the Lean
  sources of the workspace's package, so that a change of the library makes the kept results
  stale. The modules under `Generated` are left out of that hash: what they say comes from the
  statements and programs an entry has the hashes of already, and a type added to the module is
  no reason to prove every statement of the workspace again.
- The lakefile of a workspace has a second library, `<Package>Proofs`, of every module under
  `<Package>/Generated` and `<Package>/Proofs` (`globs := #[.submodules …]`), so
  `lake build <Package>Proofs` checks the generated modules without an import. The default target
  is the root module alone, which is all a check needs ([statements as Lean
  modules](lean-export.md#a-proof-by-hand)). `LeanWorkspaceTool.init` writes such a lakefile;
  `LinearVesting` has one.

The vesting example writes its types from a test without Lean,
`VestingVerificationSupportTest`: `Config`, `Action` and the script context of version 3. Where
the file is not what the Scala types give, the test writes it and fails once, and the file is
committed with the change. There is no sbt task for it yet ("What comes next").

## The ledger's types: Scalus's own, or a library's

IOG's CardanoLedgerApiBlaster has the ledger's types in Lean: a script context of each Plutus
version, with `IsData` instances and the ledger's validity predicates (`validScriptContext`,
`validSpendingContext`, `validMintingContext`, …). The Universal Annotation Language's examples
state their properties over them. So there are two ways to have a script context in Lean.

**Written from Scalus's types** is what happens unless something else is said. A script context
is the `structure` written from `scalus.cardano.onchain.plutus.v3.ScriptContext`.

- Its layout is, by construction, what a program compiled by Scalus reads.
- Its parts have the names of the Scala source: `ctx.txInfo.inputs`, as a statement written in
  Scala says. The library's are Plutus's: `ctx.scriptContextTxInfo.txInfoInputs`.
- Every part is typed. The library keeps some as `Data`: a `Value` is a `List (Data × Data)`,
  and the validity range is `Data`.
- It needs no package besides those the workspace has.

**Bound to a library's type.** `LeanTypes.of(types, bound)` takes, for a Scala declaration's full
name, the codec of a Lean type that exists: its name, how a term of it is encoded, and what a
value of it has to meet. Nothing is declared for that type, nor for what only it is made of, and
everything around it uses the codec. A contract's own datum and redeemer are still written; a
library has no types for them.

```scala
LeanTypes.of(types, Map(
  "scalus.cardano.onchain.plutus.v3.ScriptContext" -> LeanTypes.Codec(
    "ScriptContext", "CardanoLedgerApi.V3.Contexts.ScriptContext",
    x => s"(IsData.toData $x)", d => s"(IsData.fromData $d)",
    Some(x => s"(validScriptContext $x)"), exact = false)))
```

A statement reaches a program through `toData` only, so no conversion between the two script
contexts is needed: a statement over the library's type is one over the `Data` its values encode
to. What binding gains is the library's validity predicates as the premise, which say more than
the shape (the inputs are valid, the fee is positive, the transaction is balanced), and statements that read as
IOG's do.

What it costs, and why it is not the default:

- **A package to pin.** The library's sources import PlutusCore only, so it may build against the
  revisions the workspace has; its lakefile names branches of Blaster and PlutusCore, which the
  root's requirements override, as in the USDCx workspace
  ([overview §8.4](../verification-overview.md)). That it builds against the fork of PlutusCore
  pinned here is not tried. A new package in the manifest makes every kept result of the
  workspace stale once.
- **That the encodings agree is to be checked**, as `LeanTypesTest` checks the written types: for
  the `Data` of sample contexts, and better as a lemma. `of` takes the codec as said.
- **A selection in a statement needs the library's names.** A statement written in Scala selects
  `ctx.txInfo.inputs`; rendered over the library's type it has to select
  `ctx.scriptContextTxInfo.txInfoInputs`, by a table of names that someone keeps.

The binding is built and tested with a stand-in type; CardanoLedgerApiBlaster is not required by
any workspace of `scalus.verify` yet. It belongs with the export to the Universal Annotation
Language ("What comes next"), whose consumers have that library.

## Tests

`LeanTypesTest`, with the tactic's tests:

- what is declared for a case class, an enum with a list and a sorted map, and a type that
  contains itself, as text;
- that Lean takes the module of a script context and of the other shapes, without an error or a
  warning;
- that Lean encodes as Scalus does: for a `Data` that Scalus's `toData` gave, `isEncoding` holds,
  and `fromData` reads a value whose `toData` is that `Data` again;
- that a `Data` of another shape is no encoding, and that a map whose keys descend is read by
  `fromData` and is not well formed;
- that a value written in Lean by the names of its parts encodes to the `Data` Scalus gives.
- that a type bound to a Lean type that exists is not declared, and is used by its codec;
- that a generated module changes nothing a kept result rests on, and a proof by hand does.

## What comes next

1. **Agreement with the predicate.** For a type whose encoding is exact,
   `T.isEncoding d = <predicate of DataEncoding> d` for every `d`. It is to be stated as a lemma
   of the generated module and proved once per type, so that a statement over `Data` with the
   predicate and one over the typed variable are the same statement. Proposed.
2. **Statements over the types.** A statement is written into the workspace as a Lean module
   ([statements as Lean modules](lean-export.md)), over the variables the tactic quantifies:
   fields and `Data`, not yet the types here. A statement over a typed variable passes `toData x`
   to its programs, with `wellFormed x` as a premise. Proposed.
3. **The ledger's validity in a check.** `UplcBlaster`'s check restricts a context variable `x0`
   to the encodings of its type only. A statement of valid contexts would add the library's
   predicate as a premise of the check, `match IsData.fromData x0 with | some c =>
   validSpendingContext c | none => false` (`validMintingContext` and the others alike), so that
   Lean's counterexamples are transactions the ledger accepts and a statement may rest on what
   only a valid context has, as the balance of the transaction. It needs:
   - CardanoLedgerApiBlaster in the workspace the check runs in. It builds against the revisions
     of Blaster and PlutusCore pinned there (2026-11-11), and Blaster translates all of
     `validSpendingContext` as a premise of such a check: the one pinned before, 6c27ff9, did not
     translate its `isBalanced`, whose `merge` recurses on two lists. A check that holds took 35 s
     with it; one Lean refutes without it did not finish in 15 minutes with it, as the solver has
     to build a balanced transaction;
   - a quantifier, or an option of `forAllData`, that says the contexts are valid. Its premise is
     a Lean predicate beside the type's, not a program Lean runs, so no budget cuts it;
   - the replay of a counterexample to check validity too, so that a context the premise
     excludes is not reported as a refutation: by Lean's predicate evaluated on the values, as
     Lean evaluates a closed statement, or by one written in Scala, which is then a second
     reading of the ledger's rules to keep in agreement.

   Validity bounds no list: the signatories are ascending, of any number. A statement that walks
   a list still needs its limits (`forAllDataLimited`). Proposed.
