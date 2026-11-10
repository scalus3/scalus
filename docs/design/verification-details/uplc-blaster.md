# `UplcBlaster`: the `blaster-uplc` tactic

How `blaster-uplc` from [the verification overview](../verification-overview.md) (§6.2) is built.
Code: `scalus-verification/src/main/scala/scalus/verify/uplcblaster/UplcBlaster.scala` and
`scalus-verification/src/main/lean/ScalusProofs/Run.lean`. Tests: `UplcBlasterTest`,
`PreludeProofsTest` and `UplcBlasterLimitsTest`, and `VestingVerificationTest`,
`HtlcVerificationTest`, `PaymentSplitterVerificationTest` and `MintingPolicyVerificationTest` in
`scalus-examples`. For how to use it, see `scalus-verification/README.md`.

## Pipeline

```
Prop ──lower──► Lowered(binders, body: LeafFormula, leaves: Vector[Program])
                  │
                  ▼
     the check's text, its leaves in files ──► Lean's language server (LSP) of the workspace
                  │
   with binders:  ├─ ✅ Valid ─────────────► Proven (ProofKind.Blaster)
                  ├─ ❌ Falsified ─────────► replay on the Scalus CEK ──► Refuted, or Inconclusive
                  ├─ ❌ Falsified, a witness is asked for ► Inconclusive(no replayable certificate)
                  ├─ ⚠️ Undetermined ──────► Inconclusive
   closed:        ├─ native_decide holds ──► Proven (ProofKind.LeanNative)
                  ├─ native_decide false ──► replay on the Scalus CEK ──► Refuted, or Inconclusive
                  ├─ Lean's own limit of work ► Inconclusive(reason)
                  ├─ backend/tool error ─────► Failed(reason)
                  └─ no result in time ────► Inconclusive(reason)
```

## The fragment

Preparation accepts statements built with:

- universal and existential quantifiers over `BigInt`, `Boolean`, `ByteString`, `String`,
  `Data`, lists and maps, and case classes and enums whose fields are of these types or such
  case classes and enums, at any proposition position;
- `existsLet(w)(x => p)`, which is lowered as the strict binding `let x = w in p` and introduces
  no SMT quantifier;
- `&&`, `||`, `!`, `==>:` and `<=>` over these leaves:
  - a test, `Prop.Bool`;
  - a total `call` whose continuation is a test or another total call;
  - a partial call, `whenReturns(f, a)(r => p)`, whose continuation is the same;
  - `denotes(e)`, where `e` has any type;
  - `equal(a, b)` over `BigInt`, `Boolean`, `ByteString`, `String` or `Data`.

Only quantified variables are limited, because Lean builds their values, and it builds only
`BigInt`, `Boolean`, `ByteString`, `String` and `Data` constants. A call's arguments and result, and the
operand of `denotes`, can have any type: they live inside a test's program, and the compiler passes
them.

**A variable of a case class** is expanded by `lower`, since Lean cannot build its value. It
ranges over the values of its constructor
([statement semantics](prop-semantics.md#1-values-and-quantifiers)), so it stands for one
variable per field, named `variable.field` in `Lowered.binders` and in a counterexample, and a
field that is a case class is expanded in turn. A class or an enum that would be more than 20
Lean variables so, as a `ScriptContext`, is one `Data` variable instead
([Mapping a variable](#mapping-a-variable), rule 3). Each test that uses the variable
starts with `let variable = Constructor(fields…)`, which the compiler lowers to the class's own
representation, as it does for a constructor written in the test. `Quantifiable` has an instance
for every case class whose fields have one. The exception is a function registered without SIR,
which has no declared type: a call to it can pass only `BigInt`, `Boolean`, `ByteString`,
`String` and `Data` values (see "The calling convention").

**A variable of an enum**, or of another type of several constructors such as `Option`, ranges
over the values of all of them. It stands for an integer `variable.case`, which chooses the
constructor, and for the fields of every constructor, named `variable.Constructor.field`, each
expanded as above. Each test that uses it starts with
`let variable = if case == 0 then C0(…) else if case == 1 then C1(…) … else Cn(…)`: the
constructors in the order of their declaration, with the last one for every integer not taken
before, so that each integer is a value and the integer needs no premise. A counterexample
names the constructor by that integer. A constructor passes its type parameters on to the
enum's, so `Option[BigInt]` is the integer and the `BigInt` of `Some`. A type that contains
itself and is passed through its constructors has values of every size, which no set of
variables stands for: where it repeats, its node is one `Data` variable
([Mapping a variable](#mapping-a-variable), rule 1), whose premise is a recursive
predicate ([Premises of a type that contains itself](#premises-of-a-type-that-contains-itself)),
exact where the type's other parts are.
`Quantifiable` has an instance for every type of several constructors; the tactic checks their
fields.

**A variable of a list or a map** is one Lean variable in the form the compiler passes it, not
built from constructors. The form is the lowering's default representation of the type
(`SirTypeUplcGenerator.defaultRepresentation`): a `List` of a type that is `Data` is a builtin
list of `Data`, which Lean has as `List Data` and passes as `Const.ConstDataList`; a list of
pairs is a builtin list of pairs, `List (Data × Data)` passed as `Const.ConstPairDataList`; and
`SortedMap`, `AssocMap` and `Value`, which is its map, are one `Data.Map`, `List (Data × Data)`
passed as `Const.Data (Data.Map …)`.

To Lean the items of such a variable are any `Data`, which are more values than its type has. So the
quantifier restricts it with a predicate derived recursively from the type, along the type's `Data`
representation: `∀ xs, p xs = true → …`, and an `exists`, written `¬∀¬`, becomes `∃ xs, p xs ∧ …`.
An integer is `I`, a byte string `B`, a string `B` of its UTF-8 (which the predicate does not
check), a Boolean `Constr 0|1 []`, a case class or an enum's constructor `Constr` with its tag and
its fields' encodings, a list `List` of its items', a map `Map` of its keys' and values', with the
keys of a `SortedMap` of byte strings or integers in ascending order, and a type whose one field it
wraps, as `PubKeyHash` or `Value`, that field's. The few definitions the predicates use
(`scalusIsI`, `scalusAscendingBytes`, …) are written into a check that needs them, and not into the
library, whose hash every kept result rests on.

A list or a map is checked by a walk of its own kind of elements, a recursive definition that the
check writes before the predicates (`DataEncoding.leanRecursions`):

```lean
def scalusAll_5d0c… : List Data → Bool | [] => true | x :: rest => scalusIsB x && scalusAll_5d0c… rest
```

Its name is a digest of what it checks, so the predicates of one check share it. No definition is
given a predicate as an argument: Blaster gives the solver such an argument as a lambda, with
quantified axioms that define it and say when two functions are equal. On the shaped vesting
guarantee (one input, at most one output and one signatory), Z3 had not decided the check after
15 minutes with three sets of its options; with a walk of each list, it answered the same check in
0.3 s.

The description is public, as `scalus.verify.DataEncoding`: `DataEncoding.of[A]` derives it,
`leanPredicate` and `leanDefinition(name)` give the Lean predicate, a `Data → Bool`, for a Lean
file of one's own. The file has, in order, `DataEncoding.leanDefinitions`, then `leanRecursions`
(the walks of the predicate's lists and maps and the definitions of its types that contain
themselves; `DataEncoding.leanRecursions` of several encodings writes each once), then the
definitions, with PlutusCore's `Data` opened as `Data`. `outside(data)` checks a `Data` on the
JVM. `exact` says whether the predicate holds of exactly the type's values.

Where the predicate is exact, the variable's values are its type's, and it can be quantified
anywhere; Lean's counterexamples are of the type. Where it is not, as for a string, a function, or
keys whose order is not checked, the variable has more values than
its type: it is refused where it would claim a value (an `exists` in a positive position, a `forAll`
in a negative one, either under `<=>`), and a counterexample with a value outside the type is
checked on the JVM and reported as inconclusive. Z3 rarely finds a witness through a recursive
predicate: an `exists` over a list comes back undetermined, and `existsLet` with a witness proves
it. Its length is any length, so a program that walks the list meets [the loops Lean does not
finish](#statements-that-do-not-finish).

**A variable over the `Data` encodings of a type**, `forAllData[A](x => …)`, ranges over the
values of `A` as `forAll[A]` does, each given as its encoding, and states the same: the macro
builds the statement `forAll[A]` builds, and the tactic maps its variable as any
([Mapping a variable](#mapping-a-variable)), by fields, or as one `Data` where it would be more
than 20 Lean variables so, as a `ScriptContext`. A variable as one
`Data` is one Lean `Data` variable, not built from constructors: each test that uses it starts with
`let x = d.to[A]`, so the programs read the `Data` themselves. The quantifier restricts `d` with the
predicate of `A`'s encoding, `DataEncoding.of[A]`, derived as for the items of a list, and the same
rule applies: where it is exact, as for a case class of integers or a tree of them, `x` can be
anywhere; where it is not, as for a type with a string field, it is a variable only where it stands
for every value, and a counterexample outside the type is inconclusive. A variable with limits,
`forAllDataLimited`, is one `Data` whatever its size, as its guards walk its `Data`
([Limits of a variable's collections](#limits-of-a-variables-collections)).
`forAllData` takes a `FromData[A]`, which says that `A` has encodings, and which it does not read;
`forAllDataLimited` reads its variable with `d.to[A]`, which needs it. Its values are the encodings of `A`'s
values, the `Data` that `toData` gives: a map's keys in order, a constructor's tag and number of
fields as declared. That is the `Data` the node gives a script as its context, which it builds
itself, so `forAllData[ScriptContext]` stands for every context a script can get. It is not every
`Data` that `d.to[A]` reads without failing: the decoder reads what it needs and checks no more, so
`Data` with unordered keys, or with more fields than the constructor has, is read as a value too. A
parameter that a transaction supplies, as a datum or a redeemer the script decodes itself, can be
such `Data`: a statement about it quantifies over `Data`, `forAll[Data](d => …)`, and decodes it in
the test.

A parameter of a quantifier is such a variable where its type is written annotated in the lambda,
`forAll((ctx: ScriptContext @UplcRepr(UplcRepresentation.Data), n: BigInt) => …)`, or through an
alias that is: in `forAll`, `exists`, `existsLet`, `returnsWhen` and `failsWhen`, of one variable or
several. The annotation is read from the parameter: on a type argument, `forAll[A @UplcRepr(Data)]`,
the compiler has dropped it by the time the statement is built. The parameter's type is the
quantifier's, which it ranges over, and not a wider one. A call's result and a contract's variables
are given, not quantified over, and take no such annotation: it is a compile error there, and on the
variable of `forAllData`, which is one already. Another representation is the compiler's, as it was.
Such a parameter is a variable of its type, as a `forAllData` one is, and needs no `FromData`.
The decoder of a variable with limits is `d.to[A]`, which the compiler compiles to its own reading of
the type's `Data` representation, the one the premise is derived from, whatever `FromData` is given;
the tactic checks that it is. Another variable as one `Data` is decoded by the tactic's own
`UniversalDataConversion.fromData`.

`<=>` becomes two implications, because each of its operands occurs in both polarities. Each leaf
program takes only the binders in scope at that leaf. Anything else produces a
`CompatibilityReport`, which the one-step API returns as `Unsupported`. That covers other binder
types, a call whose continuation
uses connectives or `whenReturns`, a polymorphic function, and a function whose program takes its
arguments differently from how the tests pass them.

A Valid result for an existential statement is a proof in the same sense as any other Blaster
result. The generated Lean proposition writes `∃ x, p x` in the equivalent form
`¬ ∀ x, ¬ p x`, so the generated quantifier binders all use the universal path. A Falsified
model is not by itself a replayable refutation of a statement that asks for a witness: showing that
none exists requires checking the whole quantified domain, not one concrete CEK run. Such a result
is therefore `Inconclusive`.

A quantifier asks for a witness by its position, not by its name: an `exists` in a positive
position, and a `forAll` in a negative one, under `!` or in the premise of `==>:`. Either side of
`<=>` is in both. `!forAll(n => n * n != 4)` is true, with the witness 2; at a budget no run
finishes in, Lean falsifies it, and the body evaluated at whatever value the model leaves for `n`
would say that it is false. Where no quantifier asks for a witness, every variable ranges over all
its values, and the replayed assignment refutes the statement. That includes `!exists(x => p)`,
which is `∀ x, ¬ p x`: a value with `p` refutes it. `Refuted` is also available for `existsLet`,
because its witness has already been fixed and eliminated.

## Mapping a variable

The tactic chooses how a variable of a case class or an enum reaches Lean, by its fields or as one
`Data`, part by part of the variable's type, from the statement and the types alone. Before, the
statement chose: `forAll` mapped it by its fields, and `forAllData` as one `Data`.

Status: built, the five steps of [the steps](#steps). `lower` records the plan as it maps each
variable, and `Lowered.plan`, a `MappingPlan`, carries it; a node below the root can be `Data`; a
type that contains itself is `Data` where it repeats (rule 1); a node that would be more than 20
Lean variables by fields is `Data` (rule 3), unless the variable asks for a witness, or its `Data`
has more values than its type where its fields have not (rule 2). A `forAllData` variable, and a
parameter written `A @UplcRepr(UplcRepresentation.Data)`, is mapped as a `forAll` one (step 5); a
`forAllDataLimited` variable is `Data` at its root, which its guards walk.

### The mapping does not change what a statement means

The values of a type and their `toData` encodings correspond one to one, so `forAll[A]` and
`forAllData[A]` state the same ([The fragment](#the-fragment)). The mapping changes only whether
the tactic accepts a statement and what its check costs. Both mappings are sound, and a refutation
counts only once its values are replayed on the Scalus CEK, so one mapping cannot prove what the
other refutes. Without a bound, they can differ only between a verdict and none: a proof or a
refutation under one, and under the other an inconclusive result, an undetermined one, a time
limit, or a refusal.

Two things depend on the mapping:

- the budget that `Budget.Auto` finds: decoding a `Data` takes steps that building a value from
  fields does not, and the other way round. A proof at any budget is the same proof;
- the inputs a `ProvenWithin(n)` is of: those on which each test's program ends within `n` steps,
  and its steps include the decoding or the building. So within a budget, one mapping can refute a
  statement on an input whose run ends within `n` steps, where the other proves it within the
  budget, because there the same input's run takes more than `n` steps. The refutation is of the
  statement; the proof within the budget is of a weaker one, which the refutation does not
  contradict.

The mapping shows in the check's text as its variables and its programs, and so in its
fingerprint: two plans that map a node differently give two checks. The reasons of the plan are
not in the text.

### Nodes

A node is a part of the variable's value, named by its path of fields and constructors from the
variable, which is the root. The nodes are the parts `lower` expands a variable into: a case class
into its fields, an enum into the fields of its constructors. For a variable `out : TxOut`:

```
out : TxOut                                    case class  ← decided
├─ address : Address                           case class  ← decided
│  ├─ credential : Credential                  enum        ← decided
│  │  ├─ PubKeyCredential.hash : PubKeyHash     case class  ← decided, of one ByteString
│  │  └─ ScriptCredential.hash : ValidatorHash  ByteString
│  └─ stakingCredential : Option[StakingCredential]   enum ← decided
│     └─ Some.value : StakingCredential        enum        ← decided
│        ├─ StakingHash.cred : Credential      enum        ← decided
│        │  └─ … as credential above
│        └─ StakingPtr.a / .b / .c : BigInt    BigInt
├─ value : Value                               a map: one variable
├─ datum : OutputDatum                         enum        ← decided
│  ├─ OutputDatumHash.datumHash : DatumHash    ByteString
│  └─ OutputDatum.datum : Datum                Data
└─ referenceScript : Option[ScriptHash]        enum        ← decided
   └─ Some.value : ScriptHash                  ByteString
```

A node is decided where it is a case class or an enum, and is not inside a list or a map.

- A leaf, an integer, a byte string, a Boolean, a string or a `Data`, is one Lean variable of its
  own type. There is nothing to decide.
- A class of one field, as `PubKeyHash`, is a node like any case class. By fields it is its
  field's variable; as `Data` it is its field's encoding, which the compiler gives a class marked
  `@UplcRepr(ProductCaseOneElement)`. The choice matters for a field of a string, which is exact
  by fields and not as `Data` (rule 2 below).
- A list or a map is one Lean variable, and nothing below it is decided: a list of any length
  cannot be split into fields, and its items are restricted by the list's predicate. `Value` is
  one, a map.
- The root of a variable mapped as `Data` is a node whatever its type, as that of a
  `forAllDataLimited` variable of a list.

A decided node is mapped one of two ways.

- **By fields.** The node is its children, and each of them is decided in turn. A case class is
  its fields. An enum is an integer that chooses the constructor, and the fields of every
  constructor ([The fragment](#the-fragment)).
- **As `Data`.** The node and all below it are one Lean `Data` variable, restricted by the premise
  of the node's encoding, as a `forAllDataLimited` variable is. The test decodes it with
  `UniversalDataConversion.fromData`, the form the compiler gives `d.to[A]`, which the tactic writes
  itself. Nothing below the node is decided.

Before the rules, `forAll` was the plan with every node by fields, and `forAllData` the one with
the root as `Data`. By fields, `out` is 17 Lean variables, of which `stakingCredential` is 8: the case of the
`Option`, that of the `StakingCredential`, the case of its `Credential` and two hashes, and the
three integers of a pointer. As `Data` at the root, it is one. A plan between them maps `address`
by fields and `address.stakingCredential` as `Data`, in 10 variables:

```lean
∀ (x0 : Integer) (x1 : ByteString) (x2 : ByteString) (x3 : Data) (x4 : List (Data × Data))
  (x5 : Integer) (x6 : ByteString) (x7 : Data) (x8 : Integer) (x9 : ByteString),
  ((p3 x3 = true) ∧ (p4 x4 = true)) → …
```

with each test starting with

```scala
val out = TxOut(
  Address(
    if x0 == 0 then PubKeyCredential(PubKeyHash(x1)) else ScriptCredential(x2),
    fromData[Option[StakingCredential]](x3)
  ),
  x4,
  if x5 == 0 then NoOutputDatum else if x5 == 1 then OutputDatumHash(x6) else OutputDatum(x7),
  if x8 == 0 then Some(x9) else None
)
```

where `p3` is the predicate of the encoding of `Option[StakingCredential]`, and `p4` that of a
`Value`'s map.

### The plan

`lower` decides each node of a variable as it maps the variable, from the root down, and a node's
mapping is final before any test that uses the variable is compiled. The rules decide a node
before anything below it is mapped: rule 3 counts the node's Lean variables by fields with an
expansion of its own, which it does not record. The plan is recorded by the same walk that makes
the Lean variables, with each node's final mapping, so it is what they are. A decision reads only
the statement and the types: the node's type and the types on its path, the encoding of the
node's type (`DataEncoding.of`) and whether it is exact, whether the compiler gives the type a
`Data` form at all (`canBeConvertedToData`), how many Lean variables the node is by fields, the
limits, and whether the variable asks for a witness, which the quantifier's position tells as
`lower` goes.

1. **What the type forces.**
   - A type that contains itself is a node of the same type again on its path, and by fields would
     have no end: the node where it repeats is `Data`. The walk tells it by the types on the path,
     as `expand` does now, not by the encoding. As `Data`, the node's premise is the type's
     recursive predicate ([Premises of a type that contains itself](#premises-of-a-type-that-contains-itself)),
     exact where the type's other parts are, so the variable may be anywhere.
   - A node that the compiler gives no `Data` form, as a class with a function field, cannot be
     `Data`: it is by fields, or the statement is refused where its fields cannot be either. Nor is
     a node `Data` whose encoding `DataLayout` cannot describe at all, as a type nested too deep:
     its premise would hold of every `Data`.
   - A variable with limits (`forAllDataLimited`) is `Data` at the root: the guards walk its `Data`
     ([Limits of a variable's collections](#limits-of-a-variables-collections)).
2. **Where `Data` would lose what the fields have, by fields.** A node that rule 3 would map as
   `Data` is by fields instead (`Reason.LargerByFields(cause)`):
   - where the variable asks for a witness (`NotAsData.Witness`). The solver finds a witness
     over typed variables, and seldom through the premise of a `Data`: an `exists` through even a
     constructor's premise came back undetermined (2026-10-26), and a `Data` that is not exact
     cannot stand for a witness at all;
   - where its encoding as `Data` is not exact while its fields are (`NotAsData.Inexact`), as a
     class with a string field: a string is a Lean `String` variable, while as `Data` it is a byte
     string whose UTF-8 the premise does not check, so a counterexample can be no value of the
     type, and a test that reads the string decodes it with `decodeUtf8`, which the tactic
     refuses (input-output-hk/Lean-blaster#389). Found in step 2: by fields, a class with a
     string field is refused wherever a test builds the value in its `Data` representation, as
     `PlanNamed(name, amount)` is, because building it applies `encodeUtf8`. So by fields is
     exact for a class the compiler builds otherwise, as one marked `@UplcRepr(UplcConstr)`. A
     list or a map whose own predicate is not exact, as a map with keys in an order that is not
     checked, is one variable either way, and is not exact either way: a `ScriptContext` is
     `Data`.
3. **Otherwise by fields,** unless the node by fields is more than 20 Lean variables
   (`MappingPlan.mostByFields`): then it is `Data` (`Reason.Larger`), where it can be one: a node
   with a part that has no `Data` form, or whose encoding is not told, is by fields
   (`Reason.LargerByFields`), as is one of rule 2. The count is of the node with every node below
   it by fields, but where a type repeats, which is one variable (rule 1), and stops past 20. So
   the node that is `Data` is the highest one that is too large, and nothing below it is decided
   or in the plan: a `ScriptContext`, 141 variables by fields, is one `Data` variable, and its
   check is the one `forAllData` gave before step 5, to the bytes of its programs, so the results
   the suites keep of their statements over contexts stand. The number was measured in step 4
   (below).

   By fields, a test builds its value from typed variables, with no run that fails, and the
   solver reasons over integers and byte strings. But a function takes a case class in its `Data`
   representation, so the test encodes every field into `Data` first, and an enum's tag is a
   branch of the symbolic run for each constructor, whether or not the function reads it. As
   `Data`, the value is no work to build, and is read only where the function reads it; but every
   decoding of a symbolic `Data` is also a run that fails, which its premise does not prune
   (`#prep_uplc_run` prepares each test for every value), and the solver works through the
   premise, and seldom finds a witness through a recursive one. The small node is cheaper by
   fields, and the large one as `Data`.

   Counting after the nodes below are decided, a node of a few large fields would be by fields,
   its fields `Data`: a `ScriptContext` would be three variables, its `txInfo` and `scriptInfo`
   each one `Data`. That plan was measured too, and is slower than the context as one `Data`.

The plan depends only on the statement and the types, never on a time or an earlier run, so a
statement has the same plan, and the same check, in every run. Nothing chooses another mapping:
`forAllData` and a parameter written `A @UplcRepr(UplcRepresentation.Data)` state what `forAll`
states, and are another way to write it (step 5).

### What is reported

`Lowered.plan`, a `MappingPlan`, carries the plan (built): each decided node's variable, by its
place among the statement's variables, since two variables can have one name; its path; its type;
its mapping; and why, a `MappingPlan.Reason`, which is `ByFixedPlan` for a node a fixed plan
maps. The result's notes are to keep it. The variables of a
counterexample are named by their nodes' paths, as now
(`out.address.credential.PubKeyCredential.hash.hash`, the field of the `PubKeyHash`); a node as
`Data` is one value, written as Plutus writes `Data`.

### Steps

1. The analysis and the plan in `Lowered`, with the plans of today: by fields for `forAll`, `Data`
   at the root for `forAllData`. No check changes. Built: `lower` records the plan as it maps each
   variable (`MappingPlan`); `MappingPlanTest` also pins the 17 Lean variables of `out` above.
2. A node as `Data` below the root: `fromData` of a variable as a part of the value a test builds.
   Built. `Expansion.decide` maps a node as `Data` where a fixed plan says so
   (`MappingPlan.Fixed`, given to `lower` or with `UplcBlaster.withFixedPlan`), as no rule does
   yet. It is internal, for the tactic's tests and for step 4: a statement still has no way to
   choose. A check under a fixed plan keeps no result: it is a test or a measurement, and a
   statement keeps one result, which its check under the tactic's own plan has. A plan names its
   paths, each of which must be a node the plan was asked about, or lowering fails. A node is decided before it is expanded, so a plan can map
   a type that contains itself as `Data` where it repeats, which rule 1 does (step 3). The node is one
   Lean `Data` variable named by its path, of the type a `forAllData` variable has, so its
   premise, its default value and the check of a counterexample's value are those of the node's
   type. The test reads it with `UniversalDataConversion.fromData`, written as the linker writes
   it. A node with a part that has no `Data` form is refused, as is a `forAllData` variable: a
   function, `Unit`, a value of a Miller loop, or a case class or an enum that holds one, in a
   field or a type argument. Of a case class or an enum the compiler says which
   (`canBeConvertedToData`, false where it gives the type a representation without `Data`), and
   `DataLayout` asks it before it asks for the representation. So is a node whose encoding is not
   told. `MappingPlanTest` pins the 10 variables of `out` above, and
   `UplcBlasterTest` checks `Data` nodes in Lean, in seconds: it proves and refutes through
   `o.owner` of a `BlasterOwned`, whose key a test reads, and proves through the subtrees of a
   `BlasterTree`, one of which a test decodes and matches on. For
   step 4: with `out` mapped as above, a check of a test that reads only which constructor
   `out.address.stakingCredential` has, `None` or `Some`, did not finish within 3 minutes at 100
   steps, as Z3 works through the exact premise of the `Option[StakingCredential]`; a match down
   to the pointer's fields did not finish within 5 minutes at 100 steps by fields either
   (2026-11-03).
3. Rules 1 and 2. A type that contains itself is then a `forAll` variable, where it stands for
   every value. Rule 1 is built: `Expansion.decide` maps a node as `Data` where its type is on its
   own path already (`Reason.ContainsItself`), so `forAll[BlasterTree]` is the root by fields and
   `t.Node.left` and `t.Node.right` as `Data`, the same check as the fixed plan of those two
   nodes. Such a node has no by-fields mapping. Its premise is exact since the recursive premise
   (the next step, below), so it may be anywhere a variable may; before, it was exact at its first
   level only, and a variable that asked for a witness was refused. A node of a type with a part
   that has no `Data` form is refused. Rule 2's choice of fields over an inexact
   `Data` had no node to apply to: no rule mapped as `Data` a node that has fields, until rule 3.
   It was left to step 4, with the knowledge it needs as `lower` goes, whether a quantifier asks
   for a witness. `UplcBlasterTest`
   proves a statement that decodes a subtree of a `BlasterTree`, and refutes one with the
   counterexample `Node(Leaf, Leaf)`, replayed.

   Then, a step of its own: the recursive premise of a type that contains itself
   ([Premises of a type that contains itself](#premises-of-a-type-that-contains-itself)), built.
   It lifts the limit of rule 1's nodes, whose premise described their first level only: a test
   may read the value at any depth below such a node, and a witness Lean gives is a value of the
   type.
4. Rule 3, and rule 2 with it: the suites run under fixed plans, to measure the number. Built,
   with 20. `lower` gives each quantifier its position, and so knows whether the variable asks
   for a witness; the same record says where a variable that is not exact must stand for every
   value. A fixed plan's `asData` maps the nodes above the ones it names by fields, and
   `onlyAsData` every other node, but neither where a type repeats. `MappingPlanTest` checks the
   rules on the plan; no kept result changed, as no variable of the suites' statements is more
   than 20 Lean variables by fields but a `forAllData` one, which stays `Data` until step 5.
   Measured for `forAll`, not for a witness: rule 2 keeps a witness by fields on the earlier
   finding that the solver seldom finds one through a `Data` premise.

   Measured on 2026-11-09 by `MappingMeasurementTest` (scalus-examples, run where
   `SCALUS_MEASURE_PLANS` names the file of its results), which checks statements of the suites
   under fixed plans, each with `Budget.Auto` and 8 minutes, in seconds; the budget the search
   came to is in brackets. A case class by fields and as one `Data`:

   | Statement | The check's Lean variables by fields | By fields | As `Data` |
   |---|---|---|---|
   | `vesting.schedule.zero_before_start` | 5 | 23 (with the server's start) [213] | 3 [135] |
   | `vesting.schedule.returns` | 5 | 2 [261] | 6 [183] |
   | `vesting.schedule.does_not_decrease` | 6 | 5 [397] | 9 [319] |
   | `vesting.schedule.does_not_decrease_for_any_amount`, refuted | 6 | 2 [349] | 6 [271] |
   | `one_shot_mint.mint.seed_mints_the_tokens` | 19 | 59 [2098] | 58 [1681] |
   | `one_shot_mint.mint.seed_mints_any_quantity`, refuted | 19 | 31 [2098] | 12 [241] |
   | `one_shot_mint.mint.burns_without_the_seed` | 19 | 73 [2020] | 60 [1603] |
   | `one_shot_mint.mint.accepted_only_with_the_seed`, refuted | 19 | 29 [1743] | 69 [1843] |

   A `Config` is 4 variables, a `Mint` 19, and the other variables are integers. As `Data` the
   programs take fewer steps, as the test does not encode the fields, but the solver works
   through the premise: a `Config` is two to three times faster by fields, a `Mint` about as fast
   either way.

   A statement over every script context, `forAll[ScriptContext]`, under five plans: the context
   as one `Data`; its `txInfo` and `scriptInfo` as `Data`, which is what counting after the nodes
   below are decided gives; its `scriptInfo` as `Data`; only the certificate and the proposal of
   its `scriptInfo` as `Data`; and every node by fields.

   | Statement | 1 variable | 3 | 25, of 4 tags | 45, of 10 tags | 141, of 43 tags |
   |---|---|---|---|---|---|
   | `htlc.context.output_without_a_datum_is_rejected` | 28 [138] | 38 [187] | 232 [761] | over 480, past 963 steps | over 480, past 1953 steps |
   | `htlc.context.only_spending_is_validated` | 24 [107] | 22 [156] | 248 [730] | over 480 | over 480 |
   | `vesting.validator.no_datum_is_rejected` | 19 [210] | 32 [259] | 156 [833] | not run | not run |

   The context as one `Data` is the check of the statement as the suites write it, with
   `forAllData`, to the bytes of its programs, and took as long. Every node of a context that is
   by fields is a field the test encodes into `Data` before the validator reads it, and a tag a
   branch of the symbolic run: with its `txInfo` by fields, 25 variables, a check takes 8 to 10
   times as long as with one `Data`. Hence 20, between a `Mint` and a `txInfo`, and the count from the root down.
5. `forAllData` as `forAll`, and its parameter annotation with it. Built. The macros build a
   variable over the `Data` encodings of its type, without limits, as one of its values: the
   statement of `forAllData[A]`, of a parameter `A @UplcRepr(Data)` and of `unlimitedData[A]` is
   that of `forAll[A]`, with its identity, the rules decide its nodes, and no decoder is compiled
   for it. The `FromData` `forAllData` takes is not read, and the annotation needs none.
   `Reason.OverEncodings` is gone; a variable with limits stays `Data` at its root
   (`Reason.Limited`), as its guards walk its `Data`. The suites' `forAllData` variables are script contexts, one `Data` by rule 3 with the
   check as before, to the bytes: no kept result changed, and a run without Lean took every one of
   them as kept. The tactic's tests of a variable as one `Data` at its root, over small types, are
   under a fixed plan (`Fixed.asData("p")`) now. A `forAllData` variable of a small type is by
   fields: an `exists` over a `BlasterPair @UplcRepr(Data)` is proved, where through the premise of
   its `Data` Z3 answered unknown, and one over a class with a string field, which as `Data` was
   refused where it asks for a witness, is a variable by fields (2026-11-09).

### Follow-ups

Left from two reviews of the recursive premise (2026-11-05). Their findings were fixed then: the
guard reads a repeat as its type; a layout cut off where types nest too deep is named apart; a
type's name has 64 bits of its key's hash; the JVM checks of a `Data` walk it in a loop, without
carrying a path below a repeat; and the largest value within the limits takes a repeat's example
through the types around it.

- **One way to follow a repeat to its type.** `DataEncoding.outside`, `Limits.largest`, the check
  of a value's lengths, `LimitGuards`' test of whether a part has a collection, and a test's sample
  each keep a map of the types around a part that contain themselves. A shared one, an unfolding of
  `Again` on the root layout, would close the class of gaps that the guard had: it did not follow
  a repeat at all.
- **A type's definition from its own layout.** A type's definition is written once, from the
  first of its layouts that a check meets, though a layout depends on where the type is: inside a
  type that contains it, the outer type is `Again`; on its own, it is expanded. They mean the same
  where no layout is cut off, which naming the cut ones apart keeps. A definition from the type's
  own closed layout would not rest on that, nor on the order the layouts are met in.
- **One Lean renderer.** `leanPredicate` and the block writer (`LeanRecursions`) each render an
  encoding to Lean: integers, byte strings, constructors, lists, maps, the order of keys. A change
  to one, as a check of UTF-8, would have to be made to both. Both could be one renderer of a test
  on a variable, `leanPredicate` its `fun d => …`.
- **Two layouts cut off at different depths.** Both are named `…_cut`, and Lean defines the name
  from the first met, so one variable's premise can be the other's layout, while the JVM checks a
  value against the variable's own. Both are inexact, so a variable over them stands for every
  value and nothing is unsound; a counterexample that the two disagree on is replayed against a
  layout Lean did not read, and ends as inconclusive. A name of the depth the layout is cut at
  would tell them apart, with the types in it that repeat one of them named for it too.
- **A claim compiled total.** The rejecting form needs a claim that returns, and falls back to
  the statement as written where one fails. A claim compiled so that each place it can fail, an
  `error`, a partial builtin, returns `false` instead would make the rejecting form exact for
  every claim, with neither the claim that it returns nor the fallback: in effect exceptions in
  UPLC, a transformation of the claim and of what it calls.
- **The limits of what the rejected runs read.** A limit on a part the runs never read multiplies
  the guard's shapes (Vesting: 31 min with the outputs limited, 5 without). The tactic does not
  tell which parts a run reads, so the limits are the author's; it could tell from the programs'
  reads, or warn of a limited part that no program reads.
- **Vesting's third guarantee and liveness over real contexts.** Both are tagged `Unfinished`
  (see [What `VestingVerificationTest` covers](#what-vestingverificationtest-covers)): the third
  guarantee's search for its budget climbs past 2500 steps by spurious counterexamples, and an
  accepted run is the whole validator on every path. Lean's profiler on a check written at a small
  budget would show which reads branch; a clause that read the datum once, as the validator does,
  or tighter limits for liveness alone, are what to try.
- **The payment splitters' payouts.** Their statements about a payout, and the optimized spend's
  withdrawal, are tagged `Unfinished` (see
  [What `PaymentSplitterVerificationTest` covers](#what-paymentsplitterverificationtest-covers)):
  their runs are long, 1500 to 3400 steps for a rejection and 3652 to 5569 for an accepted run,
  and Lean's time grows far faster than the budget past about 1100 steps. Lean's profiler on one
  of them at a small budget would show which reads branch most; exact lengths in the limits, one
  shape of each value, are the next thing to try.
- **Liveness under its premise.** `premise ==>: succeeds(validator, …)` has Lean run the validator on
  every context, the premise pruning none. Two accepting forms, which run the validator only where
  the premise holds, were built and measured on the suites (2026-11-09), and dropped:
  - One program, `if premise then validator else true`, which returns `true` exactly where the
    statement holds, for a premise that returns. The suites' premises do not: HTLC's and the
    splitter's read the redeemer with a decoder that fails on other `Data`. The attempt in that
    form ended at a context where the premise fails, after 24 to 52 s, and the statement as
    written was checked after it.
  - `premise ==>: (if premise then validator else true)`, which states the same on every value,
    so with no fallback. One program runs the premise and then the validator, so the budget must
    cover both, and a check has one budget for every program: the premise's own is explored at
    it too. The three `Unfinished` liveness statements, Vesting's withdrawal after the end and the
    splitters' honest payouts, did not finish in 20 minutes with `Budget.Auto`: after a spurious
    counterexample at about 2030 steps, Lean was in the premise's symbolic run at 2280 to 2859.
    Of the statements that finish, HTLC's refund from the timeout took 50 s at 4000 steps (24 s as
    written), the mint's two were 10 to 20% faster at budgets 15% higher, and the optimized
    splitter's spend with its withdrawal was no longer proved at its given 1600 steps.

  A budget of each program, the accepting program's the premise's and the validator's, would
  keep the premise's at its own; that is a change of the Lean check.
- **An exact order of the keys of a map keyed by a case class.** Lean's restriction of a map,
  and the tactic's check of a value, test the order of the keys only where they are byte strings
  or integers. A map keyed by a credential, as a context's withdrawals, can then be one whose keys
  are out of order: such a counterexample refutes nothing, and where a statement reads the order,
  as `get` does, no budget proves it. An order derived from the key type's `Ord`, of a type the
  compiler knows it of, would make the restriction exact for these maps.
- **Two walks of one name throw.** `LeanRecursions` fails with an exception where two walks of
  different text have one 64-bit name, inside `lower`, whose other failures are a `Left`. It is
  an invariant, not an error a statement can meet, but it could be told as a reason.

## Premises of a type that contains itself

`DataLayout` marks where a type repeats. The encoding of a type that contains itself is
`DataEncoding.Recursive(name, body)`, and the type inside it, where it repeats, is `Again(name)`.
The name is the type's declaration name and 64 bits of a hash of its key, as
`BlasterTree_3fa2c0d4e1b5a796`: the same in every check, and different for two types of one name or
one type with other arguments. The key is what the type is made of, its declarations and their
arguments through proxies (`SirTypes.recursionKey`), and not the type as shown, which shows every
proxy as `Proxy`.

A type's definition is written once, from the first layout of it that a check meets. A layout of a
type deep in another may be cut off where types nest more than 32 deep, its parts there any `Data`,
so it holds less than the type: it is named apart, `…_cut`, as is every type in it that repeats one
cut so. The name of a type is then always that of a layout that holds all of it, whose definition
is exact where the type's encoding is, and an exact variable never reads a cut one.

Its premise is a recursive Lean definition, `scalusRec_<name>`, which a check writes after the
definitions the predicates use (`DataEncoding.leanDefinitions`), only where a variable's premise
uses it.

```lean
mutual
def scalusRec_BlasterTree_… (d0 : Data) : Bool := (match d0 with | Data.Constr t1 fs2 =>
  (if t1 == 0 then fs2.isEmpty else (if t1 == 1 then (match fs2 with
    | [fs2_0, fs2_1] => scalusRec_BlasterTree_… fs2_0 && scalusRec_BlasterTree_… fs2_1
    | _ => false) else false)) | _ => false)
end
```

- **Lean has to accept it as total.** Each call is of a part the definition took apart with
  `match`, so the recursion is structural on `Data`, through the list of a constructor's fields.
  So the parts are matched as they are, `match fs2_0 with …`, and not applied to a lambda.
- **A list or a map whose elements repeat a type** has a walk of its own in the block
  (`…_items0`, `…_entries0`), which calls the type's definition; a list of other elements has the
  shared walk of its kind, before the block.
- **Types that contain each other** have their definitions in one `mutual` block. A type inside
  that repeats none of the enclosing ones has a block of its own, before.
- **Blaster** translates each definition to an SMT `define-fun-rec`.

So the premise is exact where the type's other parts are: the variable may be anywhere, a witness
Lean gives is a value of the type, and a test may read the value at any depth. On the JVM,
`outside` follows `Again` to its `Recursive`, so a counterexample is checked to any depth.

Measured in Lean alone (2026-11-04), over a tree:

| Statement | Result |
|---|---|
| a valid tree is a `Leaf` or a `Node` (one unfolding) | proved, in seconds |
| the left subtree of the left subtree is a tree (three) | proved, 8 s |
| a valid tree is a `Leaf` (false) | falsified, with a counterexample |
| some valid tree has a left-left part that is neither (false) | falsified |
| some valid tree is a `Node` whose left is a `Node` (true) | undetermined after 60 s |

Z3 seldom finds a witness through a recursive premise, as for a list: `existsLet` gives one.

Not told: a type that contains itself under other arguments each time, as a `T[A]` that holds a
`T[List[A]]`, does not repeat: its key differs at each level. Its layout ends at the most parts,
unknown there.

`DataEncodingTest` holds the encodings to `FromData` and `ToData`, on `Data` the encoding gives,
on such `Data` changed in one part, and on `Data` of no type:

- every `Data` the premise admits is read by `FromData` as a value from which `ToData` gives it
  back;
- every `Data` read so is one the premise admits, but for a type with a `SortedMap`, whose
  `FromData` checks neither the order of the keys nor that they differ.

So a misreading of a representation by `DataLayout` shows in a test, not in a proof.

## Leaves and `LeafFormula`

The leaves of a statement are the nodes of its `Prop` tree that are not connectives. `lower`
compiles each leaf to a program in `Lowered.leaves`. `LeafFormula` keeps the connectives, and
replaces each leaf with its index in `Lowered.leaves`.

| `Prop` leaf | Program, over the binders `x1 … xn` | `LeafFormula` |
|---|---|---|
| `Bool(b)` | `λ x1 … xn. b` | `Test(i)` |
| `Call(f, a, r, total = true, k)` | `λ x1 … xn. (λ r. k)(f a1 … am)` | `Test(i)` |
| `Equal(a, b)` | `λ x1 … xn. a = b`, with `equalsInteger`, `equalsData` or Boolean equality | `Test(i)` |
| `Denotes(e)` | `λ x1 … xn. e` | `Denotes(i)` |
| `Call(f, a, r, total = false, k)` | `λ x1 … xn. f a1 … am`, and the total call's program | `Implies(Denotes(i), Test(i + 1))` |

Every program takes the binders in scope at its leaf, in declaration order, whether that leaf uses
all of them or not. This matters when a connective has a quantified proposition on only one side.

Lean's variables are named by their places (`x0`, `x1`, …), and a program takes the binders as
parameters of their own names, and builds, decodes or binds the values of some of them under those
names. The macros name a variable after its lambda's parameter and the place of the lambda in its
file, so two variables in scope have one name where one inline helper is nested in itself, where two
files have a lambda of one parameter name at one place, or in a statement built by hand. Before it
lowers a statement, `lower` names the variables of every scope apart (`withDistinctNames`): a
variable whose name is that of one in its scope, or where one name is that of a field of the other
variable (`x` and `x.amount`), is renamed, with the uses of its name in its scope. So a test, an
`existsLet`'s witness and a call's argument read the innermost variable of a name, as a Scala
lambda's parameter hides an outer one, and a witness of the outer scope is not captured by an inner
variable (`VariableScopeTest`). The new name is written as the old one, and a statement without
such a variable is unchanged.

Through an inline helper, a test cannot read the variable of an outer expansion: the parameter of
its caller's lambda is no variable of the statement, and lowering refuses the test. A test whose
own variables have one name, which an expansion of a helper could give, is a compile error.

A partial call, `whenReturns(f, a)(r => k)`, is rewritten to `denotes(f(a)) ==>: call(f, a)(r => k)`
before it is lowered: if the function returns, the continuation holds of its result. The
`denotes` leaf returns the function's result rather than a Boolean. Where the call is a
conclusion, the polarity rule reads that `denotes` weakly, so a run of `f` that exhausts the
budget counts as one that may return, and the continuation must then hold within the budget. A
partial function that loops on some inputs therefore cannot be proved. Both leaves run the
function's program.

**Why one program per leaf.** A statement's truth depends on more than two outcomes per leaf. A
leaf can return a value, fail, or exhaust the budget. `!` on a `Prop` is true when a test fails,
while a failure inside one UPLC program would fail the whole program. And the polarity rule reads
each leaf by its position. So the connectives stay in Lean, over one run per leaf.

### Calls

A call is one SIR expression, `(result => continuation)(fn(arguments…))`, where `fn` is an
`ExternalVar` that carries the function's name. A call in the continuation nests the same way.

A function of several parameters is called with a tuple written out. `lower` takes that tuple's
components, the fields of `SIR.Constr("scala.TupleN")`, and keeps the compiler's `Let` and `Decl`
wrappers around each component. The program then applies the function to the components one at a
time, because compiled programs are curried. The data declarations of all the pieces move outside
the whole expression.

### Linking

`compileSirFunction` turns a leaf's SIR into a program:

1. `unlinkModuleDefinitions` removes the module definitions that `compile` put around the
   expression for functions in the function table. It keeps the definitions of other functions
   that the expression still uses, and those are compiled together with the leaf.
2. Each `ExternalVar` that names a table function becomes an outer parameter, before the binders.
3. The SIR is lowered to UPLC with `UplcBlaster.options`
   (`Options.releaseUntagged.copy(valueBuiltins = false)`). The result is applied, with a plain
   `Term.Apply`, to each function's own UPLC program (`Representation.Uplc`).

So a registered function's compiled bytes appear unchanged inside the predicate, and no
optimization runs across that boundary. A `@Compile` function that is not registered is compiled
together with the test, so the optimizer may inline it. Other external references, such as the
compiler's own support functions behind `d.to[A]`, are left to the lowering.

**The calling convention.** The V3 lowering passes every value across a function's boundary in
its type's default representation (`SirTypeUplcGenerator.defaultRepresentation`): an integer as a
constant, a plain case class as a builtin `list data` of its fields (`ProdDataList`), a sum type
as `Data` (`DataConstr`), a type marked `@UplcRepr(UplcConstr)` as `constr` terms. A function
value's representation is `LambdaRepresentation(default(input), default(output))`, and a compiled
program's top-level value is converted to it, so a test calling a linked function and the
function's own program agree when both are lowered with the same options:

- the call's `ExternalVar` has the function's declared type, from its SIR, so an enum constructor
  such as `Circle(r)` is passed as the enum;
- `FunctionDef.fromCompiled` records the program's `UplcSignature`, the representation of each
  parameter and of the result, computed with the options it was compiled with;
- `lower` computes, from the same declared type, the signature the tests call it with under
  `UplcBlaster.options`, and returns `Left` on a mismatch, such as a program lowered by another
  backend. Both sides come from one type, so they differ only where the options do;
- a function registered with a UPLC program but no SIR has no declared type and no signature.
  A call to it takes its type from the values, which fixes the form only of `BigInt`, `Boolean`,
  `ByteString`, `String` and `Data`, so other values are refused: an enum constructor would
  otherwise be passed as a list of its fields.

## Reading leaves by polarity

`renderFormula` reads each leaf by its position. `s` is the leaf's final state after at most
`budget` steps of `runFor` (next section).

| Leaf | Positive: a conclusion | Negative: a premise, or under `¬` |
|---|---|---|
| `Test` | `fromFrameToBool s = some true` | `fromFrameToBool s ≠ some false ∧ failed s = false` |
| `Denotes` | `halted s = true` | `failed s = false` |

`→` flips the polarity of its premise and `¬` that of its operand; `∧` and `∨` keep it. A
positive reading is strong: the program halts within the budget with the right result. A
negative reading is weak: the program neither halts with the wrong result nor fails within the
budget, so a run that exhausts the budget counts as possibly right. The overview (§6.2) shows why
a proof at any budget then holds without the budget. A check within a budget (`Budget.Within`)
reads a run the budget cuts the other way, in the statement's favour, and proves less: [A check
within a budget](#a-check-within-a-budget).

Every reading is an equation between `Bool` values, for two reasons:

- Blaster translates `fromFrameToBool` on every goal. A `Prop`-valued match on
  `.Halt (.VCon (Const.Bool true))` fails to translate, on `Fin`, whenever the goal is
  falsifiable.
- `native_decide` can evaluate them (closed statements, below).

Replay reads the same `LeafFormula`, over the outcomes of the Scalus CEK (`holds`, below).

### `runFor`

PlutusCore's `runSteps` returns `State.Error` both when a program fails and when the step budget
runs out. `ScalusProofs/Run.lean` defines:

- `runFor v s n`, which makes the same steps as `runSteps`, but ends a run that is still going
  after `n` steps in the state it reached, not in `State.Error`. After `runFor`, `State.Error`
  means that the program failed.
- `runProgramFor p params n`, which is `cekExecuteProgram` over `runFor`, and is computable.
- `fromFrameToBool`, `failed` and `halted`, the readings above.
- `#prep_uplc_run name script inputs budget`, which is `#prep_uplc` over `runProgramFor`. It
  builds `fun xs => runProgramFor script (inputs xs) budget`, optimizes it with Blaster's
  `Optimize.main`, and defines `name` as noncomputable.

  Blaster's optimizer can write a case split the kernel rejects (Lean-blaster#154). It rewrites
  the type of a branch's binder apart from the condition: `¬ true = b` becomes `false = b`, and
  `¬ (¬ p ∧ ¬ q)` becomes `p ∨ q`, where `Blaster.dite' c t f` asks for `c` and `¬ c`.
  `repairedDite` restates the binder of each branch that does not use it as `c` or `¬ c`: a binder
  the body does not use may be of any type, so the run means what it meant. Where the kernel still
  rejects the run, as for a branch that uses its binder, `name` is the run as written, and
  `#blaster` expands it again, in its own optimization. On the deployed one-shot minting script,
  with the lists it reads limited to one element, the run as written cost `#blaster` 858 s of
  optimization, and the repaired run 81 s.

The weak readings need `runFor`. With `runSteps`, the negative reading of `denotes` could not
reject `State.Error`, which might be an exhausted budget, so it would be `True`. A premise
`denotes(f(x))` would then say nothing, and `!denotes(e)` could never be proved.

## The rejecting form

A statement that a success implies a claim, `succeeds(validator, ctx) ==>: signedBy(ctx)`, is as
written two programs: the validator's, whose run Lean explores on every path to its end, and the
claim's. The rejecting form is one program, `if claim then error else validator`, which fails on
every value where the statement holds. Lean's run of it ends where the claim holds, and on every
other path at the check that rejects it: it does not run the validator's paths where the claim
holds, which are most of them, and the longest.

Measured on Vesting's "every accepted spend is signed by the beneficiary" (2026-11-05), with the
budget fixed from a replayed context:

| Statement | Limits | Result |
|---|---|---|
| as written, one input | outputs and signatories at most one, values of one token | proved at 2444 steps, 8 min |
| rejecting form, two inputs | as above | proved at 3600 steps, 31 min |
| rejecting form, two inputs | inputs, signatories and the inputs' values only | proved at 3300 steps, 5 min |

A limit on what no run that matters reads costs: the guard walks it, and each shape it lets
through runs the program again. A rejected run reads no output.

`lower` uses the rejecting form for `premise ==>: claim` in a positive position, where the premise
is a success, of an expression (`denotes`) or of a call (`succeeds`), the claim is tests (a
`Bool`, an `Equal` or a conjunction of them, one claim `a && b`), and the statement quantifies over
values. Elsewhere the rewritten statement, which is stronger, would prove less: under `!`, in a
premise, inside `<=>`. A test as the premise may guard a conclusion that fails where it is false,
as `(x != 0) ==>: (10 / x > 1)`, and so may a call whose continuation is a test; the expansion of
`whenReturns` calls the function in its conclusion, which need not return; and a closed statement
has nothing to prune. A check within a budget (`Budget.Within`) is of the statement as written:
there a run the budget cuts counts for the statement, and where the claim is false the rejecting
program runs the validator after it, so its bound would not be that of the validator's run.

The claim and the premise go inside the rejecting program without the definitions the compiler put
around them of the functions the statement registers, as the operands of `==` do: a call of such a
function inside the program is linked to its registered program, as a call at the top of a test is,
and not compiled again from its source.

The rewritten statement claims that the claim returns on every value, and that the rejecting
program fails: a claim that failed would fail the program whatever the validator does, where the
statement is false if the validator succeeds. `lower` lowers the statement as written too
(`Lowered.unrewritten`), over the same binders, and the replay of a counterexample decides by it,
running the premise's program, which Lean does not:

- false as written: a refutation;
- true as written and false as rewritten: the claim fails where the premise does not hold, as
  `denotes(unIData(d)) ==>: equal(iData(unIData(d)), d)` does where `d` is no integer. The rejecting
  form decides nothing.

The rejecting form is an attempt. Where it ends inconclusive, for that reason or another, the
statement as written is checked: a check at a given budget again at that budget, and a search
again from its start. The attempt has half the time, and the statement as written what is left of
it, so the time limit is that of both; where neither decides, the reason is both forms'. Without
a time limit the attempt has none either, and the statement as written is checked only where the
attempt ends. An attempt that failed, as on an error of Lean's, is reported as it is, without the
statement as written. A spurious counterexample of a check at a given budget is not measured in
the attempt, but in the check as written; in a search, a counterexample of the solver's
characters, on whose renamed bytes both forms hold, ends the attempt, as no budget changes it.
Another reason is the solver's byte strings: they
may hold characters no byte string has (Lean-blaster#389), and the claim that the claim returns,
which the statement as written does not make where the premise fails, can be false of them only.
The one-shot mint's deployed statement met it: its counterexample was of such characters, which
replay as bytes on which both forms hold.

A result the statement as written decided says so (`Artifact.asWritten`), with the programs of
the statement as written, and is kept with the note `form: as written`: a later run checks the
statement as written first, and the rejecting form in the time left, where the statement as
written decides nothing now. What is kept rests on both lowerings: the fingerprint has the text
and the programs of both. So a claim that returns `false` where it does not hold gets the speed,
and a statement the statement as written decided is checked as written first.
`premise ==>?: claim` (`Prop.Rejects`) is the rejecting form itself:
it holds where the claim fails ([its semantics](prop-semantics.md#4-connectives)), so its check is
the rejecting program alone, in any position, with no claim that the claim returns and no fallback.
A claim of several tests is the one test `a && b`, which fails where `a` fails, and where `a` holds
and `b` fails. Within a budget its bound is that of its rejecting program. A claim that failed
wherever the premise holds would be proved of every value: `LeanProofs.holdsAsImplicationOn`
reads the statement with `==>:` on a value, on the Scalus CEK, so that a sample the function
accepts shows the claims return `true` there, as the vesting suite's samples do for `spend`'s
clauses.

The one-shot mint's deployed statement as the script reads its context is stated with `==>?:`:
its claim reads the context with the script's own deserializer, and fails where that cannot read
it, where the statement of the context's own type says what the script does. Its rejecting program
runs the deserializer and then the script, so its budget is about twice theirs: 5000 steps, proved
in 75 s.

A guarantee the Verifier makes of a function's clauses, `denotes(body) ==>: clause`, is in the
rejecting form too. A clause that fails where the function fails makes the attempt end
inconclusive, and the first run spends up to half its time on it. The one-shot mint's clauses read
the policy's own tokens, and failed where the mint names none, as the policy does; `ownTokens`
returns none there now, and the rejecting form proves them at 4010 steps, where the statement as
written took 5740.

A call obligation (`Origin.Obligation`) is checked as written. Its premise is the caller's code up
to the call, `reach`, and its claim that code with the precondition at the call, `check`: the
rejecting form would prune nothing, would run that code once more, and its claim fails wherever
that code fails, as at a `require` before the call. Follow-up: where the precondition returns on
every value of its variables, `check` fails exactly where `reach` does, and the obligation is the
one program `check`, which never returns `false` (`true ==>?: check`), so that the caller's code up
to the call runs once instead of twice. That the precondition returns would be a statement of its
own, over the callee's variables.

## The Lean check

For a statement over `x0 : BigInt` and `x1 : Boolean` with one leaf, the tactic writes the file
below. A `Data` binder is declared as `(x2 : Data)` and passed as `Const.Data x2`.

```lean
import ScalusProofs.Run

set_option maxHeartbeats 400000

namespace ScalusProofs.Runtime
open …

#import_uplc leaf0 PlutusV3 single_cbor_hex "/tmp/scalus-uplc-blaster-…/Leaf0.flat"

def arguments (x0 : Integer) (x1 : Bool) : List Term :=
  [Term.Const (Const.Integer x0), Term.Const (Const.Bool x1)]

#prep_uplc_run prepared0 leaf0 arguments 100

#blaster (gen-cex: 1) [∀ (x0 : Integer) (x1 : Bool), (fromFrameToBool (prepared0 x0 x1) = some true)]

end ScalusProofs.Runtime
```

The tactic runs no Lean process for a check. It talks to Lean's language server, `lake serve`,
started once for the workspace and spoken to over the Language Server Protocol: JSON-RPC
messages, each after a `Content-Length` header, on the server's standard input and output
(`JsonRpcSession`, `LeanServer`; see [the Lean server](lean-server.md)).

- **A check is an edit.** The text above is the next version of the server's one document, which
  is never written to disk: `textDocument/didOpen` sends the first check, and
  `textDocument/didChange` every later one. The workspace's libraries are loaded once, and Lean
  elaborates again only from the first command that changed. The header, the imports, is the same
  text in every check, so a check pays for its own commands and not for loading the libraries.
- **The result is the document's diagnostics.** The tactic waits with Lean's request
  `textDocument/waitForDiagnostics` for the version it sent, and takes what
  `textDocument/publishDiagnostics` published for that version: Blaster's messages and Lean's
  errors are the output of the check. `$/lean/fileProgress` says which commands Lean has not
  finished, which is where a check that is given up was.
- **The leaves are files** in a directory of the server's, which the text names, and are removed
  after the check.
- **The server does not build** the workspace, so it must be built (`lake build`).
- **One check at a time.** A server runs the checks it is given one after another, and the
  provider `LeanServers` keeps one server per workspace, started at the first check that needs it.
  A check that Lean's worker does not survive, for want of stack or memory, fails its statement;
  where the server itself ends, `LeanServers` starts another for the next check.

- **Time limit.** A check is given up after a time by the clock: ten minutes
  (`UplcBlaster.defaultTimeout`), or the `timeout` of `UplcBlaster(budget, servers, timeout)`.
  The server then closes its document (`textDocument/didClose`), which ends Lean's worker and the
  solver it started, and the tactic is inconclusive. The next check opens the document again,
  and loads the libraries anew. Its reason says where the check was: still running one of the
  programs symbolically, in Blaster's simplification of the statement, or after Blaster had
  started the solver, with the time the solver had worked. On some statements Lean does not return
  ([Statements that do not finish](#statements-that-do-not-finish)), and no other limit ends
  them all: Lean's own, below, is one of work, and Blaster's `timeout` option limits only the
  solver. `withoutTimeout` lifts the limit, and the tactic then waits for Lean.
- **Lean's own limit.** Lean gives a command up when it has done `maxHeartbeats` of work, with
  the error `(deterministic) timeout`. A heartbeat is a unit of Lean's own work, counted the
  same on every machine; it has nothing to do with the connection. The statement is then not
  decided, and the result is `Inconclusive` as well.
  - A command is the symbolic run of one leaf, `#prep_uplc_run`, or Blaster's translation of
    the statement. The solver's time is none of Lean's work and is not counted: a statement the
    solver takes minutes over is not given up for it.
  - A check sets the limit itself, to 400000, twice Lean's default
    (`UplcBlaster.defaultMaxHeartbeats`). In the server Lean's default is about 17 s of the
    symbolic run of a validator.
  - The limit ends a command only where Lean's code looks at it, and more of it is not
    proportionally more time. The vesting statement with open outputs
    ([Statements that do not finish](#statements-that-do-not-finish)) is given up at Lean's
    default after 17 s. At twice that the same run gets past that point, and gave no result in
    8 minutes: a statement like it is ended by the tactic's time limit.
  - `tactic.withMaxHeartbeats(n)` sets another, for a statement known to be large. `0` is no
    limit, and leaves the check to the tactic's time limit.
- **Keeping a check.** `UplcBlaster.writeCheck(lowered, budget, directory)` writes the leaves and
  `Check.lean` into a directory of the caller's, to read or to run by hand with
  `lake lean Check.lean`, for instance with `set_option profiler true`, which reports the time
  of each `#prep_uplc_run`. (`lake env lean` runs it as well, with Blaster interpreted and not
  loaded as the native library it is built as, and several times slower.) With the environment variable `SCALUS_LEAN_KEEP_CHECKS` set to
  a directory, the tactic writes every check it runs there, each in a directory of its own.
  `tactic.withChecksKeptIn(directory)` says the same in code, in place of the variable, and so do
  `LeanProofs` and a `LeanProofsSuite`'s `keptChecksDirectory`.
- **Tracing a run.** With the environment variable `SCALUS_BLASTER_TRACE` set, the tactic says
  what it does, a line at a time on the standard output, each line starting with
  `[uplc-blaster] <statement>:`: the lowering of each statement (its programs and its formula,
  with `(as written)` after the name for the lowering as written), the rejecting form's attempt
  and what it came to, each check with its budget, its time and its outcome, the budget Lean's
  machine counted next, and on each replay what each program did on the values (`#n` is the
  program `n` of the formula), in each lowering.

The verdict is read from the messages Lean reports, which are Blaster's:

- `✅ Valid`;
- `❌ Falsified`, followed by lines `- xN: value` for the counterexample;
- `⚠️ Undetermined`.

Blaster has no machine-readable output: it reports only these log messages. Each counterexample
value is Z3's answer to `(eval xN)`, an SMT-LIB term that can span several indented lines, as a
`Data` value does. `SmtValues` reads it: `(- 1)` is a negative integer, `List.cons` and
`(as List.nil …)` build a list, `Prod.mk` a pair, and `(….ByteString.mk "ABC")` a byte string,
one character per byte, with SMT-LIB string escapes. Z3 abbreviates a large value, such as a long
list, with `(let ((a!1 …)) …)`, and starts each nested `let` on a new line without indentation; a
value is read up to the line that closes its parentheses, and the names are replaced by their
terms. Internally, `Blaster.Smt.Translate.main`
returns a structured `Result` (`Valid`, `Falsified` with the `name: value` strings, or
`Undetermined`). If the log format changes, a command in `Run.lean` can call it and print JSON
instead; the values would still be SMT-LIB terms.

Any other error, such as a translation error or a workspace without the library, is returned in
the `Failed` message. The message drops the `Successfully decoded` lines and is cut to 500
characters.

The artifact records the SHA-256 of each leaf's CBOR, the budget, Lean's output, the
counterexample and the proof kind.

## Closed statements

A statement without binders, such as a sample
`callRef(clamp.ref, (BigInt(9), BigInt(1), BigInt(5)))(r => r == BigInt(5))`, has nothing for
Blaster to search. Blaster can also fail to reduce a concrete program. PlutusCore's
`integerToByteString` uses `Nat.log2`, and Blaster cannot reduce `Nat.log2` on a numeral
([input-output-hk/Lean-blaster#273](https://github.com/input-output-hk/Lean-blaster/issues/273)).
Even the samples of `exp2` and `sqrt` failed to translate.

So the tactic decides a closed statement by evaluation:

```lean
example : (fromFrameToBool (runProgramFor leaf0.script [] 80) = some true) := by native_decide
```

- **No error among Lean's diagnostics:** the result is `Proven`, with `ProofKind.LeanNative`. That
  proof trusts the Lean compiler, through `Lean.ofReduceBool`, instead of Blaster and Z3.
- **`native_decide` reports that the proposition is false:** the statement is replayed, as a
  falsification without values.

All leaves are evaluated in the one check, by the server's worker.

## Replay

A falsification under the budget can be spurious: a positive leaf that needs more steps reads as
false. `replay` works in four steps:

1. It reads Blaster's counterexample and completes it: a binder the model leaves unconstrained
   takes `0`, `false` or `I 0`. Blaster omits such a binder, or Z3 answers with its SMT name,
   such as `$0`. A value it cannot read, such as a byte string with a character
   above 255, makes the result `Inconclusive`.
2. It applies each leaf's program to the values.
3. It evaluates each program on the Scalus CEK, with a budget of a hundred times the mainnet
   per-transaction limit. That budget only guards against a program that does not terminate.
4. Each leaf's outcome is `Returned`, `Failed` or `Exhausted`. `holds` evaluates the
   `LeafFormula` over them, under the semantics of overview §3.3.

The result depends on what `holds` finds:

- **False:** `Refuted`, with the counterexample in the artifact.
- **True:** `Inconclusive`: "Lean's counterexample (x = …) is spurious: the statement holds
  there on the Scalus CEK", and why. At a budget that was given, Lean's machine then counts what
  each leaf's program does on those values (`measureRun`, as a search does to find the next
  budget), and the message ends in one of:
  - "On Lean's machine it holds there from M steps on, so a test needs more than N steps": the
    budget is too small, and M would do for this counterexample.
  - "On the values read from Lean's counterexample the statement holds within N steps of Lean's
    machine, so they do not show what Lean found": more steps change nothing. Where a program
    applies a builtin that Lean's library declares `opaque`, a hash, a signature check or the
    arithmetic of a curve, the message names it: Lean knows of it only that equal arguments give
    equal results, so the solver's counterexample can take a result for what the builtin does
    not give. A statement about such a result writes it as the builtin applied, as
    `sha3_256(preimage)` for an image.
  - that the statement takes more steps there than a search counts, or holds at no budget on
    Lean's machine.

  Within a budget (`Budget.Within`) nothing is counted. A run the budget cuts counts for the
  statement there, and a run that ended within the budget ends so with more steps, so the budget
  is never at fault: the message says that the values do not show what Lean found, and names the
  `opaque` builtins the programs apply.
- **Undecided, because a leaf exhausted the replay budget:** `Inconclusive`.

## Budgets

The budget counts Lean CEK transitions, both `Eval` and `Return`. It is not Plutus `ExUnits`.
Measured, the Lean count is about 1.85 times the number of steps Scalus counts: `min` needs 23
against 13, `abs` 26 against 14, and `gcd 12 18` 161 against 87. A budget of twice Scalus's step
count therefore leaves almost no headroom. In the original spike it produced a spurious
counterexample: optimizer equivalence of `gcd` failed at `x = -19, y = 14` because the programs
halt at steps 282 and 307, while the budget was 300.

Proof cost grows faster than linearly in the budget. On a `gcd` property whose worst input needs
203 steps:

| Budget | Proof time |
|---|---|
| 250 | 2 s |
| 350 | 52 s |
| 500 | no result in 500 s |

Programs whose step count does not depend on their input, such as `abs`, `min`, `max` and
`clamp`, prove over the whole domain at a small budget. A program whose step count grows with its
input, such as `gcd`, needs the input range written into the statement.

### Finding the budget

The budget is `Budget.LeanSteps(n)`, or `Budget.Auto`, with which the tactic finds it: from the
counterexamples that a budget too small gives, each of which says how many steps the statement
needs on it. [Finding the budget](blaster-budget-search.md) is the note about that search, and about
keeping what it found.

### A check within a budget

`Budget.Within(n)` checks a statement at `n` steps with another reading of the runs the budget cuts.
A run that has neither halted nor failed at `n` steps is read in the statement's favour: a test the
statement claims holds of it, and one it assumes does not, and so for `denotes`. The readings are: a
claimed test is `some true` or cut, an assumed one halted and not `some false`, a claimed `denotes`
did not fail, an assumed one halted. Where every run ends within the budget the reading is the
statement's own; so a proof is a proof of the statement on every input on which each test's program
ends within `n` steps. It is a `VerificationResult.ProvenWithin`, with that bound in words, and not
a theorem: no later statement uses it as a lemma, and a kept file keeps it as `proved within`, apart
from a proof. Its fingerprint has its budget, unlike that of a proof: a proof within one budget is
none within another, which reaches other inputs. A kept file has one result for a statement's name,
so a run that checks one statement in two ways, within two budgets or within one and without,
fails, and the two need names of their own. A counterexample is one where the runs the statement rests on ended, so it is replayed
as any other, and refutes the statement itself.

A run the budget cuts is no witness: a check within a budget is of a statement whose quantifiers all
stand for every value, a `forAll` in a positive position or an `exists` in a negative one, and
another is not supported. So a statement about every list, which no budget proves ([Statements that
do not finish](#statements-that-do-not-finish)), is proved within a budget of the lists whose runs
end within it: that a list's `length` is not negative within 400 steps, that `filter` does not
lengthen a list within 100.

### Limits of a variable's collections

`forAllDataLimited[A](x => limit)(x => body)` is `forAllData[A](x => (limit) ==>: body)`, where `limit`
says how many elements the collections of `x` have. A collection is a list or a map of the type, as
its `Data` encoding has it: the fields of a case class are none, and neither is a list in a field of
type `Data`. The macro turns `limit` into `Limits` at compile time, a list of `Limit`s:
`MaxCollectionLength(n)`, every collection, and `Length(path, min, max)`, the collections at a path
of fields, a list's elements, or a map's keys or values. A condition of another kind is a compile
error: a limit is of the shape of the values, and the statement's other conditions are its premise.

A premise does not prune the symbolic run: `#prep_uplc_run` prepares each test's program on its
own, so the run of `body`'s tests follows every length of every list it reads, as without the
premise. The limits act inside the programs instead:

- The tactic writes a guard, a program from `x`'s `Data` to whether it meets the limits
  (`LimitGuards`). It walks the `Data` along the limited paths with `unConstrData`, `sndPair`,
  `headList` and `tailList`, and checks a collection's length with a walk unrolled to one cell more
  than its most, or to its least where it has no most.
- The guard is a test of its own, the premise of the rest under `x`'s quantifier: `∀ x,
  guard(x) → body`.
- Every test in `x`'s scope runs the guard first, and fails where it is false: `if guard(x) then
  test else error`.

Where the limits hold, every test is what it was. Where they do not, the premise is false, and what
the tests do there says nothing: their failing at once is what prunes the run. Lean's run of a test
takes each limited list apart as the guard walks it, into its empty list and a cell, and the test's
own reads of the list find it taken apart. So a loop over a limited list runs to its end, and the
statement of the values within the limits is proved as a theorem. The guard may check less than the
limits say, and never more: it does not look into the constructors of a type of several, nor into
the type again where it repeats, nor at the elements of a list that has no most. Where what it does
not look into has a collection, a constructor's field that is the type again having the type's
collections, the guard is not exact: its variable has more values than
the limits allow, and is a variable only where it stands for every value, as one whose encoding is
not exact is. Under `!` or in a premise, where the statement asks for a witness, the guard could
let one through that the limits exclude, so the tactic refuses it there. A counterexample is checked
against the limits (`Limits.outside`) as it is against the type: a value outside them refutes
nothing. A bound is at most 64 elements, and a guard of nested walks at most 20000 variables, which
the tactic counts before it writes the guard: the walks are unrolled, and larger limits are
refused.

Lean's run relates a read to what an earlier read established only where the two are the same
terms. Blaster's optimizer resolves `tailList` of a list it has taken apart, but not `List.drop`,
Lean's reading of `dropList`, which it does not unfold. Code compiled from a field selection reads a
field with `dropList`, and a hand-written deserializer with `tailList`: two such reads of one field
take it apart as two lists, and a guard of one bounds the other no more. So in a test the guard
checks, the tactic writes each `dropList` of a constant as its steps, the list where it is empty and
its tail otherwise (`DropLists`), and every read of a field is one of tails. Other tests keep their
`dropList`s: the steps take more of Lean's machine. A proof of a statement with limits is then of the
programs written so, and the rewriting is trusted as the lowering is: the two agree on the Scalus
CEK (`DropListsTest`), and a kept result's fingerprint is of the programs written so, not of the
deployed script's bytes. Blaster resolving `List.drop` of a constant count itself would keep the
proved programs as compiled.

A search of a statement with limits starts where every test ends on the largest values that meet
them: each limited list as long as it may be, a map's keys distinct (`Limits.largest`). Below, no
run ends, the guard's neither, and a counterexample would be of runs the budget cuts, which the
solver seldom finds: at 100 steps, Blaster was undetermined.

The deployed one-shot minting script on contexts of at most two inputs, two policies and two tokens
of each (`MintingPolicyVerificationTest`) is proved at 2500 steps, in about 2 min in Lean. Before
the guards and the steps of `dropList`, it did not finish at any bound. By the bound of the inputs,
the policies and the tokens of each:

| Bound | Paths of the two tests | Lean | Solver |
|---|---|---|---|
| 1 | 83 and 142 | 32 s | 0.1 s, valid |
| 2 | 1711 and 2080 | 1 min 50 s | 0.6 s, valid |
| 3 | more than 35000 each | 3.5 h in `#prep_uplc_run` | |

Bounds 1 and 3, and the solver's times, were measured by a spike: it also wrote out the paths and
the solver's query, and took 7 min at bound 2. The paths multiply: each input is compared with the
seed, each policy and token with the policy's own, and each comparison with a symbolic value is two
paths to the end of the script; 88% of them end in the script's error.

The statement can also read the context as the script reads it: with the script's deserializer,
matching the minting context as the policy does (`onlyTheSeedMintsAsRead`). Then the two tests read
the `Data` with the same builtins, neither has a `dropList`, and the proof does not rest on the
steps written for it. At bound 2 it is proved in 1 min 39 s, against 1 min 50 s for the statement
that reads the context through its type: its test has 1525 paths, against 2080, and the script's
test has its 1711 whatever the statement reads. So how the statement reads costs little once the
reads meet; the paths are the script's comparisons. Such a statement takes the deserializer as
given.

## Lean workspace and pins

The workspace is `scalus-verification/src/main/lean`: Lean 4.24.0 (`lean-toolchain`),
`lakefile.lean`, and a committed `lake-manifest.json`. `ScalusProofs.lean` only imports
`ScalusProofs.Run`. The checks are generated for each statement and are never stored.

- **PlutusCore** comes from the fork `nau/PlutusCoreBlaster` @ `fix/case-scrutinee-smt-blowup`,
  until input-output-hk/PlutusCoreBlaster#40 merges. Without the fix, Blaster does not terminate
  on UPLC `case`, which every PV11 program uses. The fix addresses two problems in the
  `Frame.CaseScrutinee` handler:
  - splitting `Const.Bool` left the decision tree stuck on `Bool.casesOn` for a symbolic value;
  - `Ms[n.toNat]?` is an opaque recursive application for a symbolic tag.

  Before the fix `Math.min` at PV11 gave no result in 10 minutes; after it, it proves in seconds.
- **Blaster** comes from `input-output-hk/Lean-blaster` @ `main`, not from the
  `beta-lambda-cache-optimization` branch. The two have diverged, and `main` has the
  `Int.ediv`/`Int.emod` fix that integer programs need.
- **Upstream force-pushes.** A lost revision shows up as `git exited with code 128`. Run
  `lake update` again, and commit the new manifest.
- **The toolchain** is `elan` and `z3`, from the default and `ci` nix shells.

CI: `.github/workflows/lean-proofs.yml` runs on a push that changes the verification module,
the vesting, HTLC, payment splitter or minting policy example or the workflow itself, daily, and
on demand. The daily run is for changes elsewhere, such as in the compiler, which change the
programs the proofs are about. It builds the workspace of the library and that of the vesting
example, then runs `sbt scalusVerification/test` and the four example suites with
`SCALUS_REQUIRE_LEAN=1`, so a test that cannot run Lean fails instead of being canceled. In
`ci-jvm`, without Lean, a suite that keeps its results passes on them, and a test that asks Lean
is canceled.

## Limits

- **Types.** A quantified variable is a `BigInt`, a `Boolean`, a `ByteString`, a `String`, a `Data`,
  a list or a map, or a case class or an enum of them. A list or a map is restricted to its type's
  values by a predicate derived from the type; where that cannot be exact it is a variable only in a
  universal position. A type that contains itself and is passed through its constructors is
  quantified over with `forAll` or `forAllData` by fields down to where it repeats, and as `Data`
  there, and with `forAllDataLimited` as one `Data`: either way with a recursive premise, exact
  where its other parts are. `forAll[Data](d => denotes(d.to[A]) ==>: …)` does not stand for it: `d.to[A]`
  checks nothing itself, and the premise holds of every `d` that the program's conversion to `A`'s
  representation reads without failing, which is part of what makes `d` a value of `A`, and a part
  that depends on the representation. Nor can one constructor of `Data`, such as `Data.I`, be
  quantified over: Lean's variable for it would be any `Data`. Quantify over its field. The operands
  of `equal` must be `BigInt`, `Boolean`, `ByteString`, `String` or `Data`. Call arguments and
  results can have any type.
- **The type's values, not the ledger's.** A variable's values are those of its type, and an exact
  predicate is exact for the type: a `Value` with an amount of zero or a policy without tokens is a
  value of `Value`, which the ledger never puts in a context. An `exists` can be shown by such a
  value, and a statement about contexts the ledger builds needs the ledger's rules as premises.
  `forAllData[A]` ranges over the encodings `toData` gives, which are not all the `Data` that
  `d.to[A]` reads: for `Data` a transaction supplies, quantify over `Data`.
- **Byte strings.** Lean's model stores a byte string as a `String`, so a `ByteString` variable
  ranges over more than byte strings. A proof holds of them all the same. A counterexample can
  have characters above 255: each is read as one of the highest bytes that neither the
  counterexample nor a constant of the programs uses, in the order of the characters, the same
  one wherever it is, so equality, length and order see what the solver saw where the bytes in
  use are all below them. The replay decides (`UplcBlaster.renaming`). A renamed counterexample
  under which the statement holds says nothing of the budget: the search doubles it, as for a
  falsification without values. Where more such characters than unused bytes are left, the value
  is outside its type, and the result is inconclusive.
- **Strings.** A Plutus string, like Lean's, is a sequence of Unicode scalar values, up to
  `0x10FFFF` without the surrogates. Blaster gives a `String` variable the solver's string sort,
  whose characters are the code points up to `0x2FFFF`, surrogates among them. `SmtValues` reads
  each `\u{…}` escape as one character, so two surrogates are not taken for the pair that would
  make one character. A counterexample's surrogates are read as characters that are used nowhere,
  from U+E000 on, and the replay decides; its reason says how the values were read. Appending and
  comparing strings translate. Renaming characters one to one keeps both, so a statement over
  them whose variables are all universal, or all existential, has the same truth over the
  solver's characters as over Plutus's. `encodeUtf8` and `decodeUtf8` see which characters a
  string has. A statement with variables whose programs apply them is refused when it is lowered.
  Blaster cannot translate them today, because they go through a character's `UInt32` value, a
  `BitVec 32`. Once it can, the solver's characters would decide such a statement. A closed
  statement, which Lean evaluates, may apply them.
- **Blaster's strings are unsound where characters matter.** The solver has no character above
  `0x2FFFF`, and Lean has no surrogate. So `blaster` proves Lean statements that are false, and
  derives `False` from one, through its `blasterProven` axiom: "every one-character string is at
  most U+2FFFF". It also falsifies true ones with a surrogate (Blaster `6c27ff9`, 2026-10-21;
  reported as [input-output-hk/Lean-blaster#389](https://github.com/input-output-hk/Lean-blaster/issues/389)).
  The statements of this tactic do not reach that. Of the string builtins, only `appendString` and
  `equalsString` translate, and neither depends on which characters there are. `encodeUtf8` and
  `decodeUtf8`, which do, are refused on values of variables. A real byte string, ordered or
  measured, has only characters below 256, which the solver has all of.
- **Byte-level builtins.** Blaster cannot translate `BitVec`, whose width is a value index rather
  than a type parameter. The model stores a byte string as a `String`, so comparing, appending
  and measuring byte strings translate, but reading one byte (`indexByteString`) goes through a
  `Char`, whose `UInt32` value is a `BitVec 32`, and the bitwise builtins use `BitVec` directly. A
  statement with binders fails to translate, at every budget, when its programs reach
  `indexByteString`, `shiftByteString`, `integerToByteString` or `byteStringToInteger` on a
  symbolic value. `Math.exp2`'s branch for `exp ≥ 0` is
  `byteStringToInteger(shiftByteString(hex"01", exp % 8) ++ integerToByteString(true, exp / 8, 0))`,
  so it is proved only for `exp < 0`.
- **`Value` and array builtins.** PlutusCoreBlaster's flat decoder has the CIP-153 `Value` and
  CIP-138 array builtins commented out. As of 2026-09-23, the upstream PRs were
  input-output-hk/PlutusCoreBlaster#15 (`Value`: conflicting, no review since 2026-05) and #12
  (arrays: mergeable). Until they land, programs are compiled with `valueBuiltins = false`.
- **One check at a time per workspace.** The workspace's language server elaborates one check
  after another, so the statements of a suite take turns; a pool of servers is a later step
  ([the Lean server](lean-server.md)).
- **No proof reconstruction.** Blaster closes a goal with an axiom; see overview §7.

## What `PreludeProofsTest` covers

Every function has samples: closed calls with their expected results. These are checked on the
Scalus CEK and decided in Lean by `native_decide`.

| Function | Properties proved | Negative control |
|---|---|---|
| `abs`, `min`, `max`, `clamp` | fully | yes |
| `exp2` | only for `exp < 0` | yes |
| an `Option` match, a fold over a two-element `List` | as stated | yes |
| `gcd`, `gcd` without the optimizer, `sqrt` | samples only | no |

The properties call their function with a total `call`, whose conclusion is read strongly, so each
one also claims that the function returns. The hand-written suite needed separate totality
theorems for that.

What is not stated, and why:

- **`gcd` properties.** Euclid's algorithm needs more steps for larger inputs, so a property needs
  an input range in the statement. The budgets it then needs are where proof cost explodes.
- **`sqrt` and `isSqrt`.** `sqrt` needs about 405 steps, which is also where proof cost explodes.
- **`log2` and `pow`.** They reach the bitwise builtins.
- **`Data` round-trips** of case classes. `forAllData[A]` can state them, with `equal` over
  their `Data`; none is stated yet.
- **Codegen equivalence** (optimizer on and off, PV10 and PV11, the lowering backends). It can now
  be stated:

  ```scala
  forAll[BigInt, BigInt]((x, y) =>
      callRef(gcd.ref, (x, y))(r => callRef(gcdUnoptimized.ref, (x, y))(s => r == s))
  )
  ```

  For `gcd`, though, it hits the budget problem. The spike proved it at PV10 in 53 s; at PV11,
  attempts at budget 350 did not finish. Start with a function whose step count is constant.

## What `VestingVerificationTest` covers

`scalus-examples/jvm/src/test/scala/scalus/examples/vesting/VestingVerificationTest.scala` states
properties of the vesting example. The JVM tests of `scalus-examples` depend on this module and
on its ScalaTest layer (`LeanProofsSuite`), so a contract's proofs sit next to its other tests.

Its statements about the validator quantify over real script contexts, `forAllData[ScriptContext]`
or a `forAllDataLimited` one, never over a transaction built in the statement: a closed sample is
the only context written out. Each statement over limited contexts has its counterpart over every
context: proved where the validator rejects before it reads a list of the context, and otherwise
an `Unfinished` test, which shows what the limits are for.

| About | Statement | Quantified over | Budget | Time |
|---|---|---|---|---|
| `linearVesting` | nothing is vested before the start | the datum, a `Config`, and the time | 400 | 7 s |
| | everything is vested from the end on | | | 1 s |
| | it returns: no division by a zero duration | | | 1 s |
| | its contract, read from its body: it returns, and for `initialAmount >= 0`, `0 <= vested <= initialAmount` | | | 4 s |
| | it does not decrease with time, for `initialAmount >= 0` | and a second time | | 2 s |
| the validator | an output without a datum cannot be spent | every script context, `forAllData[ScriptContext]` | 600 | 13 s |
| | a non-positive amount is rejected | | | 9 s |
| | nothing but spending is validated | | | 9 s |
| a withdrawal | samples: everything after the end is accepted, and rejected unsigned; halfway, what has vested is accepted, and one lovelace more rejected | closed, `native_decide` | 20000 | under 1 s each |
| | `spend`'s guarantees, stated with `==>?:`: the beneficiary signed | every context of at most two inputs and one signatory, the inputs' values of one token, `limitedData` | 3300 | 149 s |
| | something is withdrawn | every script context, `unlimitedData` | | 10 s |
| | at least the unvested amount stays locked | of at most two inputs, as the first | | not finished (`Unfinished`) |
| | the first and the third | every script context | | not finished (`Unfinished`) |
| | had `linearVesting` expected `initialAmount >= 0`, its call would owe it: refuted; proved under the validator's own contract | of at most two inputs, as the first guarantee | 3000 | 289 s; 310 s |
| | the same two | every script context | | not finished (`Unfinished`) |
| | after the end, everything locked can be withdrawn | every context of at most two inputs and two outputs | | not finished (`Unfinished`) |
| | the same | every script context | | not finished (`Unfinished`) |

The times are of single checks with Blaster compiled, in the suite's Lean server (2026-11-06).

The validator's specification is written in `VestingValidator` itself. `spend` states its three
guarantees at its head; `Verifier.guarantees(spends.ref, fewInputs, Connective.Rejects)` finds them
in the code of `spends`, the validator on a typed context, where the inlined handler ends up, over
the contexts `fewInputs` says ([`Props.limitedData`](../verification-overview.md#37-function-contracts-pre--and-postconditions)). Each
is proved alone: one rejecting program of all three would run every way one of them is false on
into the validator. They are stated with `==>?:`, as a clause reads the datum, the redeemer and the
spent input as the validator does, and fails where they cannot be read, where the validator
rejects too: there it says nothing, and the rejecting program ends at once. A clause written to
return there, reading a default instead, lets each of the datum's twenty-odd unreadable shapes run
on into the validator, which reads the datum after it walks the inputs, and Lean does not finish.

The schedule's contract is an `ensuring` clause of `linearVesting`, read with
`FunctionContract.inSource` and proved over a `Config` variable. Its bound is conditional, not a
precondition, because the validator would not establish one. It has two negative controls, stated
with `contract(linearVesting)(expects, ensures)`: a wrong postcondition and a bound without its
condition; the monotonicity statement has one. The samples are what the statements about
withdrawals are not vacuous of: the validator accepts a withdrawal, and the bound of what stays
locked is exact.

The obligation is `Verifier.obligations` of `spends` over the same contexts; see
[call-site obligations](prop-semantics.md#call-site-obligations). It is checked as written, as
every obligation is ([The rejecting form](#the-rejecting-form)).

What the suite shows about the tactic:

- **A budget just beyond the runs a statement is about.** A rejected run ends at the check that
  rejects it; every further step lets the runs that have not ended read on, and doubles the time.
  The first two guarantees prove at 3300; the third's search climbed from 1173 to 2517 steps, by
  counterexamples of a minute or two each, and did not finish there.
- **An accepted run is the whole validator on every path.** Liveness has Lean run the validator to
  its end on every context within the limits, and Lean was still in that run after 16 minutes.
- **The proofs are not about the published script.** `VestingContract` compiles with
  `Options.release`, which uses the `Value` builtins that Lean's model lacks
  ([Limits](#limits)); the statements are about the validator compiled with `UplcBlaster.options`.
- **Nested `inline def`s of a class cannot be used in a statement.** One that uses another leaves a
  reference to the class's `this` in the leaf, which the plugin rejects. They are in an object.

## What `HtlcVerificationTest` covers

`scalus-examples/jvm/src/test/scala/scalus/examples/htlc/HtlcVerificationTest.scala` states
properties of the hashed timelock example, in the library's Lean workspace. Its statements
quantify over real script contexts, as the vesting suite's do: `forAllData[ScriptContext]`, or a
`forAllDataLimited` one of at most two signatures, with what a statement is about stated as a
premise over the typed context (a refund, a reveal, a validity range). The validator reads the
datum, the redeemer, the validity range and the signatures; a statement holds whatever the rest
of the context is, and of every datum and redeemer that the premise does not exclude. A premise
reads the context as the validator does, so where the datum is not a `Config` it fails, and does
not hold. A test without Lean checks that the samples the validator accepts meet the premises of
the statements that it accepts, which are so not vacuous.

Its source states no contract: the validator is the read of a bound and two or three `require`s
on each of two branches, and a clause would say them again.

| About | Statement | Quantified over | Budget | Time |
|---|---|---|---|---|
| the validator | an output without a datum cannot be spent | every script context, `forAllData[ScriptContext]` | 600 | 19 s |
| | nothing but spending is validated | | | 10 s |
| | a redeemer that is neither action is rejected | | | 11 s |
| a refund | rejected unless the committer signs; negative control: whoever signs | every context of at most two signatures | 4000 | 20 s; 20 s |
| | rejected unless its validity range starts at the timeout or later; negative control: after it | | | 22 s; 22 s |
| | from the timeout on, the committer's is accepted; negative control: at any time | | | 24 s; 20 s |
| a reveal | rejected unless the receiver signs | | | 21 s |
| | rejected unless its validity range ends by the timeout | | | 22 s |
| | of what is not a preimage of the image, rejected | | | 20 s |
| | up to the timeout, the receiver's of a preimage is accepted | | | 23 s |
| both | of two spends under one datum, where the refund starts before the reveal ends, one is rejected | two such contexts | | 80 s |
| | the validator accepts a context where it accepts the one with the closure of either bound of its validity range the other way | every context of at most two signatures | | 39 s; 41 s |
| samples | a reveal and a refund are accepted, the reveal up to the timeout and the refund from it, and rejected unsigned, late or with another secret | closed, `native_decide`, on a transaction the ledger's types build | | under 1 s each |

The times are of single checks with Blaster compiled, in the suite's Lean server (2026-11-06).
The suite keeps the results of 23 statements, three of them negative controls that are refuted,
and takes about 8 min with Lean. On its kept results it takes under a minute: a refutation is
replayed, and one statement is asked of Lean every time, the second point below.

What the suite shows about the tactic:

- **A hash of unknown bytes is a function.** Lean's model declares `sha3_256` `opaque`, so a
  proof knows nothing of it but that equal bytes have equal hashes. That is enough for both
  directions of the hash lock: a reveal is accepted under the premise
  `sha3_256(preimage) == image`, and rejected under its negation, where premise and validator
  apply the one function to the one preimage. A proof holds whatever the function is, and says
  nothing of how hard a preimage is to find. A closed statement runs the real hash.
- **A counterexample cannot name a preimage.** To "no reveal is accepted", the solver gives a
  context whose datum has an image it takes for the hash of the preimage. Replayed with the real
  hash that reveal is rejected, so the counterexample is spurious and the result inconclusive:
  one test states this. The tactic's message says that the budget is not at fault, and names the
  hash ([Replay](#replay)). Over a context variable the image is the datum's, a value the solver
  chooses, not one a statement computes, so no negative control can need an accepted reveal: the
  samples, run with the real hash, show a reveal accepted up to the timeout and not after it.
- **A real context costs no more than a written one where the validator reads little.** With the
  context as one variable, a check of a refund or a reveal takes about 20 s, as the former
  statements over a context written field by field, with sixteen parts of it any `Data`, did:
  every run ends within the budget, at most two signatures searched, so a larger budget costs
  nothing. The three statements over every context are rejections before the signatures, at 600.
- **`.toData` on an enum case without fields did not lower**: `Action.Timeout.toData` gave
  "Unsupported conversion for `Action$.Timeout` from ProdDataList to DataConstr", in any compiled
  code and not only in a statement
  ([#377](https://github.com/scalus3/scalus/issues/377)). The suite wrote the redeemer out as
  `constrData(0, [])` until that was fixed, and now writes `Action.Timeout.toData`.

What it does not show: that a bound of the validity range is inclusive or not, which the
validator does not read and the ledger decides; a transaction of more than two signatures; and
the published script, which `HtlcContract` compiles with other options.

## What `PaymentSplitterVerificationTest` covers

`scalus-examples/jvm/src/test/scala/scalus/examples/paymentsplitter/PaymentSplitterVerificationTest.scala`
states properties of the naive and the optimized payment splitters, in the library's Lean
workspace. Its statements quantify over the payees, a `forAllDataLimited` list of two keys, and
over real script contexts, as the vesting and HTLC suites' do. A rejection is stated as what an
accepted context is, `succeeds(validator, …) ==>: claim`, which the tactic checks in the rejecting
form: the validator runs only where the claim is false. Stated as `premise ==>: fails(…)`, the
validator ran on every context within the limits, and Lean did not finish the first statement in
ten minutes. A claim that reads the context as the validator does, and fails where it cannot read
it, is stated with `==>?:`; a test on the Scalus CEK shows each claim true on the samples the
validators accept, and a JVM test shows that the samples meet the premises of the statements that
the validators accept.

| About | Statement | Quantified over | Budget | Time |
|---|---|---|---|---|
| the naive validator | an accepted payout spends an input of a payee | two payees; contexts of two inputs of values of one token, any outputs | | not finished (`Unfinished`) |
| | and no input of another key than a payee's and the contract's | | | not finished (`Unfinished`) |
| | and no contract input with a token besides ADA | the same, values of two tokens | | not finished (`Unfinished`) |
| | the other payee is paid exactly the split; negative control: half | two payees; contexts of two inputs and two outputs at most, values of one token | | not finished (`Unfinished`) |
| | nobody but the payees is paid; every payee is paid | | | not finished (`Unfinished`) |
| | an honest payout is accepted; negative control: any payout | | | not finished (`Unfinished`) |
| the optimized validator | an accepted spend withdraws from the stake credential of the script of the input its redeemer names | any parameter; contexts of two inputs and two withdrawals at most | | not finished (`Unfinished`) |
| | its redeemer names the spent input | | 1730, found | 490 s |
| | a spend that names its input, with its withdrawal, is accepted | | 1431, found | 128 to 160 s |
| | the stake validator pays the other payee exactly the split | as the naive validator's payouts | | not finished (`Unfinished`) |
| | the stake validator accepts an honest payout | | | not finished (`Unfinished`) |
| | an accepted certificate registers or deregisters the stake credential; registration is accepted | any parameter; every script context | 297; 190, found | 8 s; 8 s |
| samples | an honest payout is accepted by the naive validator, and by the optimized one's spend and stake validator | closed, `native_decide` | 5569, 693, 3652 | under 1 s each |

The times are of single checks with Blaster compiled, in the suite's Lean server (2026-11-07).

What the suite shows about the tactic:

- **A limit on what the rejected runs do not read costs the most.** The statements about a
  payout's inputs are rejected in the walk of the inputs, before an output is read. With the
  outputs limited too, to two of one token each, the guard's shapes were thirteen times as many,
  and the rejecting form was still in Lean's symbolic run at 2002 steps after five minutes;
  without, a check at 1295 steps took 26 s.
- **The rejected runs of a payout's inputs are long.** They walk both inputs and the values of the
  contract's (`withoutLovelace`, `getLovelace`): on Lean's counterexamples they took up to 3357
  steps. The budget search climbed there by spurious counterexamples, each round longer than the
  one before, 26 s at 1295 steps, 47 s at 1673, 108 s at 2099, and at 3357 it did not finish in 27
  minutes.
- **A counterexample outside the type stops the search.** Lean's restriction of a context checks
  the order of the keys of a map whose keys are bytes, not of one whose keys are credentials, as
  the withdrawals': its variable ranges over unsorted maps too, which is sound, as a proof is then
  of more values than the type's. But a counterexample with such a map is no context, and the
  search neither replays it nor counts its steps. In the rejecting form it gave up at 747 steps,
  where Lean's machine counts 772 on that counterexample; as written, and with `==>?:`, it doubles
  the budget. The spend's statements are stated with `==>?:`, so that the search doubles.
- **A claim's cost is part of every rejected run.** The rejecting program runs the claim first.
  Stated as "some input of the spent reference has its script withdrawn", the claim searched the
  inputs and looked each one's script up, 1854 steps on two inputs, and the rejecting program
  took 2816; at 3200 steps it did not finish in 15 minutes, where at 1000 a check took 46 s. Stated
  of the input the redeemer names, which the validator reads, the claim reads one input, and with
  "the redeemer names the spent input" it says the same. The search for its budget then climbed
  by spurious counterexamples, 11 s at 728 steps, 16 s at 856, 38 s at 1115, 230 s at 1487 and
  582 s at 1661, and at 1743 it did not finish in the 15 minutes left of 30 (2026-11-09): the
  claim and the validator read the input and look its script up each, and the time grew far
  faster than the budget past 1100 steps. Before the solver's characters in a counterexample were
  counted as Lean's own, the search doubled the budget at 1661, to 3322.
- **A liveness statement wants the budget of its longest accepted run.** That a spend with its
  withdrawal is accepted is stated as written, and Lean runs the validator on every context: its
  search climbs by spurious counterexamples, 633, 752, 1081, 1103, 1122 and 1146 steps, and then
  by counterexamples of two withdrawals, a map keyed by a credential, whose keys' order neither
  Lean nor the tactic checks: they may be no context, but Lean's variable has them, and the
  statement holds on them on the Scalus CEK, so their runs are counted too, 1315, 1366 and 1431
  steps. At 1431 it is proved in 128 to 160 s, after a search of about 10 minutes
  (2026-11-10). Before they were counted, the search doubled the budget there, which did not
  finish, and the test gave 1600 by hand, at which it is proved in 333 s. On the longest spend
  measured, the premise takes 1434 steps of Lean's machine and the validator 1347.
- **What a payout pays is beyond Lean's symbolic run.** The runs of a statement about the outputs
  walk the inputs, the outputs, and the sums of the payees, which the naive validator builds as an
  association list; an accepted payout is 5569 steps on the sample. Lean was still in the
  symbolic run at about 1900 to 2400 steps after five to ten minutes, in both forms.

## What `MintingPolicyVerificationTest` covers

`scalus-examples/jvm/src/test/scala/scalus/examples/oneshotmint/MintingPolicyVerificationTest.scala`
states properties of the one-shot minting policy example, `scalus.examples.oneshotmint`, in the
library's Lean workspace. The transaction that spends the policy's seed output mints
`tokensToMint`, and any other transaction can only burn the policy's tokens.

The specification is written in `MintingPolicy` itself. `mint` is the policy on the
`MintingContext` that a deserializer reads, an inline function that `mintingPolicyScript` calls.
It states two guarantees at its head with `Spec.ensures`:

1. a transaction that does not spend the seed burns each of the policy's tokens that it names,
   and so mints none;
2. the transaction that spends the seed mints exactly `tokensToMint`: each token has the same
   quantity in the mint as there, where a token that is not named has none. The clause is
   written with its own functions, `mintsExactly` and `quantityOf`, and not with the
   `Value.equalsAssets` the code checks, so that the proof compares two descriptions.

Their functions (`spendsSeed`, `ownTokens`, `burnsOnly`, `mintsExactly`, `quantityOf`) are only
called by the clauses, so they are erased with them. When the clauses were added, the scripts'
bytes were compared before and after, and were the same: the version 1 scripts with either
deserializer, the version 2 script and the Scott-encoded one, with and without error traces and
the optimizer. That was a check made once, not a test. What a test pins is the three sizes
`MintingPolicyExampleTest` asserts, with the optimizer, and that erasure removes the clauses and
their functions from the program's SIR.

**The shape is the ledger's.** For a script of version 1 or 2 the ledger's mint starts with an
entry of zero ADA (`LedgerToPlutusTranslation.getMintValueV1V2`). A `Mint` variable is the
policy's parameters (the seed and one token) and what the policy reads of a transaction: one
input, or two where a Boolean field says so; the policy the script runs as; and a mint of the
entry of ADA, a policy of one token, and, where another Boolean field says so, a policy of two.
The solver chooses the Booleans, so one statement covers the four shapes, among them the
example's minting as the ledger gives it to the policy: the seed alone, and after the entry of
ADA the policy's token alone. The example's own test builds its contexts without that entry, and
its burns without an input, so they are not of these shapes. The statements are about the
`MintingContext` built from a `Mint`, not about a script context: the deserializers and the
script on the ledger's context are not covered.

| About | Statement | Quantified over | Budget | Time |
|---|---|---|---|---|
| `mint` | the two guarantees it states, for a mint whose token names are different | a `Mint`, the context built from it | 5740 | 82 s, together |
| | the transaction that spends the seed mints the tokens, wherever the seed is among the inputs | | 2098 | 50 s |
| | a transaction that does not spend the seed burns the tokens | | 2020 | 46 s |

The tactic found the budgets (`Budget.Auto`): from 2020 to 5740. The times are of the tests, each
with its negative control, recalculated at the kept budgets. The suite keeps five statements. Two
of them are negative controls, and both are refuted with a counterexample. In the first, a
transaction spends the seed and mints the token in another quantity, and the policy rejects it.
In the second, the policy accepts a transaction that does not spend the seed: a burn. With Lean
the suite takes about 3 min at the kept budgets.

What the suite shows:

- **The policy relies on the mint being a map.** Clause 2 was first stated for every mint, and
  was refuted. The policy's own tokens were `[("", -1), ("", 0)]`, and `tokensToMint` was
  `[("", -1)]`. `equalsAssets` compares two lists of tokens one entry at a time and allows
  trailing zeros, so the policy accepts. But the name `""` has two quantities there, which no
  mint of the ledger has: its mint is a map, with every name once under a policy, and no
  quantity is zero but that of the entry of ADA. So the guarantees are stated with a
  precondition, the contract `mints_of_distinct_token_names`: the two tokens of the second policy
  have different names. That is all of the ledger's rule the proof needs, and less than the rule,
  which also orders the policies and the names and has no zero quantity.
  `Verifier.guarantees` of that contract proves them where it holds. A clause written with
  `equalsAssets`, as the code is, would have been proved for every mint and said nothing of
  this.
- **Clauses are read only where inlining puts them.** `Verifier.guarantees` reads the clauses in
  the code of the function it is given, outside function values. The policy is the lambda that
  `mintingPolicyScript` returns. Clauses in that lambda would be inside a function value. A
  function that calls `mintingPolicyScript` would not contain them at all: it is a module
  definition, which is not inlined. So the clauses are in `mint`, which is inline, and the
  statement is about a function that calls `mint` on a minting context of the shape. The script
  as a whole is covered by explicit statements.
- **Symbolic Booleans in a shape cost time, not steps.** Choosing the length of two lists with
  two Booleans multiplies the paths by four. In the first version of the shape, which had
  neither the entry of ADA nor the Booleans, the transaction that spends the seed was proved to
  mint the tokens in 16 s at 1996 steps; now it takes 50 s, at 2098 steps.
- **Scala has no `inline def` inside an `inline def`.** The policy's local `check` became a
  function of the object when the body moved into `mint`. Inlining expands it as before, and the
  bytes are the same.

**The script on every context is stated, and not finished.** A test tagged `Unfinished` states of
the script as the ledger runs it, `deployed` (the optimized deserializer and the policy, with a
seed and tokens of the test's own), that only the seed mints:
`forAllData[v1.ScriptContext](ctx => succeeds(deployed, ctx.toData) ==>: onlyTheSeedMints(ctx))`.
The context is one `Data`, with no shape. Within 2000 steps (`Budget.Within`) Lean was still in the
symbolic run of a test after 10 minutes. Within smaller budgets it finishes, and the time grows
steeply with the budget:

| Within | The script alone | The statement |
|---|---|---|
| 300 | 15 s | 11 s |
| 600 | 9 s | 32 s |
| 900 | 69 s | no result in 3 min |
| 1200 | no result in 3 min | no result in 3 min |

The budget bounds the steps of a path, not the length of each list, so the paths are the
combinations of the lengths of the context's lists whose runs fit in it.

**With limits, the script on its context is proved.** The same statement, of the contexts of at
most two inputs, two policies and two tokens of each (`forAllDataLimited`), is proved at 2500 steps:
of the `Data` the node gives the script, deserializer and all. Its other lists are not limited, and
the script does not read them. It is proved also reading the context with the script's own
deserializer (`onlyTheSeedMintsAsRead`), which that statement takes as given.
[Limits of a variable's collections](#limits-of-a-variables-collections) says how, and what larger
bounds cost.

What it does not show: a transaction of any other shape (more than two inputs, policies or tokens
for the deployed script, longer lists for the policy's statements, a transaction without an input,
or a mint without the entry of ADA, which the ledger does not give a script of version 1 or 2); that
the tokens are minted once, which follows from the ledger spending an output once; the script on a
larger context; and the published scripts, which `MintingPolicy` compiles with other options.

## Statements that do not finish

Some statements are natural to write, and Lean does not return on them. `UplcBlasterLimitsTest`,
tests of `VestingVerificationTest` and tests of `PaymentSplitterVerificationTest` state them, with
a time limit: the third of vesting's `spend` guarantees, and that everything can be withdrawn after
the end, over contexts of at most two inputs; and the payment splitters' statements of what a
payout pays. In each, how long a loop runs depends on a quantified value, or the paths of the
symbolic run multiply over the walks of several lists.

These tests carry the ScalaTest tag `Unfinished`. The build leaves it out of `test` and
`testQuick`, in `Test / test / testOptions`, so `testOnly` still runs them: by suite, by name with
`-z`, or all of a suite's with `-n scalus.verify.uplcblaster.Unfinished`. A tag excluded in
`Test / testOptions` itself could not be included again from the command line, and a test marked
`ignore` cannot be run at all.

| Statement | Budget | Result | Time |
|---|---|---|---|
| a list's `length` is not negative | 100 to 800 | spurious counterexample | 5 to 7 s |
| `filter` does not lengthen a list | 100 | spurious counterexample | 4 s |
| | 200 | spurious counterexample | 8 s |
| | 400 | spurious counterexample | 101 s |
| `gcd(x, y) >= 0` | 200 | spurious counterexample | 5 s |
| | 400 | none | over 200 s |
| vesting, of one shape of withdrawal (the suite's former statement): at least the unvested amount stays locked, for any outputs | 1000 | spurious counterexample | 10 s |
| | 1500 | spurious counterexample | 18 s |
| | 2000 to 12000 | none | over 150 s; at 4000, over 7 min |

The times are of `lake env lean` for every check, as the tactic ran before it used the language
server.
That command loads no plugin, so Lean interpreted Blaster's code. The server runs Blaster as the
native library it is built as, and gets through the same work several times faster. Three of the
rows, measured again with Blaster compiled:

| Statement | Budget | Result | Time |
|---|---|---|---|
| `filter` does not lengthen a list | 400 | spurious counterexample | 20 s |
| | 800 | none | over 2 min |
| vesting: at least the unvested amount stays locked, for any outputs | 1600 | spurious counterexample | 8 s |
| | 1610 to 1700 | proved | 8 to 12 s |
| | 1800 | proved | 27 s |
| | 1900 | proved | 62 s |
| | 2000 | proved | 3 min |
| | 2300 | none | over 3 min |
| | 12000 | Lean's limit of work, at Lean's default of 200000 heartbeats | 17 s (159 s interpreted) |
| | 12000 | none, at the tactic's default of 400000 | over 8 min |

So a budget that was beyond reach is now within it, and the next one is not: the doubling of
the paths is the same. A check that ends at Lean's limit is inconclusive like one that reaches
its time limit.

**The vesting statement is proved at the right budget.** It says that a withdrawal is rejected,
and the rejection takes just over 1600 steps: the validator's check of the amount is its last
before it reads the outputs. From there every 100 steps let the runs that pass the check read
another output, and double the time. The budget of a whole accepted withdrawal, 12000, was the
wrong one for it. A statement that a program fails wants the budget of the failing run, not of
the program's longest. `VestingVerificationTest` proves it with `Budget.Auto`, which comes to
1608 ([Finding the budget](blaster-budget-search.md)), and keeps the run at 12000 as a statement
Lean does not finish.

**The others cannot be proved at any budget.** Those of `UplcBlasterLimitsTest` are about runs
that go through the whole list, or through every round of the recursion. Some input needs more
steps than the budget, and there a conclusion, read strongly, is false. Lean's counterexample is
that input, a long list for instance, and the replay finds the statement true on it:
inconclusive. Proving them takes induction, which this tactic does not do. Within a budget
([A check within a budget](#a-check-within-a-budget)) they are proved of the inputs whose runs end
within it.

**Where the time goes.** There are two mechanisms. The reason of a check that is given up names
the one it was in. They are also told apart with `set_option profiler true` in a check written
by `writeCheck`, and by which process is busy.

- **Paths in Lean.** `#prep_uplc_run` has Blaster run the CEK machine symbolically for the whole
  budget, and every test of a symbolic value splits the run. `length` asks one question per
  element, whether the list ends, and one answer ends the run: one path per length, and a cost
  that hardly grows with the budget. `filter` also keeps or drops each element, and both runs go
  on: the paths double with every element the budget reaches. At budget 400 each of its two
  leaves takes 47 s in `#prep_uplc_run`, and the solver a few seconds.
- **Arithmetic in the solver.** `gcd` splits once per round, like `length`, and Lean prepares it
  in 5 s. But each path's condition nests one more remainder, which is nonlinear. Z3 does not
  return, and holds 7 GB after 34 s.

**The premise does not prune.** In the vesting statement the premise says that the withdrawal
would leave too little, and under it the validator fails before it reads the outputs. But each
leaf's program is prepared on its own, for all values of the variables, so Lean also follows the
runs that pass the check. Those search the outputs for the beneficiary's: a list of unknown
length, with a choice at every element. The cliff between budgets 1500 and 2000 is where the runs
reach it. The budget does prune, where the premise does not: one that ends just after the
rejection cuts those runs before they are far into the list (above).

What that statement costs, by how much of the outputs is left open, at budget 12000:

| Outputs | Result | Time |
|---|---|---|
| one output to the beneficiary, of any amount | proved | 43 s |
| one output to an address that is any `Data`, in lovelace | proved | 89 s |
| one output that is any `Data` | Lean's heartbeat limit, in `#prep_uplc_run` | 137 s |
| any `Data` | none | over 7 min |

An output that is any `Data` has a value that is a map of unknown size, which the validator sums.
So a variable can be left open where the program only passes it on or compares it, and needs its
shape written out where the program loops over it.

What could change this:

- **A budget for the runs the statement is about.** Done: `Budget.Auto` finds it from Lean's
  counterexamples ([Finding the budget](blaster-budget-search.md)). It needs a program whose runs of
  interest end before its loop starts.
- **Prepare a conclusion under its premise.** For `p ==>: fails(f(a))`, one program that tests
  `p` first and runs `f(a)` only then. The runs that `p` excludes disappear only if the symbolic
  run asks the solver whether a path is feasible, which Blaster's does not.
- **Bound a list in the statement.** Done for a variable's collections, with a guard that every
  test runs first instead of one shape per length: `forAllDataLimited`
  ([Limits of a variable's collections](#limits-of-a-variables-collections)). The withdrawal
  statements still write their shapes by hand.
- **Induction,** in `lean-direct` (overview §6.3), for statements about every length.
