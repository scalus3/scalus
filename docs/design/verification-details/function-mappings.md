# Function mappings: a Scala function equals a Lean function

How Scalus shows that a Scala function `f` computes what a Lean function `g` computes, so that what
Lean proves of `g` holds of `f`. The question has two instances: `f` is compiled to UPLC, or `f`
runs on the JVM only. The first is §6.3 of [the overview](../verification-overview.md#63-lean-direct-mapping-to-functions-and-statements-defined-in-lean)
in more detail. The second is new: no tactic of Scalus reaches code that is not compiled.

Status: proposed, 2026-11-20. Nothing of it is built but what [What exists](#what-exists) lists.

"Mapping" here is a function mapping. It is not the mapping of a statement's variable to fields
or to `Data` of [the `blaster-uplc` tactic](uplc-blaster.md#mapping-a-variable). Which functions a
protocol's step is made of, so that there is something to map, is
[a protocol step in Scala and in Lean](protocol-steps.md).

## Why

A mapping is the claim that for every argument in a domain, `f` returns, and returns what `g`
returns. Three uses need it:

- **A library's theorems for a Scalus function.** `Math.gcd` mapped to `Int.gcd` gets what Lean
  knows of `Int.gcd` ([overview §6.3](../verification-overview.md#63-lean-direct-mapping-to-functions-and-statements-defined-in-lean)).
- **Induction over a loop.** Blaster runs a program for a bounded number of steps, so a loop whose
  steps grow with its input is out of its reach. A proof by hand does induction on a Lean function,
  and a mapping carries the result to the program. The AMM's `Math.sqrt`, of a first deposit, is
  the first case ([overview §6.4](../verification-overview.md#64-lean-manual)).
- **A model written in Lean.** A project keeps the rules of its protocol as a second text, a Lean
  model, with one Lean function for each pure function of its validators, of the same name and
  arguments, and proves its theorems there. A mapping of each function of the code to the function
  of the model ties the two.

The third use is where the off-chain instance comes from. A protocol's off-chain code computes what
its on-chain code checks: a builder computes a datum, a root, the result of a search, and the
validator checks them. A theorem such as "every transaction the builder makes passes the validator"
needs both sides in Lean. Today the off-chain side is tied to a model by random tests only.

## The two instances

| | `f` runs as | Lean can run `f` | Example |
|---|---|---|---|
| A. compiled | UPLC, compiled from SIR | yes: PlutusCoreBlaster's model of UPLC | `AmmMath.swapResult` |
| B. off-chain | JVM bytecode | no | a builder's square root, `BigInt(n.bigInteger.sqrt())` |

In instance A the semantics of `f` is the UPLC program and Lean's model of it, which the Plutus
conformance tests check. In instance B there is no Lean model of the JVM. Something must give `f`
a meaning in Lean, and that something is trusted, or checked some other way.

## The claim

Let `f : A₁ → … → Aₙ → R` be a Scala function, and `g : A₁' → … → Aₙ' → R'` a Lean function,
where `Aᵢ'` is the Lean type of `Aᵢ` ([Lean types for Scala types](lean-types.md): `BigInt` is
`Integer`, which is `Int`; a case class is a `structure`). A mapping is one of these:

| Form | Claim | Use |
|---|---|---|
| total on a domain | `∀ x, D x → f x returns g x` | the default: `D` is the success condition of `f`, `True` where it has none |
| partial | `∀ x, f x returns r → r = g x` | a function that fails on inputs nobody names, the form `succeeded(run) ==> property` |
| with failure | the total form, and `∀ x, ¬ D x → f x fails` | where the failures matter, as a validator's |

`g` is total, as every Lean function is. Where `f` fails, `g` returns some value, and only `D`
tells the two apart: a zero divisor fails on chain and on the JVM, and `Int.tdiv a 0 = 0` in Lean.

**The arithmetic must be that of `f`.** `/` on `BigInt` truncates toward zero, on the JVM
(`BigInteger.divide`) and on chain (`quotientInteger`), so `g` uses `Int.tdiv`, not `/` on `Int`,
which is Euclidean. `divFloor` is `Int.fdiv`. A mapping claims equality on negative arguments too,
and that is where a wrong choice shows. A refutation shows it at once, with values replayed on the
Scalus CEK: of `afterFee(amount) >= 0`, with `afterFee(amount) = amount * 97 / 100`, the
counterexample is `amount = −2`, not `−1`: `−97 / 100` truncates to 0, where the floor is −1.

**A result is compared through its encoding.** In instance A the program returns a constant or a
`Data`, and the claim is that it returns the encoding of `g x`: the constant of an `Integer`, a
`Bool` or a `ByteString`, or `toData (g x)` of a generated type. In instance B the comparison is
of Lean values.

**`g` takes the generated types.** A hand-written `structure` needs a proof that it matches the
generated one. So `g` takes primitives and the types that [`LeanTypes`](lean-types.md) writes,
and a model that wants its own records converts at its edge.

## Instance A: compiled code

Three ways, by what `f` does:

| `f` | Way | The proof rests on | Proof kind |
|---|---|---|---|
| no loop; Z3 decides the arithmetic | A1. `blaster-uplc` | Lean's model of UPLC, Blaster, Z3 | `Blaster` |
| no loop; Z3 does not finish | A2. `lean-manual`, a proof by hand with Mathlib | Lean's kernel, over Blaster's optimized run | `LeanKernelOfOptimizedRuns` |
| a loop: recursion whose steps grow with the input | A3. a generated translation `f_sir`, and `f_sir = g` in Lean | the translation | new |

### A1. Blaster

A function with no loop runs a number of steps that does not depend on its input, so one budget
covers every input, as `Budget.Auto` finds. The check is that of any statement, with a claim that
names `g`. In outline, not the text the tactic writes:

```lean
-- leanMapping(AmmMath.redeemDatum, "Amm.Spec.redeemDatum", expects = (d, lp) => d.lpSupply > 0)
∀ (d : AmmDatum) (lp : Integer), d.lpSupply > 0 →
  runFor (redeemDatum_prog [AmmDatum.toData d, lp]) budget
    = some (AmmDatum.toData (Amm.Spec.redeemDatum d lp))
```

What Scalus lacks for it:

- **A statement whose claim is a Lean term.** A statement's tests are Scala Booleans, compiled
  to UPLC, and `g` is not Scala. The form is the overview's `leanMapping(f, "g")`, with the domain
  as a Scala premise, `leanMapping(f, "g", expects = …)`. The tactic lowers it as above, with the
  premise as the guard of a statement.
- **The check imports `g`.** `g`'s package is required by the workspace, and the check imports
  `g`'s module, which the request names.
- **The kept result rests on `g`.** `LeanWorkspace.environment` hashes a package required by a
  path by its `<name>.lean` and the modules under `<name>/`. A module of `g` elsewhere changes no
  result, so a changed `g` would pass on a stale proof. Either the mapping's entry hashes `g`'s
  module and its imports, or a mapping is refused where `g` is outside what the environment hashes.

A protocol's math functions are mostly of this kind. `AmmMath.swapResult` and `redeemDatum` are
straight-line arithmetic, whose contracts `blaster-uplc` proves today in 614 and 556 steps.
`depositDatum` is too, but for the square root of a first deposit.

### A2. A proof by hand

Where Z3 does not finish, as with a check that raises an unknown to a high power (a root check
`r^k ≤ x < (r+1)^k`), the mapping is a theorem written by hand of the statement's module
([checking a proof by hand](lean-export.md#checking-a-proof-by-hand-lean-manual)). The written
statement is about Blaster's optimized run, a term of integer arithmetic over the variables, so
`simp` with `g`'s definition and `ring_nf` or `nlinarith` may close it. The proof rests on
Blaster's optimizer, until `prepared = run` is proved
([overview §6.4](../verification-overview.md#64-lean-manual)).

### A3. A loop

Blaster cannot run a loop for every input. Two steps instead:

1. **Translate** `f`'s SIR into a Lean function `f_sir`, as the overview's generated `lean-direct`
   does: `BigInt` to `Int`, a case class to its `structure`, a `LetRec` to a recursive `def`
   ([Termination](#termination)).
2. **Prove** `f_sir = g` in Lean, by induction. Lean's kernel checks it.

The result rests on the translation, which nothing checks. Two ways to check it, both to try:

- **Bounded agreement.** A statement `f = f_sir` for the inputs whose runs end within a budget
  (`Budget.Within`), proved by Blaster: a `ProvenWithin`, not a proof of the loop.
- **A loop lemma.** The body of one iteration has no loop, so "the program's one iteration is
  `f_sir`'s one iteration" is an A1 statement. One lemma about how a recursion lowers to UPLC
  would give the whole run from that, once for every function. Research.

## Instance B: off-chain code

Nothing in Lean runs JVM bytecode, so instance B starts by giving `f` a Lean meaning. Three ways:

| Way | What gives `f` a meaning in Lean | Trusted | Fits |
|---|---|---|---|
| B1. compile it | the UPLC of the same source | that the JVM and UPLC agree on the `@Compile` subset | code written in the `@Compile` subset |
| B2. translate it | a translation of a subset of Scala to Lean | the translator, and a table of JVM primitives | pure code in a larger subset |
| B3. test it | none | none: evidence only | anything |

### B1. The same source, compiled for the proof

Write `f` in the `@Compile` subset, and instance A applies to its UPLC. The JVM runs the same
source. That the JVM and UPLC agree on the subset is what every Scalus test that runs `@Compile`
code on the JVM assumes already. A test of the two runs on the same random inputs (the Scalus CEK
beside the JVM) checks it for `f`.

The AMM does this: `AmmOffchain.deposit` builds its datum with `AmmMath.depositDatum`, the function
the validator calls, so the builder's datum is the compiled function's. A function used only
off-chain can be compiled for the proof only, and never deployed.

The subset has no `while`, no assignment and no `return` (`SIRCompiler` refuses them), no JVM
library, and no `Double`. A builder that takes its square root with `BigInteger.sqrt`, for speed,
needs a square root written in the subset instead (`Math.sqrt`), a recursion, which then needs A3.
So B1 fits code that is written in the subset anyway, not code written for speed.

### B2. A translation of a subset of Scala to Lean

The plugin translates functions marked for it, an object `@Lean` or a function, into Lean
functions in the workspace's `Generated`, one module for each object. Each module has the hash of
its source in its header, as a statement's module has the hashes of its programs
([statements as Lean modules](lean-export.md)), so a proof is held to the code as it is.

| Scala | Lean |
|---|---|
| `BigInt`: `+`, `-`, `*`, `/`, `%`, comparisons, `abs` | `Int`: `+`, `-`, `*`, `Int.tdiv`, `Int.tmod`, comparisons, `natAbs` |
| `BigInt.pow(n)` | `^` with a `Nat` exponent; a negative `n` fails |
| `Int`, `Long` | a choice: wrapping, as `BitVec 32` / `BitVec 64`, or `Int` with a premise of no overflow (open question 1) |
| `Boolean`, `if`, `val`, a `match` on case classes and enums | `Bool`, `if`, `let`, `match` |
| a case class, an enum | the `structure` and the `inductive` of [`LeanTypes`](lean-types.md) |
| `List`, `Option`, an immutable `Map` | `List`, `Option`, a list of pairs |
| local functions, lambdas, a function parameter (`fits: BigInt => Boolean`) | Lean functions |
| a `@tailrec` recursion, a `while` with local `var`s | a recursive `def` ([Termination](#termination)); a `while` becomes a local tail recursion first |
| `require(c, msg)`, a primitive's failure | `Except String`, or the success condition `D` of the claim |
| a call of a `@Compile` function | its translation, through the SIR the plugin has of it |
| a JVM primitive of the table: `BigInteger.sqrt`, … | its Lean meaning, with its failure condition |
| not in the subset: `Double`, `Float`, `BigDecimal`, mutable collections, IO, `null`, a caught exception, any other JVM call | refused, with the position, as `@Compile` code is |

**The table of primitives** gives each JVM call a Lean meaning and the condition under which it
fails: `BigInteger.sqrt(n)` is the floor square root, and fails for `n < 0`. Each entry is an
assumption about the JVM, and a proof lists those it rests on, as a proof by hand lists its axioms.
A meaning can be a relation rather than a definition: the square root is the `r ≥ 0` with
`r·r ≤ n < (r+1)·(r+1)`, a fact Z3 handles far better than an iterative definition.

**The translation is executable, so testing it costs nothing.** Every generated function is
computable. A runner that Lake builds evaluates it on JSON input. A test then gives the JVM `f`
and the Lean `f` the same random inputs and requires equal results. That checks the translator and
the table, for this function, on every run of the tests, though it proves nothing.

`f = g` is then a theorem of Lean about two Lean functions, which the kernel checks, with
induction. The kind of such a proof is new: Lean's kernel, over the translation of the JVM source,
with the primitives it names.

### B3. Tests

Random differential tests of the JVM `f` against an executable `g`. It is the fallback for code
outside every subset, such as `AmmOffchain.swapQuote`, which returns a `BigDecimal`.

### Not taken: UPLC with host calls

B1 could reach code with JVM calls if UPLC had them: a non-standard builtin for each primitive,
which the Scalus CEK evaluates by calling the JVM. That needs a new tag in the flat format, the
builtin in PlutusCoreBlaster's model and decoder, and a Lean meaning that Blaster translates. B2
gets what it would buy at less cost: replay runs the JVM function itself, and `blaster` is a Lean
tactic, which works on generated Lean functions as well as on runs of UPLC. A loop is also far
cheaper as a recursive `def` than as a symbolic run of the CEK model.

## One translator for both

A3 translates SIR, and B2 translates Scala. With two translators, one construct would be
translated twice, and a call from off-chain code to on-chain code (a builder's search that calls
the validator's cost function) would cross between them. The proposal is one translator, from SIR:

- the plugin compiles a `@Lean` object to SIR too, after two rewritings that the UPLC lowering
  never sees: a `while` with its `var`s into a local tail recursion, and an early `return` into a
  branch;
- a JVM primitive of the table is a new kind of SIR node, a host call, which the lowering to UPLC
  refuses with its position;
- types go through [`LeanTypes`](lean-types.md), as the types of a statement do.

Instance A3 and instance B2 are then the same translation, of SIR with or without host calls.
The alternative, a translator of typed Scala trees beside SIR, is simpler to start with, and gives
two texts of every construct.

## Termination

A recursive Lean `def` needs a termination argument. In order of preference:

1. **A measure from the source.** A clause in the Scala code, `decreasing(hi - lo)`, becomes
   `termination_by (hi - lo).toNat`, with `decreasing_by omega`. A bisection has one:
   `bisect(lo, hi)` stops where `hi − lo <= 1`.
2. **Fuel.** Without a measure, the function takes a `Nat` of fuel and returns `Option`. The claim
   becomes "for some fuel", or "for fuel at least `b x`". The search for the largest amount that
   fits, by doubling and then bisection, has a doubling with no measure in its source:
   `bound(hi) = if fits(hi) then bound(2·hi) else hi` stops because `fits` is false from some point
   on, which is a premise about `fits`, not a fact of the code.
3. **`partial def`.** Executable, but nothing can be proved of it. Only for the runner of
   [B2's tests](#b2-a-translation-of-a-subset-of-scala-to-lean).

## An example: the AMM

| Function | Instance | Way |
|---|---|---|
| `AmmMath.swapResult`, `redeemDatum` | A | A1 |
| `AmmMath.depositDatum` | A | A1 into a pool with liquidity; A3 for a first deposit (`Math.sqrt`) |
| the datum of `AmmOffchain.deposit` | B | B1: it is `AmmMath.depositDatum` |
| `AmmOffchain.swapQuote` | B | B3: a `BigDecimal` |
| a builder with `BigInteger.sqrt`, or a search by bisection | B | B2 |

With A1 for the validator's math and B1 or B2 for the builder, a liveness theorem ("every deposit
that `AmmOffchain.deposit` builds passes the validator's rules") can be about the code, and not only
about a Lean model of it.

## What exists

- Lean types of Scala types, with their encodings ([`LeanTypes`](lean-types.md)).
- Statements as Lean modules, over programs imported from `.flat` files, held to the code by hashes
  ([statements as Lean modules](lean-export.md)).
- Proofs by hand, checked and classified by their axioms (`lean-manual`).
- The refutation that shows a wrong division, replayed on the Scalus CEK.

## Steps

1. **A1.** `leanMapping(f, "g", expects)` in the verifier, lowered by `blaster-uplc`; the kept
   result rests on `g`'s module. First mappings: `AmmMath.redeemDatum` and `swapResult` to the
   functions of a Lean model of the AMM, beside a refuted control (a model with `/` on `Int` in
   place of `Int.tdiv`).
2. **B2 without loops.** The translator for `BigInt` functions with no recursion, the table with
   `BigInteger.sqrt` and `pow`, and the runner and its differential test.
3. **Loops.** The `while` rewriting, measures and fuel. First: a search by doubling and bisection.
4. **A3.** The same translator on on-chain SIR, and a try at bounded agreement on `Math.sqrt`.

## Open questions

1. **`Int` and `Long`.** Wrapping (`BitVec`), which Blaster does not translate, or `Int` with a
   premise that nothing overflows?
2. **The table of primitives.** Where it lives, who reviews an entry, and whether each entry gets a
   differential test of its own.
3. **Where mappings are declared** (overview, open question 9): in the verifier, or as an
   annotation on `f`.
4. **The kind of a B2 proof**, and how a result says which primitives it rests on.
5. **`prepared = run`** (overview §6.4), which decides whether A2 reaches the program or stays
   over the optimized run.
6. **The loop lemma** of A3: one lemma for every recursion, or a check of each translation.
