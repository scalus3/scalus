# The case-constr application encoding: exact cost model

Date: 2026-09-05. Status: measured, closed form, no open questions.

Canonical source for the cost of encoding an N-argument application as
`(case (constr 0 [a1..aN]) [f])` instead of `[[[f a1] .. aN]`. Cited by the T5
design doc, `PGO_SGD_OPTIMIZATION_RESEARCH.md`, and the public article.

**`PGO_SGD_OPTIMIZATION_RESEARCH.md` needs a one-line fix.** Its closed-form table
lists case-constr-apply as *"Yes, parameter-free — saves Apply steps per call, apply
whenever legal"*. That is true only if size is ignored. The row should read:

```
| Case-constr-apply | Yes, but not parameter-free | (N-2) steps vs 19-3N bits; see CASE_CONSTR_COST_MODEL.md |
```

## 1. Steps

The CEK machine charges per node visited. The apply chain visits one `Apply` per
argument; the case-constr form visits exactly two nodes, `Case` and `Constr`,
at any arity. Argument evaluation and lambda entry are identical in both.

```
Δsteps = N - 2        (positive = saved, per execution)
```

## 2. Size

In the flat encoding the apply chain is pure tags, `4N` bits. The case-constr form
pays fixed framing first:

```
Case tag                4 bits
Constr tag              4 bits
constructor index       8 bits
field-list framing    N+1 bits    (one bit per field, plus a terminator)
branch-list framing     2 bits
                    ---------
                    19 + N bits
```

```
Δbits = (19 + N) - 4N = 19 - 3N        (positive = larger)
```

Verified against the encoder for N = 1..12; the formula reproduces every row exactly.
The encoding is **larger below N = 7** and smaller from N = 7 up.

| N | 2 | 3 | 4 | 5 | 6 | 7 | 10 | 12 |
|---|---:|---:|---:|---:|---:|---:|---:|---:|
| Δbits | +13 | +10 | +7 | +4 | +1 | −2 | −11 | −17 |

## 3. Fee

The two quantities are billed on different schedules, which is the whole difficulty:

- a machine step costs **6.92 lovelace** (`100 mem × 0.0577 + 16,000 cpu × 0.0000721`),
  paid **per execution**;
- a script byte costs **15 lovelace** of reference-script fee, paid **per transaction**
  whether the byte runs or not (**44 lovelace** if the script is witness-carried,
  `minFeeA`).

So the break-even, in executions per transaction, is the general form from
`PGO_SGD_OPTIMIZATION_RESEARCH.md` section 2(c),
`count(s) = 15·Δbytes / (6.92·Δsteps)`, instantiated:

```
count* = 1.875·(19 - 3N) / (6.92·(N - 2))
```

| N | count* | net @1 execution |
|---:|---:|---:|
| 2 | never profitable | −24.38 |
| 3 | 2.71 | −11.83 |
| 4 | 0.95 | +0.72 |
| 5 | 0.36 | +13.27 |
| 6 | 0.07 | +25.82 |
| ≥7 | always | +38.37 (N=7) |

## 4. Why the threshold is 5 and not 4

Dead code is eliminated, so every surviving site executes at least once, and the table
above collapses: from N = 4 up, `count*` is below 1. That would make 4 the threshold.

It is not, because the realized byte cost is **quantized**. Flat is bit-packed, so a
single group's theoretical fractional byte materialises as either `floor` or `ceil` of
it, depending on where the surrounding encoding happens to sit. Taking both cases:

| N | Δbytes (theory) | net if it rounds down | net if it rounds up |
|---:|---:|---:|---:|
| 3 | 1.25 | −8.08 | −23.08 |
| 4 | 0.875 | +13.85 | **−1.15** |
| 5 | 0.5 | +20.77 | +5.77 |
| 6 | 0.125 | +27.69 | +12.69 |

**N ≥ 5 is profitable under either rounding. N = 4 changes sign. N = 3 is negative
under either.** That is the real content of the threshold, and it is confirmed by
measurement: over the ten example validators, a threshold of 4 nets +451 lovelace but
makes 2 of them worse (`payment_splitter` paid 1.5 bytes per group and lost 17
lovelace), while 5 nets +458 and makes none worse.

`LetChainRegroup.MinRunSize = 5` is therefore not conservatism — it is the first arity
whose margin exceeds one byte of granularity.

## 5. The per-transaction caveat

"Executed at least once" is a property of a *site in a transaction*, not of a site in a
program. Dead-code elimination removes provably unreachable code; it does not remove the
branch this transaction did not take.

A 3-argument case-constr behind a dispatch arm costs its bytes in **every** transaction
and saves its steps only in transactions that take that arm, so its expected count is
below 1. `linear_vesting` is the shape: 5 of its 6 decode branches are `Error`, and of
the two real paths exactly one runs per transaction.

So the quantity in section 3 is *expected* executions per transaction, and for branchy
code it is fractional.

## 6. Decision rule

| N | rule |
|---:|---|
| ≤ 2 | never — the encoding costs a step and bytes |
| 3 | only for genuinely hot sites (loop or recursion body); needs ~3 executions |
| 4 | common path only; sign depends on byte rounding |
| ≥ 5 | unconditionally, for any live site |
| ≥ 7 | unconditionally — smaller *and* faster, no analysis needed |

### The cheap proxy for `count(s)`

Only N ∈ {3, 4} depends on execution count, so most of the value needs **no profile
infrastructure at all** — in particular none of Step 3 of the PGO research (stable site
ids from SIR through lowering), which is the expensive prerequisite.

A static proxy suffices: the **once-entered region** analysis designed for T18 (see the
T5 design doc, section 7). A site reachable from the root without descending into a
lambda-in-value-position or a `Delay` body is entered at most once per execution, so
`count(s) <= 1` and the N ≥ 5 rule applies. A site inside a lambda a loop enters can run
many times, so `> 2` is right there.

That distinction costs one traversal and no new IR plumbing.

## 7. Consequences for the current pipeline

- **`LetChainRegroup`** (T5) operates on prologue chains — cold by construction — and
  already uses 5. Correct as-is.
- **`CaseConstrApply`** uses `args.size > 2` globally. That is right where it mostly
  fires (recursive self-calls under the T2 encoding are maximally hot) and wrong in
  prologues, where it currently loses ~12 lovelace per site per transaction. The fix is
  the once-entered proxy above, not a new global constant. **Not yet measured**: flipping
  it to 4 across the corpus, which would quantify how much the cold sites cost today.

## 8. Reproducing the numbers

- **Sizes and budgets**: build both encodings for N = 1..12, compare
  `summon[Flat[Term]].bitSize` and `evaluateDebug.budget`. The probe used for the table
  above is ~60 lines.
- **Corpus effect**: compile each example validator twice, under `Options.release` and
  `.copy(letChainRegroup = true)`, diff `program.cborByteString.size`, and sum the
  pass's own log lines for steps.
- **Real ExUnits**: flip the `Options.letChainRegroup` default to `true` and read the
  pinned-budget failures from `scalusExamplesJVM/testOnly scalus.examples.cape.*`.

Fee constants are mainnet at time of writing: `price_mem` 0.0577, `price_steps`
0.0000721, `minFeeRefScriptCostPerByte` 15, `minFeeA` 44.
