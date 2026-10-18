# Finding the budget

How the `blaster-uplc` tactic finds the number of steps a check lets its programs run
([Budgets](uplc-blaster.md#budgets)), and how a budget that was found is kept for the next run.

Status: built. The search is `Budget.Auto`, and the budget it finds is kept with the statement's
result, among the results the verifier keeps for every tactic
([overview §5.6](../verification-overview.md#56-kept-results)).

## Why

The budget is part of what Lean checks: each test's program is run for at most `N` steps of
Lean's machine. A proof at any budget holds without the budget, so the budget cannot make a
proof wrong. It can only keep one from being found.

- **Too small,** and the program of a test has not ended on some input. Lean falsifies the
  statement there, and the replay on the Scalus CEK finds it true: a spurious counterexample.
- **Too large,** and Lean follows the programs further than the statement needs, through a loop
  for instance, where every round can split the run. The check does not finish.

The budget that works is the one of the runs the statement is about. For "an early withdrawal
is rejected, whatever the outputs are" that is the rejection, about 1610 steps, and not a whole
accepted withdrawal, 12000. The window can be narrow: that statement is proved from 1610 to
about 2000, and its time doubles with every 100 steps.

So a budget written into a test is a number that follows the validator's code and the compiler.
When either changes, the proof turns into a spurious counterexample, or stops finishing.

## The budget of a tactic

```scala
enum Budget {
    /** The tactic finds the budget. */
    case Auto
    /** This many steps of Lean's machine. */
    case LeanSteps(steps: Int)
}

UplcBlaster(Budget.Auto, servers)
UplcBlaster(Budget.LeanSteps(1700), servers)
```

The steps are those of Lean's CEK machine, which counts a term it evaluates and a value it
returns alike: about 1.85 times what Plutus counts. The result of a proof names the budget it
was found at, `Artifact.budget`, whichever way the budget came.

## The search

1. Check the statement at a small budget, 100 steps.
2. A proof, or a counterexample that replays as false on the Scalus CEK, is the result.
3. A counterexample that is spurious says what is missing. Lean's machine counts what each
   test's program does on its values. The next budget is the least at which the statement, read
   as Lean reads it under a budget, holds on those values. Go to 2.

### Counting on Lean's machine

The Scalus CEK counts other steps than Lean's machine, so the count cannot come from the replay.
`ScalusProofs.Run.measureRun` runs a program on arguments and says how many steps it made and
how it ended: `true`, `false`, `returned`, `failed`, or `running` where it had done neither
after the most steps that are counted. The tactic asks in a small check of its own, with the
counterexample's values written as Lean terms.

- **The values go back as terms, not as an encoded program.** Lean's model can hold a value that
  the Scalus CEK takes in memory and that an encoded program does not carry to Lean, such as a
  `Data` constructor with a tag of more than 64 bits. Written as a term it is the value Lean
  falsified the statement with.
- **The programs are the ones of the check.** A search writes its programs once, into a
  directory of its own, and every check and count of the search imports them from there. The
  lines that import them are then the same text in every document, and the server elaborates
  them once.

### The next budget is the statement's

The reading under a budget is that of the check itself (`renderFormula`): a program that has
not ended counts against what the statement claims of it, and for what the statement assumes.
`holdsWithin` evaluates the same reading on the counted runs. Its truth changes only where a
program ends, so the candidates for the next budget are the programs' step counts, and the
least of them at which the statement holds is taken.

That is not the most steps a program makes on the counterexample. The first counterexamples are
values the premise is false of: under the small budget the premise's program was cut, and
counted as holding. The statement holds on them as soon as the premise is seen to be false,
after a few hundred steps. The program of the conclusion on those values is an accepted run of
thousands of steps, and taking its count is the oversized budget again.

### Where it ends

- **A proof or a refutation.** A refutation is one at whatever budget showed it: the replay is
  without the budget.
- **No values to count on.** A statement that asks for a witness is falsified without values
  that could be replayed, and a counterexample can be outside its variable's type. The budget
  is then doubled, a few times: a false statement of that kind would else cost one check
  after the other for the same answer.
- **The most that is counted,** 100000 steps. A counterexample that needs more gives no budget.
- **Time.** The tactic's time limit is that of the whole search. `withAttemptTimeout` gives one
  check of it a limit of its own. The two guard against different things: one check that
  follows a program through a loop would take all the time that is left, and a search that
  goes from one quick check to the next without an end is ended only by the limit of the
  whole. A statement about every length of a list is the second kind: each check gives a
  longer list.

A result that is no proof says which budgets were tried, and gives the last counterexample.

### What was measured

On "what stays locked is at least what has not vested yet, whatever the outputs are":

| Budget | 100 | 262 | 649 | 694 | 1315 | 1573 | 1584 | 1608 |
|---|---|---|---|---|---|---|---|---|
| Result | spurious | spurious | spurious | spurious | spurious | spurious | spurious | proved |

Eight checks of 3 to 10 seconds, 45 seconds together, against 12 seconds for one check at a
budget that is given. The steps between are the places where the validator can stop earlier:
each counterexample gets one check further. The last budget is the least at which the statement
is proved; at 1600 it is not.

## Keeping what was found

A search costs several checks for a number that is the same on the next run, as long as nothing
the statement is about has changed.

The budget is kept with the statement's result, among the results the verifier keeps for every
tactic ([overview §5.6](../verification-overview.md#56-kept-results)). An entry there is under
the statement's name, and counts only where its fingerprint is that of the statement now: the
programs, the statement as Lean is given it, and the Lean side. The budget is the tactic's own
note in that entry.

### What the budget is kept for

The fingerprint leaves the budget out. A proof at any budget holds without it, so a kept proof
stands whatever budget it was found at, and a run that takes the kept result does not ask Lean
at all. The kept budget is for the runs that do ask Lean: the one that recalculates every
result, and any run of a statement whose result was not kept.

### A run that asks Lean

1. No entry under the name, or one with another fingerprint: search from the start, and keep the
   result with the budget it was found at. A change of a program means a search, once.
2. An entry with the statement's fingerprint: check at its budget.
   - A proof, or a refutation: done, in one check.
   - Anything else, a spurious counterexample or a check that is given up: the kept budget does
     not serve any more. Search from the start, and keep what is found. A change of the solver
     can come to this, where it leaves the fingerprint as it was.
3. A search that finds no budget leaves the entry as it is. The result is inconclusive, and for
   the run that recalculates, a proof that no longer comes out.

## Tests

- **Without Lean:** the reading under a budget and the next budget, on runs written by hand:
  values the premise is false of, values it is true of, a program that has not ended. The
  settings of the two time limits.
- **With Lean:** a closed statement and one with variables, each found and found to be the
  least, by one step fewer being spurious; a false statement refuted; an attempt given up at
  its own limit.
- **The vesting statement with open outputs** is proved with `Budget.Auto`.
- **Tagged `Unfinished`:** a statement about a whole list, for which no budget is found, and
  whose reason lists the budgets tried.

## Open questions

1. **Start from the old budget.** Where the fingerprint changed, the old budget is likely near
   the new one. Starting the search there would save checks after a small change, and cost one
   that is given up where the programs became shorter. Not proposed: a change is rare, and a
   search from the start is always right.
2. **A step over the least.** The search ends at the least budget, and so makes a check for
   every place the validator can stop. A budget a few percent above each counterexample's
   would skip some of them, and could step over a narrow window.

## Limits

- The search needs a counterexample to follow. For a statement that asks for a witness it only
  doubles the budget.
- A statement whose runs go through a loop for its whole length has no budget, and the search
  ends at a time limit.
- The counts are of Lean's machine as the workspace has it. A change of that machine changes
  them, and a kept budget then costs a search.
