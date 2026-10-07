# Finding the budget

How the `blaster-uplc` tactic finds the number of steps a check lets its programs run
([Budgets](uplc-blaster.md#budgets)), and how a budget that was found is kept for the next run.

Status: the search is built, as `Budget.Auto`. "Keeping what was found" is proposed.

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

Proposed. A search costs several checks for a number that is the same on the next run, as long
as nothing the statement is about has changed.

### What is kept

For a statement, the budget its search came to, with a hash of the statement.

It is not a proof that is kept. Lean checks the statement on every run, at the kept budget, and
what is trusted is that run. A kept budget that is wrong costs a search, and no more.

### The statement's hash

The hash is over what the check is made of, without the budget: the program of each test
(`Artifact.programHashes` are those), the formula over them, and the types of the variables.

Anything that changes a program changes the hash: the validator, a function it calls, the
compiler, its options. The budget follows exactly these, so a kept budget is used only for the
programs it was found for. A change means a search, once.

Not in the hash: Lean's toolchain, Blaster and the solver. The budget is a property of the
programs' runs, not of who checks them. Where one of those changes what a check at the kept
budget gives, the search runs again, as below.

### An entry

```
<name>  <hash>  <budget>  <result>
```

- **The name** says whose entry it is: the suite, the test, and the place of the statement among
  those of the test. A test framework has all three. ScalaTest gives a running test its name,
  and Scala gives the place of a call in its source at compile time
  (`org.scalactic.source.Position`, which every `test` and assertion already takes).
- **The name is what an entry is replaced by.** A statement whose hash changed gets a new entry
  under its name, in place of the old one. So the file does not grow with every change of the
  compiler, and needs no time after which an entry is dropped.
- **The hash is what an entry is used by.** A kept budget is taken only where the hash is that
  of the statement now.
- **The result** is `proved` or `refuted`, for the reader of the file.

### A run with what is kept

1. No entry under the name, or one with another hash: search, and write the entry.
2. An entry with the statement's hash: check at its budget.
   - A proof, or a refutation: done, in one check.
   - Anything else, a spurious counterexample or a check that is given up: the kept budget does
     not serve any more. Search from the start, and write what is found. This is what a change
     of Lean, of Blaster or of the solver comes to.
3. A search that finds no budget leaves the entry as it is, and the result is inconclusive.

### Where it is kept

A file of the suite, one line for an entry, in the order of the names, so that it reads and
compares as text. Tests of one suite run one after the other, and no two suites share a file.
The file is written whole, to a file beside it that then takes its place.

- **In the tactic** it is a parameter, like the provider of the server:
  `tactic.remembering(budgets, name)`, where `budgets` is a `BudgetCache` and one that keeps a
  file is `BudgetCache.in(file)`. Without it `Budget.Auto` searches every time.
- **In the tests** `LeanProofs` gives it: a file beside the suite's source, or in the suite's
  Lean workspace, and the name from the test that runs.

Whether the file is committed is open ("Open questions").

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

1. **Is the file committed?**
   - *Committed* (recommended): the proofs in CI and on another machine take one check each. A
     change of a budget is seen in review, as the budgets of the scripts are. The price is a
     change of the file with every change of a program, the compiler's among them.
   - *Not committed,* under the build's directory: nothing to review and nothing to merge, and
     only a second run on the same machine is faster.
2. **Entries nobody uses.** A test that is renamed or removed leaves its entry. They do no
   harm. Dropping them needs to know that a whole suite ran, which a run of one test does not.
   A time after which an entry is dropped would write a date on every run, and change a
   committed file each time.
3. **Start from the old budget.** Where the hash changed, the old budget is likely near the new
   one. Starting the search there would save checks after a small change, and cost one that is
   given up where the programs became shorter. Not proposed: a change is rare, and a search
   from the start is always right.
4. **A step over the least.** The search ends at the least budget, and so makes a check for
   every place the validator can stop. A budget a few percent above each counterexample's
   would skip some of them, and could step over a narrow window.
5. **A kept proof.** The same hash with the budget, the toolchain and the libraries would name
   a check whose result is known, and Lean need not run. That is a different trust: the proof
   of this run would be one of an earlier run.

## Limits

- The search needs a counterexample to follow. For a statement that asks for a witness it only
  doubles the budget.
- A statement whose runs go through a loop for its whole length has no budget, and the search
  ends at a time limit.
- The counts are of Lean's machine as the workspace has it. A change of that machine changes
  them, and a kept budget then costs a search.
