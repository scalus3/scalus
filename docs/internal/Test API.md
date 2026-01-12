# Testing API Design

Let's design the testing API and then refactor to use it everywhere.

## Goals

I want API to be able to:

- get a code, evaluate it with given PlutusVM and assert result against either expected Term,
  expected value (convertable to term with `LiftValue`). Needed in `CompilerPluginToSIRTest`.
- get `Compiled[?]` value, evaluate it on JVM (`compiled.code()`), assert either success with
  expected value or failure with exception or a message
- evaluate on given `PlutusVM`, `UplcCli`, compare
- evaluate as code, evaluate on given `PlutusVM`, evaluate on `UplcCli`, assert the same result,
  assert the same budget
- compile code using multiple backends (`TargetLoweringBackend`), ensure the same result, assert
  expected budgets on each options. Needed for comparing evaluation costs on different backends and
  ensuring the same result
- ideally, if possible, low level API should not be bound to a particular testing frameworks like
  ScalaTest or MUnit.
- we could have a default implementation for ScalaTest as we support it out of the box.

The idea is to create a low level testing primitives and use them to construct higher level useful
testing API.

Use `Compiled` and `PlutusV3.compile` to get the compiled code.
The `Compiled`/`PlutusV3` has the actual Scala code to be executed on JVM/JS/Native, and
`SIR` to be lowered using different backends (`TargetLoweringBackend`) and `Options`.

The API should be readable, simple, DSL like.

Use infix notation only when it makes sense.

## Use Cases

### Scalus Plugin compilation tests

- compile code to SIR

### Scalus Core

#### Prelude/Stdlib tests

Should be tested on all PlutusVMs
Should be tested on every `TargetLoweringBackend`.
Should check execution budgets on success cases for every VM/backend
Should ensure same result on JVM/JS/Native, our PlutusVM, external `uplc` using `UplcCli`.

### Scalus Testkit

The idea is to test validators in a way that we evaluate testing cases on JVM (using
`Compiled.code`) to make sure it works natively and to enable debugging during testing.
And also run on PlutusVM and ensure that the results are the same.

We need to be able to validate the expected `ExUnits` budget for successful evaluations.

We need to be able to assert the logs on failures.

Ideally, successful cases should be tested within a transaction building (TxBuilder),
where it evaluates the tested script(s)

It should be possible to easily create and test failing cases.

### Scalus Examples

Should use scalus-testkit

## Refactoring Plan

Should be series of commits, each should work, tests should pass. To be able to ensure intermediate
results still work.

## Action

Analyze @docs/internal/assert-functions-refactoring.md, all relevant source code. 
Ultrathink and suggest the APIs and all required changes.