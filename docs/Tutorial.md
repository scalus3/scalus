---
sidebar_position: 2
---

# Tutorial

## Basic workflow

The basic workflow is to write a Scala program and then compile it to a Plutus script,
similar to how PlutuxTx works.

You can store the Plutus script in a `*.plutus` file and use it with the Cardano CLI.
Or use one of the Java/JavaScript libraries to construct transactions with the script.

[This example](https://github.com/nau/scalus/blob/master/examples/src/main/scala/scalus/examples/SendTx.scala) shows how
to use the [Cardano Client Lib](https://github.com/bloxbean/cardano-client-lib) to send transactions.

## How it works

You write a script using a subset of [Scala 3](https://scala-lang.org) language,
which is then compiled to a Scalus Intermediate Representation (SIR) with `compile` function.

The SIR can be pretty-printed and reviewed.

The SIR is then compiled
to [Untyped Plutus Core](https://plutus.cardano.intersectmbo.org/docs/essential-concepts/plutus-core-and-plutus-tx#untyped-plutus-core)
(UPLC) that can be executed on the Cardano blockchain.

## Simple validator example

```scala mdoc
import scalus.Compiler.compile
import scalus.*
import scalus.builtin.Data
import scalus.uplc.Program

// Compile Scala code to Scalus Intermediate Representation (SIR)
val validator = compile {
    // A simple validator that always succeeds
    (context: Data) => ()
}
// pretty print the SIR
validator.show
// convert the SIR to UPLC and pretty print it with colorized syntax highlighting
validator.toUplc().showHighlighted
// get a double CBOR encoded optimized UPLC program as HEX formatted string
validator.toUplcOptimized().plutusV3.doubleCborHex
```

## What Scala features are supported?

UPLC is a form of lambda calculus, so not all Scala features are supported.

Supported:

* simple `val`s and `def`s of supported built-in types or case classes/enums
* lambda expressions
* recursive functions
* passing/returning functions as arguments (higher-order functions)
* `if-then-else` expressions
* simple `match` expressions on case classes and enums
    * only simple bindings are supported like `case Costr(field, other) => ...`
* `given` arguments and `using` clauses
* `throw` expressions but no `try-catch` expressions
* built-in functions and operators
* simple data types: case classes and enums
* `inline` vals, functions and macros in general
* implicit conversions
* opaque types (non top-level) and type aliases
* extension methods
* tuples
* value destructuring in vals: `val Some((a, b)) = optionOfTuple` 

## Scala features that are not supported

* `var`s and `lazy val`s
* `while` loops
* classes, inheritance and polymorphism aka virtual dispatch
    * you can't use `isInstanceOf`
* pattern matching with guards
* pattern matching on multiple constructors (`case A | B => ...`)
* pattern matching using type ascriptions (`case x: BigInt => ...`)
* `try-catch` expressions
* overloaded functions
* forward references between local `def`s inside `compile` blocks - put mutually
  recursive functions in a top-level `@Compile` object instead

## Constants and primitives

Plutus V3 supports multiple builtin primitive types.
Here's a table showing correspondence between Plutus, Scalus, and Aiken types:

| Plutus V3              | Scalus                 | Aiken              |
|------------------------|------------------------|--------------------|
| `unit`                 | `Unit`                 | `()`               |
| `bool`                 | `Boolean`              | `Bool`             |
| `integer`              | `BigInt`               | `Int`              |
| `bytestring`           | `ByteString`           | `ByteArray`        |
| `string`               | `String`               | `String`           |
| `data`                 | `Data`                 | `Data`             |
| `list`                 | `BuiltinList`          | `List`             |
| `pair`                 | `BuiltinPair`          | `Pairs`            |
| `BLS12_381_G1_Element` | `BLS12_381_G1_Element` | `G1Element`        |
| `BLS12_381_G2_Element` | `BLS12_381_G2_Element` | `G12Element`       |
| `BLS12_381_MlResult`   | `BLS12_381_MlResult`   | `MillerLoopResult` |

We use Scala native types to represent `Unit`, `Boolean`, `BigInt`, and `String`.

Here is an example of how to define constants and use built-in types.

```scala mdoc:compile-only
import scalus.Compiler.compile
import scalus.*
import scalus.builtin.*
import scalus.builtin.ByteString.*
import scalus.prelude.*

val constants = compile {
    val unit = () // unit type
    val bool = true || false // boolean type
    val int = BigInt(123) // integer type
    val bigint = BigInt("12345678901234567890") // large integer value
    val implicitBigIng: BigInt = 123
    val emptyByteString = ByteString.empty
    val byteString = ByteString.fromHex("deadbeef")
    val byteStringUtf8 = ByteString.fromString("hello") // from utf8 encoded string
    val byteString2 = hex"deadbeef" // ByteString from hex string
    val string = "Scalus Rocks!" // string type
    val emptyList = List.empty[BigInt] // empty list
    val list = List[BigInt](1, 2, 3) // list of integers
    val pair = BuiltinPair(true, ()) // pair of boolean and unit
}
```

## Builtin Functions

Cardano Plutus provides a set of built-in functions that can be used in the scripts.
Scalus implements them in the [Builtins](https://scalus.org/api/scalus/builtin/Builtins$.html) object.

Here are some examples of using built-in functions.

```scala mdoc:compile-only
import scalus.builtin.*
import scalus.builtin.ByteString.*
import scalus.prelude.{*, given}
compile {
    // See scalus.builtin.Builtins for what is available
    val data = Builtins.iData(123)
    val eqData = data == Builtins.iData(123) || data != Builtins.iData(123)
    val eq = Builtins.equalsByteString(hex"deadbeef", ByteString.empty)
    val byteStringEq = hex"deadbeef" == ByteString.empty || hex"deadbeef" != ByteString.empty
    val stringEq = "deadbeef" == "" || "deadbeef" != ""
    val a = BigInt(1)
    val sum = a + 1 - a * 3 / 4 // arithmetic operators
    val intEquality = a == sum || a != sum
    val bool = !true || (false == true) != false && true // boolean operators
    val equals = a === sum // comparison operators
}
```

## Data types

You can define your own data types using Scala case classes and enums.

```scala mdoc:compile-only
import scalus.builtin.ByteString
import scalus.prelude.{*, given}

case class Account(hash: ByteString, balance: BigInt)

enum State:
    case Empty
    case Active(account: Account)

import State.*
compile {
    // Tuple2 literals are supported
    val tuple = (true, BigInt(123))
    val empty = State.Empty // simple constructor
    // Use `new` to create an instance
    val account = new Account(ByteString.empty, tuple._2) // access tuple fields
    // or use a companion object apply method
    val active: State = State.Active(account)
    val hash = account.hash // access case class fields
    // A simple pattern matching is supported
    // no guards, no type ascriptions.
    // Inner matches can be done only on single constructor case classes
    // Wildcard patterns are supported
    active match
        case Empty                                 => true
        case Active(account @ Account(_, balance)) => balance == BigInt(123)
    // all cases must be covered or there must be a default case
    val isEmpty = active match
        case Empty => true
        case _     => false
}
```

## Control flow

```scala mdoc:compile-only
import scalus.prelude.{*, given}
compile {
    val a = BigInt(1)
    // if-then-else
    if a == BigInt(2) then ()
    // throwing an exception compiles to Plutus ERROR,
    // which aborts the evaluation of the script
    // the exception message can be translated to a trace message
    // using sir.toUplc(generateErrorTraces = true)
    else throw new Exception("not 2")
}
```

## Functions

```scala mdoc:compile-only
compile {
    val nonRecursiveLambda = (a: BigInt) => a + 1

    def recursive(a: BigInt): BigInt =
        if a == BigInt(0) then 0
        else recursive(a - 1)
}
```

## Modules and reusable code

You can define reusable code in a Scala object annotated with `@Compile`.
Scalus will compile the code to *.sir files and include them in the jar file.
This way you can distribute your code as a library.

Use `@Ignore` to exclude a definition from the compilation.

The `compile` will link the modules together and compile them to a single script.

```scala

@Compile
object ReusableCode {
    val constant = BigInt(1)
    def usefulFunction(a: BigInt): BigInt = a + 1
    @Ignore // this function is not compiled to UPLC
    def shouldNotBeInUplc() = ???
}

val modules = compile {
    ReusableCode.usefulFunction(ReusableCode.constant)
}
```

## FromData

FromData type class is used to convert a Data value to a Scalus value.

```scala mdoc:compile-only
import scalus.builtin.*, Builtins.*, Data.*

case class Account(hash: ByteString, balance: BigInt)

enum State:
    case Empty
    case Active(account: Account)


val fromDataExample = compile {
    // The `fromData` function is used to convert a `Data` value to a Scalus value.
    val data = iData(123)
    // fromData is a summoner method for the `FromData` type class
    // there are instances for all built-in types
    val a = fromData[BigInt](data)
    // also you can use extension method `to` on Data
    val b = data.to[BigInt]

    // you can define your own `FromData` instances
    {
        given FromData[Account] = (d: Data) => {
            val args = unConstrData(d).snd
            Account(args.head.to[ByteString], args.tail.head.to[BigInt])
        }
        val account = data.to[Account]
    }

    // or your can you a macro to derive the FromData instance
    {
        given FromData[Account] = FromData.derived
        given FromData[State] = FromData.derived
    }
}
```

You can derive `FromData` instances for your case classes and enums via standard Scala 3 `derives` syntax.
One caveat - you must always supply companion object with @Compile annotation on it even if it is empty.
This is because Scalus needs to compile the companion object to include the `FromData` instance in the UPLC script.

```scala
import scalus.builtin.*, Builtins.*, Data.*

case class Account(hash: ByteString, balance: BigInt) derives FromData, ToData

@Compile
object Account

enum State derives FromData, ToData:
    case Empty
    case Active(account: Account2)

@Compile
object State

```

## Writing a validator

Here is a simple example of a Plutus V3 validator written in Scalus.

```scala mdoc:compile-only
import scalus.ledger.api.v1.PubKeyHash
import scalus.ledger.api.v3.*
import scalus.builtin.ByteString.*
import scalus.prelude.List

// Use Scala 3 indentation syntax. Look ma, no braces! Like Python!
val pubKeyValidator = compile:
    def validator(ctxData: Data) = {
        val ctx = ctxData.to[ScriptContext]
        ctx.txInfo.signatories.find {_.hash == hex"deadbeef"}.getOrFail("not signed")
    }
    validator
```

## Troubleshooting

Firstly, you can use a debugger and debug the Scala code.

You can use `log` and `trace` functions to log messages to the execution log.

And there is a `?` operator that can be used to log the value of a boolean expression when it is false.

```scala mdoc
import scalus.builtin.Builtins.trace
import scalus.prelude.*
import scalus.prelude.log
import scalus.uplc.eval.PlutusVM
given PlutusVM = PlutusVM.makePlutusV3VM()
val sir = compile {
    val a = trace("a")(BigInt(1))
    val b = BigInt(2)
    log("Checking if a == b")
    val areEqual = a == b
    areEqual.? // logs "areEqual ? False"
}.toUplc().evaluateDebug.toString
```

## Converting the Scalus code to Flat/CBOR encoded UPLC

The `compile` function converts the Scalus code to a `SIR` value, Scalus Intermediate Representation.
You then need to convert the `SIR` value to a UPLC value and encode it to Flat and then to CBOR.

Many APIs require the HEX encoded string of double CBOR encoded Flat encoded UPLC program,
like `Hex(CborEncode(CborEncode(FlatEncode(Program(version, uplc)))))`.

```scala mdoc:compile-only
import scalus.*
import scalus.builtin.ByteString.*
import scalus.cardano.ledger.Language
import scalus.ledger.api.v1.PubKeyHash
import scalus.ledger.api.v3.*
import scalus.prelude.List
import scalus.uplc.Program

val serializeToDoubleCborHex = {
    val pubKeyValidator = compile {
        def validator(datum: Data, redeamder: Data, ctxData: Data) = {
            val ctx = ctxData.to[ScriptContext]
            ctx.txInfo.signatories.find {_.hash == hex"deadbeef"}
        }
    }
    // convert to UPLC
    // generateErrorTraces = true will add trace messages to the UPLC program
    val uplc = pubKeyValidator.toUplc(generateErrorTraces = true)
    val program = uplc.plutusV2.deBruijnedProgram
    val flatEncoded = program.flatEncoded // if needed
    val cbor = program.cborEncoded // if needed
    val doubleEncoded = program.doubleCborEncoded // if needed
    // in most cases you want to use the hex representation of the double CBOR encoded program
    program.doubleCborHex
    // also you can produce a pubKeyValidator.plutus file for use with cardano-cli
    import scalus.utils.Utils
    Utils.writePlutusFile("pubKeyValidator.plutus", program, Language.PlutusV2)
    // or simply
    program.writePlutusFile("pubKeyValidator.plutus", Language.PlutusV2)
}

```

## Evaluating scripts

Scalus provides a high-level API to evaluate UPLC scripts.

```scala mdoc
compile(BigInt(2) + 2).toUplc().evaluateDebug.toString
```

You get a `Result` object that contains the result of the evaluation, the execution budget, the execution costs, and the logs.

You can also use the low-level API to evaluate scripts.

```scala mdoc:compile-only
import scalus.builtin.*
import scalus.cardano.ledger.Language
import scalus.ledger.api.*
import scalus.uplc.*, eval.*

def evaluation() = {
    import scalus.*
    import scalus.uplc.eval.PlutusVM
    val sir = compile {
        def usefulFunction(a: BigInt): BigInt = a + 1
        usefulFunction(1)
    }
    val term = sir.toUplc()
    // setup a given PlutusVM for the PlutusV2 language and default parameters
    given v2VM: PlutusVM = PlutusVM.makePlutusV2VM()
    // simply evaluate the term with CEK machine
    term.evaluate.show // (con integer 2)

    // you can get the actual execution costs from protocol parameters JSON from cardano-cli
    lazy val machineParams = MachineParams.fromCardanoCliProtocolParamsJson(
      "JSON with protocol parameters",
      Language.PlutusV3
    )
    // or from blockfrost API
    lazy val machineParams2 = MachineParams.fromBlockfrostProtocolParamsJson(
      "JSON with protocol parameters",
      Language.PlutusV3
    )
    // use latest PlutusV3 VM with explicit machine parameters
    val v3vm: PlutusVM = PlutusVM.makePlutusV3VM(machineParams)
    // evaluate a Plutus V3 script considering CIP-117
    // calculate the execution budget, all builtins costs, and collect logs
    val script = term.plutusV3.deBruijnedProgram
    script.evaluateDebug(using v3vm) match
        case r @ Result.Success(evaled, budget, costs, logs) =>
            println(r)
        case Result.Failure(exception, budget, costs, logs) =>
            println(s"Exception: $exception, logs: $logs")

    // evaluate a flat encoded script and calculate the execution budget and logs

    // TallyingBudgetSpender is a budget spender that counts the costs of each operation
    val tallyingBudgetSpender = TallyingBudgetSpender(CountingBudgetSpender())
    val logger = Log()
    // use NoLogger to disable logging
    val noopLogger = NoLogger
    try {
        v3vm.evaluateScript(script, tallyingBudgetSpender, logger)
    } catch {
        case e: StackTraceMachineError =>
            println(s"Error: ${e.getMessage}")
            println(s"Stacktrace: ${e.getCekStack}")
            println(s"Env: ${e.env}")
    }
    println(s"Execution budget: ${tallyingBudgetSpender.budgetSpender.getSpentBudget}")
    println(s"Logs: ${logger.getLogs.mkString("\n")}")
    println(
      s"Execution stats:\n${tallyingBudgetSpender.costs.toArray
              .sortBy(_._1.toString())
              .map { case (k, v) =>
                  s"$k: $v"
              }
              .mkString("\n")}"
    )
}
```
