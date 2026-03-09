# Scalus - DApps Development Platform for Cardano

![CI/Release](https://github.com/scalus3/scalus/actions/workflows/release.yml/badge.svg)
![Maven Central](https://img.shields.io/maven-central/v/org.scalus/scalus_3)
[![Discord](https://img.shields.io/discord/1105852427346911252.svg?label=&logo=discord&logoColor=ffffff&color=404244&labelColor=6A7EC2)](https://discord.gg/ygwtuBybsy)

## Vision

Scalus is a platform for developing decentralized applications (DApps) on the Cardano blockchain.

The goal is to make a full-stack development experience for Cardano DApps as smooth as possible.
Using the same language, tools and code for frontend, backend and smart contracts development.

## Write Cardano smart contracts in Scala 3

* Scala is a modern functional programming language that runs on JVM, JavaScript and natively via LLVM.
* Reuse your Scala code for your validator, frontend and backend.
* Write once, run anywhere. True full-stack development.
* Scala's powerful type system helps you write correct code.
* Benefit from a huge ecosystem of libraries and tools.
* Utilize testing frameworks like ScalaTest and ScalaCheck
  for [property-based testing](https://en.wikipedia.org/wiki/Property-based_testing).
* Enjoy comprehensive IDE support: IntelliJ IDEA, VSCode and syntax highlighting on GitHub.
* Advanced debugging support.
* Enhanced code formatting and linting, navigation, and refactoring.
* Scala code coverage and profiling tools.

## How It Works

Scalus compiles a subset of Scala code to Plutus Core, the language of Cardano smart contracts.

Scalus gives full control over the generated Plutus Core code.
Write efficient and compact smart contracts and squeeze the most out of the Cardano blockchain.

## Features

* Scala 3 to Cardano Plutus Core compiler
* Standard library for Plutus contracts development
* Plutus V1, V2 and V3 support
* Plutus VM Interpreter and execution budget calculation for Plutus V1, V2 and V3
* Plutus VM library works on JVM, JavaScript and Native platforms!
* UPLC optimizer with compile-time partial evaluation for smaller and cheaper scripts
* Source position tracking and diagnostic replay for easier debugging
* Cross-platform transaction builder (TxBuilder) for JVM and JavaScript
* Emulator for off-chain transaction testing without a running node
* Conway era ledger rules support
* HD wallet with BIP-39/BIP32-Ed25519/CIP-1852
* Property-based testing library
* Untyped Plutus Core (UPLC) data types and functions
* Flat, CBOR, JSON serialization
* UPLC parser and pretty printer
* Type safe UPLC expression builder, think of Plutarch
* Bloxbean [Cardano Client Lib](https://cardano-client.dev) integration

## Scalus Starter Project

You can use the [Scalus Starter Project](https://github.com/nau/scalus-starter) to get started with Scalus.
Clone the repository and follow the instructions in the README.

## Show Me The Code

### Hello Cardano Validator Example

Here is a simple validator that checks if the redeemer is "Hello, World!" and if the transaction is signed by the
owner. Below example is taken
from [HelloCardano](https://github.com/scalus3/scalus/blob/master/scalus-examples/jvm/src/main/scala/scalus/examples/HelloCardano.scala).

```scala 3
@Compile
object HelloCardano extends Validator {
    inline override def spend(
        datum: Option[Data],
        redeemer: Data,
        tx: TxInfo,
        ownRef: TxOutRef
    ): Unit = {
        val owner = datum.getOrFail("Datum not found").to[PubKeyHash]
        val signed = tx.signatories.contains(owner)
        require(signed, "Must be signed")
        val saysHello = redeemer.to[String] == "Hello, World!"
        require(saysHello, "Invalid redeemer")
    }
}

// compile to Untyped Plutus Core (UPLC)
val compiled = PlutusV3.compile(HelloCardano.validate)
// HEX encoded Plutus script, ready to be used with cardano-cli or Blockfrost
val plutusScript = compiled.program.doubleCborHex
```

Look at [SendTx](https://github.com/scalus3/scalus/blob/master/scalus-examples/jvm/src/main/scala/scalus/examples/SendTx.scala)
example for a full example of how to create a transaction with this validator.

### Scalus for budget calculation with Cardano Client Lib

Scalus can calculate the execution budget for your validator using the Cardano Client Lib. Just provide
`ScalusTransactionEvaluator` to your `QuickTxBuilder`:

```scala 3
val signedTx = quickTxBuilder
  .compose(scriptTx)
  .withTxEvaluator(ScalusTransactionEvaluator(protocolParams, utxoSupplier))
  // build your transaction
  .buildAndSign()
```

This will calculate the execution budget for your validator and add it to the redeemer of the transaction.

### AdaStream Example

Sources: [AdaStream Contract](https://github.com/nau/adastream/blob/main/src/contract.scala)

This project is a Cardano implementation of the [BitStream](https://github.com/RobinLinus/BitStream) protocol by Robin
Linus, inventor of [BitVM](https://bitvm.org/)

Original paper: [BitStream: Decentralized File Hosting Incentivised via Bitcoin Payments
](https://robinlinus.com/bitstream.pdf)

#### TL;DR

* Alice wants to buy a file from Bob.
* Bob encrypts the file with a random key and sends it to Alice.
* Bob creates a bond contract on Cardano with a collateral and a commitment to the key and the file.
* Alice pays Bob for the file via a HTLC (Hashed Timelock Contract), using Cardano or Bitcoin Lightning Network.
* Alice decrypts the file with the key from the HTLC or takes the money back after the timeout.
* If Bob cheats, Alice can prove it and get the collateral from the bond contract.
* Bob can withdraw the collateral by revealing the key.

The project includes a bond contract and a HTLC contract for a fair exchange of files for ADA or other Cardano Native
Tokens.

It's a CLI tool and a REST API server that allows you to create a bond contract, pay for a file, and decrypt it.

It has a set of tests that check the contract logic and its execution costs.

### Minting/Burning Example

Here is a full example of a token minting/burning validator that works on both JVM and JavaScript:

[MintingPolicy.scala](https://github.com/scalus3/scalus/blob/master/scalus-examples/shared/src/main/scala/scalus/examples/MintingPolicy.scala)

And here is a project that uses it in web frontend:
[Scalus Minting Example](https://github.com/scalus3/scalus/tree/master/scalus-examples/js/)

### Minimal Size Withdrawal Validator

The challenge was to create the smallest possible validator that checks a certain withdrawal exists in the transaction.

[The result is 92 bytes long script](https://gist.github.com/nau/b8996fe3e51b0e21c20479c5d8548ec7)

Here's Plutus V3 version:

```scala 3
val validator = compile: (scriptWithdrawalCredential: Data, ctx: Data) =>
  def contains(list: builtin.List[Pair[Data, Data]]): Unit =
    if list.head.fst == scriptWithdrawalCredential then ()
    else contains(list.tail) // fails on empty list

  contains(ctx.field[ScriptContext](_.txInfo.withdrawals).toMap)

println(validator.toUplc().plutusV3.flatEncoded.length) // 86 bytes
```

## Scalus Multiplatform

Scalus implements a Plutus VM (CEK machine) that works on JVM, JavaScript and Native platforms
thanks to Scala multiplatform capabilities.
All from the same Scala codebase.

### Scalus on TypeScript/JavaScript via Scala.js

The Scalus npm package provides a JavaScript/TypeScript API for evaluating Plutus scripts and building transactions.

Here's how you can evaluate a Plutus script from a TypeScript program:

```typescript
import {Scalus} from "scalus";

const script = "545301010023357389210753756363657373004981";
const applied = Scalus.applyDataArgToScript(script, JSON.stringify({"int": 42}));
const result = Scalus.evaluateScript(applied);
console.log(result);
console.log(result.budget.memory);
```

Install the `scalus` npm package:

```shell
npm install scalus
```

### Scalus Native

Here's how you can evaluate a Plutus script from a C program:

```c
#include "scalus.h"
// Plutus V3, protocol version 10
machine_params* params = scalus_get_default_machine_params(3, 10);
ex_budget budget;
char logs_buffer[1024];
char error[1024];
int ret = scalus_evaluate_script(
    script, // script hex
    3, // Plutus V3
    params, // machine params
    &budget,
    logs_buffer, sizeof(logs_buffer),
    error, sizeof(error));

if (ret == 0) {
    printf("Script evaluation successful. CPU %lld, MEM %lld\n", budget.cpu, budget.memory);
    printf("Logs: %s\n", logs_buffer);
} else {
    printf("Script evaluation failed: %d\n", ret);
    printf("Units spent: CPU %lld, MEM %lld\n", budget.cpu, budget.memory);
    printf("Error: %s\n", error);
    printf("Logs: %s\n", logs_buffer);
}
```

See the full example in the [main.c](https://github.com/scalus3/scalus/blob/master/scalus-examples/native/main.c) file.

### How to build a native library

```shell
sbt scalusNative/nativeLink
```

will produce a static library in the `scalus-core/native/target/scala-3.3.7` directory.

## Roadmap

### DApp development framework

A framework that will help you build DApps faster and easier.

You define your smart contracts, data types, and interaction endpoints in Scala.
The framework will generate the frontend and backend code for you.

Yes, your REST API, WebSocket, and GraphQL endpoints for your DApp.
And the JavaScript code to interact with your DApp from the browser.

## Comparison to PlutusTx, Aiken, Plutarch

### PlutusTx

PlutusTx compiles almost any Haskell program to UPLC.
Cons are that you can barely understand how the UPLC is generated and how to make it smaller.
PlutusTx also tends to generate a lot of boilerplate code making the final script size bigger and more expensive to run.

### Aiken

Aiken is a new and young programming language which is a pro and a con.
Can only be used for writing on-chain smart contracts.
Can't reuse code for on-chain and off-chain parts.
Doesn't have macros.
Doesn't allow a low-level control over the generated UPLC code.

### Plutarch

Plutarch is very low-level. Use it when you need precise control over a script generation.
It's a Haskell library so be prepared to write Haskell code with all its developer experience drawbacks.

### Plu-ts

Plu-ts is a TypeScript DSL for writing Plutus scripts.
With Scalus you can do the same and much more but in Scala, and produce JavaScript code.

### Scalus

Scalus aims to be a better version of all the above.

* You can actually reuse Scala code for your validator, frontend and backend!
  The goal that PlutusTx failed to achieve.

* You can use existing Scala libraries for testing, including ScalaCheck and ScalaTest.

* Scala has a powerful type system that helps you write correct code. Check
  out [Stainless – Formal Verification for Scala](https://stainless.epfl.ch/) for formal verification.

* Scalus leverages all the development tools that Scala has, including IntelliJ Idea, VSCode, sbt, even GitHub CoPilot
  and ChatGPT! No need to learn new tools and languages.

* Debugger! It works!

* UPLC optimizer with compile-time partial evaluation produces competitive script sizes,
  often smaller than hand-optimized Aiken or Plutarch scripts.

* Full emulator for testing smart contracts without a running Cardano node.

* Cross-platform transaction builder (TxBuilder) that works on JVM and JavaScript.

* Source position tracking in error messages makes debugging failed scripts easy.

* Scalus allows only a limited subset of Scala, that can be reasonably efficiently
  compiled to UPLC without bloating the code.

* It's compiled to a fairly high-level human-readable intermediate representation, SIR.

* The huge part of any useful script is `ScriptContext` deserialization from `Data` representation.
  Scalus also provides primitives to do your custom deserialization to reduce validator size.

## Support

You can ask questions on Scalus Discord: https://discord.gg/ygwtuBybsy

The project is looking for funding to make it production ready.
If you are interested, please contact me at [@atlanter](https://x.com/atlanter) on X.
Follow the official Scalus account on X: [@Scalus3](https://x.com/Scalus3).

You can support the project by donating ADA or BTC to the following addresses:

ADA: addr1qxwg0u9fpl8dac9rkramkcgzerjsfdlqgkw0q8hy5vwk8tzk5pgcmdpe5jeh92guy4mke4zdmagv228nucldzxv95clqe35r3m

Please, consider becoming a sponsor on GitHub.

And vote for the project on Cardano Catalyst!
