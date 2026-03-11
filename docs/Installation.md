---
sidebar_position: 1
---

# Quick Start

## Prerequisites

You need a usual Scala development environment: JDK, [sbt](https://www.scala-sbt.org/), scala-cli an IDE of your choice.

### Install Scala 3 using Coursier

[Install Scala 3 on your machine](https://docs.scala-lang.org/getting-started/install-scala.html#install-scala-on-your-computer)
using [Coursier](https://get-coursier.io/).

Along with managing JVMs, `cs setup` also installs useful command-line tools:

| Commands             | Description                                                                 |
|----------------------|-----------------------------------------------------------------------------|
| `scalac`             | the Scala compiler                                                          |
| `scala`, `scala-cli` | [Scala CLI](https://scala-cli.virtuslab.org), interactive toolkit for Scala |
| `sbt`, `sbtn`        | The [sbt](https://www.scala-sbt.org/) build tool                            |
| `amm`                | [Ammonite](https://ammonite.io/) is an enhanced REPL                        |
| `scalafmt`           | [Scalafmt](https://scalameta.org/scalafmt/) is the Scala code formatter     |

For more information about `cs`, read
[coursier-cli documentation](https://get-coursier.io/docs/cli-overview).

You also can use [Scala CLI](https://scala-cli.virtuslab.org/) instead of SBT.
See [AdaStream](https://github.com/nau/adastream) for an example of a project that uses Scala CLI.

### Alternative: use Nix package manager

The easiest way to get started is to use [Nix](https://nixos.org/) package manager
and the [Scalus Starter Project](https://github.com/nau/scalus-starter).

## Clone Scalus Starter Project

Clone the [Scalus Starter Project](https://github.com/nau/scalus-starter) to get started with Scalus.

If you use [Nix](https://nixos.org/), we provided `flakes.nix` file to get a development environment with all the
required tools installed.

```bash
git clone https://github.com/nau/scalus-starter.git
cd scalus-starter
nix develop
code . # opens VSCode 
```

or open the project in your favorite IDE

Run

```bash
sbtn
```

to enter the interactive sbt shell.

Learn about using sbt in the [sbt documentation](https://www.scala-sbt.org/1.x/docs/).

### Building with sbt

Run `sbtn` to enter the sbt shell. `sbtn` is a thin wrapper around `sbt` for faster startup.

Run `compile` to compile the project.

Run `test` to run the tests.

Run `integration/test` to run the integration tests.

Run `scalafmtAll` to format the code.

Run `~compile` to automatically recompile the project when the source code changes.

Run `~test` to automatically run the tests when the source code changes.

## IDE integration

In case of using Metals (e.g. with the VSCode),
to export sbt build, run

```sh
sbt bloopInstall
```

Additional setup see

- https://scalacenter.github.io/bloop/docs/build-tools/sbt
- https://scalameta.org/metals/docs/editors/user-configuration/

## Adding Scalus to existing sbt project

If you already have an sbt project, you can add Scalus to it.
Add the following to your `build.sbt` file:

```scala
scalaVersion := "@SCALA3_VERSION@"
libraryDependencies += "org.scalus" %% "scalus" % "@VERSION@"
addCompilerPlugin("org.scalus" % "scalus-plugin" % "@VERSION@" cross CrossVersion.full)
```

That's it! You can now start using Scalus in your project.
