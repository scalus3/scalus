# sbt issue – filed as https://github.com/sbt/sbt/issues/9945

The filed text uses master 19008f4ed (56 s) as the reproducer; this is the earlier draft.

**Title:** Command aliases make every command line parse slowly: nested `aliasBody` parses take 2 minutes before the first command runs

## Summary

In a build with 18 command aliases, sbt needs about 2 minutes before it runs the first command of
the command line `++3.3.8;clean`. Without the aliases the same line takes 5 s. Thread dumps show
the main thread in the command parser, in nested `BasicCommands.aliasBody` frames. The same holds
for sbt 1.10.11, 1.11.7, 1.12.0 and 1.13.0.

## Reproduce

Public repository: https://github.com/scalus3/scalus, build file of commit `3a1653c0d` (it uses
`addCommandAlias` for 18 aliases; later commits replaced them).

```
git clone https://github.com/scalus3/scalus && cd scalus && git checkout 3a1653c0d
sbt "++3.3.8;clean"          # ~120 s on a 14-core Apple M-series laptop, ~2-9 min on GitHub runners
```

Comparison, only `build.sbt` changed:

| build.sbt | `sbt "++3.3.8;clean"` | `sbt "++3.3.8" "clean"` |
|---|---|---|
| with the 18 aliases | 122 s | 23 s |
| aliases replaced by `Command.command` sequences | 8 s | 4 s |
| without the aliases | 5 s | – |

Removing single aliases (all others kept):

| removed | time |
|---|---|
| none | 120 s |
| `ci` (`"ci-jvm;ci-js;ci-native"`) | 5 s |
| `precommit`, `cleanpile`, `quick`, `it` | 6 s |
| `ci-jvm` (`"ci-jvm-3_3_8"`) | 14 s |
| `mima`, `mima-jvm`, `mima-js` | 24 s |

Keeping only subsets (the four `++…` aliases; or `ci`, `ci-jvm`, `ci-js`, `ci-native`,
`ci-jvm-3_3_8`) is fast (5 s). Several alias names are prefixes of other names (`ci` /
`ci-jvm` / `ci-jvm-3_3_8`; the task `clean` / the alias `cleanpile`).

We could not reproduce it in a minimal build: 30 empty projects with the exact same alias block,
also with 88 000 settings, also with the same plugins, stay at 3-7 s. So the build's size or shape
(Scala.js and Scala Native cross projects) also matters.

## Evidence

Main thread, sampled every 15 s during the silent phase (sbt 1.13.0):

```
sbt.internal.KeyIndex0.keys(KeyIndex.scala:265)
sbt.internal.Act$.key(Act.scala:313)
sbt.internal.Act$.$anonfun$taskKeyExtra$3(Act.scala:160)
...  sbt.internal.util.complete.*.derive ...
sbt.BasicCommands$.aliasBody(BasicCommands.scala:549)
sbt.BasicCommands$.$anonfun$newAlias$1(BasicCommands.scala:543)
sbt.Command$.$anonfun$argParser$1(Command.scala:172)
sbt.Command$.$anonfun$simpleParser$3(Command.scala:183)
   (the four frames above repeat 4 times)
sbt.internal.util.complete.DefaultParsers$.parse(Parsers.scala:351)
sbt.Command$.process(Command.scala:192)
sbt.MainLoop$.processCommand(MainLoop.scala:312)
```

`aliasBody` (BasicCommands.scala, 1.13.0) parses the alias body with the combined parser of all
other commands, including all other aliases:

```scala
val aliasRemoved = removeAlias(state, name)
val partiallyApplied = Parser(aliasRemoved.combinedParser)(value)
```

So each alias that can still match while the command line is parsed expands into a parse of its
body, which can expand the parsers of the other aliases again.

## Workaround

Define multi-step "aliases" as commands that queue their steps:

```scala
def commandSequence(name: String, steps: String*): Def.Setting[?] =
    commands += Command.command(name)(state => steps.toList ::: state)
```

## Environment

sbt 1.10.11 / 1.11.7 / 1.12.0 / 1.13.0, JDK 21 (CI) and 25 (local), macOS and Ubuntu 26.04.
