# The Lean server

How the `blaster-uplc` tactic runs its checks in one Lean process that stays, in place of one
process per check ([the Lean check](uplc-blaster.md#the-lean-check)). It describes a Scala
representative of a running Lean language server: start it, give it a check, cancel a check,
and shut it down, on request or when the JVM exits. The server is the only way the tactic runs
Lean.

Status: built, up to and with the tactic. `LeanServer` is in `scalus.verify.lean`, with
`JsonRpcSession` under it and `LeanServers`, which gives a tactic its server, over it.
`UplcBlaster` and the tests' `LeanProofs` run on them. "Exporting
to the workspace" and "Keeping prepared leaves" are proposed. "What was measured" records an
experiment made before any of it, outside the code base, with checks written by
`UplcBlaster.writeCheck`, on Lean 4.24.0 (language server 0.3.0), the version in
`lean-toolchain`.

## Why

A check was one run of `lake env lean Check.lean`. Three things followed.

- **Most of a small check is start-up.** A check of `Math.clamp` takes 4.7 s on the command
  line. The same check in a running server takes 0.2 to 0.4 s. Lean starts, and loads the
  workspace's libraries, for every statement.
- **Nothing is kept between statements.** A program that two statements share is run
  symbolically twice. The three guarantees of `VestingValidator.spend` take 3 min one by one,
  and 67 s as one statement, in which the validator's program is run once
  ([statement semantics §7](prop-semantics.md#7-specifications-in-the-functions-body)).
- **A check is stopped by killing processes.** The time limit ends `lake`, Lean and the solver
  by their process handles, and a child started in between is not found.

## Lean's server

`lake serve`, run in the workspace, starts Lean's language server with the workspace's libraries
on its search path. It speaks the Language Server Protocol: JSON-RPC 2.0 messages, each after a
`Content-Length` header, on its standard input and output.

- **Processes.** The server is a watchdog process. It starts one worker process for each open
  document, and the worker elaborates that document. Blaster's solver is one more process below
  the server.
- **Documents.** A document's text comes through the protocol, in `textDocument/didOpen` and
  `textDocument/didChange`. It need not exist on disk; its URI only places it in the workspace.
- **Incremental elaboration.** After an edit Lean elaborates again from the first command that
  changed. The imports of the header are loaded once for a worker, while the header stays as it
  is.
- **Messages.** What a command reports is a diagnostic, with the lines of the command and a
  severity. `#blaster` reports `✅ Valid` as information, and `❌ Falsified` and each line of
  the counterexample as errors.

Two messages are Lean's own, not part of the protocol: the request
`textDocument/waitForDiagnostics`, which is answered when a version of a document is elaborated
and its diagnostics are sent, and the notification `$/lean/fileProgress`, which lists the ranges
still being elaborated.

## What was measured

Three generated checks about `Math.clamp` at a budget of 200, two leaves each, and two of the
[statements that do not finish](uplc-blaster.md#statements-that-do-not-finish).

| Step | Time |
|---|---|
| command line, one check | 4.7 s |
| start the server, and `initialize` | 1.0 s |
| the first check in a document, which loads the workspace | 3.5 s |
| a later check with other leaves, by an edit of the same document | 0.4 s |
| a later check that changes the goal only | 0.2 s |
| a false statement, with its counterexample | 0.4 s |
| a closed statement, decided by `native_decide` | 0.2 s |
| a check whose header imports a module that is not there | 1.4 s |
| the next check after that one, which loads the workspace again | 3.4 s |
| a check after the document was closed, and opened again | 5.5 s |
| `shutdown` and `exit` | 0.1 to 0.5 s |

- **An edit is answered at once, and ends most of what ran.** `gcd` at budget 400 leaves the
  solver running, and `filter` at budget 400 leaves Lean in its own symbolic run. In both, an
  edit to another check was answered in 0.2 to 0.7 s, the solver's process was gone, and the
  worker used no processor after it.
- **An edit does not end every evaluation.** `#eval` of a function that never returns goes on
  after the edit: the edit is answered in 0.2 s, and the worker keeps one processor busy. An
  evaluation ends only where it looks for its cancellation. A closed statement is decided by
  such an evaluation.
- **Closing the document ends the worker.** After `textDocument/didClose` the worker's process
  is gone within two seconds, with the solver or the evaluation, in all three cases. The next
  check opens the document again, and loads the workspace again.
- **Shutdown leaves nothing.** After `shutdown` and `exit` no `lean`, `lake` or `z3` process is
  left, also when a check was canceled before.
- **The server ends when its input closes.** With the client's end of the pipe closed, as when
  a JVM is killed, `lake serve` exits at once, and the worker and a running solver are gone
  within a second.
- **The server asks, too.** It sends the requests `client/registerCapability`,
  `workspace/semanticTokens/refresh` and `workspace/inlayHint/refresh`, which want an answer.
- **Closed statements read as on the command line.** One that holds reports nothing. One that
  does not reports the error ``Tactic `native_decide` evaluated that the proposition … is
  false``, the text the tactic looks for today.
- **A server has the libraries of one workspace.** They are those of the workspace it was
  started in, wherever a document's URI points. A server started in the Scalus workspace
  elaborates a Scalus check that is placed in another workspace, and does not find that
  workspace's modules. A server started there does the opposite.
- **A bad header does not end the server.** The failing import is an error on the first line,
  and the next check is answered. A changed header starts the worker anew, so the header of
  every check has to be the same text, as the tactic's is.
- **The server says which command it is at.** `$/lean/fileProgress` lists the ranges of the
  document that Lean has not finished, and the first of them starts with the command it
  elaborates: Lean takes one command after the other. Only an earlier command that leaves
  work behind, as the proof of a theorem does, stays first while Lean goes on. On the `filter`
  check at budget 400: the imports until 2.9 s, the symbolic run of the first program until
  10.8 s, that of the second until 17.9 s, then `#blaster`. The solver's process was there from
  19.0 s, and the check was answered at 19.2 s.
- **Blaster starts the solver before it translates.** In `#blaster`, Blaster first simplifies
  the statement, then starts the solver's process, translates the statement for it, and asks
  it (`Translate.main`). So no solver process means the simplification, and a solver process
  that has run without working means the translation. The time a process has worked is what
  the system tells of it, beside the time it has run. The JVM gets it of another process on
  Linux, and not on macOS, where only the time since Blaster started the solver is known.
- **A worker that ends does not end the server.** Two cases were tried: the worker killed from
  outside in the middle of a check, as the system does for memory, and `#eval` of a recursion
  too deep for the worker's stack. In both `waitForDiagnostics` is answered at once with the
  error `Server process for … crashed, likely due to a stack overflow or a bug` (code -32902),
  and an empty list is published for the version. The server lives on. The next check is
  answered in 2.8 s, the time of loading the workspace, as an edit and after the document was
  closed and opened again.

Not tried: what a kept leaf saves on a heavy program; the memory of a worker after many checks.

## Design

### `LeanServer`

One class in `scalus-verification`, which knows Lean's server and nothing of statements. A
server is created explicitly, for a workspace directory, by the code that needs one. There is
none behind the scenes: which workspace it serves is a choice someone has to make ("The
workspace directory").

```scala
package scalus.verify.lean

/** A running Lean language server for one workspace. */
final class LeanServer extends AutoCloseable {

    /** Elaborates `source` as the server's document, and returns what Lean reported. */
    def check(source: String, timeout: Option[FiniteDuration]): LeanServer.Result

    /** The same, for a text that refers to files: `source` is given a directory for them, which
      * is there for as long as the check runs.
      */
    def check(source: Path => String, timeout: Option[FiniteDuration]): LeanServer.Result

    /** Whether the server takes no further check. */
    def isClosed: Boolean

    /** Ends the server. It is ended at the latest when the JVM exits. */
    def close(): Unit
}

object LeanServer {

    /** Starts `lake serve` in the workspace `directory`, or says why it cannot. */
    def start(directory: Path): Either[String, LeanServer]

    enum Severity { case Error, Warning, Information, Hint }

    /** What a command of the document reported: a diagnostic, by the line of its command. */
    final case class Message(line: Int, severity: Severity, text: String)

    /** How far a check had come: where in the document, for how long, and the processes the
      * check had started, with the time each had run and had worked.
      */
    final case class Progress(reached: Reached, spent: FiniteDuration, started: List[Started])
    enum Reached { case NotStarted, Unreported, Command(line: String), End }
    final case class Started(name: String, running: FiniteDuration, working: Option[FiniteDuration])

    enum Result {
        /** The document was elaborated: its messages, in the order of their commands. */
        case Finished(messages: List[Message])

        /** The time limit passed. The check is given up, and what it started has ended. */
        case TimedOut(progress: Progress)

        /** The check was not made. The server takes the next one, unless it ended itself. */
        case Failed(reason: String)
    }
}
```

### The protocol in use

| | Message | Use |
|---|---|---|
| request | `initialize` | once: the workspace as `rootUri`, no capabilities |
| notification | `initialized` | once |
| notification | `textDocument/didOpen` | the first check: the document's text, with Lean's `dependencyBuildMode` set to `never` |
| notification | `textDocument/didChange` | every later check: the whole new text, the next version |
| notification | `textDocument/didClose` | to give a check up: Lean ends the document's worker |
| request | `textDocument/waitForDiagnostics` | answered when that version is elaborated |
| received | `textDocument/publishDiagnostics` | the messages of a version so far; those at the moment of the answer are the result |
| received | `$/lean/fileProgress` | the first command Lean has not finished, or that it has finished them all: where a check was when it was given up |
| received request | `client/registerCapability`, `workspace/*/refresh` | answered with a null result |
| request, notification | `shutdown`, `exit` | to end the server |

- **The result is taken with the answer.** The diagnostics of a check are those published when
  `waitForDiagnostics` is answered, read on the thread that reads the server. What the server
  publishes later is not the check's: when it ends a worker, on `didClose` and on `shutdown`,
  it publishes an empty list for the document, which would read as a check that found nothing.
  A check that is answered while the server is being closed is `Failed` for the same reason.
- **A version without diagnostics is a failure.** Lean publishes for every version, an empty
  list where there is nothing to report. A version it answers for without having published is
  not read as one that elaborated cleanly.
- **No build inside a check.** By default the server builds what a document imports. With a
  workspace that is not built, that build would run inside a check's time limit, be ended with
  the check, and start again with the next. `never` leaves it out: the header is then reported
  as an error, as `lake env lean` reports it.

A thread reads the server's output and hands each answer to the request that waits for it.
Another writes to its input, so that no sender waits for the server to read: not a check with a
time limit, and not the reading thread, which answers the server's requests and has to go on
reading. The server's error output is kept, and its end is quoted in a `Failed`.

### One document, one check at a time

A server has one document, `ScalusCheck.lean` in the workspace, which is never written to disk.
A check is a new version of it: the first is opened, each later one is an edit. After a check
that was given up it is opened again.

- **Why one document.** A second document is a second worker, which loads the workspace again.
  One document pays the 3.5 s once.
- **Checks do not overlap.** A later version cancels the one before, so two checks in one
  document would not both finish. A check that is asked for while another runs waits for it,
  in whatever thread. Its time limit counts that wait, and the waiting thread can be
  interrupted. Checks in parallel need several servers, each with its own worker and solver.
- **Leaf files.** `#import_uplc` reads a program from a file, by its path. The server gives a
  check a directory of its own for them, and removes it when the check ends. The files are
  written when the check's turn has come and the server is known to be open, so a server that
  closes meanwhile does not take the directory from under them.

### The workspace directory

A server is started in a workspace directory, and that is its one setting. The directory has the
`lakefile`, the `lean-toolchain` and the built libraries, so it decides which Lean runs and what
a check can import.

- **Today** the workspaces are directories in Scalus's own sources: the library's,
  `scalus-verification/src/main/lean`, and the vesting example's ("Several workspaces").
  `LeanProofs` looks them up from the working directory upwards. That serves the Scalus
  repository, and no project that uses Scalus as a library.
- **Given when the server is created.** `LeanServer.start(directory)` takes it, and the tactic
  takes the server. Neither has a default: a tactic without a server cannot be made.
- **In the tests** a suite names it, as `leanWorkspace` of `LeanProofs`. Unless the suite
  overrides it, it is the library's: the environment variable `SCALUS_LEAN_WORKSPACE`, and then
  the directory in Scalus's sources, looked up from the working directory.

### Several workspaces

A server has the libraries of one workspace, so every workspace has its own server, and code
that works with two workspaces creates two. Each costs a watchdog, a worker with its solver,
and the load of its libraries.

One server does serve several Lean projects where one workspace requires the others: its search
path then has the libraries of them all. They need one toolchain among them, and one version of
every dependency they share. A workspace of proofs written by hand that requires `ScalusProofs`
is that case: its server runs the generated checks as well.

The vesting example has such a workspace, `scalus-examples/jvm/src/test/lean/LinearVesting`, and
`VestingVerificationTest` runs its checks there. It requires `ScalusProofs` by its path, so the
library is built where it lies, once. Lake would still clone Blaster and PlutusCore a second
time, into the requiring workspace, and build them there. So the example sets its `packagesDir`
to that of the library's workspace, and the clones and what is built of them are shared.
`lake build` in the example then builds its own module, in seconds. The price is that the two
manifests have to name the same revisions: Lake checks out what the manifest of the workspace
it runs in names, for both. The example's suite compares them.

### Time limit and cancellation

`check` waits for at most `timeout`: for the check that runs before it, if one does, and then
for the answer of `waitForDiagnostics`. A check whose limit passes while it still waits for
another is `TimedOut` without having started. When the limit passes in the check itself, the
check is given up by closing the document:

1. the processes below the server that were not there before a document was opened are listed:
   the worker, and what it started. They and the command Lean is at are the check's progress,
   which `TimedOut` carries;
2. `textDocument/didClose` is sent, on which Lean ends the worker;
3. when those processes have ended, the result is `TimedOut`;
4. if they have not ended after a few seconds, they are ended by their handles; if that fails
   too, the server is closed, and the result is `Failed`.

A thread that is interrupted while it waits gives its check up in the same way, and the
interruption goes on to its caller.

An edit would be cheaper, and is not enough. It ends Blaster's run and the solver, and it leaves
an evaluation running that never looks for its cancellation, as that of a closed statement.
Closing the document ends the process, whatever runs in it, and `TimedOut` then says that
nothing of the check is left. The price is the 5.5 s of the next check. A check that is given up
must be stopped at once, and not by the next check: the solver of `gcd` at budget 400 holds 7 GB
after 34 s.

### Shutdown

- **On request.** `close` lists the server's processes, sends `shutdown`, then `exit`, and waits
  a few seconds for the process to end. Whatever the outcome, it then ends those that are left
  of the listed ones, the children before `lake`, and removes `directory`. They are listed first because `lake` no longer
  names them once it has ended. A session that has ended is not asked. `close` can be called
  more than once.
- **When the JVM exits.** Every started server is in a registry, and one JVM shutdown hook
  closes those still in it. `close` takes its server out. The hook is registered only while a
  server is open: a hook that stayed would hold the classes of a test run in sbt's JVM, which
  goes on after the tests.
- **When the JVM is killed.** No hook runs. The server's input closes, and it ends by itself.

### A check that fails, and a server that ends

A check is `Failed` when it was not made: `waitForDiagnostics` was answered with an error, or
not at all. What becomes of the server depends on which.

- **The worker ended.** Lean answers with an error, and goes on ("What was measured"). The
  check has failed and the server has not: the document is closed, as for a check that is given
  up, and the next check opens it with a new worker. So a statement whose check runs out of
  stack or of memory costs that statement, and not those after it. A process per check had
  that by itself; a server has to see to it.
- **The server ended.** When its output closes, every waiting request ends with `Failed`, with
  the end of the server's error output. The `LeanServer` is then closed, `isClosed` says so,
  and every later `check` of it is `Failed`. It does not start itself again: whoever created
  it creates another.

### JSON-RPC without a library

The protocol is written out in `JsonRpcSession`, on jsoniter-scala, which the module has through
`scalus-core` and Scalus uses elsewhere.

- **What is used is small.** About ten kinds of message, and the framing is a length and a
  body.
- **The two that matter are Lean's own.** A library types the standard messages.
  `waitForDiagnostics` and `fileProgress` need their own declarations either way.
- **No new dependency.** LSP4J would bring four jars (`lsp4j`, `lsp4j.jsonrpc`, Gson and its
  annotations), and a Java API of futures and `java.util` collections to wrap.

LSP4J is the alternative. It has the framing, the matching of answers to requests and the error
answers written and tested, and the module is not published, so the jars cost little. It becomes
the better choice if more of the protocol is used.

## Using it from `UplcBlaster`

The tactic runs every check in a server. The run of `lake env lean` for each check is gone, with
the code that started and stopped it: there is one way to run Lean, not two to keep alike.

- **The tactic is given a provider of its server.** `UplcBlaster(budget, servers)`, whose
  checks have ten minutes each, and `UplcBlaster(budget, servers, timeout)`, where `servers` is
  a `LeanServerProvider`: one method,
  `server()`, which gives the server for the next check, or the reason there is none. There is
  no constructor without one. The tactic asks before every check, and a reason is the check's
  `Failed` result.
  - A tactic is made, and prepares a statement, without Lean. Its arguments are checked, and a
    statement outside its fragment is `Unsupported`, where no Lean runs.
  - The provider decides how long a server lives. The tactic neither starts nor ends one, and a
    tactic that is kept sees the server that runs now, not the one that ran when it was made.
- **`LeanServers` is the provider for a workspace.** `LeanServers.in(directory)` starts nothing.
  The first check that asks gets a server started for it. A later one gets the same server, or
  another where that one has ended, so the statements after one that ended Lean's server are
  still checked. Whoever made it closes it, which ends the server that runs. One server of the
  caller's own is the provider `() => Right(server)`: once it is closed, its checks fail, and
  say so.
- **`LeanProofs` has the servers of its suite's workspace.** They are closed after the suite's
  last test. Its provider also asks whether Lean can run here: without Lean no server starts,
  and the test is canceled, or fails under `SCALUS_REQUIRE_LEAN`, when its first check runs. A
  test that runs no check is not canceled.
- **One reading of the result.** The text of a check is the one `writeCheck` writes. Its leaves
  are files in a directory of the server's while it runs. The messages of `Finished` are joined
  into the output that `verdict` reads, with a flag for whether one of them is an error.
  `Valid`, `Falsified` with its counterexample, a closed statement that is false, and
  everything else as a failure are told apart there, in one place.
- **Where a check stopped.** The reason of a check that was given up says how far it had come,
  from the progress of `TimedOut`: still in the symbolic run of one of its programs, in
  Blaster's simplification of the statement, or after Blaster had started the solver, with the
  time the solver had worked. The solver is the process of the check that is not Lean's own.
  The first means that the budget lets a program run through a loop, and is the case a smaller
  budget or a fixed shape helps. A solver that worked all its time is a statement the solver
  finds hard.
- **Results.** `TimedOut` is `Inconclusive`, and `Failed` is `Failed`. A check that Lean gives up
  itself, at its limit of work, is `Inconclusive` too: the error `(deterministic) timeout`
  means the statement is not decided, not that something broke. A check sets that limit,
  `maxHeartbeats`, to twice Lean's default, and `withMaxHeartbeats` of the tactic sets another
  ([the tactic](uplc-blaster.md#the-lean-check)).

What it changed, measured on the same machine:

| | a Lean process per check | in the server |
|---|---|---|
| the tests of `scalus-verification`, 136 of them | 8 min 49 s | 1 min 15 s |
| `VestingVerificationTest` | about 5 min | 2 min |
| the three guarantees of `VestingValidator.spend`, together | 67 s | 17 s |
| the vesting statement with any outputs, until Lean's default limit of work | 159 s | 17 s |

There were two costs, and the server removes both.

- **The start of Lean for every check,** about 4.3 s of a small check's 4.7 s. That is the
  first row.
- **Blaster ran interpreted.** Blaster is built as a native library, which Lean loads as a
  plugin. The tactic ran `lake env lean`, which sets the search path and loads no plugin, so
  Lean interpreted Blaster's code. The server gets the workspace's setup from Lake, plugins
  included, and runs Blaster compiled. That is the last two rows: the same check takes 159 s
  with `lake env lean`, and 22 s with `lake lean`, which uses the setup as the server does.

### A check outside the server

For looking into one check, not as a second way of the tactic. With the environment variable
`SCALUS_LEAN_KEEP_CHECKS` set to a directory, the tactic also writes every check it runs there,
each in a directory of its own: `Check.lean` and its leaf files, as `UplcBlaster.writeCheck`
writes them. Such a check runs by hand, `lake lean Check.lean` in the workspace: in a process
of its own, with nothing of the server, and with any option, such as the profiler. `lake lean`
loads Blaster as the server does. `lake env lean` runs the check too, with Blaster interpreted,
and several times slower.

A variable that switched the tactic itself to a process per check would do the same for a
whole run, and would keep the second runner in the code to maintain. It is left out until the
need shows.

## Exporting to the workspace

A further step. A project that uses Scalus as a library has no checkout of it, and so no
workspace to start a server in. Three things are written into a workspace directory for it.

- **The library.** The Lean sources of `ScalusProofs`, with the `lakefile`, the toolchain file
  and the manifest, go into the module's jar as resources. An export writes them into the
  directory where they are missing or differ, and builds them with `lake build`. The first
  build fetches Blaster and PlutusCore and compiles them, which takes minutes and the network.
  After it the directory is a workspace like the one in the sources. A stamp with the hash of
  the exported sources tells an export that is current from one to renew.
- **The checks.** With a workspace of its own, what the tactic writes lies in it: the leaf
  files of the checks that run, and the kept checks of `SCALUS_LEAN_KEEP_CHECKS`, which then
  has a directory in the workspace as its default. A kept check that lies in the workspace
  opens in an editor with Lean's own tooling, and finds the libraries.

- **The generated Lean.** `LeanExporter` renders a statement as a Lean proposition, and
  `lean-direct` maps the functions it uses to Lean functions
  ([overview §6.3](../verification-overview.md)). They are written into the workspace as
  modules. Unlike a check they have to be files: a proof imports them, and Lean builds what a
  document imports from the workspace's sources.

Proofs written by hand are the fourth thing a workspace holds. They are not exported: they are
the project's own sources, in the workspace the generated modules are written to, or in one
that requires it ("Several workspaces").

## Keeping prepared leaves

A further step, not measured on a heavy program.

Lean elaborates again from the first command that changed. So the document is the header, then
the leaves, then the goal, and a check only adds to it:

- a leaf is named after its program's hash, its variables and the budget, and its file after
  the hash, so the same leaf is the same text in every check;
- a check appends the leaves it needs that are not in the document yet, and replaces the goal;
- everything before the first new leaf is unchanged, so Lean keeps it.

A leaf is then imported and run symbolically once for a server, whatever the number of
statements that use it. The guarantees of a validator, proved one by one, would cost what they
cost together. The document is opened anew when it has grown beyond a number of leaves.

## Tests

- **With Lean,** `LeanServerTest`, canceled without it like the other tests that run Lean: a
  check and the next; a message, an error, and a header that cannot be loaded; a time limit on
  an evaluation that does not return, the end of its worker, and a check after it; an
  interrupted thread; a check whose worker ends of a recursion too deep, and the next check
  of the same server; a check that waits for another, to its time limit and to an
  interruption; the files of a check; `close` during a check, and no process of the server
  left after it; a server whose processes were ended from outside; the servers of a
  workspace, one at a time, another after one that ended, and none once they are closed.
- **Without Lean,** `JsonRpcSessionTest`, in every build: the framing and the matching of
  answers, against a stand-in on piped streams in the same JVM; a request of the other end's
  that is answered; a sender that does not wait for the other end to read; an output that
  closes with a request waiting.
- **The module's tests run in sbt's JVM,** as before. Every test closes the server it starts,
  whatever its outcome. Running them in a JVM of their own was tried, and added a quarter of an
  hour to a full run of the build's tests: sbt runs the test JVMs of different modules one
  after another, and almost all of these tests' time is the start of a Lean process for every
  check, which then no longer overlaps with anything.
- **The tactic's own suites** are the test of its move: their statements run as they were, on
  the server of their suite. `UplcBlasterTest` adds a workspace without the library, a closed
  server, a provider without a server, and a kept check.

## Design questions

1. **Steps.** The server alone, with its tests, and then the tactic moved onto it, with the run
   of a process per check removed: both done. After those, in either order, the export of a
   workspace and kept leaves.
2. **Who owns a server.** Decided: the code that creates its provider. A server is for one
   workspace directory, there can be several, and one may first have to be exported, so no
   place is left to choose a server silently.
   - *A provider, created explicitly and passed to the tactic* (decided): `LeanServers` for a
     workspace, which `LeanProofs` has for a test suite, and a runner for its run. The JVM's
     shutdown hook closes what was not closed.
   - *A server, created explicitly and passed to the tactic:* the first form. A tactic then
     needed a running Lean to be made at all, also for what needs none, and a tactic that was
     kept failed every check after one that ended its server.
   - *One per JVM and workspace, found by the directory:* no caller changes, and the workspace
     becomes a hidden setting of the tactic.
3. **Library or not.** Written out is recommended; see "JSON-RPC without a library".
4. **Checks in parallel.** Not in the first step. A pool of servers is the way, each with its
   own worker; what it costs in memory is not measured.
5. **Structured results.** The verdict stays text in a message. A command of the workspace that
   reports JSON, or Lean's RPC over the same connection, would remove the reading of text.
6. **Where an exported workspace lies.**
   - *One per project, under its build directory:* nothing shared and nothing to clean up, and
     every project builds Blaster and PlutusCore for itself.
   - *One per Scalus version, in a cache directory of the user* (recommended): the build of the
     dependencies is done once. Checks of different projects then lie in one workspace, each
     under its project's name.
   - *One per project, with the packages of one per Scalus version:* as the vesting example has
     those of the library ("Several workspaces"). The dependencies are still built once, and a
     project's checks and proofs lie in a workspace of its own.

## Limits

- `waitForDiagnostics` and `fileProgress` are Lean's extensions, and can change with the
  toolchain. They were checked with the version the workspace pins. With the server as the only
  way, such a change stops every proof until this code follows it; a kept check still runs by
  hand.
- That Lean ends the worker of a closed document is observed, not documented. Hence the ending
  of the worker's processes by their handles where it does not.
- A worker that lives long keeps what it loaded. Its memory after many checks is not measured.
- `lake` and a built workspace are needed, as now.
