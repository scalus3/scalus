# LLM documentation for TypeScript and Java users

Status: approved design, 2026-11-04. Phase 1 targets the 1.5.0 release. Phase 2 is deferred.

## Purpose

A TypeScript, JavaScript or Java developer reaches Scalus documentation through one of two entry
points:

1. They install the `scalus` Claude Code plugin (`scalus-skills/`).
2. They point their agent at `https://scalus.org/llms.txt`.

Today both entry points serve Scala developers only. The plugin hook stays silent in a project
without an `.sbt`, `project.scala` or Mill file. `llms.txt` has one link to the JS Emulator page.
`llms-api.txt` lists Scala signatures only. This design routes TS/JS and Java developers to the API
they can call, and keeps that text out of the context of Scala agents.

## Definitions

- **TS user**: a developer whose project declares the npm package `scalus` in `package.json`.
- **Java user**: a developer whose project declares a Scalus artifact in `pom.xml`, `build.gradle`
  or `build.gradle.kts`.
- **npm README**: `scalus-cardano-ledger/js/src/main/npm/README.md`.
- **d.ts file**: `scalus-cardano-ledger/js/src/main/npm/scalus.d.ts`. It is generated from TASTy.
  The `checkDtsUpToDate` task fails `ci-js` when it is stale.
- **TS bundle**: the file `llms-ts.txt` on scalus.org.
- **Java bundle**: the file `llms-java.txt` on scalus.org.
- **Release tag**: the git tag `v<version>` that the Deploy Site workflow builds.
- **Plugin**: the `scalus` Claude Code plugin in `scalus-skills/`.
- **Hook**: `scalus-skills/hooks/session-start`.

## Constraints from the current site build

- `scalus-site/scripts/build-version.sh` builds `/llms/<version>/` from the release tag. It runs the
  generators that exist in that tag.
- `scalus-site/scripts/finalize-versions.sh` copies the latest version's llms files to the site
  root.
- So a generator reaches the root of scalus.org only after a release tag contains it.

**Why this matters for the release:** if the TS generator lands after the v1.5.0 tag, TS users get
no TS bundle until 1.5.1.

## Phase 1: TypeScript (1.5.0)

### TS bundle generator

- **Purpose**: write the TS bundle from the npm README and the d.ts file.
- **Who**: the new script `scalus-site/scripts/generate-llms-ts.mjs`.
- **Trigger**: `pnpm build` (`postbuild`) and `build-version.sh`.
- **Structure**:

  | Input | Output |
  |---|---|
  | npm README, d.ts file | `scalus-site/out/llms-ts.txt` |

- **Checks enforced here**:
  - The generator MUST write a header line that names the npm package `scalus` and its version
    from `package.json`. `[TS-1]`
  - The generator MUST copy the npm README verbatim after the header. `[TS-2]`
  - The generator MUST copy the d.ts file verbatim after the npm README, inside a ```` ```ts ````
    fence. `[TS-3]`
  - The generator MUST exit with a non-zero status when either input file is missing or empty.
    `[TS-4]`
  - The generator MUST NOT write any other file. `[TS-5]`

**Why:** both inputs are already kept current. CI checks the d.ts file against the code, and the
npm README is the published package documentation. A verbatim copy cannot drift. A hand-written
summary would drift at the next API change.

*Example, illustrative.* The first lines of `llms-ts.txt`:

```
# Scalus for TypeScript and JavaScript (npm package scalus 1.5.0)

The npm package README, then the full TypeScript definitions (scalus.d.ts).

# Scalus - Cardano Toolkit for JavaScript
...
```

### Site build integration

- `scalus-site/package.json` `postbuild` MUST run `node scripts/generate-llms-ts.mjs`. `[TS-6]`
- `build-version.sh` MUST run `generate-llms-ts.mjs` when the tag has it, and copy `llms-ts.txt`
  to `/llms/<version>/`. `[TS-7]`
- `build-version.sh` MUST warn and continue when the tag has no `generate-llms-ts.mjs`, as it does
  for the other generators (`[VB-12]`). `[TS-8]`
- The link rewrite in `build-version.sh` MUST rewrite `/llms-ts.txt` to
  `/llms/<version>/llms-ts.txt` in the versioned `llms.txt`. `[TS-9]`
- `finalize-versions.sh` MUST copy `llms-ts.txt` of the latest version to the site root, as it
  does for `llms-api.txt`. `[TS-10]`

*Example, illustrative.* After the deploy of v1.5.0, these URLs exist:

| URL | Content |
|---|---|
| `https://scalus.org/llms-ts.txt` | TS bundle of 1.5.0, without the version line |
| `https://scalus.org/llms/1.5.0/llms-ts.txt` | TS bundle of 1.5.0, with the version line |
| `https://scalus.org/llms/1.4.1/llms-ts.txt` | absent: v1.4.1 has no generator |

### llms.txt index

- `scalus-site/public/llms.txt` MUST have a section "For TypeScript and JavaScript agents" directly
  after "For LLM agents". `[TS-11]`
- That section MUST link `/llms-ts.txt` and `/docs/testing/js-emulator`. `[TS-12]`
- That section MUST tell the agent to read `node_modules/scalus/scalus.d.ts` and
  `node_modules/scalus/README.md` when the package is installed. `[TS-13]`
- That section MUST tell the agent that `llms-api.txt` and `llms-examples.txt` are Scala only.
  `[TS-14]`

**Why:** the npm package ships `scalus.d.ts` and `README.md` (`files` in `package.json`). The
installed copy matches the version the project uses. The site copy matches only the latest release.
`[TS-14]` stops a TS agent from reading Scala signatures as its API.

### `scalus:typescript` skill

- **Purpose**: give a TS agent the entry points and the pitfalls of the npm package.
- **Who**: the new file `scalus-skills/skills/typescript/SKILL.md`.
- **Trigger**: a TS user asks for work that uses Scalus: an Emulator test, script evaluation,
  parameter application, budget or fee checks, or transaction balancing.
- **Checks enforced here**:
  - The skill MUST tell the agent to read the installed d.ts file and npm README before it writes
    code. `[TS-15]`
  - The skill MUST name the fallback `https://scalus.org/llms-ts.txt` when the package is not
    installed. `[TS-16]`
  - The skill MUST give one short example for each of these entry points: `Emulator`, script
    evaluation with `PlutusVM` or `Scalus`, parameter application, and transaction evaluation.
    `[TS-17]`
  - The skill MUST state that the package is an ES module, and that older Node versions need
    `await import("scalus")` instead of `require`. `[TS-18]`
  - The skill MUST state that `EvaluationOptions.mainnet` uses a bundled PV11 mainnet snapshot,
    and name `EvaluationOptions.fromProtocolParams` for other parameters. `[TS-38]`
  - The skill MUST state that a Plutus V3 validator that does not return unit fails with
    `INVALID_RETURN_VALUE`, and that the budget is still reported. `[TS-39]`
  - The skill MUST NOT copy API signatures from the d.ts file. `[TS-19]`
  - The skill MUST state that validators are written in Scala, and point to `scalus:contract` for
    that work. `[TS-20]`

**Why `[TS-19]`:** copied signatures go stale. The installed d.ts file is always correct for the
project.

*Implementation note.* Take every example in `[TS-17]` from the npm README or `js-emulator.mdx`,
then check it against the d.ts file of the release. The writer of the skill MUST NOT invent an
entry point name.

### Hook and routing

- The hook MUST treat a project as a TS user project when `$PROJECT_DIR/package.json` names
  `"scalus"` as a key in `dependencies`, `devDependencies` or `peerDependencies`. `[TS-21]`
- The hook MUST inject the `using-scalus` skill for a TS user project, as it does for a Scala
  project. `[TS-22]`
- The hook MUST NOT depend on `jq`, `node` or `python`. `[TS-23]`
- `using-scalus` MUST have a routing row: "Use Scalus from TypeScript or JavaScript (Emulator,
  script evaluation, parameters) → `scalus:typescript`". `[TS-24]`
- The opening of `using-scalus` MUST describe both Scala and TS/JS projects. `[TS-25]`
- `using-scalus` MUST state that its on-chain rule applies only to Scala code. `[TS-26]`

*Example, illustrative.* Hook result per project:

| Project files | Hook output |
|---|---|
| `build.sbt` with `scalus` | `using-scalus` |
| `package.json` with `"scalus": "^1.5.0"` in `devDependencies` | `using-scalus` |
| `package.json` with only `"scalus-helper": "1.0.0"` | nothing |
| `package.json` without the word scalus | nothing |
| both of the first two | `using-scalus`, once |

**Why `[TS-23]`:** the current hook uses only bash for the same reason. A missing tool must not
break the session. A grep for `"scalus"\s*:` is enough: it matches the exact package key and
rejects `scalus-helper`.

### Documentation

- `scalus-site/content/get-started/ai-assisted-development.mdx` MUST describe the TS bundle and the
  `scalus:typescript` skill. `[TS-27]`
- The "Agent artifacts" table in `CONTRIBUTING.md` MUST list `llms-ts.txt`, its source files and
  when it reaches users. `[TS-28]`
- The "Agent artifacts" section in `CONTRIBUTING.md` MUST say that a change to the npm README or
  the d.ts file reaches the TS bundle at the next release tag. `[TS-29]`
- `version` in `scalus-skills/.claude-plugin/plugin.json` MUST get a minor bump. `[TS-30]`
- The npm README MAY gain a short "For AI agents" note that names `llms-ts.txt` and the plugin.
  `[TS-31]`

### Verification

- A test MUST run `generate-llms-ts.mjs` and check `[TS-1]` to `[TS-4]`. `[TS-32]`
- A test MUST run the hook on the five projects of the hook example table and check each output.
  `[TS-33]`
- `build-version.sh` MUST be run once against a local tag that has the generator, and the output
  MUST contain `llms/<version>/llms-ts.txt`. `[TS-34]`
- The `scalus:typescript` skill MUST be tried once with `claude --plugin-dir ./scalus-skills` in a
  scratch TS project that installs `scalus` from npm. `[TS-35]`

*Implementation note.* `[TS-32]` and `[TS-33]` can be plain shell or `node --test` scripts under
`scalus-site/scripts/` and `scalus-skills/hooks/`. If CI runs neither directory today, add the
tests to the existing `Skills` workflow.

### Release ordering

- The Phase 1 commits MUST land on master before the v1.5.0 tag. `[TS-36]`
- If `[TS-36]` cannot hold, the site part ships with 1.5.1 and the plugin part MAY ship before it.
  The skill then uses the installed d.ts file (`[TS-15]`), which does not depend on the site.
  `[TS-37]`

## Phase 2: Java (deferred)

Phase 2 reuses the Phase 1 pattern. Its scope grows with the interop work: an API enters the Java
bundle only when it follows the cross-language interop guide
(`docs/superpowers/specs/2026-07-11-cross-language-interop-style-guide-design.md`).

Java-ready today: `Emulator` and its chain API (`JavaEmulatorInterop.java`,
`JavaEmulatorChainInterop.java`), and `SubmitResult`. Candidates once checked: `PlutusVM` and
script evaluation. Not ready: `TxBuilder`, `Data`, `Transaction` (interop milestone M2).

Planned requirements. Their wording becomes normative when Phase 2 starts.

- A new site page `scalus-site/content/java/index.mdx` SHALL show each Java-ready API with a Java
  example taken from a compiled Java test. `[JV-1]`
- A new generator SHALL write the Java bundle from that page and the Java test sources. `[JV-2]`
- `build-version.sh`, `finalize-versions.sh` and `llms.txt` SHALL handle `llms-java.txt` as
  `[TS-7]` to `[TS-12]` handle `llms-ts.txt`. `[JV-3]`
- A new skill `scalus:java` SHALL follow `[TS-15]` to `[TS-20]`, with the Javadoc or sources jar
  as the installed API reference. `[JV-4]`
- The hook SHALL detect a Java user project from `pom.xml`, `build.gradle` or `build.gradle.kts`
  that names a Scalus artifact. `[JV-5]`
- Each new Java-ready API SHALL add its example to the Java page in the same commit. `[JV-6]`

**Why examples come from compiled Java tests (`[JV-1]`):** Java sees Scala types through erasure,
companion objects and `$default$` methods. An example that was never compiled is the most likely
place for a wrong call. The TS side has the d.ts file for this; the Java side has no equivalent
generated reference yet.

## Out of scope

- A TS or Java version of `llms-examples.txt`.
- Writing validators in TS or Java. Validators stay in Scala.
- Non-Claude agent formats (Cursor rules, `AGENTS.md` templates). `llms.txt` already serves them.
