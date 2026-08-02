# LLM Developer Experience (Plan 1: scalus repo) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Ship the scalus.org LLM artifacts (`llms-full.txt`, `llms-api.txt`, `llms-examples.txt`, per-page markdown) and consolidate the five Scalus skills into `scalus-skills/`.

**Architecture:** Two Node scripts run in the site's `postbuild` and write generated artifacts into `out/`. One new sbt subproject (`llmApiGen`) uses TastyInspector to extract the public API into a committed `scalus-site/public/llms-api.txt`. Skills move to `scalus-skills/` with symlinks from `.claude/skills/` for dogfooding.

**Tech Stack:** Node ≥ 20 (ESM scripts, no new npm deps), Scala 3.3.8, `scala3-tasty-inspector`, sbt.

**Spec:** `docs/superpowers/specs/2026-08-02-llm-developer-experience-design.md`. Layers 2 (g8 scaffold) and 3 (smoke eval) are separate plans; the g8 templates are sibling repos (`../hello.g8`, `../validator.g8`).

## Global Constraints

- Scala 3.3.8; follow project code style from `CLAUDE.md` (braces for top-level defs, indentation syntax for small `if`/`match`).
- Run `sbtn scalafmtAll` before every commit that touches Scala code.
- Conventional commit messages (`feat:`, `docs:`, `chore:`). Never add a `Co-Authored-By: Claude` trailer.
- Use an en dash (–), never an em dash (—), in any authored prose (skills, docs, llms.txt).
- `git add` every new file.
- Commit on the currently checked-out branch (`feature/uplc-source-view`); do not switch branches.
- Node scripts must exit non-zero on failure (empty input, missing dirs) so the site build fails loudly.
- Canonical site URL: `https://scalus.org`.
- No new npm dependencies in `scalus-site/package.json`.

---

### Task 1: `generate-llms.mjs` – llms-full.txt and per-page markdown

**Files:**
- Create: `scalus-site/scripts/generate-llms.mjs`
- Modify: `scalus-site/package.json` (postbuild)

**Interfaces:**
- Consumes: `scalus-site/content/**/*.mdx`, `content/**/_meta.js` (ESM `export default {ordered keys}`).
- Produces: `out/llms-full.txt`; `out/docs/<page-path>.md` for every docs page (e.g. `out/docs/smart-contracts/validators.md`). Task 4 links these from `llms.txt`.

- [ ] **Step 1: Write the script**

```js
// scalus-site/scripts/generate-llms.mjs
// Generates out/llms-full.txt and per-page markdown copies under out/docs/.
// Runs from scalus-site/ as part of postbuild. Fails loudly on empty input.
import fs from 'fs';
import path from 'path';
import { pathToFileURL } from 'url';

const CONTENT = 'content';
const OUT = 'out';
const SITE = 'https://scalus.org';

async function orderedEntries(dir) {
  const names = fs.readdirSync(dir).filter(n => !n.startsWith('_') && !n.startsWith('.'));
  const metaPath = path.join(dir, '_meta.js');
  let order = [];
  if (fs.existsSync(metaPath)) {
    const meta = (await import(pathToFileURL(metaPath))).default;
    order = Object.keys(meta);
  }
  const rank = n => {
    const base = n.replace(/\.mdx$/, '');
    const i = order.indexOf(base);
    return i === -1 ? order.length : i;
  };
  return names.sort((a, b) => rank(a) - rank(b) || a.localeCompare(b));
}

// Strips frontmatter, top-level import/export lines, and pure-JSX lines.
// Lines inside ``` fences are kept verbatim.
export function mdxToMarkdown(src) {
  let lines = src.split('\n');
  if (lines[0] === '---') {
    const end = lines.indexOf('---', 1);
    if (end !== -1) lines = lines.slice(end + 1);
  }
  const outLines = [];
  let inFence = false;
  for (const line of lines) {
    if (/^\s*(```|~~~)/.test(line)) { inFence = !inFence; outLines.push(line); continue; }
    if (inFence) { outLines.push(line); continue; }
    if (/^import\s/.test(line) || /^export\s/.test(line)) continue;
    if (/^\s*<\/?[A-Z][A-Za-z0-9.]*[^>]*\/?>?\s*$/.test(line)) continue; // pure JSX tag line
    outLines.push(line);
  }
  return outLines.join('\n').replace(/\n{3,}/g, '\n\n').trim() + '\n';
}

async function collectPages(dir, urlPath) {
  const pages = [];
  for (const name of await orderedEntries(dir)) {
    const full = path.join(dir, name);
    if (fs.statSync(full).isDirectory()) {
      pages.push(...await collectPages(full, `${urlPath}/${name}`));
    } else if (name.endsWith('.mdx')) {
      const base = name.replace(/\.mdx$/, '');
      const url = base === 'index' ? urlPath : `${urlPath}/${base}`;
      pages.push({ url, md: mdxToMarkdown(fs.readFileSync(full, 'utf8')) });
    }
  }
  return pages;
}

const pages = await collectPages(CONTENT, '/docs');
if (pages.length < 10) {
  console.error(`generate-llms: only ${pages.length} pages found, aborting`);
  process.exit(1);
}

let full = `# Scalus documentation (full)\n\nGenerated from ${SITE}. One section per page; each section header is the canonical URL.\n`;
for (const { url, md } of pages) {
  full += `\n\n---\nSource: ${SITE}${url}\n---\n\n${md}`;
  const mdPath = path.join(OUT, url.replace(/^\//, '') + '.md');
  fs.mkdirSync(path.dirname(mdPath), { recursive: true });
  fs.writeFileSync(mdPath, `Source: ${SITE}${url}\n\n${md}`);
}
fs.mkdirSync(OUT, { recursive: true });
fs.writeFileSync(path.join(OUT, 'llms-full.txt'), full);
console.log(`generate-llms: ${pages.length} pages → ${OUT}/llms-full.txt`);
```

Note: the root `content/index.mdx` maps to URL `/docs`; that collision with the directory itself is fine because Nextra's docs live under `/docs` and the `.md` copy lands at `out/docs.md` – acceptable.

- [ ] **Step 2: Run it standalone to verify it fails/succeeds correctly**

Run: `cd scalus-site && node scripts/generate-llms.mjs`
Expected: prints `generate-llms: N pages → out/llms-full.txt` with N ≥ 40.
Then verify per-page output: `head -5 out/docs/smart-contracts/validators.md` shows `Source: https://scalus.org/docs/smart-contracts/validators` and no `import` lines or frontmatter. Check a page with JSX (grep `<Callout` in content, inspect its output) – JSX tag lines are gone, inner text kept.

- [ ] **Step 3: Wire into postbuild**

In `scalus-site/package.json`, change the `postbuild` script to:

```json
"postbuild": "next-sitemap && pagefind --site .next/server/app --output-path out/_pagefind && node scripts/generate-redirects.mjs && node scripts/generate-llms.mjs && node scripts/generate-llms-examples.mjs"
```

(The examples script is Task 2; add both now so package.json is touched once. Until Task 2 lands, run only `generate-llms.mjs` when testing.)

- [ ] **Step 4: Commit**

```bash
git add scalus-site/scripts/generate-llms.mjs scalus-site/package.json
git commit -m "feat(site): generate llms-full.txt and per-page markdown for LLM agents"
```

---

### Task 2: `generate-llms-examples.mjs` – curated examples corpus

**Files:**
- Create: `scalus-site/scripts/generate-llms-examples.mjs`

**Interfaces:**
- Consumes: `../scalus-examples/jvm/src/{main,test}/scala/scalus/examples/**` (script runs from `scalus-site/`).
- Produces: `out/llms-examples.txt`. Format per entry: `## <Name>` header, `### <relative path>` per file, Scala code fence.

- [ ] **Step 1: Write the script**

```js
// scalus-site/scripts/generate-llms-examples.mjs
// Concatenates curated example validators + tests into out/llms-examples.txt.
import fs from 'fs';
import path from 'path';

const EX = '../scalus-examples/jvm/src';
const OUT = 'out/llms-examples.txt';

// Curated order: HTLC (gold standard) first, then alphabetical.
const EXAMPLES = [
  'htlc', 'amm', 'auction', 'betting', 'crowdfunding', 'escrow',
  'linkedlist', 'lottery', 'paymentsplitter', 'pricebet', 'simpletransfer',
  'simplewallet', 'storage', 'vault', 'vesting',
];

function scalaFiles(dir) {
  if (!fs.existsSync(dir)) return [];
  return fs.readdirSync(dir, { recursive: true })
    .filter(f => f.endsWith('.scala'))
    .map(f => path.join(dir, f))
    .sort();
}

let out = '# Scalus example contracts\n\nWorking validators with tests, from the Scalus repository.\nStudy these before writing new Scalus code. HTLC is the reference style.\n';
let missing = [];
for (const name of EXAMPLES) {
  const main = scalaFiles(path.join(EX, 'main/scala/scalus/examples', name));
  const test = scalaFiles(path.join(EX, 'test/scala/scalus/examples', name));
  if (main.length === 0) { missing.push(name); continue; }
  if (test.length === 0) console.warn(`llms-examples: ${name} has no tests`);
  out += `\n\n# Example: ${name}\n`;
  for (const f of [...main, ...test]) {
    const rel = path.relative('..', f);
    out += `\n## ${rel}\n\n\`\`\`scala\n${fs.readFileSync(f, 'utf8').trimEnd()}\n\`\`\`\n`;
  }
}
if (missing.length) {
  console.error(`llms-examples: missing example dirs: ${missing.join(', ')}`);
  process.exit(1);
}
fs.mkdirSync('out', { recursive: true });
fs.writeFileSync(OUT, out);
console.log(`llms-examples: ${EXAMPLES.length} examples → ${OUT}`);
```

- [ ] **Step 2: Verify the curated list against reality, then run**

Run: `ls ../scalus-examples/jvm/src/main/scala/scalus/examples/` and adjust `EXAMPLES` so every listed dir exists (drop or add names; keep `htlc` first). Then:

Run: `cd scalus-site && node scripts/generate-llms-examples.mjs`
Expected: `llms-examples: 15 examples → out/llms-examples.txt` (count matches the list). Spot-check: `grep -c '^# Example:' out/llms-examples.txt` equals the list length; the htlc section contains `HtlcValidator.scala` and `HtlcTest.scala`.

- [ ] **Step 3: Commit**

```bash
git add scalus-site/scripts/generate-llms-examples.mjs
git commit -m "feat(site): generate llms-examples.txt corpus from example contracts"
```

---

### Task 3: `llmApiGen` – version-pinned API cheatsheet

**Files:**
- Create: `llm-api-gen/src/main/scala/scalus/llmapi/LlmApiGen.scala`
- Modify: `build.sbt` (new subproject + task)
- Create (generated, committed): `scalus-site/public/llms-api.txt`

**Interfaces:**
- Consumes: `.tasty` files from the class directories of `scalusJVM`, `scalusCardanoLedgerJVM`, `scalusTestkitJVM`.
- Produces: sbt task `generateLlmsApi` that writes `scalus-site/public/llms-api.txt`. Main signature: `LlmApiGen.main(Array(outPath, version, tastyDir1, tastyDir2, ...))`.

- [ ] **Step 1: Locate the exact packages to whitelist**

Run: `grep -rn "trait Validator\b\|trait ParameterizedValidator\|trait DataParameterizedValidator" scalus-core/shared/src/main/scala --include="*.scala" | head` and `grep -rln "object PlutusV3" scalus-core/shared/src/main/scala | head`.
Record the packages (expected: `scalus.prelude` for validator traits, `scalus` or `scalus.compiler` for `PlutusV3`). Use the recorded set in Step 2's `packages` value, starting from: `scalus.prelude`, `scalus.builtin`, `scalus.cardano.txbuilder`, plus the recorded ones, plus the testkit package (`grep -rn "trait ScalusTest" scalus-testkit`).

- [ ] **Step 2: Write the inspector**

```scala
package scalus.llmapi

import scala.quoted.*
import scala.tasty.inspector.*
import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters.*

/** Extracts public API signatures from tasty files into a plain-text cheatsheet
  * for LLM consumption. Not published; run via the `generateLlmsApi` sbt task.
  */
object LlmApiGen {

    // Adjust after Task 3 Step 1: the exact packages that hold the public surface.
    val packages: Set[String] = Set(
      "scalus", "scalus.prelude", "scalus.builtin", "scalus.cardano.txbuilder",
      "scalus.testkit"
    )

    def main(args: Array[String]): Unit = {
        val outPath = args(0)
        val version = args(1)
        val tastyFiles = args.drop(2).toList.flatMap { dir =>
            Files.walk(Paths.get(dir)).iterator().asScala
                .filter(p => p.toString.endsWith(".tasty"))
                .map(_.toString).toList
        }.sorted
        val sb = new StringBuilder
        sb ++= s"# Scalus $version public API cheatsheet\n"
        sb ++= s"# Generated – do not edit. Signatures are Scala 3.\n"
        TastyInspector.inspectTastyFiles(tastyFiles)(new ApiInspector(sb, packages))
        Files.writeString(Paths.get(outPath), sb.toString)
        println(s"llms-api: wrote $outPath (${sb.length} chars)")
    }
}

class ApiInspector(sb: StringBuilder, packages: Set[String]) extends Inspector {

    def inspect(using Quotes)(tastys: List[Tasty[quotes.type]]): Unit = {
        import quotes.reflect.*

        def isPublicApi(sym: Symbol): Boolean =
            !sym.flags.is(Flags.Private) && !sym.flags.is(Flags.Protected)
                && !sym.flags.is(Flags.Synthetic) && !sym.flags.is(Flags.Artifact)
                && !sym.name.contains("$")

        def sig(dd: DefDef): String = {
            val tps = dd.leadingTypeParams match
                case Nil => ""
                case ps  => ps.map(_.name).mkString("[", ", ", "]")
            val vps = dd.termParamss.map { clause =>
                clause.params.collect { case v: ValDef => s"${v.name}: ${v.tpt.show}" }
                    .mkString("(", ", ", ")")
            }.mkString
            s"def ${dd.name}$tps$vps: ${dd.returnTpt.show}"
        }

        def walk(tree: Tree, pkg: String): Unit = tree match {
            case PackageClause(pid, stats) =>
                val p = if pkg.isEmpty then pid.show else s"$pkg.${pid.show}"
                stats.foreach(walk(_, p))
            case cd: ClassDef if packages.contains(pkg) && isPublicApi(cd.symbol) =>
                val kind =
                    if cd.symbol.flags.is(Flags.Module) then "object"
                    else if cd.symbol.flags.is(Flags.Trait) then "trait"
                    else if cd.symbol.flags.is(Flags.Enum) then "enum"
                    else "class"
                sb ++= s"\n$pkg.$kind ${cd.name.stripSuffix("$")}\n"
                for
                    stat <- cd.body
                    dd @ (_: DefDef) <- Seq(stat)
                    if isPublicApi(dd.symbol) && !dd.symbol.isClassConstructor
                do sb ++= s"  ${sig(dd.asInstanceOf[DefDef])}\n"
            case _ => ()
        }

        for tasty <- tastys do walk(tasty.ast, "")
    }
}
```

- [ ] **Step 3: Add the subproject and task to build.sbt**

Find how existing JVM-only projects are declared in `build.sbt` (e.g. `bench` or `scalusUplcJitCompiler`) and follow that pattern:

```scala
lazy val llmApiGen = project
    .in(file("llm-api-gen"))
    .dependsOn(scalusJVM, scalusCardanoLedgerJVM, scalusTestkitJVM)
    .settings(
      name := "llm-api-gen",
      publish / skip := true,
      libraryDependencies += "org.scala-lang" %% "scala3-tasty-inspector" % scalaVersion.value
    )

lazy val generateLlmsApi = taskKey[Unit]("Generate scalus-site/public/llms-api.txt")
generateLlmsApi := {
    val dirs = Seq(
      (scalusJVM / Compile / classDirectory).value,
      (scalusCardanoLedgerJVM / Compile / classDirectory).value,
      (scalusTestkitJVM / Compile / classDirectory).value
    ).map(_.getAbsolutePath)
    val args = Seq("scalus-site/public/llms-api.txt", version.value) ++ dirs
    (llmApiGen / Compile / runMain).toTask(s" scalus.llmapi.LlmApiGen ${args.mkString(" ")}").value
}
```

Adjust project identifiers to the actual names in `build.sbt` (they may be `scalus.jvm` style; check `lazy val scalus` cross-project declarations).

- [ ] **Step 4: Run and iterate until output is sane**

Run: `sbtn generateLlmsApi` (compiles dependencies first; expect several minutes).
Expected: `scalus-site/public/llms-api.txt` exists, header pins the version. Verify content:
- `grep "def filter" scalus-site/public/llms-api.txt` shows `scalus.prelude.List` methods.
- `grep "trait Validator" -A3` shows validator entry points.
- `grep "def spend\|def payTo"` shows TxBuilder methods.
If a surface is missing, adjust `packages` (Step 1 findings) and re-run. If signatures render poorly (long dealiased types), prefer `tpt.show(using Printer.TreeShortCode)`.

- [ ] **Step 5: Guard the release workflow**

In `.github/workflows/release.yml`, before the publish step, add:

```yaml
      - name: Verify llms-api.txt is fresh
        run: |
          sbt generateLlmsApi
          git diff --exit-code scalus-site/public/llms-api.txt \
            || (echo "llms-api.txt is stale – run 'sbt generateLlmsApi' and commit" && exit 1)
```

Match the workflow's existing sbt invocation style (sbt vs sbtn, setup steps).

- [ ] **Step 6: Format, commit**

```bash
sbtn scalafmtAll
git add llm-api-gen build.sbt scalus-site/public/llms-api.txt .github/workflows/release.yml
git commit -m "feat: llmApiGen task generates version-pinned llms-api.txt cheatsheet"
```

---

### Task 4: Update the `llms.txt` index

**Files:**
- Modify: `scalus-site/public/llms.txt`

**Interfaces:**
- Consumes: artifact URLs from Tasks 1–3.
- Produces: the agent-facing entry point.

- [ ] **Step 1: Add an "For LLM agents" section right after the intro blockquote**

```markdown
## For LLM agents

Fetch these before writing Scalus code; do not rely on trained knowledge – Scalus APIs changed at 1.0:

- /llms-api.txt – version-pinned public API signatures (prelude, builtins, validators, TxBuilder, testkit). Fetch first; the API you remember is probably outdated.
- /llms-examples.txt – complete working validators with tests. Imitate these.
- /llms-full.txt – all documentation pages as one markdown file.
- Every docs page is also available as markdown: append `.md` to its URL.
```

- [ ] **Step 2: Fix any stale links while in the file**

Run: for 3–4 random `/docs/...` paths in `llms.txt`, check the corresponding `content/**/*.mdx` exists. Fix mismatches.

- [ ] **Step 3: Commit**

```bash
git add scalus-site/public/llms.txt
git commit -m "docs(site): llms.txt section pointing agents at API/examples/full artifacts"
```

---

### Task 5: Consolidate skills into `scalus-skills/`

**Files:**
- Create: `scalus-skills/contract/SKILL.md` (from `.claude/commands/contract.md`)
- Create: `scalus-skills/contract-test/SKILL.md` (from `.claude/commands/contract-test.md`)
- Move: `.claude/skills/local-development/SKILL.md` → `scalus-skills/local-development/SKILL.md`
- Delete: `.claude/commands/contract.md`, `.claude/commands/contract-test.md`
- Create symlinks: `.claude/skills/{contract,contract-test,local-development,optimize-contract,smart-contract-security-review}` → `../../scalus-skills/<name>`

**Interfaces:**
- Consumes: existing command/skill content; artifact URLs from Tasks 1–4.
- Produces: five self-contained skill dirs usable in ANY project that depends on published Scalus (this is what Plan 2 copies into the g8 templates).

Genericization rules (apply to `contract` and `contract-test`):
1. Add SKILL frontmatter (`name`, `description` – keep the existing descriptions).
2. Replace repo-relative doc paths (`scalus-site/content/...`) with a dual reference: "In the Scalus repo read `scalus-site/content/<fixed path>`; otherwise fetch `https://scalus.org/docs/<path>.md`." Fix the stale paths while doing so – the current commands point at `content/smart-contract/…` but the real dir is `content/smart-contracts/`; verify each referenced page exists and pick the nearest current page when it does not (e.g. `from-data.mdx` → `plutus-data.mdx`; `optimisations/*` → `smart-contract-optimisations/*`; `language-guide/builtin-functions.mdx` – verify, else drop).
3. Replace repo-relative example paths with: "fetch `https://scalus.org/llms-examples.txt`" plus the repo path as the in-repo alternative.
4. Add one line near the top: "Before writing Scalus code from memory, fetch `https://scalus.org/llms-api.txt` and check the signatures you plan to use."
5. Keep all key-pattern content (annotations, validation helpers, script purposes, compile snippet, testing patterns) as-is – it is correct and project-independent.

- [ ] **Step 1: Create `scalus-skills/contract/SKILL.md`**

Start from `.claude/commands/contract.md`, apply the genericization rules above. Verify every referenced docs page: `ls scalus-site/content/smart-contracts/ scalus-site/content/language-guide/`.

- [ ] **Step 2: Create `scalus-skills/contract-test/SKILL.md`**

Same treatment for `.claude/commands/contract-test.md`. Verify the testing docs paths (`content/testing/`, `content/ledger/emulator.mdx`) exist and fix.
Also verify the code snippets against the current testkit API: `grep -rn "def assertEval\|trait ScalusTest" scalus-testkit/shared/src/main/scala | head`. If `assertEval`/`assertEvalEq` do not exist under those names, rewrite the snippets to the real API (check `scalus-examples` tests for current usage, e.g. `HtlcTest.scala`).

- [ ] **Step 3: Move local-development, delete superseded commands, create symlinks**

```bash
git mv .claude/skills/local-development scalus-skills/local-development
git rm .claude/commands/contract.md .claude/commands/contract-test.md
cd .claude/skills
ln -s ../../scalus-skills/contract contract
ln -s ../../scalus-skills/contract-test contract-test
ln -s ../../scalus-skills/local-development local-development
ln -s ../../scalus-skills/optimize-contract optimize-contract
ln -s ../../scalus-skills/smart-contract-security-review smart-contract-security-review
cd ../.. && git add .claude/skills
```

- [ ] **Step 4: Verify skills resolve**

Run: `ls -L .claude/skills/contract/SKILL.md .claude/skills/local-development/SKILL.md` – both readable through the symlinks. Run `head -5` on each of the five `scalus-skills/*/SKILL.md` – all have frontmatter with `name` and `description`.

- [ ] **Step 5: Commit**

```bash
git add scalus-skills
git commit -m "feat(skills): consolidate five Scalus skills into scalus-skills/ with dogfood symlinks"
```

---

### Task 6: Full site build verification

**Files:** none new.

- [ ] **Step 1: Full build**

Run: `cd scalus-site && pnpm install && pnpm build`
Expected: build green; postbuild prints the three generator lines (redirects, llms, llms-examples).

- [ ] **Step 2: Verify artifacts in `out/`**

```bash
ls -la out/llms-full.txt out/llms-examples.txt out/llms.txt out/llms-api.txt
head -3 out/docs/smart-contracts/validators.md
```

`llms.txt` and `llms-api.txt` come from `public/` (Next copies `public/` into `out/`). All four exist and are non-trivial (`wc -c` > 10 kB each except llms.txt).

- [ ] **Step 3: Commit any fixes, then run repo checks**

Run: `sbtn quick` (repo-wide format + compile + tests). Fix and commit anything it flags.

---

## Follow-up plans (not in this plan)

- **Plan 2 – g8 scaffold:** `AGENTS.md` + `CLAUDE.md` pointer + `.claude/skills/` (copied from `scalus-skills/`) in `../hello.g8` and `../validator.g8` (sibling repos, root-level g8 layout).
- **Plan 3 – smoke eval:** release-CI script scaffolding a fresh project, running `claude -p` on canonical tasks, asserting compile + green tests.
- **Plugin (later):** thin Claude Code plugin packaging `scalus-skills/`.
