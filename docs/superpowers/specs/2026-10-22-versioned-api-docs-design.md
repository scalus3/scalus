# Versioned API docs and llms files on scalus.org

Status: approved design, 2026-10-22. Implement directly from this spec; there is no separate plan.

## Goal

scalus.org serves the Scaladoc API reference and the llms files for each released Scalus version
(for example 1.3.0 and 1.4.0), not only for the latest. A version dropdown in the Scaladoc header
switches between versions, as on `scala-lang.org/api/3.x/`. Existing URLs keep working and show the
latest version.

## Definitions

- **version**: a release number without the `v` prefix, for example `1.3.0`.
- **tag**: the git tag of a version, for example `v1.3.0`.
- **latest version**: the highest version, by `sort -V`, among the versions present in the site.
- **version folder**: `api/<version>/` or `llms/<version>/` in the site output.
- **root API**: the files directly under `/api/` that are not in a version folder.
- **llms files**: `llms.txt`, `llms-api.txt`, `llms-examples.txt` and `llms-full.txt`.
- **build script**: `scalus-site/scripts/build-version.sh`, described below.
- **deploy workflow**: `.github/workflows/deploy-site.yml`.

## Evidence (measured 2026-10-22)

| Fact | Value |
|---|---|
| Published site (gh-pages tree) | 950 MB, of which `api/` is 861 MB |
| GitHub Pages limit for a published site | 1 GB |
| Share of `scalus/uplc/Term.html` that is the embedded side menu | 370 KB of 391 KB |
| Scaladoc 3.3.8 unidoc of 1.4.0, default | 827 MB, 2,491 files |
| Same, with `-dynamic-side-menu` | 66 MB, 2,492 files (adds `dynamicSideMenu.json`) |
| gh-pages branch history | 80 commits, pack 314 MB |
| `scalusStableVersion` in tag v1.4.0 | `"1.3.0"` (the MiMa baseline, not the release) |

With `-dynamic-side-menu`, the side menu rendered and expanded to the current page in a browser
check. The version dropdown is client-side: `scaladoc-scalajs.js` fetches `versionsDictionaryUrl`,
parses `{"versions": {label: url}}`, and caches it in `sessionStorage`. A dropdown item links to
the root URL of that version, not to the same page.

**Why this design is possible:** without `-dynamic-side-menu`, one version already uses 86% of the
1 GB limit. With it, 1.3.0 and 1.4.0 plus the root API copy plus the rest of the site come to
about 290 MB. With 1.4.0 alone (see `[VF-1]`), it is about 220 MB.

## URL layout

- The site MUST serve the Scaladoc of each built version at `/api/<version>/`. `[VD-1]`
- The site MUST serve a copy of the latest version's Scaladoc as the root API. `[VD-2]`
- The site MUST serve `/api/versions.json` with one entry per version folder under `api/`. `[VD-3]`
- Each entry in `versions.json` MUST map the version to `https://scalus.org/api/<version>/`. `[VD-4]`
- The site MUST serve the llms files of each built version at `/llms/<version>/`. `[VD-5]`
- The site MUST serve the latest version's llms files at the root, as today. `[VD-6]`

**Why:** `[VD-2]` and `[VD-6]` keep every existing link working: `_meta.js`, docs pages, the
scalus skills, the g8 templates and external sites all link to `/api/...` and `/llms-api.txt`.
GitHub Pages has no server redirects, and a meta-refresh page per file cannot cover 2,500 pages.

*Example, illustrative.* With versions 1.3.0 and 1.4.0 built:

```
/api/index.html                    1.4.0 (root API copy)
/api/scalus/uplc/Term.html         1.4.0
/api/1.4.0/scalus/uplc/Term.html   1.4.0
/api/1.3.0/scalus/uplc/Term.html   1.3.0
/api/versions.json                 {"versions": {"1.4.0": "https://scalus.org/api/1.4.0/",
                                                 "1.3.0": "https://scalus.org/api/1.3.0/"}}
/llms-api.txt                      1.4.0
/llms/1.3.0/llms-api.txt           1.3.0
```

- `versions.json` MUST list versions in descending `sort -V` order. `[VD-7]`

**Why:** Scaladoc shows the entries in JSON key order, so the newest version appears first.

## Build script: `build-version.sh <tag>`

Purpose: build the Scaladoc and the llms files of one tag into a staging folder.
Who: the deploy workflow; a developer for a local check.
Trigger: one call per tag.

| Input | Type |
|---|---|
| `<tag>` | a tag that exists in the repository |
| `OUT` env var | staging folder; default `scalus-site/public` |

| Output | Content |
|---|---|
| `$OUT/api/<version>/` | Scaladoc of the tag |
| `$OUT/llms/<version>/` | llms files of the tag, as available |

Checks enforced here:

- The build script MUST build from a git worktree of the tag, not from the current checkout. `[VB-1]`
- The build script MUST NOT modify any file inside that worktree that git tracks. `[VB-2]`
- The build script MUST pass these Scaladoc options to `docs / ScalaUnidoc / unidoc` with an sbt `set` command: `[VB-3]`
  - `-dynamic-side-menu`
  - `-versions-dictionary-url https://scalus.org/api/versions.json`
  - `-project Scalus`
  - `-project-version <version>`
- The build script MUST set the unidoc target to a folder outside the worktree's `scalus-site/public/api`. `[VB-4]`
- The build script MUST run `generateLlmsApi` when the tag's `build.sbt` defines it. `[VB-5]`
- The build script MUST run the tag's own `scalus-site/scripts/generate-llms.mjs` and `generate-llms-examples.mjs` when the tag has them. `[VB-6]`
- The build script MUST keep only `llms-full.txt` and `llms-examples.txt` from the output of `[VB-6]`. `[VB-7]`
- The build script MUST copy the tag's `scalus-site/public/llms.txt` to `llms/<version>/llms.txt` when it exists. `[VB-8]`
- In that copy, the build script MUST rewrite each root llms link to its `/llms/<version>/` form. `[VB-9]`
- In that copy, the build script MUST rewrite each `https://scalus.org/api` link to `https://scalus.org/api/<version>/`. `[VB-10]`
- The build script MUST add a first line to each versioned llms file that names the version and the latest URL. `[VB-11]`
- When an input of `[VB-5]`, `[VB-6]` or `[VB-8]` is absent, the build script MUST print a warning and continue. `[VB-12]`
- When unidoc fails, the build script MUST exit with a non-zero status. `[VB-13]`
- The build script MUST remove the worktree on exit, also on failure. `[VB-14]`
- The build script MUST make the side-menu links in `scripts/ux.js` absolute paths to the docs root. `[VB-15]`
- When `[VB-15]` cannot be applied, the build script MUST exit with a non-zero status. `[VB-16]`

**Why `[VB-15]`:** Scaladoc's `renderDynamicSideMenu` (unfixed up to 3.10.0) puts an unjoined
array into the link: `${baseUrl}/${loc.join("/")}.html`. Under `/api/`, `baseUrl` is
`["", "api"]`, so every side-menu link becomes `,api/scalus/builtin.html` and a click loads
nothing. Docs at the domain root escape the bug, because `[""]` prints as an empty string. The
first deploy (2026-10-22) shipped this bug; the fix joins the array.

*Example, illustrative.* For the side-menu entry of `scalus/builtin.html`:

| Page | Link before `[VB-15]` | Link after |
|---|---|---|
| `/api/index.html` | `,api/scalus/builtin.html` | `/api/scalus/builtin.html` |
| `/api/1.4.0/scalus/uplc/Term.html` | `,api,1.4.0/scalus/builtin.html` | `/api/1.4.0/scalus/builtin.html` |

**Why `[VB-3]` passes `-project-version`:** the tag's own `scalusStableVersion` is the MiMa
baseline, so v1.4.0 would label itself 1.3.0. The unidoc project also does not inherit the root
`Compile / doc / scalacOptions` today. Without `-project`, the header and the page title show the
sbt project name "docs", as the live site did before this change.

**Why `[VB-6]` uses the tag's scripts:** the scripts read paths relative to `scalus-site/` and
import only `fs`, `path` and `url`, so they run without `pnpm install`. The tag's example list
matches the tag's examples. Master's list can name an example the tag does not have, and the
script then exits with an error.

**Why `[VB-7]`:** `generate-llms.mjs` also writes one `.md` file per docs page under `out/docs/`.
The prose docs are not versioned, so those copies would be dead weight.

*Example, illustrative.* `[VB-9]`, `[VB-10]` and `[VB-11]` for version 1.3.0:

| Line in the tag's `llms.txt` | Line in `/llms/1.3.0/llms.txt` |
|---|---|
| (none) | `# Scalus 1.3.0. Latest version: https://scalus.org/llms.txt` |
| `- /llms-api.txt - version-pinned ...` | `- /llms/1.3.0/llms-api.txt - version-pinned ...` |
| `- https://scalus.org/api - Scaladoc ...` | `- https://scalus.org/api/1.3.0/ - Scaladoc ...` |

Links to `/docs/...` pages stay unchanged. They point to the latest prose docs.

## Deploy workflow

Purpose: publish the site with all version folders kept and the requested versions rebuilt.
Who: a maintainer, by manual `workflow_dispatch`, as today.

| Input | Type | Default |
|---|---|---|
| `versions` | space-separated tags | empty, which means the newest `v*` tag by `git describe --tags --abbrev=0 --match 'v*'` |

The `api_mode` input (`preserve` / `regenerate`) is removed.

Checks enforced here:

- The deploy workflow MUST restore every version folder from the current gh-pages tree. `[VW-1]`
- The deploy workflow MUST NOT restore the root API from gh-pages. `[VW-2]`
- The deploy workflow MUST run the build script for each tag in `versions`. `[VW-3]`
- A rebuilt version MUST replace its restored version folder completely. `[VW-4]`
- The deploy workflow MUST copy `api/<latest version>/` to the root API. `[VW-5]`
- The deploy workflow MUST copy `llms-api.txt`, `llms-examples.txt` and `llms-full.txt` of `llms/<latest version>/` to the site root, without the `[VB-11]` header line. `[VW-6]`
- The root `llms.txt` MUST be the file committed on the deployed branch, not a versioned copy. `[VW-13]`
- The deploy workflow MUST write `api/versions.json` from the list of version folders under `api/`. `[VW-7]`
- The deploy workflow MUST append a "Versions" section to the root `llms.txt` that lists each `/llms/<version>/llms.txt`. `[VW-8]`
- The deploy workflow MUST fail before publishing when the site output is larger than 900 MB. `[VW-9]`
- The deploy workflow MUST publish with `gh-pages --no-history`. `[VW-10]`
- The deploy workflow MUST NOT generate `llms-api.txt` outside the build script. `[VW-11]`
- After `pnpm build`, the deploy workflow MUST overwrite `out/llms-full.txt` and `out/llms-examples.txt` with the latest version's files. `[VW-12]`

**Why `[VW-2]`:** the first deploy after this change finds the old full-size, non-versioned root
API on gh-pages. Restoring it would publish 861 MB of stale pages.

**Why `[VW-6]` replaces the current step:** today's step builds `llms-api.txt` from the newest tag
while `postbuild` builds `llms-examples.txt` from master, so the examples can use API that is not
released. After this change all root llms files come from the latest version.
**Why `[VW-13]`:** a versioned `llms.txt` has its links rewritten to `/llms/<version>/` by `[VB-9]`,
and it lists the docs pages of the tag. The root `llms.txt` indexes the root files and the prose
docs the site serves, which come from the deployed branch.

**Why `[VW-12]`:** `postbuild` still writes `out/llms-full.txt` and `out/llms-examples.txt` from
master. The per-page `.md` copies under `out/docs/` stay from master, because the prose docs are
master-only.

**Why `[VW-9]`:** GitHub Pages rejects sites above 1 GB. The guard fails the job before a partial
or rejected publish. At 66 MB per version, the limit leaves room for about 9 more versions; a
retention rule is a later decision.

**Why `[VW-10]`:** every deploy adds hundreds of MB to the gh-pages history, and a plain
`git clone` of the repository fetches that branch. With `--no-history`, the branch holds one commit.

*Example, illustrative.* `[VW-8]`, with versions 1.3.0 and 1.4.0:

```
## Versions

The files above describe the latest release, 1.4.0.
If your project depends on an older Scalus, fetch the index of that version:

- /llms/1.4.0/llms.txt
- /llms/1.3.0/llms.txt
```

## Search engines

- `next-sitemap.config.js` MUST add `Disallow: /api/1.` to `robots.txt`. `[VS-1]`
- `next-sitemap.config.js` MUST add `Disallow: /llms/` to `robots.txt`. `[VS-2]`

**Why:** the version folders duplicate the root API. Search results then point to the latest
version only. A 2.x release needs one more `Disallow` line.

## First deploy

- The first deploy MUST build only `v1.4.0`. `[VF-1]`

An empty `versions` input does this, because v1.4.0 is the newest tag. The site then holds one
version folder, `/api/1.4.0/`, and the dropdown lists only 1.4.0.

**Why:** the owner chose to start with 1.4.0 only (changed from `v1.3.0 v1.4.0` on 2026-10-22).
The same workflow adds older versions later, for example `versions: v1.3.0`. Tag v1.0.0 has no
`generateLlmsApi`; `[VB-12]` covers it.

## Testing

1. Run `OUT=/tmp/site build-version.sh v1.4.0` and `build-version.sh v1.3.0` locally.
   Do not use a running sbtn server: a warm server on JDK 25 crashed Scaladoc 3/3 on 2026-10-22.
2. Write `versions.json` as `[VW-7]` does, serve `/tmp/site` over HTTP and check in a browser:
   - the side menu renders on a deep page, for example `/api/1.3.0/scalus/uplc/Term.html`;
   - a click on a side-menu link loads that page, both under `/api/` and under `/api/<version>/`;
   - search finds `TxBuilder`;
   - the dropdown lists 1.4.0 and 1.3.0 and switches between them;
   - the header shows the right version on each.
3. Check the llms output: header lines `[VB-11]`, rewritten links `[VB-9]` and `[VB-10]`.
4. After the first deploy, `curl -sI` each URL of the `[VD-*]` example and expect 200.

## Out of scope

- Versioned prose docs under `/docs/`.
- An automatic deploy from `release.yml`.
- A retention rule for old versions.

## Follow-up: how agents find the versioned llms files

Separate change, after the first deploy:

- `scalus-skills/skills/contract/SKILL.md` and `contract-test/SKILL.md`: read the Scalus version
  from `build.sbt` or `project.scala`, then fetch `https://scalus.org/llms/<version>/llms-api.txt`.
  Fall back to `/llms-api.txt` when no version is found. This needs a skills version bump, per
  "Agent artifacts" in `CONTRIBUTING.md`.
- `hello.g8/AGENTS.md` and `validator.g8/AGENTS.md`: use the template's Scalus version variable in
  the llms URLs, so a generated project keeps URLs that match its pinned version.
