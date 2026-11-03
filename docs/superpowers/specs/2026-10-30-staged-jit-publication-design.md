# Staged JIT publications: design

Status: draft for review, 2026-10-30.

## Goal

Turn the ScalaDays 2026 talk into written work that reaches Scala developers first. Post 1 is a
long article on lantr.io that follows the talk. Post 2, a short article for Cardano developers,
comes later. A Scala Workshop proposal and a paper follow after that.

Decisions the author made on 2026-10-30:

- Reach comes before the paper.
- The series is one long post and one short post.
- Post 1 shows the talk numbers and the public numbers side by side.
- Post 1 embeds the talk video.
- The author says the photos in the slides are theirs and has the originals.

## Definitions

| Term | Meaning |
|---|---|
| Post 1 | The long article "Blazing-Fast Lambda Calculus via Metaprogramming" on lantr.io |
| Post 2 | The short article "A budget-exact JIT for Cardano" on lantr.io |
| staged JIT | The Scalus UPLC JIT (`scalus-uplc-jit-compiler`). Never "Hybrid JIT" or "Meta-JIT" |
| Scalus JIT | The name of the staged JIT in the benchmark tables and in running text, as on the public benchmark page |
| toy engines | The three engines of `tiny-uplc-jit`: CEK machine, closure compiler, staged compiler |
| talk numbers | The benchmark numbers in the slides: run of 2026-10-11, Apple M3 Max |
| public numbers | The numbers of the public benchmark run of 2026-10-27, AMD EPYC |
| public benchmark | lantr-io/plutus-vm-benchmark, published at https://plutus-vm-bench.lantr.io |
| placement rule | "Create each Expr where it will live": a generator runs inside the splice of its result |
| talk video | https://www.youtube.com/watch?v=xwRdY8Ht-o0 (channel: Scala Days Conferences) |
| talk tag | The tag `scala-days-2026` of `tiny-uplc-jit`, on commit `200c31a` |
| site source | Branch `nextjs` of `lantr-io/lantr-io.github.io`: the Next.js source of lantr.io |

## Inputs

| Input | Location |
|---|---|
| Slides, claude.ai artifact: the master copy, 26 slides in 4 acts | https://claude.ai/artifact/C4Fev7JDQgB1CW15D7dmRB |
| Slides, PPTX export of the artifact, 2026-10-30, with the final speaker notes | `~/Downloads/Blazing-Fast Lambda Calculus via Metaprogramming (1).pptx` |
| Final speaker notes, extracted | `~/projects/lantr/tiny-uplc-jit/talk-assets/speaker-notes-final.md` (local only) |
| Slides, PDF, 2026-10-13: the same slide text as the artifact | `~/Downloads/Blazing-Fast Lambda Calculus via Metaprogramming.pdf` |
| Slides, PPTX, 2026-10-12: an older version, do not use | `~/Downloads/Blazing-Fast Lambda Calculus via Metaprogramming.pptx` |
| Talk code | https://github.com/lantr-io/tiny-uplc-jit/tree/scala-days-2026 (the talk tag) |
| Compile-time data, before and after the placement rule | `~/projects/lantr/tiny-uplc-jit/talk-assets/jit-compile-times{,-after-fix}.{csv,md}` (local only: `talk-assets/` is gitignored) |
| Talk benchmark run | `~/projects/lantr/tiny-uplc-jit/talk-assets/vm-benchmark-2026-10-11/` (local only) |
| Public benchmark run | `cardano-plutus-vm-benchmark/results/2026-10-27/` (commit `5f64541`) |
| JIT design | `scalus-uplc-jit-compiler/docs/DESIGN.md` and `docs/superpowers/specs/2026-10-19-uplc-jit-simplification-design.md` |
| Staging leak fix | https://github.com/scala/scala3/pull/27335, merged 2026-10-19 |
| Site source | https://github.com/lantr-io/lantr-io.github.io/tree/nextjs (no local checkout yet) |

### How lantr.io works

- DNS points `lantr.io` to GitHub Pages. GitHub Pages serves the branch `gh-pages` of
  `lantr-io/lantr-io.github.io`.
- The branch `nextjs` holds the source: Next.js 16, `next-mdx-remote`, Tailwind, KaTeX.
- Posts are MDX files in `app/blog/posts/<slug>.mdx`. The URL is `https://lantr.io/blog/<slug>`.
- The front matter has `title`, `author`, `tags`, `publishedAt` and `summary`.
- The MDX components include `YouTubeEmbed`, `ZoomableImage`, `Video`, `Tweet` and GFM tables.
- `sugar-high` highlights fenced code blocks.
- `pnpm run deploy` builds the site and pushes the static export to `gh-pages`. Nothing deploys
  automatically.
- The branch `master` and the local checkout `~/projects/lantr/lantr-io.github.io` hold the old
  Jekyll site (al-folio). lantr.io does not serve them.

## Post 1

**Purpose:** explain how to specialize an interpreter per program with Scala 3 staging, from a
CEK machine to a staged JIT.

**Audience:** Scala developers. The post needs no Cardano knowledge.

### Sections

Table 1. Sections of Post 1, their source slides, and the acts of the talk.

| # | Section | Slides | Content |
|---|---|---|---|
| 0 | Talk video and summary | – | The video, then the result in three sentences |
| | *Act 1: the quest: a bet, the problem, and the interpreter we start from* | | |
| 1 | The problem | 3–4 | A contract is a pure, metered, deterministic function. Why lambda calculus |
| 2 | Mini-UPLC | 5–6 | `fib` in Scala, its Scalus UPLC output, the `Term` enum |
| 3 | Tier 0: the CEK machine | 7–8 | The machine and its loop |
| 4 | The bet | 9–10 | Haskell or the JVM: the answer at the talk, and the answer now |
| | *Act 2: the climb: closures, then Scala 3 staging, then the live demo* | | |
| 5 | Tier 1: closure compiler | 11–12 | A tree of closures, 16x on `fib 20` |
| 6 | Tier 2: Scala 3 staging | 13–15 | Quotes, splices, `power`, the first Futamura projection |
| 7 | Generated code and the JVM | 16–18 | The code for `fib`, the four stages, the demo results |
| 8 | Exactness | 19 | A different answer is a consensus bug |
| | *Act 3: the twist: minutes of compile time, the investigation, the fix* | | |
| 9 | The twist: compile time | 20–22 | Minutes to compile, the investigation, the placement rule |
| | *Act 4: lessons: real-world numbers, where a JIT pays, what to take home* | | |
| 10 | Real world | 23 | The talk numbers and the public numbers |
| 11 | Where a JIT pays | 24 | Mainnet usage, a cache keyed by script hash, Phil Wadler's correction |
| 12 | Take home | 25–26 | The tier table, the links |

### Rules

- Post 1 MUST contain the sections of Table 1. `[P1-1]`
- Post 1 SHOULD keep the order of Table 1. `[P1-2]`
- Section 0 MUST embed the talk video. `[P1-3]`
- Post 1 MUST take every code snippet from `tiny-uplc-jit` at the talk tag, or from Scalus at a
  release tag. `[P1-4]`
  *Implementation status, decision of 2026-10-30:* a snippet that Post 1 marks as illustrative is
  exempt from `[P1-4]`. Two snippets use this: the validator signature and the `power` example.
- Every code snippet MUST compile as shown, or Post 1 MUST mark it as trimmed. `[P1-5]`
- Each snippet from `tiny-uplc-jit` SHOULD link to its file at the talk tag. `[P1-6]`
- Section 4 MUST give the answer at the talk: Haskell won, 220 µs against 256 µs, flat-decode plus
  evaluation. `[P1-7]`
- Section 4 MUST give the answer now in both modes of the public benchmark: the Scalus CEK wins,
  268 µs against 711 µs with decode, and 171 µs against 276 µs for evaluation only. `[P1-8]`
- Section 4 MUST name both changes between the two answers: the CEK rewrite in Scalus 1.4.1
  (`a3a1f1521`) and the new benchmark harness. `[P1-9]`
- Post 1 MUST NOT credit the flip to the CEK rewrite alone, unless an A/B run on one machine shows
  it. `[P1-16]`
- Section 8 MUST state the depth limit of the staged JIT and its fallback to the CEK machine. `[P1-10]`
- Section 8 MAY give the bound on stack depth from the notes of slide 19. `[P1-17]`
- If Section 8 gives that bound, it MUST say that the frame sizes were measured on a model of the
  generated code. `[P1-18]`
- Section 11 MUST give Phil Wadler's correction from the notes of slide 24. `[P1-19]`
- Post 1 MAY reuse text from the final speaker notes. `[P1-20]`
- Post 1 MUST NOT contain the stage directions of the notes, such as `[SHOW OF HANDS]`. `[P1-21]`
- Section 9 MUST keep the joke "Or, as we used to call it back in the Scala 2 days: fast." `[P1-22]`
- Section 9 MUST keep the story "Claude, --dangerously-skip-permissions, figure out what's going on"
  and its answer "scalac wasn't slow. You were." `[P1-23]`
- Section 9 MUST state the placement rule in one sentence, with the code before and after. `[P1-11]`
- Section 9 MUST give the compile numbers before and after the placement rule (Table 2). `[P1-12]`
- Section 9 MUST link the staging leak fix, scala/scala3#27335. `[P1-13]`
- Section 12 MUST link `tiny-uplc-jit` at the talk tag, Scalus, the public benchmark, the slides
  and the talk video. `[P1-14]`

**Why:** `[P1-7]` to `[P1-9]` keep the video and the post consistent. A viewer hears "Haskell
wins" in the talk, so the post explains why the answer changed. `[P1-9]` names both changes
because the talk numbers are flat-decode plus evaluation on the old harness, and the public
numbers come from the new harness, so `[P1-16]` waits for open question 4. `[P1-11]` is the one
finding that nobody has written up, so it gets a sentence of its own. `[P1-18]`: the note itself
says "verify before relying on it", because big scripts have bigger frames. `[P1-19]`: on its own
the engine is a staged compiler; only behind an interpreter that compiles hot scripts is the
system a JIT. The correction explains why the post calls the toy engine a staged compiler.
`[P1-20]`: the notes are raw material, not a script. A post reads differently from a talk: it has
more code, links and both tables of numbers, and no show of hands.

Table 2. Compile numbers for Section 9, from `jit-compile-times-after-fix.md`.

| | Before | After |
|---|---:|---:|
| Median compile | 21.89 s | 2.32 s |
| Max compile | about 197 s | 7.65 s |
| Our generator, share of compile | 93 % | 6 % |
| Break-even against the CEK, median | 175,078 runs | 15,768 runs |

- Post 1 MUST say under Table 2 that the "Before" column covers only the 80 scripts that compiled
  within 120 s. `[P1-15]`

**Why:** the 9 slowest scripts are missing from the "Before" column, so its medians are biased low.

## Numbers

*Decision of 2026-10-30, by the author:* Post 1 trades precision for length. It keeps every
explanation, rounds numbers and leaves out run metadata. These rules are withdrawn for Post 1:
`[NUM-1]`, `[NUM-2]`, `[NUM-5]`, `[NUM-6]`, `[NUM-9]`, `[NUM-10]`, `[P1-8]`, `[P1-15]`,
`[P1-17]`, `[P1-18]`. Every number in Post 1 MUST still be true for the run it comes from.
`[NUM-11]` Post 1 MUST keep the talk run and the public run in separate columns, and say to
compare within a column. `[NUM-12]`

- Section 10 MUST show the talk numbers and the public numbers in two separate tables. `[NUM-1]`
- Each table MUST state the date, machine, JVM, Scalus version, harness and timing mode. `[NUM-2]`
- The public numbers MUST match the page revision of the public benchmark that Post 1 cites. `[NUM-3]`
- Post 1 MUST say that the staged JIT rows exclude compile time. `[NUM-4]`
- Section 10 MUST explain in one paragraph why the two tables differ. `[NUM-5]`
- The paragraph of `[NUM-5]` MUST name these causes: machine, harness, the CEK rewrite, the JIT
  rewrite, and the warm-up of the JIT on the `stablecoin_*` scripts. `[NUM-6]`
  *Implementation status, decision of 2026-10-30:* the JVM is withdrawn from this list and the JIT
  rewrite is added. Both runs used GraalVM 25 (the talk run's report says "GraalVM JDK 25"), and
  Scalus 1.4.1 has the CEK-exact JIT that the talk run did not have.
- Post 1 MUST NOT quote the evaluation time of a single script from one run. `[NUM-7]`
- Every aggregate speed-up ratio MUST be a geometric mean over the same set of scripts. `[NUM-8]`
- A distribution of per-script ratios, such as the median contract or the best contract, MUST say
  that it is per contract. `[NUM-10]`
- Toy-engine numbers MUST say they come from `fib 20` on the stage laptop. `[NUM-9]`

**Why:** `[NUM-7]`: single scripts moved 2–4x between runs with the same code
(`jit-compile-times-after-fix.md`). `[NUM-3]`: a reader who opens the benchmark page must see the
numbers of the post. `[NUM-4]`: the JIT compiles in the setup of each benchmark, so its rows
measure evaluation only.

*Example, illustrative.* The two tables of Section 10:

| Talk numbers: 2026-10-11, Apple M3 Max, SAIB harness | Flat-decode + eval, geo mean, 89 scripts |
|---|---:|
| Scalus staged JIT | 71 µs |
| plutus-core (Haskell) | 220 µs |
| Scalus CEK | 256 µs |
| uplc-turbo (Rust) | 363 µs |

| Public numbers: 2026-10-27, AMD EPYC, Scalus 1.4.1 | Decode + eval | Eval only |
|---|---:|---:|
| Scalus staged JIT | – | 94 µs |
| Scalus CEK | 268 µs | 171 µs |
| plutigo (Go) | 306 µs | 205 µs |
| uplc-turbo (Rust) | 467 µs | 365 µs |
| plutus-core (Haskell) | 711 µs | 276 µs |

The staged JIT row times evaluation only. On evaluation only, the staged JIT is 2.9x as fast as
Haskell and 1.8x as fast as the Scalus CEK. Seven scripts are slower on the JIT than on the CEK.

*Implementation status:* the public numbers above come from the CSV files of the run. Check them
against the page before Post 1 goes out (`[NUM-3]`).

## Assets

- The author MUST supply the original photos: the PlutusFest 2018 napkin, and the photo with Phil
  Wadler at IOHK Summit 2019. `[AST-1]`
- The caption of each photo MUST state the event and the credit that the author gives. `[AST-2]`
- Post 1 MUST include a chart of compile time against term size, before and after the placement
  rule. `[AST-3]`
- The chart of `[AST-3]` MUST use the data of `jit-compile-times.csv` and
  `jit-compile-times-after-fix.csv`. `[AST-4]`
- Post 1 MAY show the tables of Section 10 as bar charts. `[AST-5]`
- Post 1 SHOULD publish the CSV file behind each chart in `public/blazing-fast-lambda-calculus/`
  and link it under the chart. `[AST-8]`
- Images MUST go in `public/blazing-fast-lambda-calculus/` of the site source. `[AST-6]`
- The slides PDF MUST go in `public/blazing-fast-lambda-calculus/` of the site source. `[AST-7]`

**Why:** `[AST-2]`: the napkin slide says "Photo: IOHK", and the author says the photos are
theirs. The author decides the credit line (open question 3). `[AST-8]`: the compile-time data is
in no public repo, so a reader cannot check the numbers of Section 9 without it.

## Style

- Post 1 and Post 2 MUST NOT use em dashes. Where a dash is needed, use an en dash. `[STY-1]`
- Post 1 and Post 2 MUST NOT carry any AI attribution. `[STY-2]`
- Post 1 and Post 2 MUST use the defined terms of this spec. `[STY-3]`
- Post 1 and Post 2 MUST use "I" for the author's own story and "we" for the work of Lantr. `[STY-4]`
- Post 1 MUST be the MDX file `app/blog/posts/<slug>.mdx` on the branch `nextjs`. `[STY-5]`
- The slug of Post 1 SHOULD be `blazing-fast-lambda-calculus-scala-3-staging`. `[STY-6]`
- Section 0 MUST embed the talk video with the `YouTubeEmbed` component. `[STY-7]`
- Post 1 MUST give its references as links in the text. `[STY-8]`

**Why:** `[STY-4]`: the latest post (case/constr, 2026-09-06) uses "I" for the author's own
finding. `[STY-6]`: the slugs of the site name the topic for search, for example
`plutus-v3-case-constr-function-application`. `[STY-8]`: the site has no citation component.

## Publishing

- Post 1 SHOULD go out now, while the talk is recent. `[PUB-1]`
- The author MUST run `pnpm build` of the site source and check the video, the code blocks and the
  images before `pnpm run deploy`. `[PUB-2]`
- The author runs `pnpm run deploy` and promotes Post 1. An agent MUST NOT post to any external
  channel. `[PUB-3]`
  *Implementation status, 2026-10-30:* the site source moved to the default branch `main`. The
  author deploys by running `.github/workflows/deploy.yml` by hand (Actions tab or
  `gh workflow run deploy.yml`); a push to `main` does not deploy. `pnpm run deploy` still works
  locally. The old Jekyll site is on the branch `jekyll-backup`.
- Promotion SHOULD cover r/scala, users.scala-lang.org, This Week in Scala, Hacker News, X and
  LinkedIn. `[PUB-4]`
- The README of `tiny-uplc-jit` MUST link Post 1 and the talk video after Post 1 goes out. `[PUB-5]`

## Post 2 (later)

**Purpose:** show Cardano developers that the staged JIT gives the same result and the same budget
as the CEK machine, and when it pays.

- Post 2 MUST NOT go out before the staged JIT emits fewer, larger methods for large scripts. `[P2-1]`
- Post 2 MUST use the numbers of a public benchmark run made after the change of `[P2-1]`. `[P2-2]`
- Post 2 MUST cover the conformance suite, the exact budget per cost category, the cache keyed by
  script hash, and Hydrozoa. `[P2-3]`

**Why:** `[P2-1]`: on the public numbers, the `stablecoin_*` scripts compile to about 3,200 small
methods. C2 on EPYC needs 48–60 s to compile the hot ones, which is longer than the warm-up of the
benchmark. A post for Cardano developers about speed cannot go out with that gap open.

## Other tracks

- The Scala Workshop 2027 proposal MUST present the placement rule, in 2 pages. `[TRK-1]`
- The author SHOULD submit it when the call opens. The 2026 deadline was 23 Mar 2026. `[TRK-2]`
- The paper target is open: ICFP 2027 Experience Report (25 Feb 2027) or MPLR 2027 (11 Jun 2027,
  to be confirmed). `[TRK-3]`

**Why:** `[TRK-3]`: Post 1 is public. ICFP allows a preprint on arXiv. MPLR is double-blind and
asks authors not to publicize the work during review, so a choice of MPLR delays the arXiv
preprint.

## Open questions

1. Resolved 2026-10-30. The mainnet numbers come from the `BlocksValidation` test of Scalus (last
   at `a72ca9001e`, deleted in `a590821ec7`) run over `it-data/blocks`: blocks 11,544,518 to
   11,662,495, 28 Feb to 28 Mar 2025, epochs 543 to 548. The output is in the author's Google Sheet
   "CEK Performance". The talk's "one epoch" and its merged top-10/top-100 shares were wrong; Post 1
   gives the per-version figures and says so.
2. Withdrawn. The artifact has the same slide text as the PDF of 2026-10-13. It adds the act labels
   and the final speaker notes.
3. The credit line of each photo (`[AST-2]`).
4. How much of the flipped bet comes from the CEK rewrite, and how much from the new harness? An
   A/B run of Scalus 1.4.0 against 1.4.1 on the public benchmark answers it (`[P1-9]`).

## Out of scope

- Drafting Post 2.
- Writing the paper.
- The code generator change of `[P2-1]`.
