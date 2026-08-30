#!/usr/bin/env node
// Group the bytes of a Scala.js linker output by package or library.
//
// Two inputs are supported, because they answer different questions:
//
//   --sources <source-map-explorer.json>
//       Attributes the OPTIMISED linker output back to original .scala files through the source
//       map. Closest to shipped bytes, but it over-counts: output regions with no mapping are
//       absorbed into whichever source precedes them, so a source sitting in front of a large
//       generated region is credited with bytes that are not its own. Treat it as a ranking.
//
//   --modules <dir>
//       Sums the per-class ES modules emitted by a SmallModulesFor link. Every number is a real
//       file size, so the split between packages is trustworthy and nothing is guessed. The
//       totals are fastLink, roughly 2-3x the optimised output, so use ratios not absolutes.
//       This input can also say what is reachable at all: unreachable classes have no module.
//
// Usage:
//   node scripts/js-bundle-size-report.mjs --sources sizes.json [--depth 3] [--top 25]
//   node scripts/js-bundle-size-report.mjs --modules path/to/scalus-cardano-ledger-fastopt

import { readFileSync, readdirSync, statSync } from "node:fs";
import { join } from "node:path";

const args = process.argv.slice(2);
const flag = (name, fallback) => {
    const i = args.indexOf(name);
    return i === -1 ? fallback : args[i + 1];
};
const depth = Number(flag("--depth", 3));
const top = Number(flag("--top", 25));

/** Bucket a source path into a package or library name. */
function bucketOfSource(path) {
    if (path.startsWith("[")) return path; // [no source], [EOLs], [unmapped]
    // Dependencies published with source maps point at their upstream repository.
    const gh = path.match(/raw\.githubusercontent\.com\/([^/]+)\/([^/]+)\/[^/]+\/(.*)$/);
    if (gh) {
        const [, org, repo, rest] = gh;
        // scala-js keeps four very different source sets in one repository.
        const sub = rest.match(/^(javalib|scalalib|library|linker-private-library)\//);
        if (sub) return `${repo}/${sub[1]}`;
        if (repo === "scala") return "scala-library";
        return repo;
    }
    // Local sources: keep the first `depth` segments of the package path.
    const local = path.match(/src\/(?:main|test)\/(?:scala|scala-3)[^/]*\/(.*)$/);
    if (local) {
        const parts = local[1].split("/");
        const pkg = parts.slice(0, Math.min(depth, parts.length - 1));
        return pkg.length ? pkg.join(".") : parts[0];
    }
    return "other";
}

/** Bucket a per-class module file name (`scalus.uplc.eval.-Cek$.js`) into a package. */
function bucketOfModule(file) {
    const name = file.replace(/\.js$/, "");
    if (name.startsWith("internal-")) return "(shared chunk)";
    if (name === "main") return "(entry)";
    const parts = name.split(".").filter(p => !p.startsWith("-"));
    return parts.slice(0, depth).join(".") || name;
}

function render(rows, total, label) {
    const width = String(rows[0]?.[1] ?? 0).length;
    console.log(`\n${label} — ${total.toLocaleString()} bytes total\n`);
    for (const [name, bytes, count] of rows.slice(0, top)) {
        const pct = ((100 * bytes) / total).toFixed(2).padStart(5);
        console.log(
          `${String(bytes).padStart(width)}  ${pct}%  ${name}${count ? `  (${count} files)` : ""}`
        );
    }
    const shown = rows.slice(0, top).reduce((s, r) => s + r[1], 0);
    if (rows.length > top) {
        const rest = total - shown;
        console.log(`${String(rest).padStart(width)}  ${((100 * rest) / total).toFixed(2).padStart(5)}%  (${rows.length - top} more)`);
    }
}

const sourcesArg = flag("--sources", null);
const modulesArg = flag("--modules", null);

if (sourcesArg) {
    const data = JSON.parse(readFileSync(sourcesArg, "utf8"));
    const files = data.results[0].files;
    const agg = new Map();
    let total = 0;
    for (const [path, v] of Object.entries(files)) {
        const bytes = typeof v === "number" ? v : v.size;
        total += bytes;
        const b = bucketOfSource(path);
        const prev = agg.get(b) ?? [0, 0];
        agg.set(b, [prev[0] + bytes, prev[1] + 1]);
    }
    const rows = [...agg].map(([n, [b, c]]) => [n, b, c]).sort((x, y) => y[1] - x[1]);
    render(rows, total, "By source file, attributed through the source map (RANKING ONLY, over-counts)");
    console.log("\nVerify a row before acting on it: grep the linker output for a marker of that");
    console.log("library, and check 'js-bundle-size.sh modules'. This view has credited a library");
    console.log("with 821 KB in a build containing none of it, because one stray mapping absorbs");
    console.log("every following byte until the next one.");
} else if (modulesArg) {
    const files = readdirSync(modulesArg).filter(f => f.endsWith(".js"));
    if (files.length < 10) {
        console.error(`Only ${files.length} module(s) in ${modulesArg}. Run 'js-bundle-size.sh graph' first.`);
        process.exit(1);
    }
    const agg = new Map();
    let total = 0;
    for (const f of files) {
        const bytes = statSync(join(modulesArg, f)).size;
        total += bytes;
        const b = bucketOfModule(f);
        const prev = agg.get(b) ?? [0, 0];
        agg.set(b, [prev[0] + bytes, prev[1] + 1]);
    }
    const rows = [...agg].map(([n, [b, c]]) => [n, b, c]).sort((x, y) => y[1] - x[1]);
    // A stamp written by `js-bundle-size.sh graph`. Not file mtimes: the linker leaves modules
    // it did not have to rewrite untouched, so they keep timestamps from an older link.
    let stamp = null;
    try {
        stamp = readFileSync(join(modulesArg, ".graph-stamp"), "utf8").trim();
    } catch {}
    render(rows, total, `By reachable class module, ${files.length} modules (fastLink bytes, use ratios)`);
    console.log(stamp
      ? `\nLinked: ${stamp}. Re-run 'js-bundle-size.sh graph' if that is not your working state.`
      : "\nThis data has no stamp, so its age is unknown. Run 'js-bundle-size.sh graph' to refresh.");
    console.log("(entry), (shared chunk) and org.scalajs.linker are not packages: the entry module,");
    console.log("content-hashed chunks the linker shares between modules, and its own runtime.");
} else {
    console.error(readFileSync(new URL(import.meta.url)).toString().split("\n").slice(1, 22).join("\n"));
    process.exit(2);
}
