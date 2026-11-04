// Generates out/llms-ts.txt: the npm package README, then scalus.d.ts, both verbatim.
// Runs from scalus-site/ as part of postbuild and build-version.sh.
// Spec: docs/superpowers/specs/2026-11-04-llm-docs-typescript-java-design.md
//
// Usage: node scripts/generate-llms-ts.mjs [npm package dir] [output file]
import fs from 'fs';
import path from 'path';

const NPM = process.argv[2] ?? '../scalus-cardano-ledger/js/src/main/npm';
const OUT = process.argv[3] ?? 'out/llms-ts.txt';

function readInput(name) {
  const file = path.join(NPM, name);
  const text = fs.existsSync(file) ? fs.readFileSync(file, 'utf8') : '';
  if (text.trim() === '') {
    // spec [TS-4]
    console.error(`generate-llms-ts: ${file} is missing or empty, aborting`);
    process.exit(1);
  }
  return text;
}

const pkg = JSON.parse(readInput('package.json'));
const readme = readInput('README.md');
const dts = readInput('scalus.d.ts');

const out =
  // spec [TS-1]
  `# Scalus for TypeScript and JavaScript (npm package ${pkg.name} ${pkg.version})\n\n` +
  'The npm package README, then the full TypeScript definitions (scalus.d.ts).\n' +
  'When the package is installed, node_modules/scalus/README.md and node_modules/scalus/scalus.d.ts\n' +
  'match the version your project uses; prefer them over this file.\n\n' +
  // spec [TS-2]
  readme.trimEnd() +
  '\n\n# TypeScript definitions (scalus.d.ts)\n\n' +
  // spec [TS-3]
  '```ts\n' + dts.trimEnd() + '\n```\n';

// spec [TS-5]: the only file this script writes.
fs.mkdirSync(path.dirname(OUT), { recursive: true });
fs.writeFileSync(OUT, out);
console.log(`generate-llms-ts: ${pkg.name} ${pkg.version} → ${OUT}`);
