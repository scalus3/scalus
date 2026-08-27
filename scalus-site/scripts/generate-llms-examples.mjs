// Concatenates curated example validators + tests into out/llms-examples.txt.
import fs from 'fs';
import path from 'path';

const EX = '../scalus-examples/jvm/src';
const OUT = 'out/llms-examples.txt';

// Curated order: HTLC (gold standard) first, then alphabetical.
const EXAMPLES = [
  'htlc', 'amm', 'anonymousdata', 'atomictransactions', 'auction', 'betting',
  'crowdfunding', 'decentralizedidentity', 'editablenft', 'escrow', 'factory',
  'linkedlist', 'lottery', 'paymentsplitter', 'pricebet', 'simpletransfer',
  'simplewallet', 'storage', 'upgradeableproxy', 'vault', 'vesting',
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
