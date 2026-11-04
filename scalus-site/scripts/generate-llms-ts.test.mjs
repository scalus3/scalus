// Tests for generate-llms-ts.mjs. Run: node --test scalus-site/scripts/
// Spec: docs/superpowers/specs/2026-11-04-llm-docs-typescript-java-design.md [TS-32]
import { test } from 'node:test';
import assert from 'node:assert/strict';
import { spawnSync } from 'node:child_process';
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import { fileURLToPath } from 'node:url';

const SCRIPT = path.join(path.dirname(fileURLToPath(import.meta.url)), 'generate-llms-ts.mjs');
const REAL_NPM = path.join(path.dirname(SCRIPT), '../../scalus-cardano-ledger/js/src/main/npm');

const README = '# Scalus - Cardano Toolkit for JavaScript\n\nSome `code` and a ```fence```.\n';
const DTS = 'export declare class Emulator {\n    tip(): ChainTip;\n}\n';

function fixture(files) {
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'llms-ts-'));
  const npm = path.join(dir, 'npm');
  fs.mkdirSync(npm);
  for (const [name, text] of Object.entries(files)) fs.writeFileSync(path.join(npm, name), text);
  return { dir, npm, out: path.join(dir, 'out', 'llms-ts.txt') };
}

const PKG = JSON.stringify({ name: 'scalus', version: '9.8.7' });

function run(npm, out) {
  return spawnSync(process.execPath, [SCRIPT, npm, out], { encoding: 'utf8' });
}

test('[TS-1] [TS-2] [TS-3] header, then README verbatim, then d.ts in a ts fence', () => {
  const f = fixture({ 'package.json': PKG, 'README.md': README, 'scalus.d.ts': DTS });
  const r = run(f.npm, f.out);
  assert.equal(r.status, 0, r.stderr);
  const text = fs.readFileSync(f.out, 'utf8');
  const lines = text.split('\n');
  assert.equal(lines[0], '# Scalus for TypeScript and JavaScript (npm package scalus 9.8.7)');
  const readmeAt = text.indexOf(README.trimEnd());
  const dtsAt = text.indexOf('```ts\n' + DTS.trimEnd() + '\n```\n');
  assert.ok(readmeAt > 0, 'README is copied verbatim');
  assert.ok(dtsAt > readmeAt, 'd.ts follows the README inside a ts fence');
  assert.ok(text.endsWith('```\n'));
});

test('[TS-5] writes only the output file', () => {
  const f = fixture({ 'package.json': PKG, 'README.md': README, 'scalus.d.ts': DTS });
  assert.equal(run(f.npm, f.out).status, 0);
  assert.deepEqual(fs.readdirSync(path.join(f.dir, 'out')), ['llms-ts.txt']);
  assert.deepEqual(fs.readdirSync(f.npm).sort(), ['README.md', 'package.json', 'scalus.d.ts']);
});

for (const [name, files] of [
  ['missing d.ts', { 'package.json': PKG, 'README.md': README }],
  ['empty d.ts', { 'package.json': PKG, 'README.md': README, 'scalus.d.ts': ' \n' }],
  ['missing README', { 'package.json': PKG, 'scalus.d.ts': DTS }],
  ['empty README', { 'package.json': PKG, 'README.md': '', 'scalus.d.ts': DTS }],
]) {
  test(`[TS-4] fails on ${name} and writes nothing`, () => {
    const f = fixture(files);
    const r = run(f.npm, f.out);
    assert.notEqual(r.status, 0);
    assert.match(r.stderr, /missing or empty/);
    assert.equal(fs.existsSync(f.out), false);
  });
}

test('the real npm package inputs produce a bundle with the Emulator class', () => {
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'llms-ts-real-'));
  const out = path.join(dir, 'llms-ts.txt');
  const r = run(REAL_NPM, out);
  assert.equal(r.status, 0, r.stderr);
  const text = fs.readFileSync(out, 'utf8');
  const version = JSON.parse(fs.readFileSync(path.join(REAL_NPM, 'package.json'), 'utf8')).version;
  assert.ok(text.startsWith(`# Scalus for TypeScript and JavaScript (npm package scalus ${version})`));
  assert.match(text, /export class Emulator \{/);
});
