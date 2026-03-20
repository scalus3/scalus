import test from 'ava';
import { inspect } from 'node:util';
import {
  NULL_HASH,
  commonPrefix,
  merkleRoot,
} from '../lib/helpers.js';


// -----------------------------------------------------------------------------
// ---------------------------------------------------------------- commonPrefix
// -----------------------------------------------------------------------------

test('commonPrefix: empty words', t => {
  t.throws(() => commonPrefix([]));
});

test('commonPrefix: empty word', t => {
  t.throws(() => commonPrefix(['merkle-patricia-trie', '']));
});

test('commonPrefix: two identical strings', t => {
  const prefix = commonPrefix([
    'merkle-patricia-trie',
    'merkle-patricia-trie',
  ]);

  t.is(prefix, 'merkle-patricia-trie');
});

test('commonPrefix: first word is prefix', t => {
  const prefix = commonPrefix([
    'do',
    'dog',
    'dogs',
  ]);

  t.is(prefix, 'do');
});

test('commonPrefix: last word is prefix', t => {
  const prefix = commonPrefix([
    'dogs',
    'dog',
    'do',
  ]);

  t.is(prefix, 'do');
});

test('commonPrefix: prefix is anywhere', t => {
  const prefix = commonPrefix([
    'carda',
    'cardano',
    'card',
    'cardinal',
  ]);

  t.is(prefix, 'card');
});

test('commonPrefix: no common prefix', t => {
  const prefix = commonPrefix([
    'dog',
    'cat',
    'bird',
  ]);

  t.is(prefix, '');
});


// -----------------------------------------------------------------------------
// ------------------------------------------------------------------ merkleRoot
// -----------------------------------------------------------------------------

test('merkleRoot: null hashes', t => {
  t.is(
    merkleRoot([NULL_HASH], 1).toString('hex'),
    "0000000000000000000000000000000000000000000000000000000000000000"
  );

  t.is(
    merkleRoot([NULL_HASH, NULL_HASH], 2).toString('hex'),
    "0eb923b0cbd24df54401d998531feead35a47a99f4deed205de4af81120f9761",
  );

  t.is(
    merkleRoot((new Array(4)).fill(NULL_HASH), 4).toString('hex'),
    "85c09af929492a871e4fae32d9d5c36e352471cd659bcdb61de08f1722acc3b1",
  );

  t.is(
    merkleRoot((new Array(8)).fill(NULL_HASH), 8).toString('hex'),
    "b22df1a126b5ba4e33c16fd6157507610e55ffce20dae7ac44cae168a463612a",
  );
});
