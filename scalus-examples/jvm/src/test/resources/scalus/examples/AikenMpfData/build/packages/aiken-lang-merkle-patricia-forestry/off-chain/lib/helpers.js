/** @module helpers */

import assert from 'node:assert';
import { inspect } from 'node:util';
import { DIGEST_LENGTH, digest } from './crypto.js';


/* By convention, the hash of empty tries / trees is the NULL_HASH
 */
export const NULL_HASH = Buffer.alloc(DIGEST_LENGTH);

/** Turn a an object whose keys are hex-digits into a sparse vector.
 * Fill gaps with 'undefined' values.
 *
 * @example
 * sparseVector({ 1: 'foo' }) // [undefined, 'foo', undefined, ...13 more times]
 *
 * @example
 * sparseVector({ 2: 'bar' }, 3) // [undefined, undefined, 'bar']
 *
 * @param {object} obj A key:value map of nodes.
 * @return {array}
 * @throws {AssertionError} When any key in the object is not a nibble.
 */
export function sparseVector(obj) {
  let vector = [];
  for (let i = 1; i <= 16; i += 1) {
    vector.push(undefined);
  }

  for (let k in obj) {
    const ix = Number.parseInt(k, 10);

    assert(
      isHexDigit(ix),
      `object key must be an integer between 0 and 15 but it was ${k}`,
    );

    vector[ix] = obj[k];
  }

  return vector;
}


/** Find the prefix common to a list of words. Returns an empty Buffer when
 * there's no common prefix.
 *
 * @param {Array<string>} words A list of words.
 * @return {string} The common prefix, if any. An empty string otherwise.
 * @throws {AssertionError} When given an empty list.
 * @throws {AssertionError} When any word in the list is empty.
 */
export function commonPrefix(words) {
  assert(
    words.length > 0,
    'No words to compute prefix from!',
  );

  let prefix;

  words.forEach(word => {
    assert(
      word.length > 0,
      'Cannot compute common prefix of empty words!',
    );

    if (prefix === undefined) {
      prefix = word;
    } else {
      prefix = prefix.slice(0, word.length);

      for (let i = 0; i < word.length; i += 1) {
        if (prefix[i] === undefined) {
          break;
        } else if (prefix[i] !== word[i]) {
          prefix = prefix.slice(0, i);
          break;
        }
      }
    }
  });

  return prefix;
}


/** Ensures that all *values* in the 'what' object are of the given instance.
 * Uses *keys* to display nice error messages.
 *
 * @example
 * assertInstanceOf(Buffer, { foo })
 *
 * @example
 * assertInstanceOf('string', { foo }, (what, type) => typeof what === type)
 *
 * @param {Any} instance Any class/prototype to check against
 * @param {object} what A key:value dictionnary of objects to test against.
 * @param {function} [fn] An (optional) overriding test function in case where
 *                       'instanceof' isn't a good fit.
 *
 * @return {undefined} Returns nothing, but throws an exception in case of failure.
 * @throws {AssertionError} When values of 'what' aren't of the expected type.
 */
export function assertInstanceOf(instance, what, fn) {
  for (let key in what) {
    const expected = instance?.prototype?.constructor?.name || instance;
    const got = what[key]?.prototype?.constructor?.name || typeof what[key];

    assert(
      fn ? fn(what[key], instance) : what[key] instanceof instance,
      `${key} must be an instance of ${expected} but is ${got}: ${inspect(what[key])}`
    );
  }
}


/** Transform each line of a string with the given function. Treat each line as
 * a separate string.
 *
 * @example
 * eachLine(
 *  `foo
 *   bar
 *   baz`,
 *   str => str + ': ' + str.toUpperCase()
 * )
 * // foo: FOO
 * // bar: BAR
 * // baz: BAZ
 *
 *
 * @param {string} str A multiline string.
 * @param {each} each The modifier function to apply to each line.
 * @return {string} A transformed multiline string.
 */
export function eachLine(str, each) {
/**
 *
 * @callback each
 * @param {string} line A line of the original string.
 * @return {string} The modified line.
 */

  return str.split("\n").map(each).join("\n");
}


/**
 * Insert an ellipsis in the middle of a long string, so that at most
 * 'cutoff' digits of the string are displayed.
 *
 * @param {string} msg The string to display
 * @param {number} cutoff Number of digits after which insert an ellipsis
 */
export function withEllipsis(msg, cutoff, options) {
  const ellipsis = options.stylize(`..[${msg.length - cutoff} digits]..`, 'undefined');

  return msg.length > cutoff
    ? `${msg.slice(0, cutoff / 2)}${ellipsis}${msg.slice(-cutoff / 2)}`
    : `${msg}`;
}


/**
 * Convert a character into an hexadecimal digit (a.k.a nibble)
 *
 * @param {string} digit A single hex digit
 * @return {Number}
 */
export function nibble(digit) {
  return Number.parseInt(digit, 16);
}


/**
 * Convert an string of hexadecimal digits into an array of digits a.k.a nibbles
 *
 * @example
 * nibbles('ab') // Buffer<0a, 0b>
 *
 * @example
 * nibbles('0102') // Buffer<00, 01, 00, 02>
 *
 * @param {string} str An hex-encoded string.
 * @return {Buffer} A byte buffer where each byte is a nibble.
 * @throws {AssertionError} When the string contains non hex-digits.
 */
export function nibbles(str) {
  const digits = Array.from(str).map(nibble);

  assert(
    typeof str === 'string' && digits.every(isHexDigit),
    `must be a string of hex-digits, but it is: ${str}`,
  );

  return Buffer.from(digits);
}


/**
 * Test whether anything is an hex-digit integer.
 *
 * @param {any} digit
 * @return {bool}
 */
export function isHexDigit(digit) {
  return Number.isInteger(digit) && digit >= 0 && digit <= 15;
}


/**
 * Compute the Merkle root of a Sparse-Merkle-Trie formed by a node's children.
 *
 * @param {Array<{ hash: Buffer }|Buffer|undefined>} children
 *   A non-empty list of (possibly empty) child nodes (hashes) to merkleize.
 *
 * @param {number} [size=16]
 *   An expected size. Mostly exists to provide a check by default; can be
 *   overridden in context that matters.
 *
 * @return Buffer
 * @private
 */
export function merkleRoot(children, size = 16) {
  let nodes = children.map(x => x?.hash ?? x ?? NULL_HASH);

  let n = nodes.length;

  assert(
    n === size,
    `trying to compute an intermediate Merkle root of ${nodes.length} nodes instead of ${size}`);

  if (n === 1) {
    return nodes[0];
  }

  assert(
    n >= 2 && n % 2 === 0,
    `trying to compute intermediate Merkle root of an odd number of nodes.`,
  );

  do {
    for (let i = 0; 2 * i < n; i += 1) {
      nodes.push(digest(Buffer.concat(nodes.splice(0, 2))));
    }
    n = nodes.length;
  } while (n > 1);

  return nodes[0];
}


/**
 * Construct a merkle proof for a given non-empty trie.
 *
 * @param {Array<Buffer>} nodes A non-empty list of child nodes to merkleize.
 * @param {number} me The index of the node we are proving
 * @return {Array<Buffer>}
 * @private
 */
export function merkleProof(nodes, me) {
  assert(nodes.length > 1 && nodes.length % 2 === 0);
  assert(Number.isInteger(me) && me >= 0 && me < nodes.length);

  let neighbors = [];

  let pivot = 8; let n = 8;
  do {
    if (me < pivot) {
      neighbors.push(merkleRoot(nodes.slice(pivot, pivot + n), n))
      pivot -= (n >> 1);
    } else {
      neighbors.push(merkleRoot(nodes.slice(pivot - n, pivot), n));
      pivot += (n >> 1);
    }
    n = n >> 1;
  } while (n >= 1);

  return neighbors;
}


/** Turn any key into a path of nibbles.
 *
 * @param {Buffer|string} key
 *   Also accepts raw 'strings' treated as UTF-8 byte buffers.
 * @return {string}
 * @private
 */
export function intoPath(key) {
  return digest(key = typeof key === 'string'
    ? Buffer.from(key)
    : key
  ).toString('hex');
}
