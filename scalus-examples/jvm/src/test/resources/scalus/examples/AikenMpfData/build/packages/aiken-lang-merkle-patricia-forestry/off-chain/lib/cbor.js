import assert from 'node:assert';


/** Concatenate encoded values from left to right.
 *
 * @param {...Buffer} args
 * @return {Buffer}
 */
export function sequence(...args) {
  return Buffer.concat(args);
}


/** Encode an integer-like value.
 *
 * @param {number|string} val
 * @return {Buffer}
 */
export function int(val) {
  const n = Number.parseInt(val, 10);
  assert(!Number.isNaN(n));
  if (n >= 0) {
    const [size, rest] = unsigned(n);
    return majorType(0, size, rest);
  } else {
    const [size, rest] = unsigned(-n-1);
    return majorType(1, size, rest);
  }
}


/** Encode a *definite* byte buffer.
 *
 * @param {Buffer} val
 * @return {Buffer}
 */
export function bytes(val) {
  const buffer = Buffer.from(val);
  const [size, rest] = unsigned(buffer.length);
  return majorType(2, size, rest, buffer);
}


/** Begin encoding an *indefinite* byte buffer.
 *
 * @return {Buffer}
 */
export function beginBytes() {
  return majorType(2, TOKEN_BEGIN)
}


/** Encode a UTF-8 text string.
 *
 * @param {string} val
 * @return {Buffer}
 */
export function text(val) {
  const buffer = Buffer.from(val);
  const [size, rest] = unsigned(buffer.length);
  return majorType(3, size, rest, buffer);
}


/** Begin encoding an *indefinite* text string.
 *
 * @return {Buffer}
 */
export function beginText() {
  return majorType(3, TOKEN_BEGIN);
}


/** Encode a uniform finite list of elements.
 *
 * @param {function} encodeElem An encoder for each items
 * @param {Array<any>} xs
 * @return {Buffer}
 */
export function list(encodeElem, xs) {
  const [size, rest] = unsigned(xs.length);
  return majorType(4, size, rest, ...xs.map(encodeElem));
}


/** Encode an heterogenous finite array of elements.
 *
 * @param {Array<Buffer>} xs
 * @return {Buffer}
 */
export function array(xs) {
  const [size, rest] = unsigned(xs.length);
  return majorType(4, size, rest, ...xs);
}


/** Encode the beginning of an indefinite list or array.
 *
 * @return {Buffer}
 */
export function beginList() {
  return majorType(4, TOKEN_BEGIN);
}


/** Encode a uniform key:value definite map.
 *
 * @param {function} encodeKey An encoder for each key
 * @param {function} encodeValue An encoder for each value
 * @param {object} xs
 * @return {Buffer}
 */
export function map(encodeKey, encodeValue, obj) {
  const keys = Object.keys(obj);
  const [size, rest] = unsigned(keys.length);
  return majorType(5, size, rest, ...keys.reduce((xs, k) => {
    xs.push(encodeKey(k));
    xs.push(encodeValue(obj[k]));
    return xs;
  }, []));
}


/** Encode the beginning of an indefinite map.
 *
 * @return {Buffer}
 */
export function beginMap() {
  return majorType(5, TOKEN_BEGIN);
}


/** Encode a tagged value.
 *
 * @param {number} t The (integer) tag
 * @param {Buffer} val
 * @return {Buffer}
 */
export function tag(t, val) {
  const [size, rest] = unsigned(t);
  return majorType(6, size, rest, val);
}


/** Encode the end of any indefinite stream.
 *
 * @return {Buffer}
 */
export function end() {
  return Buffer.from([TOKEN_END]);
}

// -----------------------------------------------------------------------------
// -------------------------------------------------------------------- Internal
// -----------------------------------------------------------------------------

/** Mark the beginning on an indefinite sequence.
 * @type {Number}
 * @private
 */
const TOKEN_BEGIN = 31;

/** Mark the end on an indefinite sequence.
 * @type {Number}
 * @private
 */
const TOKEN_END = 255;


function unsigned(val) {
  if (val < 24) {
    return [val, Buffer.alloc(0)];
  } else if (val < 2 ** 8) {
    const buf = Buffer.alloc(1);
    buf.writeUInt8(val);
    return [24, buf];
  } else if (val < 2 ** 16) {
    const buf = Buffer.alloc(2);
    buf.writeUInt16BE(val);
    return [25, buf];
  } else if (val < 2 ** 32) {
    const buf = Buffer.alloc(4);
    buf.writeUInt32BE(val);
    return [26, buf];
  } else if (val <= Number.MAX_SAFE_INTEGER) {
    const buf = Buffer.alloc(8);
    buf.writeUInt32BE(Math.floor(val / 2**32));
    buf.writeUInt32BE(val % 2**32, 4);
    return [27, buf];
  } else {
    throw new RangeError(`Cannot encode integer values larger than ${Number.MAX_SAFE_INTEGER}`);
  }
}

function majorType(i, val, ...args) {
  return Buffer.concat([Buffer.from([i << 5 | val]), ...args]);
}
