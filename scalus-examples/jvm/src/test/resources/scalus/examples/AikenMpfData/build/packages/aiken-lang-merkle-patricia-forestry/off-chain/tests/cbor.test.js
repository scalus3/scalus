import test from 'ava';

import * as cbor from '../lib/cbor.js';


test('int', (t) => {
  sameBytes(t, cbor.int(0), '00');
  sameBytes(t, cbor.int(1), '01');
  sameBytes(t, cbor.int(10), '0a');
  sameBytes(t, cbor.int(23), '17');
  sameBytes(t, cbor.int(24), '1818');
  sameBytes(t, cbor.int(25), '1819');
  sameBytes(t, cbor.int(100), '1864');
  sameBytes(t, cbor.int(1000), '1903e8');
  sameBytes(t, cbor.int(1000000), '1a000f4240');
  sameBytes(t, cbor.int(1000000000000), '1b000000e8d4a51000');
  sameBytes(t, cbor.int(9007199254740991), '1b001fffffffffffff');
  t.throws(() => cbor.int(18446744073709551615), { instanceOf: RangeError });
  sameBytes(t, cbor.int(-1), '20');
  sameBytes(t, cbor.int(-10), '29');
  sameBytes(t, cbor.int(-100), '3863');
  sameBytes(t, cbor.int(-1000), '3903e7');
});


test('bytes', (t) => {
  sameBytes(t, cbor.bytes(Buffer.from('')), '40');
  sameBytes(t, cbor.bytes(Buffer.from('01020304', 'hex')), '4401020304');
});


test('beginBytes', (t) => {
  sameBytes(t,
    cbor.sequence(
      cbor.beginBytes(),
      cbor.bytes(Buffer.from('0102', 'hex')),
      cbor.bytes(Buffer.from('030405', 'hex')),
      cbor.end(),
    ),
    '5f42010243030405ff',
  );
});


test('text', (t) => {
  sameBytes(t, cbor.text(''), '60');
  sameBytes(t, cbor.text('a'), '6161');
  sameBytes(t, cbor.text('IETF'), '6449455446');
  sameBytes(t, cbor.text('\'\\'), '62275c');
  sameBytes(t, cbor.text('\u00fc'), '62c3bc');
  sameBytes(t, cbor.text('\u6c34'), '63e6b0b4');
  sameBytes(t, cbor.text('\ud800\udd51'), '64f0908591');
});


test('beginText', (t) => {
  sameBytes(t,
    cbor.sequence(
      cbor.beginText(),
      cbor.text('strea'),
      cbor.text('ming'),
      cbor.end(),
    ),
    '7f657374726561646d696e67ff',
  );
});


test('list/array', (t) => {
  sameBytes(t,
    cbor.list(cbor.int, []),
    '80'
  );

  sameBytes(t,
    cbor.list(cbor.int, [1, 2, 3]),
    '83010203'
  );

  sameBytes(t,
    cbor.array([cbor.int(1), cbor.list(cbor.int, [2, 3]), cbor.list(cbor.int, [4, 5])]),
    '8301820203820405',
  );

  sameBytes(t,
    cbor.list(cbor.int, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25]),
    '98190102030405060708090a0b0c0d0e0f101112131415161718181819',
  );
});


test('beginList', (t) => {
  sameBytes(t,
    cbor.sequence(
      cbor.beginList(),
      cbor.end()
    ),
    '9fff',
  );

  sameBytes(t,
    cbor.sequence(
      cbor.beginList(),
      cbor.int(1),
      cbor.list(cbor.int, [2, 3]),
      cbor.sequence(
        cbor.beginList(),
        cbor.int(4),
        cbor.int(5),
        cbor.end(),
      ),
      cbor.end()
    ),
    '9f018202039f0405ffff',
  );

  sameBytes(t,
    cbor.sequence(
      cbor.beginList(),
      cbor.int(1),
      cbor.list(cbor.int, [2, 3]),
      cbor.list(cbor.int, [4, 5]),
      cbor.end()
    ),
    '9f01820203820405ff',
  );

  sameBytes(t,
    cbor.array([
      cbor.int(1),
      cbor.list(cbor.int, [2, 3]),
      cbor.sequence(
        cbor.beginList(),
        cbor.int(4),
        cbor.int(5),
        cbor.end(),
      ),
    ]),
    '83018202039f0405ff',
  );

  sameBytes(t,
    cbor.array([
      cbor.int(1),
      cbor.sequence(
        cbor.beginList(),
        cbor.int(2),
        cbor.int(3),
        cbor.end(),
      ),
      cbor.list(cbor.int, [4, 5]),
    ]),
    '83019f0203ff820405',
  );

  sameBytes(t,
    cbor.sequence(
      cbor.beginList(),
      ...(new Array(25)).fill(0).map((_, ix) => cbor.int(ix+1)),
      cbor.end()
    ),
    '9f0102030405060708090a0b0c0d0e0f101112131415161718181819ff',
  );
});


test('map', (t) => {
  sameBytes(t,
    cbor.map(cbor.int, cbor.int, {}),
    'a0',
  );

  sameBytes(t,
    cbor.map(cbor.int, cbor.int, { 1: 2, 3: 4 }),
    'a201020304',
  );

  sameBytes(t,
    cbor.map(cbor.text, x => x, { a: cbor.int(1), b: cbor.list(cbor.int, [2, 3]) }),
    'a26161016162820203',
  );

  sameBytes(t,
    cbor.array([ cbor.text('a'), cbor.map(cbor.text, cbor.text, { b: 'c' }) ]),
    '826161a161626163',
  );

  sameBytes(t,
    cbor.map(cbor.text, cbor.text, { a: 'A', b: 'B', c: 'C', d: 'D', e: 'E' }),
    'a56161614161626142616361436164614461656145',
  );
});

test('beginMap', (t) => {
  sameBytes(t,
    cbor.sequence(
      cbor.beginMap(),
      cbor.text('a'), cbor.int(1),
      cbor.text('b'), cbor.sequence(
        cbor.beginList(),
        cbor.int(2),
        cbor.int(3),
        cbor.end()
      ),
      cbor.end(),
    ),
    'bf61610161629f0203ffff',
  );

  sameBytes(t,
    cbor.array([
      cbor.text('a'),
      cbor.sequence(
        cbor.beginMap(),
        cbor.text('b'), cbor.text('c'),
        cbor.end(),
      ),
    ]),
    '826161bf61626163ff',
  );
});


test('tag', (t) => {
  sameBytes(t,
    cbor.tag(1, cbor.int(1363896240)),
    'c11a514b67b0',
  );

  sameBytes(t,
    cbor.tag(23, cbor.bytes(Buffer.from('01020304', 'hex'))),
    'd74401020304',
  );

  sameBytes(t,
    cbor.tag(24, cbor.bytes(Buffer.from('6449455446', 'hex'))),
    'd818456449455446',
  );

  sameBytes(t,
    cbor.tag(32, cbor.text('http://www.example.com')),
    'd82076687474703a2f2f7777772e6578616d706c652e636f6d',
  );
});


// -----------------------------------------------------------------------------
// --------------------------------------------------------------------- Helpers
// -----------------------------------------------------------------------------

function sameBytes(t, got, expected) {
  t.is(got.toString('hex'), expected, expected);
}
