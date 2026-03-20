import * as cp from 'node:child_process';
import * as fs from 'node:fs';
import * as os from 'node:os';
import * as path from 'node:path';
import { inspect } from 'node:util';
import { randomBytes } from 'node:crypto';

import test from 'ava';

import { Store } from '../lib/store.js';
import { Leaf, Branch, Proof, Trie } from '../lib/trie.js';
import * as helpers from '../lib/helpers.js';
import * as cbor from '../lib/cbor.js';


const FRUITS_LIST = [
  { key: 'apple[uid: 58]', value: '🍎' },
  { key: 'apricot[uid: 0]', value: '🤷' },
  { key: 'banana[uid: 218]', value: '🍌' },
  { key: 'blueberry[uid: 0]', value: '🫐' },
  { key: 'cherry[uid: 0]', value: '🍒' },
  { key: 'coconut[uid: 0]', value: '🥥' },
  { key: 'cranberry[uid: 0]', value: '🤷' },
  { key: 'fig[uid: 68267]', value: '🤷' },
  { key: 'grapefruit[uid: 0]', value: '🤷' },
  { key: 'grapes[uid: 0]', value: '🍇' },
  { key: 'guava[uid: 344]', value: '🤷' },
  { key: 'kiwi[uid: 0]', value: '🥝' },
  { key: 'kumquat[uid: 0]', value: '🤷' },
  { key: 'lemon[uid: 0]', value: '🍋' },
  { key: 'lime[uid: 0]', value: '🤷' },
  { key: 'mango[uid: 0]', value: '🥭' },
  { key: 'orange[uid: 0]', value: '🍊' },
  { key: 'papaya[uid: 0]', value: '🤷' },
  { key: 'passionfruit[uid: 0]', value: '🤷' },
  { key: 'peach[uid: 0]', value: '🍑' },
  { key: 'pear[uid: 0]', value: '🍐' },
  { key: 'pineapple[uid: 12577]', value: '🍍' },
  { key: 'plum[uid: 15492]', value: '🤷' },
  { key: 'pomegranate[uid: 0]', value: '🤷' },
  { key: 'raspberry[uid: 0]', value: '🤷' },
  { key: 'strawberry[uid: 2532]', value: '🍓' },
  { key: 'tangerine[uid: 11]', value: '🍊' },
  { key: 'tomato[uid: 83468]', value: '🍅' },
  { key: 'watermelon[uid: 0]', value: '🍉' },
  { key: 'yuzu[uid: 0]', value: '🤷' },
];


// -----------------------------------------------------------------------------
// ------------------------------------------------------------------------ Trie
// -----------------------------------------------------------------------------

test('Trie: a new Trie is always empty', t => {
  const trie = new Trie();
  t.true(trie.isEmpty());
});

test('Trie: an empty Trie has no hash', t => {
  const trie = new Trie();
  t.is(trie.hash, null)
});

test('Trie: inspect an empty trie', t => {
  const trie = new Trie();
  t.is(inspect(trie), 'ø');
});

test('Trie: can construct from an empty list', async t => {
  t.deepEqual(await Trie.fromList([]), new Trie());
});


test('Trie: cannot prove anything on an empty trie', async t => {
  const trie = new Trie();
  await t.throwsAsync(
    () => trie.prove("foo"),
    { message(e) { return e.startsWith('cannot walk empty trie') } },
  );
});

test('Trie: can be constructed from a single value', async t => {
  const pairs = [{ key: 'foo', value: 'bar' }]
  const trie = await Trie.fromList(pairs);

  t.true(trie instanceof Leaf);
  t.false(trie.hash == null);
  t.is(trie.prefix.length, 64);
  t.is(trie.key.toString(), pairs[0].key);
  t.is(trie.value.toString(), pairs[0].value);
});

test('Trie: can be constructed from two values', async t => {
  const pairs = [
    { key: 'foo', value: '14' },
    { key: 'bar', value: '42' },
  ];

  const trie = await Trie.fromList(pairs);

  t.is(trie.size, 2);
  t.false(trie.hash == null);
  t.false(trie instanceof Leaf);

  await trie.fetchChildren();

  const foo = await trie.childAt('b');
  t.true(foo instanceof Leaf);
  t.is(foo.prefix.length, 63);
  t.is(foo.key.toString(), pairs[0].key);
  t.is(foo.value.toString(), pairs[0].value);

  const bar = await trie.childAt('8');
  t.true(bar instanceof Leaf);
  t.is(bar.prefix.length, 63);
  t.is(bar.key.toString(), pairs[1].key);
  t.is(bar.value.toString(), pairs[1].value);
});


// -----------------------------------------------------------------------------
// ----------------------------------------------------------------- Trie.insert
// -----------------------------------------------------------------------------

test('Trie.insert: into empty', async t => {
  let trie = new Trie();
  await trie.insert('foo', '14');
  t.true(trie instanceof Leaf);
  t.deepEqual(trie, await Trie.fromList([{ key: 'foo', value: '14' }]));
});

test('Trie.insert: into leaf', async t => {
  const foo = { key: 'foo', value: '14' };
  const bar = { key: 'bar', value: '42' };

  let trie = await Trie.fromList([foo]);
  await trie.insert(bar.key, bar.value);
  t.true(trie instanceof Branch);

  t.deepEqual(trie, await Trie.fromList([foo, bar]));
});

test('Trie.load', async t => {
  const store = new Store(tmpFilename());

  await store.ready();

  await Trie.fromList(FRUITS_LIST, store);

  const trie = await Trie.load(store);

  t.is(inspect(trie), unindent`
    ╔═══════════════════════════════════════════════════════════════════╗
    ║ #4acd78f345a686361df77541b2e0b533f53362e36620a1fdd3a13e0b61a3b078 ║
    ╚═══════════════════════════════════════════════════════════════════╝
     ┌─ 0 #520a7f805c5f
     ├─ 1 #58c5e4a29601
     ├─ 2 #c9431d708d20
     ├─ 3 #070a12b8b349
     ├─ 4 #79519b8cdfbd
     ├─ 5 #08434fd717ae
     ├─ 7 #aeb3a9f2e198
     ├─ 8 #b27d20a5187a
     ├─ a #c2f2115774c1
     ├─ b #da0bdb30bf45
     ├─ c #a22a7b4d767a
     ├─ d #0a747d583e2e
     ├─ e #da1771d107c8
     └─ f #117abf0e19fb
  `);
});

test('Trie.insert: whole trie in any order', async t => {
  await Promise.all([undefined, new Store(tmpFilename())].map(async store => {
    const trie = await shuffle(FRUITS_LIST).reduce(async (trie, fruit, ix) => {
      trie = await trie;
      t.is(trie.size, ix);
      t.true(trie === await trie.insert(fruit.key, fruit.value));
      await t.throwsAsync(() => trie.insert(fruit.key, fruit.value));
      return trie;
    }, new Trie(store));

    t.false(trie.children.some(node => node !== undefined && node instanceof Trie))

    t.is(inspect(trie), unindent`
      ╔═══════════════════════════════════════════════════════════════════╗
      ║ #4acd78f345a686361df77541b2e0b533f53362e36620a1fdd3a13e0b61a3b078 ║
      ╚═══════════════════════════════════════════════════════════════════╝
       ┌─ 0 #520a7f805c5f
       ├─ 1 #58c5e4a29601
       ├─ 2 #c9431d708d20
       ├─ 3 #070a12b8b349
       ├─ 4 #79519b8cdfbd
       ├─ 5 #08434fd717ae
       ├─ 7 #aeb3a9f2e198
       ├─ 8 #b27d20a5187a
       ├─ a #c2f2115774c1
       ├─ b #da0bdb30bf45
       ├─ c #a22a7b4d767a
       ├─ d #0a747d583e2e
       ├─ e #da1771d107c8
       └─ f #117abf0e19fb
    `);

    const sameTrie = await Trie.fromList(FRUITS_LIST);
    t.deepEqual(trie, sameTrie);

    await Promise.all([
      trie.fetchChildren(Number.MAX_SAFE_INTEGER),
      sameTrie.fetchChildren(Number.MAX_SAFE_INTEGER),
    ]);
    t.deepEqual(trie, sameTrie);
    t.is(inspect(trie), unindent`
      ╔═══════════════════════════════════════════════════════════════════╗
      ║ #4acd78f345a686361df77541b2e0b533f53362e36620a1fdd3a13e0b61a3b078 ║
      ╚═══════════════════════════════════════════════════════════════════╝
       ┌─ 0 #520a7f805c5f
       │  ├─ 389fd..[54 digits]..1abc #56408b9882f8 { mango[uid: 0] → 🥭 }
       │  └─ 9d230..[54 digits]..9ecc #9ca49c0d73d5 { lemon[uid: 0] → 🍋 }
       ├─ 16a4 #58c5e4a29601
       │  ├─ 3a30b..[51 digits]..a968 #86410153344f { cherry[uid: 0] → 🍒 }
       │  ├─ 8584c..[51 digits]..d4a5 #cda1c8929d05 { tomato[uid: 83468] → 🍅 }
       │  └─ b7ce0..[51 digits]..f157 #472d5ccbcae8 { plum[uid: 15492] → 🤷 }
       ├─ 245 #c9431d708d20
       │  ├─ 4c787..[52 digits]..c20e #e38b422bd7d9 { pineapple[uid: 12577] → 🍍 }
       │  ├─ a4f81..[52 digits]..90a3 #3e2491668264 { pomegranate[uid: 0] → 🤷 }
       │  └─ e3fc8..[52 digits]..e7c3 #eda213c9a1ca { strawberry[uid: 2532] → 🍓 }
       ├─ 3e #070a12b8b349
       │  ├─ d002d..[53 digits]..f3ac #b40093af0024 { lime[uid: 0] → 🤷 }
       │  └─ e659e..[53 digits]..b3b9 #242b464043b4 { banana[uid: 218] → 🍌 }
       ├─ 4 #79519b8cdfbd
       │  ├─ 07 #fdd60cf1b755
       │  │  ├─ 6d8ab..[52 digits]..73ef #c538c893306a { guava[uid: 344] → 🤷 }
       │  │  └─ c5847..[52 digits]..4a22 #785e20425cf9 { kiwi[uid: 0] → 🥝 }
       │  └─ a522f..[54 digits]..20cd #e0b9d1f525e3 { kumquat[uid: 0] → 🤷 }
       ├─ 5 #08434fd717ae
       │  ├─ cddcd..[54 digits]..aa9e #8a1256a87426 { watermelon[uid: 0] → 🍉 }
       │  └─ e #e26d8409cd76
       │     ├─ 7ccfe..[53 digits]..4440 #c387ec2e54f6 { yuzu[uid: 0] → 🤷 }
       │     └─ d71f9..[53 digits]..26d2 #cfcc9c732f50 { apple[uid: 58] → 🍎 }
       ├─ 78666..[55 digits]..7292 #aeb3a9f2e198 { raspberry[uid: 0] → 🤷 }
       ├─ 8af48..[55 digits]..04a8 #b27d20a5187a { tangerine[uid: 11] → 🍊 }
       ├─ a #c2f2115774c1
       │  ├─ 4b927..[54 digits]..3c69 #a6a35d200876 { peach[uid: 0] → 🍑 }
       │  └─ f12 #8ee8d620e9d6
       │     ├─ a1017..[51 digits]..50e7 #a241f4660aa4 { fig[uid: 68267] → 🤷 }
       │     └─ ec412..[51 digits]..71fe #63c036b16617 { passionfruit[uid: 0] → 🤷 }
       ├─ b #da0bdb30bf45
       │  ├─ 67e71..[54 digits]..c48b #f39b1b5089f8 { grapefruit[uid: 0] → 🤷 }
       │  └─ 88701..[54 digits]..949e #85acec96ac0f { blueberry[uid: 0] → 🫐 }
       ├─ c #a22a7b4d767a
       │  ├─ 5dc3c..[54 digits]..a3f3 #4c51531ac9d9 { cranberry[uid: 0] → 🤷 }
       │  └─ 8cac1..[54 digits]..c3ca #8e27b4cf47de { orange[uid: 0] → 🍊 }
       ├─ d #0a747d583e2e
       │  ├─ b3047..[54 digits]..502a #54d9ea3b162d { coconut[uid: 0] → 🥥 }
       │  └─ f779e..[54 digits]..678a #a82bdd8e07c2 { pear[uid: 0] → 🍐 }
       ├─ e5993..[55 digits]..c9ec #da1771d107c8 { apricot[uid: 0] → 🤷 }
       └─ f #117abf0e19fb
          ├─ 63c88..[54 digits]..21ca #62bda6837164 { papaya[uid: 0] → 🤷 }
          └─ b69c0..[54 digits]..2145 #c8e795f7b215 { grapes[uid: 0] → 🍇 }
    `);

    t.is(await trie.store.size(), 46); // 45 nodes + the root index
  }));
});


test('Trie.insert: already inserted', async t => {
  const trie = await Trie.fromList(FRUITS_LIST);

  await FRUITS_LIST.reduce(async (task, fruit) => {
    await task;
    return t.throwsAsync(
      () => trie.insert(fruit.key, '🤷'),
      { message(e) { return e.startsWith('element already in the trie') } },
      fruit.key,
    );
  }, Promise.resolve());

  const sameTrie = await Trie.fromList(FRUITS_LIST);

  t.deepEqual(await trie.save(), sameTrie);
});

// -----------------------------------------------------------------------------
// -------------------------------------------------------------------- Trie.get
// -----------------------------------------------------------------------------

test('Trie.get: empty trie', async t => {
  const trie = new Trie();
  t.is(await trie.get("foo"), undefined);
});

test('Trie.get: direct leaf', async t => {
  const trie = await Trie.fromList([{ key: 'foo', value: '14' }]);
  t.true(trie instanceof Leaf);

  t.is((await trie.get('foo')).toString(), '14');
  t.is(await trie.get('fo'), undefined);
  t.is(await trie.get('fooo'), undefined);
});

test('Trie.get: fruits', async t => {
  const trie = await Trie.fromList(FRUITS_LIST);

  t.true(Buffer.from('🥝').equals(await trie.get('kiwi[uid: 0]')));
  t.true(Buffer.from('🍌').equals(await trie.get('banana[uid: 218]')));
  t.is(await trie.get('banana[uid: 219]'), undefined);
});

test('Trie.insert: edge case fork 8', async t => {
  const proof = Proof.fromJSON(
    Buffer.from('541dddfaf11096844f045f162c0d3095597a16f711432a91e36cff007665efdc', 'hex'),
    Buffer.from('272f42edb4c1c334cb38ebbe0772dc3c10', 'hex'),
    [
      { type: 'branch', skip: 0, neighbors: 'dd79d1a4980ff4ab374d6ed011b6a9b963fe5fd141a669fb20ef0f08250c9d928bdce2239643c187358c1abf47ae78136d7d8dce94b9c2bd22f4f3b1427c90b4aed17c11bddfc6d68b809b2de8f5129c43804516b4e6d67625c5780032dc0dad97b4d37f1699995fb7572eef017468819b948c8a16ec9732479d1d70c80996cd' },
      { type: 'branch', skip: 0, neighbors: '63e2bed77aaf8034c17f5becd9d9e92c1820bd0609a0c92cf5746249270ad35ee6f92b945bbfa738a4d97409aaca86d9d44671b3da48a36dfb464501d9d147e4bffffd78b82dcef21e5d140e97318b8dfd1808014f2a02cf6057f460b203968e48554ea8b75d879edd62b154628ae8c30600b9e50434f0695ba6dd67465c8153' },
      { type: 'branch', skip: 0, neighbors: '054c6807d1047ebe0b79cd7fec4b341037400ef79a30e50a81062f4a7b8df0ef7c423847528893fe54621016e0df78000987b3d47f0b43769035d3ca844ab93893146efd718347306d9c6699a27db53997c1a06d89ad7fc741a615cf6848c1eb2e83ea7273c04abfabca2cce83614304df0c68d0189764d9f6ba29e9091404f9' },
      { type: 'fork', skip: 1, neighbor: { prefix: '', nibble: 12, root: 'cbf9b55ffdf4dbc9964cb51a01e6d66fae05bfb1704c057b8b0affb9eb8f6d3b' } },
    ]
  );

  t.is(
    proof.verify(false).toString('hex'),
    'd80496796601a1cdbad2912ba69af57185738594da390f36b65e237906808a89',
  );

  t.is(
    proof.verify(true).toString('hex'),
    '6308f158d046f2d7928fcae02b66e12831dc42f33e5d89591ae9fa7f92726e42',
  );
});


test('Trie.insert: edge case fork 7', async t => {
  const proof = Proof.fromJSON(
    Buffer.from('daa708d4b3fcf81fdfb8fce2ec5ff61fa38ff02fb4f4d9a218c158b2de170b20', 'hex'),
    Buffer.from('9fb48cf6f576d74b1d7d8917', 'hex'),
    [
      { type: "branch", skip: 0, neighbors: "7391436705a8141e333c007c5ea3e046f9b6ce3200988f4323b337f1eb4e476e300fc77899d6c430dc56965b5171ed48ae947e00cf886ed36bd508f01ecdcfd0a61383bae3451edfa124b8b4a0d6a36f9634c9dcdb9684492bc1f1962a38247ba4ea8e58b84473436d6b6fc5fd47a3abef4959544f8e57bc62ba48131198e476" },
      { type: "branch", skip: 0, neighbors: "a8c0876243c8203192c45e572b91b84654915f3015e99fbf2a50d2d48bbdacf73a1077fa66a5e7159d0971ce3192d128158480293bd98923ea6614f444c91684b55f810f03a8a710183c7ffff4272817d630c6ffae2600accdedc9f656fa9283571838701edb01d0ec362c174d12243a426af448fb909d32ed51d8641c3a43b0" },
      { type: "branch", skip: 0, neighbors: "72302f4a439c2294ba4f6bef321f0f7bf497bb5c24335f2e1c8d0b49237410297674c4a5f9437696d4ed2145aad20cc0ef39bc139574941c9f24a4023706e7720d1a0c3d36e6748cabab8c24cb83a17b4a771f536a9fd361e1416f673ed43708b61ff685cecf3bd4a6118e3994e36e41e8dcaee8b47b2ea947968c0afca65b6e" },
      { type: "branch", skip: 0, neighbors: "f226865e02694067e1d0a17b3cb0f6c3d7e5186642a3ff1d8299573e3cac04673fced676fe9af960d3ed3d1e6138952993109b7ec62a3f38eae39fb89a06f04436b86983490a9c2488d8b690074fb3b6a487049f21b6de07dd27b8cfb6243fc3ab5d438a30e24aee9016ffb83a2c23ed7f316efac775c6c2eec64f41967e63c2" },
      { type: "fork", skip: 1, neighbor: { nibble: 11, prefix: "0e", root: "8ffc29f174b749ee61bc9048cb600b4b7b9379227cf690a9268ffa26c5973738" } },
    ]
  );

  t.is(
    proof.verify(false).toString('hex'),
    '5032a544857633269c915dd4fb665d79a041d6d75ca795e24fc17a285cc1dece',
  );

  t.is(
    proof.verify(true).toString('hex'),
    'b4b1446e07f17da9643a597e5b3a805bc75307aec8a40edde1e41b22ffb90442',
  );
});

test('Trie.insert: edge case fork 6', async t => {
  const proof = Proof.fromJSON(
    Buffer.from('5247268e194dad520a2ab88837e2c110fbe290fa3ee8e09b8fe10402b7f9e906', 'hex'),
    Buffer.from('4852f1a645b5c206f9b97f679af0e1629954f73d43080a5ebb9bf54f12131b', 'hex'),
    [
      { type: 'branch', skip: 0, neighbors: '7e5c1b89d6ba9e8e5a012933aecd9e2e19f11294257e0e640e56c6845219261a4b1698ae92f74ac507f5680ca387481abd062fa046eb010b7b418153b9988fcf5bb476f0fc4b22846acdcf91c26b8405a09e21af7441dfe98e144b7e6f7475ce14a7917333baaf877fa3e6562e09e9e5539bb5731c89006ba3baf3317ea7cfd3' },
      { type: 'branch', skip: 0, neighbors: '4945551523513a913b0c1748f0087865b5ef401247edca73d8ce823cedc62003ed7b7f4cfcedd277e93783a47475a8ec717761fbb2274bc09e00ea1a5fbcecb4b524a3388171fafdd68d4422401a634bbd85b7bab7260585112368ccb4dfcdc8470742a8dd6e058a84e82d5842ebfad6af4a0add6f167106e3c8bf8274e3f346' },
      { type: 'branch', skip: 0, neighbors: 'ccd2c106aeec88ac21a434fde3a80f6723bdaec66dbbf7b48798444176a66b313666175fd5c77515a487b92353e8b38975faa4fe8c781d20dbcf8e047dd4fb65ef96a189a702eba153af570fcd3fe5fcc246291520c8be76a28a513e0af3df2c1c0a21c307b69cc6de481a61fb485d0b6749347e2536c50f55ce95f4c8aeb9df' },
      { type: 'branch', skip: 0, neighbors: 'e6100e9747bf02da887ba2f15c77d151a6dafe424bfc2e4a575ce3be326d98938e810047967285276146d6cec58822f67af42393612eb7bf7f3419f20f7ddaab74699bfa72ad77a7e24e80960c3913a02dbc1300aedf4efef7448a1817eaaac0739f0ca36791ab54cb58545461e737e19eda3fddcbb16f771b28987206f90778' },
      { type: 'branch', skip: 0, neighbors: '13e7ec23fb284cde60f0c5434f220256c0fcab29390bbb44be517239f63739e093b12de2213d6991bfed2f7a7a9204382975b655347e1b3c22fb35791a3ab6f60eb923b0cbd24df54401d998531feead35a47a99f4deed205de4af81120f9761807800df400ecd72d0f96b90289c6c9e30745c0f0f1b87dda505807bf113cd18' },
      { type: 'fork',   skip: 1, neighbor: { prefix: '0c', nibble: 9, root:   '03be633c718bddfa31a6cc2988f933a12fe3d630ce46981090a673023c550a75' } },
    ]
  )

  t.is(
    proof.verify(false).toString('hex'),
    '264bf8da18ca79f8da1b3907d7587a5933599c113e9fb6a43e4659dcaa7dfb14'
  )
  t.is(
    proof.verify(true).toString('hex'),
    'b59233f804a519ea064071251d1f948cd129543c2ece3932f001a593c99409dc'
  )
})

test('Trie.insert: edge case fork 5', async t => {
  const proof = Proof.fromJSON(
    Buffer.from('e4a80cba017707ab102628bb4edd6f463ad3f5592c537c69b5d7f4f6dbae5c63', 'hex'),
    Buffer.from('61255c62f82fa03634e7f2be38b0589ed878c7293e41', 'hex'),
    [
      { type: 'branch', skip: 0, neighbors: '83a353b3dfac1b00ec6333120a92996ce6e715029a72c9f224731b83d2d0da467dab0e24c4582fde651ef4f6fc292de9f674587c72439c2ebe10c5d8e0dd993cdb010bae8afbb2ef4563525f7f6df6250069efc4c6a7cfed090aa4de8a0b0943a9d4662d42439fdc9b0886bac8437ac6f870eb740c0aaac8900f0f07f6ca627c' },
      { type: 'branch', skip: 0, neighbors: 'fd83b02dec57c8d7c9c5709d216d7ece94aede0be367de0fba419efe0ea14fb5c183494e24e67b0ada22146e6c274ef2e615e31ef16a8d8e87001a3564597430615bb2668745780b8800fc1267f73736d76e804c292baf556edb21f6554b5277d6887507e528c0a75cbd0663e89ce6c1d58d425a5892588a1ad7d722c150ad35' },
      { type: 'branch', skip: 0, neighbors: 'fc880494853e132b1e5059ef097ac8052406e91c497a4b3220d461aebe24bc8011cb48bbb1fc48f83dcbdabacbadd5252b5a4aa9ce0cb11e469a487e5185e29a889fea6ad82b59c6b6545f1eb07ec79cef0b49996a0ed1f93b4d7e0fd19120bbced4fd32785b26b24acc48b334dc9cbb57d131811b209bd34e1c2eabdddd824b' },
      { type: 'branch', skip: 0, neighbors: '2ac6a5bf86058b926969d57744a861c1ed0354e4a8597b30286da55ad65bc70888f678a79d87971609b0ebcb59957b2fb7a49b974988b622b609a172a22b07113c8d0c0385974874a96f9d2a8bf40b887f1a2770e10e2ad932452625dd092e32eeb0ecc037555a8be1b495c51f193e9e4a2b0394d508d985c368f76f41845c30' },
      { type: 'branch', skip: 0, neighbors: '55b2b16593856d4f82e890477c446e0f6701065c0f8211d209e2c51ea140b4ef975439d6994d2a1320b64cd109fe3ba5f2e3cc6a1d901e18d4146c058294a0fd5199a4a23258ebe527b0f845549ae5b7c6abfb6a3ce51065788b24e0d3b6df590000000000000000000000000000000000000000000000000000000000000000' },
      { type: 'fork',   skip: 1,
        neighbor: {
          prefix: '06',
          nibble: 2,
          root:   'afa03b4357df9c84cadd1dc8c488d01718a35f4dd467f4a9be010f09f1cfa5bc',
        } },
    ]
  )

  t.is(
    proof.verify(false).toString('hex'),
    '7dc784628201add8ee3e19eef299af13d8be6b46189b5bfd95c8ab331e97bb00'
  )
  t.is(
    proof.verify(true).toString('hex'),
    '5c788efef5680d2ce466fa44943999262f5215d29263cf46069eb9a49728125f'
  )
})

test('Trie.insert: edge case fork 4', async t => {
  const proof = Proof.fromJSON(
    Buffer.from('04811fc306a2021340b15ce6f025db1dc3d402f0829c7ee2100ca8fdd6ed10cd', 'hex'),
    Buffer.from('0c43c3addce8b95e49eb0fb906', 'hex'),
    [
      { type: 'branch', skip: 0, neighbors: 'd072e11c4f761d09ebe0c1df54b08d398977aa4e98e85e5e231f52dc32fdf8053861a5ea164ac3eb460e27f96ba934832bfc7b240dbf7be24d3fb7ae16f3e44fa965498aa2e219f45428bafc4f646a8f2b4d863bf730f802f81f4f713a465246cd28ad53627981fd212ebec41068fa0f4b0ae5e0e77af0143e296373c6c8f753' },
      { type: 'branch', skip: 0, neighbors: '6c2cf6703c1b121726899e4f1de29cf483227d9e75d5d7948b62b5904c7f1011165b8313abcd4f1c33b85a5dabf8c5096039b3aba1c1fedda2e247810090173998f6f58a03bc17874bff8ba7eda08d25623911dff348f57da60b8545044dcbb175d27abc4c3e1b9aa0a3161ea0f8067ef39885c30399c164395b181747ba4f51' },
      { type: 'branch', skip: 0, neighbors: 'c5b1eb4266a20e13961f0b7b8f909a217141eecab5bbe3116665e382f87477fcf9a8a6a9e1e1cb7af32d1ffdf5c70643434337c3874d417de45f83e48f7c00afaf7180e918199dde712083a3f512483e89d756f25ddafe8b14b246499fe44dd3bda1f1a580cf7af9dd35c6ddfffa2ec8af0d41b00d7ca5ed25af8e54d4bef1f9' },
      { type: 'fork',   skip: 1, neighbor: { prefix: '', nibble: 12, root:   '136bca071d530710ba622dfd66fe1afb859d4f42d45f29ce252e862a92eb10c2' } },
    ]
  )

  t.is(
    proof.verify(false).toString('hex'),
    '76ff3670f2b81017d50354ca4a78792de31adbd23f456eec41d7a8c13fcdc91b'
  )
  t.is(
    proof.verify(true).toString('hex'),
    'a6eb3cdf9dd3da02d9463bd5cd68555ea11d6d5a77e2ece9ceb1cf6a5a9c7b27'
  )
})

test('Trie.insert: edge case fork 3', async t => {
  const proof = Proof.fromJSON(
    Buffer.from('497f99bb7565d7be2828f6580161cd27cdf8f56418adde5be871b6d0a447da15', 'hex'),
    Buffer.from('85ff67896b3cc0f2c866bef1c51e6c00055d059cd00067c10a49c74d24277c', 'hex'),
    [
      { type: 'branch', skip: 0, neighbors: '9adb4faf00cb2666b1b18dd461cbedfeb51f2d95e4158c96c222ebe84d91391eec30cd78944a11d044682818143d22b96fc31932aa2c52e00b1888c65e56dab8a53c54b7d9170432cd45530cb4f23b20d073fceaf910c296d72446eb780b12175c057d3a7768e6460dc367abaeb396095594a6cde5068afbbbd1268c0fd36d27' },
      { type: 'branch', skip: 0, neighbors: 'd9701403f8a22c78d8ea0aa13580427d33a09ad207000f96d2c8cc3f9049792a17d5489236013aba204e657d142a4cc1f92b98e6d8ce31dba37eb355a98befe64a1ef3c786c8bdefff9c6ff870a6db9f86115b4760396b91abe622de3f29d85618da02ae1a2daa6ef05164669740c58d4af735eca7129a5e7b12490508eef65f' },
      { type: 'branch', skip: 0, neighbors: '888502234def2d4ab5ce331577c00537350807cc6411f4a713db6e7c39da756de6665ece82216244b78d1ad2218775994977d8d8337f4d0d11d3f444b80d447373dac7d204349e68dd5d4303d169f22981d016b62d7f3295284c046b70bc87fc0000000000000000000000000000000000000000000000000000000000000000' },
      { type: 'fork',   skip: 1, neighbor: { prefix: '', nibble: 9, root:   '34b8236af8370a93aa648a541efebae35dec9488f6160e324e656af0be5d374a' } },
    ]
  )

  t.is(
    proof.verify(false).toString('hex'),
    '6544f125947f9b41d6e6ad0560f7174836d987fdb404df3f379985a2f661e4b9'
  )
  t.is(
    proof.verify(true).toString('hex'),
    'b6c45bc7651f957c7c6957e1c10439ad5878ef5c7bc147c8ab6b7e163bd32554'
  )
})

test('Trie.insert: edge case leaf 2', async t => {
  const proof = Proof.fromJSON(
    Buffer.from('198d70e41146654a69e08c6682310a8c35816c8584431915a0eee4a62d39eda0', 'hex'),
    Buffer.from('9e36f867a374be', 'hex'),
    [
      { type: 'branch', skip: 0, neighbors: '4c54bfc322fb7bc2e49ae21bf5fa560632e3ca42b5267eb115142e291e8ada4ecd0c58152bf064f0c7834dd72f69d12651739b32caaa3c986a87937f125b500f1426fccf2a456bce3c25b43206d9b429d56515580d086a959ca730325411b3aada6ac4d7221f787b97e1ce677fdadc412e824a9816281b1259b91addeb37bb2c' },
      { type: 'branch', skip: 0, neighbors: '098745f495c99b7627f559ac8ed8165e2392e2261ef8990291f13705adf78fcf3dcca881d4b45aabe746e7041f743baaa831029e7890df9587858d8be5dce648e02f31fe2936417a393df8def15d7d0c021a66cdb33c3fdda941ae70614913cb116fd5e6c499b71e229b88f5106975cbe83a8c44d3619541d7ddd7eae0a355bc' },
      { type: 'branch', skip: 0, neighbors: '9732c3266e468dd27c4bd16af5a6e60c1f556bf91700f51554cfa33aa26b8d30f33c27ab7c5c85ef006c78f56ecd7e8c77c5fadd7910e9b178801d554f244977026104fc4aede0864d405db792691c4e4534b06ae7f58366b640f13ecfa549afa046a157d2e9b6c0793a506942eb8ff50dfeb7c5e7a2a51814c4b3a4d6af6fa0' },
      { type: 'branch', skip: 0, neighbors: '5f3065e998b5fa89bb33d9204546c5dba2b075adc542688dcc1773a490fa739ac69ff52c5f575e9f1912664c1ebef2f9498775350b0077a6b59fe012861c3715657146a239aaea12b3091054e5846771bba6f721b1835d025fa08d1fc5c9b1c40000000000000000000000000000000000000000000000000000000000000000' },
      { type: 'leaf',   skip: 1, neighbor: { key: '2b5b0ba7a99e17d9fde58f14dee61cccda9e3e9627b2ba2732ebed551ea9eaa4', value: '3657998959985b7b75c734eb5b49d18cae9b353d00d811cb2c24ed6ed17b23d9' } },
      { type: 'leaf',   skip: 0, neighbor: { key: '2b5b063719f4b7644c71adef1439c9aa78d34e684677dd61db0adffcc21797ec', value: '4e397303e05277d98701446ee62f6f02bc013721fc12efba7300fb51ea935f9f' } },
    ]
  )

  t.is(
    proof.verify(false).toString('hex'),
    '00489b47aa866ff55da4f24fa4801a6948871258fab39f22354f35b7c4f94412'
  )
  t.is(
    proof.verify(true).toString('hex'),
    'b76dd0926602d6e9d28a0b3707db4622184d59c7392f5a0469bf775d9aa05f33'
  )
})

test('Trie.insert: edge case leaf 1', async t => {
  const proof = Proof.fromJSON(
    Buffer.from('3fe6f46456b9c223116533d90f9b0bf7c5da095e0c1d68af297a2a3c9709bfa7', 'hex'),
    Buffer.from('', 'hex'),     // value is an empty byte-string
    [
      { type: 'branch', skip: 0, neighbors: '6da036230d1cdb614137b0d5a94bfe0350eae80a7a6228e1ada0025b3c4f7b7b5527cc2fa7d7d50e6059ef33bb9d71f4135265d016affaaebc48465275528b4cc47a765a2d0a90fa7efe6c4c2afb227f8fafa193d16b98afd8e0536d8f07beef9c989638ac0ebb91ce40562b449f66d80119354630bfbd3d1f51db2369a10c7b' },
      { type: 'branch', skip: 0, neighbors: 'd1672f79764d1e73c9784121bfcc4b77a043dd07d5769c9a041b04f421572cddb53b70e36b1ae1568c438bdd94eb7d209973e669639bf970b2356b98f4f23bbc66a5aeefc3e6796bd5559a1eba9e61a86eab048c18ec8f93a787c8ea7893c010828b7a5a3d83c8f64471a9a93a606591c3823f9b718870d1bb30f99e38cbac9b' },
      { type: 'branch', skip: 0, neighbors: 'dca400d830a111355a23e3c85ebccb507a7150bc26a259fce184ce63b0ec917ce2d43e656aafa0f3de6381d4c0ef65a76c263598eaf76156819bd2c321504d808b0dbee17ff39324bb913eecd66b8f49238000c1d0c22af719d51fe0f676d23e0000000000000000000000000000000000000000000000000000000000000000' },
      { type: 'leaf',   skip: 1, neighbor: { key: '5080c2f95315f3ef1f89304d94651f0f8ae2f80daa5cff26b9a7fd27813eae0b', value: '0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8' } },
      { type: 'leaf',   skip: 0, neighbor: { key: '508010f4051f83d17de96eab544cd32a977e88fbe5a4b3b1274b11cce8aaf642', value: '0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8' } },
    ]
  )

  t.is(
    proof.verify(false).toString('hex'),
    '08b05f422582a099e63646ccf6ed5993c1718d0279a3269c140c1daed29f0f4b'
  )
  t.is(
    proof.verify(true).toString('hex'),
    'ff55eab671a6e5618b10bb8702e3e5e6ab2491d50e4a93dd5255d8666a8d4e9a'
  )
})

// -----------------------------------------------------------------------------
// ----------------------------------------------------------------- Trie.delete
// -----------------------------------------------------------------------------

test('Trie.delete: from Leaf', async t => {
  const trie = await Trie.fromList([{ key: 'foo', value: '14' }]);
  t.true(trie instanceof Leaf);

  await trie.delete('foo');

  t.true(trie.isEmpty());
  t.true(trie instanceof Trie);
});


test('Trie.delete: from Branch with 2 neighbors', async t => {
  const trie = await Trie.fromList([
    { key: 'foo', value: '14' },
    { key: 'bar', value: '42' },
  ]);
  t.true(trie instanceof Branch);

  await trie.delete('foo');

  t.true(trie instanceof Leaf);
  t.is(trie.size, 1);
  t.is(await trie.store.size(), 2);
});

test('Trie.delete: from Branch with 2+ neighbors', async t => {
  const trie = await Trie.fromList([
    { key: 'foo', value: '14' },
    { key: 'bar', value: '42' },
    { key: 'baz', value: '27' },
  ]);
  t.true(trie instanceof Branch);

  await trie.delete('foo');

  t.true(trie instanceof Branch);
  t.is(trie.size, 2);
  t.is(await trie.store.size(), 4);
});

test('Trie.delete: whole trie in any order', async t => {
  const initial_size = FRUITS_LIST.length;

  await Promise.all([undefined, new Store(tmpFilename())].map(async store => {
    const trie = await shuffle(FRUITS_LIST).reduce(async (trie, fruit, ix) => {
      trie = await trie;
      t.is(trie.size, initial_size - ix);
      t.true(trie === await trie.delete(fruit.key));
      await t.throwsAsync(() => trie.prove(fruit.key));
      await t.throwsAsync(() => trie.delete(fruit.key));
      return trie;
    }, Trie.fromList(FRUITS_LIST, store));

    t.true(trie.isEmpty())
    t.is(trie.size, 0);

    t.false(trie instanceof Leaf);
    t.false(trie instanceof Branch);

    const root = await trie.store.get('__root__', (_, x) => Buffer.from(x, 'hex'));

    t.true(root.equals(helpers.NULL_HASH));
    t.is(trie.hash, null);
    t.is(await trie.store.size(), 1);
  }));
});

// -----------------------------------------------------------------------------
// ------------------------------------------------------------------ Trie.prove
// -----------------------------------------------------------------------------

test('Trie: can create proof for leaf-trie for existing element', async t => {
  const trie = await Trie.fromList([{ key: 'foo', value: '14' }]);
  const proof = await trie.prove('foo');
  t.is(
    proof.verify().toString('hex'),
    trie.hash.toString('hex'),
  );
});

test('Trie: cannot create proof for leaf-trie for non-existing elements', async t => {
  const trie = await Trie.fromList([{ key: 'foo', value: '14' }]);
  await t.throwsAsync(() => trie.prove('bar'), {
    message(e) { return e.includes('not in trie') },
  });
});

test('Trie: can create proof for simple tries', async t => {
  const pairs = [
    { key: 'foo', value: '14' },
    { key: 'bar', value: '42' },
  ];

  const trie = await Trie.fromList(pairs);
  t.is(trie.size, 2);
  await trie.fetchChildren(1);
  t.is(inspect(trie), unindent`
    ╔═══════════════════════════════════════════════════════════════════╗
    ║ #69509862d51b65b26be6e56d3286d2ff00a0e8091d004721f4d2ce6918325c18 ║
    ╚═══════════════════════════════════════════════════════════════════╝
     ┌─ 84418..[55 digits]..e71d #b310552e86bf { bar → 42 }
     └─ b8fe9..[55 digits]..49fd #95bb7f919c90 { foo → 14 }
  `);

  const proofs = {
    foo: await trie.prove('foo'),
    bar: await trie.prove('bar'),
  };

  t.true(proofs.foo.verify().equals(trie.hash));
  t.true(proofs.bar.verify().equals(trie.hash));

  await t.throwsAsync(() => trie.prove('fo'));
  await t.throwsAsync(() => trie.prove('ba'));
  await t.throwsAsync(() => trie.prove('foobar'));
});

test('Trie: checking for membership & insertion on complex trie', async t => {
  const trie = await Trie.fromList(FRUITS_LIST);

  t.is(trie.size, 30);

  FRUITS_LIST.forEach(async fruit => {
    const proof = await trie.prove(fruit.key);

    // Prove membership
    t.true(proof.verify(true).equals(trie.hash), fruit.key);

    const trieWithout = await Trie.fromList(FRUITS_LIST.filter(x => x.key !== fruit.key));

    // Prove insertion
    t.true(proof.verify(false).equals(trieWithout.hash), fruit.key);

    // For (re-)generating Aiken code for proofs.

    // const fruitName = fruit.key.split("[")[0];
    // console.log(`// ---------- ${fruitName}\n`);
    // console.log(`const ${fruitName} = "${fruit.key}"`);
    // console.log(`fn proof_${fruitName}() {\n${proof.toAiken()}\n}\n`);
    // console.log(`fn without_${fruitName}() {\n  mpf.from_root(#"${trieWithout.hash.toString('hex')}")\n}\n\n`);
  });
});

// -----------------------------------------------------------------------------
// -------------------------------------------------------------- Non-membership
// -----------------------------------------------------------------------------

test('Trie: can prove non-membership', async t => {
  const trie = await Trie.fromList(FRUITS_LIST);

  const element = { key: "melon", value: "🍈" };

  // Can build and verify an exclusion proof.
  const proof = await trie.prove(element.key, true);
  t.true(proof.verify(false).equals(trie.hash));

  // The proof cannot work for nearby element nor any other key in the trie.
  FRUITS_LIST.forEach(fruit => {
    const falseProof = Proof.fromJSON(fruit.key, undefined, proof.toJSON());
    t.false(falseProof.verify(false).equals(trie.hash));
  });

  // The proof would work for an element similarly positioned in the trie.
  const usurper = "usurper [uid: 447]";
  t.is(
    helpers.intoPath(element.key).slice(0, 2),
    helpers.intoPath(usurper).slice(0, 2),
  );
  const steps = proof.toJSON();
  t.is(
    steps.reduce((acc, { skip }) => acc + 1 + skip, 0),
    2,
  );
  t.true(Proof.fromJSON(usurper, undefined, steps).verify(false).equals(trie.hash));

  // The proof can be reused, once assigned a value, for inclusion.
  await trie.insert(element.key, element.value);
  t.throws(
    () => proof.verify(true),
    { message(e) { return e.includes('attempted to verify an inclusion proof without value') } },
  );
  proof.setValue(element.value);
  t.true(proof.verify(true).equals(trie.hash));

  // The proof doesn't compute in exclusion anymore.
  t.false(proof.verify(false).equals(trie.hash));
});

test('Trie: cannot alter non-membership proof', async t => {
  const tangerine = "tangerine[uid: 11]";

  const trie = await Trie.fromList(FRUITS_LIST.filter(({ key }) => key !== tangerine));

  let proof = await trie.prove(tangerine, true);
  t.true(proof.verify(false).equals(trie.hash));

  const path = helpers.intoPath(tangerine);
  const json = proof.toJSON();

  t.is(path[0], '8');
  t.is(path[4], '8');

  json[0].skip = 4; // land on a '8', but with a different prefix.

  proof = Proof.fromJSON(tangerine, undefined, json);
  t.false(proof.verify(false).equals(trie.hash));
});

test('Trie: cannot prove non-membership of members', async t => {
  await Promise.all([undefined, new Store(tmpFilename())].map(async store => {
    const trie = await Trie.fromList(FRUITS_LIST);
    return Promise.all(FRUITS_LIST.map(async fruit => {
      const proof = await trie.prove(fruit.key, true);
      t.false(proof.verify(false).equals(trie.hash));
    }));
  }));
});

// -----------------------------------------------------------------------------
// ---------------------------------------------------------------- Proof.toJSON
// -----------------------------------------------------------------------------

test('Proof.toJSON (mango)', async t => {
  const trie = await Trie.fromList(FRUITS_LIST);
  const proof = await trie.prove('mango[uid: 0]');
  t.deepEqual(proof.toJSON(), [
    {
      neighbors: 'c7bfa4472f3a98ebe0421e8f3f03adf0f7c4340dec65b4b92b1c9f0bed209eb45fdf82687b1ab133324cebaf46d99d49f92720c5ded08d5b02f57530f2cc5a5f1508f13471a031a21277db8817615e62a50a7427d5f8be572746aa5f0d49841758c5e4a29601399a5bd916e5f3b34c38e13253f4de2a3477114f1b2b8f9f2f4d',
      skip: 0,
      type: 'branch',
    },
    {
      neighbor: {
        key: '09d23032e6edc0522c00bc9b74edd3af226d1204a079640a367da94c84b69ecc',
        value: 'c29c35ad67a5a55558084e634ab0d98f7dd1f60070b9ce2a53f9f305fd9d9795',
      },
      skip: 0,
      type: 'leaf',
    },
  ]);
});

test('Proof.toJSON (kumquat)', async t => {
  const trie = await Trie.fromList(FRUITS_LIST);
  const proof = await trie.prove('kumquat[uid: 0]');
  t.deepEqual(proof.toJSON(), [
    {
      neighbors: 'c7bfa4472f3a98ebe0421e8f3f03adf0f7c4340dec65b4b92b1c9f0bed209eb47238ba5d16031b6bace4aee22156f5028b0ca56dc24f7247d6435292e82c039c3490a825d2e8deddf8679ce2f95f7e3a59d9c3e1af4a49b410266d21c9344d6d08434fd717aea47d156185d589f44a59fc2e0158eab7ff035083a2a66cd3e15b',
      skip: 0,
      type: 'branch',
    },
    {
      neighbor: {
        nibble: 0,
        prefix: '07',
        root: 'a1ffbc0e72342b41129e2d01d289809079b002e54b123860077d2d66added281',
      },
      skip: 0,
      type: 'fork',
    },
  ]);
});

// -----------------------------------------------------------------------------
// ---------------------------------------------------------------- Proof.toCBOR
// -----------------------------------------------------------------------------

test('Proof.toCBOR (mango)', async t => {
  const trie = await Trie.fromList(FRUITS_LIST);
  const proof = await trie.prove('mango[uid: 0]');
  t.true(proof.toCBOR().equals(Buffer.from('9fd8799f005f5840c7bfa4472f3a98ebe0421e8f3f03adf0f7c4340dec65b4b92b1c9f0bed209eb45fdf82687b1ab133324cebaf46d99d49f92720c5ded08d5b02f57530f2cc5a5f58401508f13471a031a21277db8817615e62a50a7427d5f8be572746aa5f0d49841758c5e4a29601399a5bd916e5f3b34c38e13253f4de2a3477114f1b2b8f9f2f4dffffd87b9f00582009d23032e6edc0522c00bc9b74edd3af226d1204a079640a367da94c84b69ecc5820c29c35ad67a5a55558084e634ab0d98f7dd1f60070b9ce2a53f9f305fd9d9795ffff', 'hex')));
});

test('Proof.toCBOR (kumquat)', async t => {
  const trie = await Trie.fromList(FRUITS_LIST);
  const proof = await trie.prove('kumquat[uid: 0]');
  t.true(proof.toCBOR().equals(Buffer.from('9fd8799f005f5840c7bfa4472f3a98ebe0421e8f3f03adf0f7c4340dec65b4b92b1c9f0bed209eb47238ba5d16031b6bace4aee22156f5028b0ca56dc24f7247d6435292e82c039c58403490a825d2e8deddf8679ce2f95f7e3a59d9c3e1af4a49b410266d21c9344d6d08434fd717aea47d156185d589f44a59fc2e0158eab7ff035083a2a66cd3e15bffffd87a9f00d8799f0041075820a1ffbc0e72342b41129e2d01d289809079b002e54b123860077d2d66added281ffffff', 'hex')));
});

// -----------------------------------------------------------------------------
// --------------------------------------------------------------- Proof.toAiken
// -----------------------------------------------------------------------------

test('Proof.toAiken (mango)', async t => {
  const trie = await Trie.fromList(FRUITS_LIST);
  const proof = await trie.prove('mango[uid: 0]');
  t.is(proof.toAiken(), unindent`
    [
      Branch { skip: 0, neighbors: #"c7bfa4472f3a98ebe0421e8f3f03adf0f7c4340dec65b4b92b1c9f0bed209eb45fdf82687b1ab133324cebaf46d99d49f92720c5ded08d5b02f57530f2cc5a5f1508f13471a031a21277db8817615e62a50a7427d5f8be572746aa5f0d49841758c5e4a29601399a5bd916e5f3b34c38e13253f4de2a3477114f1b2b8f9f2f4d" },
      Leaf { skip: 0, key: #"09d23032e6edc0522c00bc9b74edd3af226d1204a079640a367da94c84b69ecc", value: #"c29c35ad67a5a55558084e634ab0d98f7dd1f60070b9ce2a53f9f305fd9d9795" },
    ]
  `);
});

test('Proof.toAiken (kumquat)', async t => {
  const trie = await Trie.fromList(FRUITS_LIST);
  const proof = await trie.prove('kumquat[uid: 0]');
  t.is(proof.toAiken(), unindent`
    [
      Branch { skip: 0, neighbors: #"c7bfa4472f3a98ebe0421e8f3f03adf0f7c4340dec65b4b92b1c9f0bed209eb47238ba5d16031b6bace4aee22156f5028b0ca56dc24f7247d6435292e82c039c3490a825d2e8deddf8679ce2f95f7e3a59d9c3e1af4a49b410266d21c9344d6d08434fd717aea47d156185d589f44a59fc2e0158eab7ff035083a2a66cd3e15b" },
      Fork { skip: 0, neighbor: Neighbor { nibble: 0, prefix: #"07", root: #"a1ffbc0e72342b41129e2d01d289809079b002e54b123860077d2d66added281" } },
    ]
  `);
});

// -----------------------------------------------------------------------------
// --------------------------------------------------------------- Fuzzy testing
// -----------------------------------------------------------------------------

const FUZZ_MAX_ITERATION = 500;

const aiken = {
  insert: aikenFFI("insert"),
  remove: aikenFFI("delete"),
  miss: aikenFFI("miss"),
};

test('Fuzz', async t => {
  const trie = new Trie();

  for (let i = 0; i < FUZZ_MAX_ITERATION; i += 1) {
    const key = randomBytes(2);
    const value = randomBytes(4);

    let previousRoot = trie.hash;

    const oldValue = await trie.get(key);

    if (oldValue !== undefined) {
      // Key already exist, we extract a proof, and delete it.
      const proof = await trie.prove(key);
      await trie.delete(key);
      t.true(proof.verify(true).equals(previousRoot))
      t.true(trie.hash === null || proof.verify(false).equals(trie.hash))

      // Also check that the on-chain code can compute the removal
      t.true(
        aiken.remove(previousRoot, key, oldValue, proof.toUPLC())
          .equals(trie.hash)
      );

      previousRoot = trie.hash;

      // Add the new value.
      await trie.insert(key, value);
      proof.setValue(value);
      t.true(proof.verify(true).equals(trie.hash))
      t.true(
        aiken.insert(previousRoot, key, value, proof.toUPLC())
          .equals(trie.hash)
      );
    } else {
      // Key doesn't exists. Let's prove it's not in the trie first.
      let proof = await trie.prove(key, true);
      const proofRoot = proof.verify(false);
      t.true(proofRoot === null && trie.hash === null || proofRoot.equals(trie.hash));
      t.true(aiken.miss(trie.hash ?? helpers.NULL_HASH, key, proof.toUPLC()))

      // Add it.
      await trie.insert(key, value);

      // Check both inclusion and exclusion proofs for this element
      proof = await trie.prove(key);
      t.true(previousRoot === null || proof.verify(false).equals(previousRoot));
      t.true(proof.verify(true).equals(trie.hash));

      // Also check that the on-chain code can compute the insertion.
      t.true(
        aiken.insert(previousRoot ?? helpers.NULL_HASH, key, value, proof.toUPLC())
          .equals(trie.hash)
      );
    }
  }
});


// -----------------------------------------------------------------------------
// ---------------------------------------------------------------- Test Helpers
// -----------------------------------------------------------------------------

function unindent(str) {
  const lines = str[0].split('\n').filter(n => n.length > 0);
  const n = (lines[0] || '').length - (lines[0] || '').trimStart().length;
  return lines.map(s => s.slice(n)).join('\n').trimEnd();
}

function shuffle(xs) {
  return xs
    .map(origin => ({ origin, score: Math.random() }))
    .sort((a, b) => a.score - b.score)
    .map(({ origin }) => origin);
}

function tmpFilename() {
  return path.join(os.tmpdir(), `merkle-patricia-forest-db_${randomBytes(8).toString('hex')}`);
}

function aikenFFI(fnName) {
  let cmd = `aiken export --module aiken/merkle_patricia_forestry --name ${fnName}_ffi 2>/dev/null`;
  const opts = { cwd: path.join(import.meta.dirname, "../../on-chain") };
  const fn = JSON.parse(cp.execSync(cmd, opts)).compiledCode;
  const filename = path.join(os.tmpdir(), `${fnName}_ffi.cbor`);
  fs.writeFileSync(filename, fn);
  return function(...args) {
    args = args.map(arg => {
      if (arg instanceof Buffer) {
          return `'(con data (B #${arg.toString('hex') }))'`;
      }
      return `'${arg}'`;
    });
    cmd = `aiken uplc eval --cbor ${filename} ${args.join(" ")}`
    const result = JSON.parse(cp.execSync(cmd, opts)).result;
    return result.startsWith("(con bool")
      ? result === "(con bool True)"
      : Buffer.from(result.slice(21, 86), 'hex');
  }
}
