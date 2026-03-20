import assert from 'node:assert';
import * as buffer from 'node:buffer';
import { inspect } from 'node:util';
import { DIGEST_LENGTH, digest } from './crypto.js'
import {
  NULL_HASH,
  assertInstanceOf,
  commonPrefix,
  eachLine,
  intoPath,
  merkleProof,
  merkleRoot,
  nibble,
  nibbles,
  sparseVector,
  withEllipsis,
} from './helpers.js'
import { Store } from './store.js';
import * as cbor from './cbor.js';


// -----------------------------------------------------------------------------
// ------------------------------------------------------------------- Constants
// -----------------------------------------------------------------------------

/* Number of nibbles (i.e. hex-digits) to display for intermediate hashes when
 * inspecting a {@link Trie}. @private
 */
const DIGEST_SUMMARY_LENGTH = 12; // # of nibbles

/* Maximum number of nibbles (i.e. hex-digits) to display for prefixes before
 * adding an ellipsis @private
 */
const PREFIX_CUTOFF = 8; // # of nibbles

/* A special database key for storing and retrieving the root hash of the trie.
 * This is useful to ensure that the root hash doesn't get lost, and with it,
 * the entire trie. @private
 */
const ROOT_KEY = '__root__';

// -----------------------------------------------------------------------------
// ------------------------------------------------------------------------ Trie
// -----------------------------------------------------------------------------

/** A Merkle Patricia Forestry is a modified Merkle Patricia Trie of radix 16
 *  whose neighbors are stored using Sparse Merkle Trees.
 *
 *  The class {@link Trie} is used as a super-class for {@link Branch} and
 *  {@link Leaf}. One shouldn't use the latters directly and prefer methods from
 *  {@link Trie}.
 */
export class Trie {
  /** The root hash of the trie.
   *
   * @type {Buffer}
   */
  hash;

  /** The size of the trie; corresponds to the number of nodes (incl. leaves)
   * in the trie
   *
   * @type {number}
   */
  size;

  /** A hex-encoded string prefix, if any.
   *
   * @type {string}
   */
  prefix;

  /** A local in-memory or on-disk store. We only keep top-level nodes in
   * memories. Children are only fetched on request or when needed, and
   * discarded once done.
   *
   * @type {Store}
   */
  store;


  /** Construct a new empty trie. This constructor is mostly useless. See
   * {@link Trie.fromList} or {@link Trie.load} instead.
   *
   * @param {Store} [store]
   *   The trie's store, default to an in-memory store if omitted.
   */
  constructor(store = new Store(), hash = null, prefix = '', size = 0) {
    assertInstanceOf(Store, { store });
    this.hash = hash;
    this.prefix = prefix;
    this.size = size;
    this.store = store;
    this.isRoot = hash === null;
  }


  /**
   * @param {Store} [store]
   *   The data-store to use for storing and retrieving the underlying trie.
   *
   * @return {Promise<Trie>}
   * @private
   */
  static async from(store) {
    const trie = new Trie(store);
    return trie.save();
  }


  /** Load a trie from disk.
   *
   * @param {Buffer} hash
   *   The hash
   * @param {Store} store
   *   The store to load the Trie from.
   */
  static async load(store) {
    const root = await store.get(ROOT_KEY, (_, str) => Buffer.from(str, 'hex'));
    const trie = root.equals(NULL_HASH)
      ? new Trie()
      : await store.get(root, Trie.deserialise);
    trie.isRoot = true;
    return trie;
  }


  /** Saves the trie into the store, removing a previous occurence of it if any.
   * Also makes sure to leave a special key to retrieve the trie root later.
   *
   * @param {Buffer} [previousHash] The previous hash of the node, to be deleted.
   * @return {Promise<Trie>}
   * @private
   */
  async save(previousHash) {
    if (previousHash !== undefined) {
      await this.store.del(previousHash);
    }

    if (this.isRoot) {
      await this.store.put(
        ROOT_KEY,
        { serialise: () => (this.hash ?? NULL_HASH).toString('hex') }
      );
    }

    return this;
  }


  /**
   * Test whether a trie is empty (i.e. holds no branch nodes or leaves).
   * @return {bool}
   */
  isEmpty() {
    return this.size == 0;
  }


  /**
   * Construct a Merkle-Patricia {@link Trie} from a list of key/value pairs.
   *
   * @param {Array<{key: Buffer|string, value: Buffer|string}>} pairs
   * @param {Store} [store] An optional store to store and retrieve nodes from.
   * @return {Promise<Trie>}
   */
  static async fromList(elements, store = new Store()) {
    async function loop(branch, keyValues) {
      // ------------------- An empty trie
      if (keyValues.length === 0) {
        return new Trie();
      }

      const prefix = commonPrefix(keyValues.map(kv => kv.path));

      // ------------------- A leaf
      if (keyValues.length === 1) {
        const [kv] = keyValues;
        return Leaf.from(
          prefix,
          kv.key,
          kv.value,
          store,
        );
      }

      // ------------------- A branch node

      // Remove the prefix from all children.
      const stripped = keyValues.map(kv => {
        return { ...kv, path: kv.path.slice(prefix.length) };
      });

      // Construct sub-tries recursively, for each remainining digits.
      //
      // NOTE(1): We have just deleted the common prefix from all children,
      // so it safe to look at the first digit of each remaining key and route
      // values based on that. Some branches may be empty, which we replace
      // with 'undefined'.
      //
      // NOTE(2): Because we have at least 2 values at this point, the
      // resulting Branch is guaranted to have at least 2 children. They cannot
      // be under the same branch since we have stripped their common prefix!
      const nodes = await Promise.all(Array
        .from('0123456789abcdef')
        .map(digit => loop(digit, stripped.reduce((acc, kv) => {
          assert(kv.path[0] !== undefined, `empty path for node ${kv}`);

          if (kv.path[0] === digit) {
            acc.push({ ...kv, path: kv.path.slice(1) });
          }

          return acc;
        }, []))));

      const children = nodes.map(trie => trie.isEmpty() ? undefined : trie);

      return Branch.from(prefix, children, store);
    }

    const trie = await loop(
      '',
      elements.map(kv => ({ ...kv, path: intoPath(kv.key) }))
    );

    trie.isRoot = true;

    return trie.save();
  }


  /**
   * Insert a new value at the given key and re-compute hashes of all nodes
   * along the path.
   *
   * @param {Buffer|string} key
   *   The key to insert. Strings are treated as UTF-8 byte buffers.
   *
   * @param {Buffer|string} value
   *   The value to insert. Strings are treated as UTF-8 byte buffers.
   *
   * @throws {AssertionError} when a value already exists at the given key.
   */
  async insert(key, value) {
    return this.into(Leaf, intoPath(key), key, value);
  }

  /**
   * Remove the value at the given key and re-compute hashes of all nodes
   * along the path.
   *
   * @param {Buffer|string} key
   *   The key to insert. Strings are treated as UTF-8 byte buffers.
   *
   * @returns {Promise<Trie>}
   *   The modified trie, eventually.
   *
   * @throws {AssertionError} when a value already exists at the given key.
   */
  async delete(key) {
    assert(false, `${key} not in trie`);
  }

  /**
   * Mutate an instance of Trie/Branch/Leaf into another. This is typically
   * used to upgrade empty Trie into Leaf, and Leaf into Branch. The method
   * takes care of preserving the inheritance chain while modifying _this_
   * appropriately.
   *
   * @param {function} target
   *   A target constructor, e.g. Leaf or Branch.
   *
   * @param {...any} args
   *   The arguments to provide the constructor.
   *
   * @return {Promise<Trie>}
   * @private
   */
  async into(target, ...args) {
    const { store, hash: previousHash, isRoot } = this;

    this.__proto__ = target.prototype;
    for (let prop in this) {
      if (this.hasOwnProperty(prop)) {
        delete this[prop];
      }
    }

    const self = Object.assign(this, await target.from(...args.concat(store)))

    self.isRoot = isRoot;

    return self.save(previousHash);
  }


  /** Conveniently access a child in the tries at the given path. A path is
   * sequence of nibbles, as an hex-encoded string.
   *
   * @param {string} path A sequence of nibbles.
   * @return {Promise<Trie|undefined>} A sub-trie at the given path, or nothing.
   */
  async childAt(path) {
    if (this.size === 0) {
      return undefined;
    }

    const loop = async (task, ix) => {
      const trie = await task;

      if (ix >= path.length) {
        return trie;
      }

      if (trie instanceof Leaf) {
        return trie.prefix.startsWith(path.slice(ix)) ? trie : undefined;
      }

      const child = trie.children[nibble(path[ix + trie.prefix.length])];

      if (child === undefined) {
        return undefined;
      }

      return loop(
        this.store.get(child.hash, Trie.deserialise),
        ix + trie.prefix.length + 1,
      )
    };

    return loop(Promise.resolve(this), 0);
  }

  /**
   * Retrieves the value at the given key from the Trie.
   *
   * @param {Buffer|string} key
   *   The key to search for. Strings are treated as UTF-8 byte buffers.
   * @returns {Promise<Buffer|undefined>}
   *   The value at the specified key, or `undefined` if the key is not found.
   */
  async get(key) {
    // Convert the key into a path of nibbles
    const path = intoPath(key);

    // Use childAt to find the node corresponding to the path
    const node = await this.childAt(path);

    key = typeof key === 'string' ? Buffer.from(key) : key;

    // If the node is a Leaf and the key matches, return the value
    if (node instanceof Leaf && node.key.equals(key)) {
      return node.value;
    }

    // Return undefined if no matching node is found
    return undefined;
  }


  /**
   * Creates a proof for a given element.
   *
   * @param {Buffer|string} key
   *  The key for the element
   * @param {bool} [allowMissing]
   *  An optional flag to allow building a proof for the element when it's not
   *  in the trie. This is useful to prove non-membership: verifying the proof
   *  in exclusion would yield the current trie root.
   *
   * @return {Promise<Proof>}
   *
   * @throws {AssertionError}
   *  When 'allowMissing' is not set and the key is not in the trie.
   */
  async prove(key, allowMissing = false) {
    try {
      return this.isEmpty() && allowMissing
        ? new Proof(intoPath(key), undefined, [])
        : await this.walk(intoPath(key));
    } catch(e) {
      if (!allowMissing) {
        throw e;
      }

      /* c8 ignore next 3 */
      if (!(e instanceof assert.AssertionError) ) {
        throw e;
      }

      /* c8 ignore next 3 */
      if (!(e.message ?? "").includes("not in trie")) {
        throw e;
      }

      try {
        return await this.store.batch(async () => {
          const hash = this.hash;
          this instanceof Branch
            ? await tryInsert(this, key, "")
            : await this.insert(key, "");
          const proof = await this.prove(key);
          proof.setValue(undefined);
          await tryDelete(this, key);
          assert(hash.equals(this.hash));
          return proof;
        });
      /* c8 ignore next 4 */
      } catch (e) {
        await this.save();
        throw e;
      }
    }
  }


  /** Walk a trie down a given path, accumulating neighboring nodes along the
   * way to build a proof.
   *
   * @param {string} path A sequence of nibbles.
   * @return {Promise<Proof>}
   * @throws {AssertionError} When there's no value at the given path in the trie.
   * @private
   */
  async walk(path) {
    throw new Error(`cannot walk empty trie with path ${path}`);
  }


  /** A custom function for inspecting an (empty) Trie.
   * @private
   */
  [inspect.custom](_depth, _options, _inspect) {
    return 'ø';
  }

  /** Recover a Trie from an on-disk serialization format.
   *
   * @param {object} blob A serialised object.
   * @param {Store} store An instance of the underlying store.
   * @return {Promise<Trie>}
   * @private
   */
  static async deserialise(hash, blob, store) {
    switch (blob?.__kind) {
      case 'Leaf':
        return Leaf.deserialise(hash, blob, store);
      case 'Branch':
        return Branch.deserialise(hash, blob, store);
      /* c8 ignore next 2 */
      default:
        throw new Error(`unexpected blob to deserialise: ${blob?.__kind}: ${blob}`);
    }
  }
}


// -----------------------------------------------------------------------------
// ------------------------------------------------------------------------ Leaf
// -----------------------------------------------------------------------------

/**
 * A {@link Leaf} materializes a {@link Trie} with a **single** node. Leaves
 * are also the only nodes to hold values.
 */
export class Leaf extends Trie {
  /** The raw Leaf's key.
   * @type {Buffer}
   */
  key;

  /** A serialized value.
   * @type {Buffer}
   */
  value;

  /** Create a new {@link Leaf} from a prefix and a value.
   * @private
   */
  constructor(hash, prefix, key, value, store) {
    super(store, hash, prefix, 1);

    this.key = key;
    this.value = value;
  }

  /**
   * @param {string} prefix
   *   A sequence of nibble, possibly (albeit rarely) empty. In the case of
   *   leaves, the prefix should rather be called 'suffix' as it describes what
   *   remains of the original key.
   *
   * @param {Buffer|string} key
   *   A key. Raw strings are treated as UTF-8 byte buffers.
   *
   * @param {Buffer|string} value
   *   A serialized value. Raw strings are treated as UTF-8 byte buffers.
   *
   * @param {Store} [store]
   *   The data-store to use for storing and retrieving the underlying trie.
   *
   * @return {Promise<Leaf>}
   */
  static async from(suffix, key, value, store) {
    key = typeof key === 'string' ? Buffer.from(key) : key;
    assertInstanceOf(Buffer, { key });

    value = typeof value === 'string' ? Buffer.from(value) : value;
    assertInstanceOf(Buffer, { value });

    assertInstanceOf('string', suffix, (what, type) => typeof what === type);

    assert(
      digest(key).toString('hex').endsWith(suffix),
      `The suffix ${suffix} isn't a valid extension of ${key.toString('hex')}`,
    );

    const leaf = new Leaf(
      Leaf.computeHash(suffix, digest(value)),
      suffix,
      key,
      value,
      store
    );

    return leaf.save();
  }


  /** Set the prefix on a Leaf, and computes its corresponding hash. Both steps
   * are done in lock-step because the node's hash crucially includes its prefix.
   *
   * @param {string} prefix A sequence of nibbles.
   * @param {Buffer} value A hash digest of the value.
   * @return {Trie} A reference to the underlying trie with its prefix modified.
   * @private
   */
  static computeHash(prefix, value) {
    // NOTE:
    // We append the remaining prefix to the value. However, to make this
    // step more efficient on-chain, we append it as a raw bytestring instead of
    // an array of nibbles.
    //
    // If the prefix's length is odd however, we must still prepend one nibble, and
    // then the rest.
    const isOdd = prefix.length % 2 > 0;

    const head = isOdd
      ? Buffer.concat([Buffer.from([0x00]), nibbles(prefix.slice(0, 1))])
      : Buffer.from([0xFF]);

    const tail = Buffer.from(isOdd
      ? prefix.slice(1)
      : prefix,
      'hex'
    );

    assert(
      value.length === DIGEST_LENGTH,
      `value must be a ${DIGEST_LENGTH}-byte digest but it is ${value?.toString('hex')}`
    );

    return digest(Buffer.concat([head, tail, value]));
  }


  /** Store a leaf on disk.
   *
   * @return {Promise<Trie>}
   * @private
   */
  async save(previousHash) {
    this.hash = Leaf.computeHash(this.prefix, digest(this.value));
    await this.store.put(this.hash, this);
    return super.save(previousHash);
  }


  /**
   * Insert a new value at the given key and re-compute hashes of all nodes
   * along the path.
   *
   * @param {Buffer|string} key
   *   The key to insert. Strings are treated as UTF-8 byte buffers.
   *
   * @param {Buffer|string} value
   *   The value to insert. Strings are treated as UTF-8 byte buffers.
   *
   * @returns {Promise<Trie>}
   *   The modified trie, eventually.
   *
   * @throws {AssertionError} when a value already exists at the given key.
   */
  async insert(key, value) {
    assert(this.key !== key, 'already in trie');
    assert(this.prefix.length > 0);

    const thisPath = this.prefix;

    const newPath = intoPath(key).slice(-thisPath.length);

    assert(
      thisPath !== newPath,
      `element already in the trie at ${key}`
    );

    const prefix = commonPrefix([thisPath, newPath]);

    const thisNibble = nibble(thisPath[prefix.length]);

    const newNibble = nibble(newPath[prefix.length]);

    assert(thisNibble !== newNibble);

    return this.into(Branch, prefix, {
        [thisNibble]: await Leaf.from(
          thisPath.slice(prefix.length + 1),
          this.key,
          this.value,
          this.store,
        ),
        [newNibble]: await Leaf.from(
          newPath.slice(prefix.length + 1),
          key,
          value,
          this.store,
        ),
    });
  }


  /**
   * Remove the value at the given key and re-compute hashes of all nodes
   * along the path.
   *
   * @param {Buffer|string} key
   *   The key to insert. Strings are treated as UTF-8 byte buffers.
   *
   * @returns {Promise<Trie>}
   *   The modified trie, eventually.
   *
   * @throws {AssertionError} when a value already exists at the given key.
   */
  async delete(key) {
    key = typeof key === 'string' ? Buffer.from(key) : key;
    assert(this.key.equals(key), `${key} not in trie`);
    return this.into(Trie);
  }


  /**
   * A custom function for inspecting a {@link Leaf}, with colors and nice formatting.
   * See {@link https://nodejs.org/api/util.html#utilinspectobject-showhidden-depth-colors}
   * for details.
   *
   * @private
   */
  [inspect.custom](depth, options, _inspect) {
    const hash = options.stylize(
      `#${this.hash.toString('hex').slice(0, DIGEST_SUMMARY_LENGTH)}`,
      'special'
    );

    const prefix = withEllipsis(this.prefix, PREFIX_CUTOFF, options);

    const key = options.stylize(buffer.isUtf8(this.key)
      ? this.key.toString()
      : this.key.toString('hex').slice(0, DIGEST_SUMMARY_LENGTH),
      'boolean'
    );

    const value = options.stylize(buffer.isUtf8(this.value)
      ? this.value.toString()
      : this.value.toString('hex').slice(0, DIGEST_SUMMARY_LENGTH),
      'string'
    );

    return `${prefix} ${hash} { ${key} → ${value} }`;
  }


  /** See {@link Trie.walk}
   * @private
   */
  async walk(path) {
    assert(
      path.startsWith(this.prefix),
      `element at remaining path ${path} not in trie: non-matching prefix ${this.prefix}`,
    );

    return new Proof(
      intoPath(this.key),
      path === this.prefix ? this.value : undefined
    );
  }


  /** Serialise a Leaf to a format suitable for storage on-disk.
   *
   * @return {object}
   * @private
   */
  serialise() {
    return {
      __kind: 'Leaf',
      prefix: this.prefix,
      key: this.key.toString('hex'),
      value: this.value.toString('hex'),
    };
  }


  /** Recover a Leaf from an on-disk serialization format.
   *
   * @param {Buffer} hash The object's id/hash
   * @param {object} blob A serialised object.
   * @param {Store} store An instance of the underlying store.
   * @return {Promise<Trie>}
   * @private
   */
  static async deserialise(hash, blob, store) {
    return new Leaf(
      hash,
      blob.prefix,
      Buffer.from(blob.key, 'hex'),
      Buffer.from(blob.value, 'hex'),
      store,
    );
  }
}


// -----------------------------------------------------------------------------
// ---------------------------------------------------------------------- Branch
// -----------------------------------------------------------------------------

/**
 * A {@link Branch} materializes a {@link Trie} with **at least two** nodes
 * and **at most** 16 nodes.
 *
 */
export class Branch extends Trie {
  /** A sparse array of child sub-tries.
   *
   * @type {Array<Trie|{ hash: Buffer }|undefined>}
   */
  children;

  constructor(hash, prefix, children, size, store) {
    super(store, hash, prefix, size);
    this.children = children;
  }

  /**
   * Create a new branch node from a (hex-encoded) prefix and 16 children.
   *
   * @param {string} prefix
   *   The accumulated prefix, if any.
   *
   * @param {Array<Trie>|object} children
   *   A vector of ordered children, or a key:value map of nibbles to
   *   sub-tries. When specifying a vector, there must be exactly 16 elements,
   *   with 'undefined' for empty branches.
   *
   * @param {Store} [store]
   *   The data-store to use for storing and retrieving the underlying trie.
   *
   * @return {Promise<Branch>}
   * @private
   */
  static async from(prefix, children, store) {
    assert(children !== undefined);

    children = !Array.isArray(children)
      ? sparseVector(children)
      : children;

    // NOTE: We use 'undefined' to represent empty sub-tries mostly because
    //
    // (1) It is convenient.
    // (2) It saves spaces/memory.
    //
    // But this is easy to get wrong due to duck and dynamic typing in JS. So
    // the constructor is extra careful in checking that children are what they
    // should be.
    children.forEach((node, ix) => {
      if (node !== undefined) {
        assertInstanceOf(Trie, { [`children[${ix}]`]: node });

        assert(
          !node.isEmpty(),
          `Branch cannot contain empty tries; but children[${ix}] is empty.`
        );
      }
    });

    // NOTE: There are special behaviours associated with tries that contains a
    // single node and this is captured as {@link Leaf}.
    assert(
      children.filter(node => node !== undefined).length > 1,
      'Branch must have at *at least 2* children. A Branch with a single child is a Leaf.',
    );

    assert(
      children.length === 16,
      'children must be a vector of *exactly 16* elements (possibly undefined)',
    );

    const size = children.reduce((size, child) => size + (child?.size || 0), 0);

    const branch = new Branch(
      Branch.computeHash(prefix, merkleRoot(children)),
      prefix,
      children,
      size,
      store,
    );

    return branch.save();
  }

  /** Set the prefix on a branch, and computes its corresponding hash. Both steps
   * are done in lock-step because the node's hash crucially includes its prefix.
   *
   * @param {string} prefix A sequence of nibbles.
   * @param {Buffer} root A root merkle tree of the node's children
   * @return {Trie} A reference to the underlying trie with its prefix modified.
   * @private
   */
  static computeHash(prefix, root) {
    assert(
      root.length === DIGEST_LENGTH,
      `root must be a ${DIGEST_LENGTH}-byte digest but it is ${root?.toString('hex')}`
    );

    return digest(Buffer.concat([nibbles(prefix), root]));
  }


  /**
   * Insert a new value at the given key and re-compute hashes of all nodes
   * along the path.
   *
   * @param {Buffer|string} key
   *   The key to insert. Strings are treated as UTF-8 byte buffers.
   *
   * @param {Buffer|string} value
   *   The value to insert. Strings are treated as UTF-8 byte buffers.
   *
   * @returns {Promise<Trie>}
   *   The modified trie, eventually.
   *
   * @throws {AssertionError} when a value already exists at the given key.
   */
  async insert(key, value) {
    try {
      return await this.store.batch(() => tryInsert(this, key, value));
    } catch(e) {
      // Ensures that children aren't kept in-memory when an insertion failed.
      await this.save();
      throw e;
    }
  }


  /**
   * Remove the value at the given key and re-compute hashes of all nodes
   * along the path.
   *
   * @param {Buffer|string} key
   *   The key to insert. Strings are treated as UTF-8 byte buffers.
   *
   * @returns {Promise<Trie>}
   *   The modified trie, eventually.
   *
   * @throws {AssertionError} when a value doesn't exists at the given key.
   */
  async delete(key) {
    try {
      return await this.store.batch(() => tryDelete(this, key))
    } catch(e) {
      // Ensures that children aren't kept in-memory when an deletion failed.
      await this.save();
      throw e;
    }
  }


  /**
   * See {@link Trie.walk}
   * @private
   */
  async walk(path) {
    assert(
      path.startsWith(this.prefix),
      `element at remaining path ${path} not in trie: non-matching prefix ${this.prefix}`,
    );

    const skip = this.prefix.length;

    path = path.slice(skip);

    const branch = nibble(path[0]);

    return this.withChildren(async (children) => {
      const child = children[branch];

      assert(
        child !== undefined,
        `element at remaining path ${path} not in trie: no child at branch ${branch}`,
      );

      const proof = await child.walk(path.slice(1));

      return proof.rewind(child, skip, children);
    });
  }


  /** A custom function for inspecting a Branch, with colors and nice formatting.
   * @private
   */
  [inspect.custom](depth, options, inspect) {
    let [head, ...tail] = this.children.filter(node => node !== undefined);

    const branches = this.children.reduce((acc, node, branch) => {
      if (node !== undefined) {
        acc[node.hash] = '0123456789abcdef'[branch];
      }
      return acc;
    }, {});

    function formatHash(hash, len) {
      return options.stylize(
        `#${hash.toString('hex').slice(0, len ?? DIGEST_SUMMARY_LENGTH)}`,
        'special',
      );
    }

    function format(node, join, vertical = ' ') {
      const nibble = branches[node.hash];

      const hash = formatHash(node.hash ?? NULL_HASH);

      if (!(node instanceof Trie)) {
        return `\n ${join}─ ${nibble} ${hash}`;
      }

      const body = inspect(node, { ...options, depth: depth + 1 });

      return node instanceof Leaf
        ? `\n ${join}─ ${nibble}${body}`
        : `\n${eachLine(
            body,
            (s, ix) =>
              (ix === 0
                  ? ` ${join}─ ${nibble}${node.prefix} ${hash}`
                  : ` ${vertical} `
              ) + s
          )}`;
    }

    // ----- First
    let first = format(head, depth === 2 && this.prefix.length === 0 ? '┌' : '├', '│');
    if (depth === 2 && this.prefix.length > 0) {
      first = `\n ${this.prefix}${first}`
    }

    // ----- In-between
    let between = [];
    tail.slice(0, -1).forEach(node => {
      between.push(format(node, '├', '│'));
    })
    between = between.join('');

    // ----- Last
    let last = tail[tail.length - 1];
    last = format(last, '└');

    const rootHash = formatHash(this.hash ?? NULL_HASH, 2 * DIGEST_LENGTH);
    const wall = ''.padStart(3 + DIGEST_LENGTH * 2, '═')

    return depth == 2
      ? `╔${wall}╗\n║ ${rootHash} ║\n╚${wall}╝${first}${between}${last}`
      : `${first}${between}${last}`;
  }


  /** Recompute a branch's size and hash after modification; also collapses
   * all children back to hashes.
   *
   * @param {Buffer} [previousHash] The previous hash of the node, to be deleted.
   * @return {Promise<Branch>} This current object, eventually modified.
   * @private
   */
  async save(previousHash) {
    this.hash = Branch.computeHash(this.prefix, merkleRoot(this.children));

    this.children = this.children.map(child => child instanceof Trie
      ? { hash: child.hash }
      : child
    );

    await this.store.put(this.hash, this);

    return super.save(previousHash);
  }


  /** Perform an operation with the node's children, without keeping them
   * around once done.
   *
   * @param {function} callback
   * @return {Promise<any>}
   * @private
   */
  async withChildren(callback) {
    return callback(await Promise.all(this.children.map(child =>
      child === undefined
        ? child
        : this.store.get(child.hash, Trie.deserialise)
    )));
  }


  /** Recursively fetch children and sub-children. Useful to pretty-print (part of)
   * a Branch node
   *
   * @param {Number} [depth=0]
   *   Depth until which fetch sub-children. 0 means only the current level.
   *   Use Number.MAX_SAFE_INTEGER to fetch all the entire sub-trie.
   *
   * @return {Trie} This trie, with children fetched.
   */
  async fetchChildren(depth = 0) {
    assert(this.children.filter(node => node !== undefined).length > 1);

    async function loop(n, node) {
      if (n < 0 || !(node instanceof Branch)) {
        return node;
      }

      node.children = await Promise.all(node.children.map(async child => {
        if (child === undefined) {
          return undefined;
        }

        return loop(
          n - 1,
          child instanceof Trie
            ? child
            : await node.store.get(child.hash, Trie.deserialise)
        );
      }));

      return node;
    }

    return loop(depth, this);
  }


  /** Serialise a Branch to a format suitable for storage on-disk.
   * @return {object}
   * @private
   */
  serialise() {
    return {
      __kind: 'Branch',
      prefix: this.prefix,
      children: this.children.map(child => child?.hash.toString('hex')),
      size: this.size,
    };
  }


  /** Recover a Branch from an on-disk serialization format.
   *
   * @param {Buffer} hash The object's id/hash
   * @param {object} blob A serialised object.
   * @param {Store} store An instance of the underlying store.
   * @return {Promise<Trie>}
   * @private
   */
  static async deserialise(hash, blob, store) {
    return new Branch(
      hash,
      blob.prefix,
      blob.children.map(child => {
        if (!child) {
          return undefined;
        }

        return { hash: Buffer.from(child, 'hex') };
      }),
      blob.size,
      store,
    );
  }
}


// -----------------------------------------------------------------------------
// ----------------------------------------------------------------------- Proof
// -----------------------------------------------------------------------------

/** A self-contained proof of inclusion for a value in a {@link Trie}. A proof
 * holds onto a *specific* value and is only valid for a *specific* {@link Trie}.
 */
export class Proof {
  static #TYPE_LEAF = Symbol('leaf');
  static #TYPE_FORK = Symbol('fork');
  static #TYPE_BRANCH = Symbol('branch');

  /** The path for which this proof is for.
   * @type {Buffer}
   */
  #path;

  /** The value for which this proof is for.
   * @type {Buffer|undefined}
   */
  #value;

  /** Proof steps, containing neighboring nodes at each level in the trie as well
   * as the size of the prefix for this level. we need not to provide the actual
   * nibbles because they are given by the value's key already.
   *
   * Step's neighbors contains root hashes of neighbors sub-tries.
   *
   * @type {Array<Step>}
   */
  #steps;

  /** Construct a new proof from a serialised value. This is mostly useful for
   * proving a {@link Leaf}.
   *
   * @param {Buffer} path
   * @param {Buffer|undefined} value
   * @param {Array<Object>} [steps]
   * @return {Proof}
   * @private
   */
  constructor(path, value, steps = []) {
    this.#path = path;
    this.#value = value;
    this.#steps = steps;
  }


  /**
   * Set or reset the value from the proof; This allows re-using the same proof path
   * in the same trie, but for different values.
   *
   * @param {Buffer|string|undefined} value
   *   The new value to insert. Strings are treated as UTF-8 byte buffers.
   *   Setting the proof's value to 'undefined' effectively makes the proof only
   *   work for exclusion (i.e. testing non-membership).
   */
  setValue(value) {
    if (value === undefined) {
      this.#value = undefined;
    } else {
      this.#value = typeof value === 'string' ? Buffer.from(value) : value;
    }
  }


  /** Add a step in front of the proof. The proof is built recursively from the
   * bottom-up (from the leaves to the root). At each step in the proof, we
   * rewind one level until we reach the root. At each level, we record the
   * neighbors nodes as well as the length of the prefix.
   *
   * @param {Trie} target Sub-trie on the path we are proving. Excluded from neighbors.
   * @param {number} skip The size of the prefix
   * @param {Array<Trie>} children A list of sub-tries.
   * @return {Proof} The proof itself, with an extra step pre-pended.
   * @private
   */
  rewind(target, skip, children) {
    const me = children.findIndex(x => (x?.hash ?? NULL_HASH).equals(target.hash ?? NULL_HASH));

    assert(me !== -1, `target not in children`);

    const nonEmptyNeighbors = children.filter((x, ix) => {
      return x !== undefined && !(ix === me)
    });

    if (nonEmptyNeighbors.length === 1) {
      const neighbor = nonEmptyNeighbors[0];

      this.#steps.unshift(neighbor instanceof Leaf
        ? {
            type: Proof.#TYPE_LEAF,
            skip,
            neighbor: {
              key: intoPath(neighbor.key),
              value: digest(neighbor.value),
            },
          }
        : {
            type: Proof.#TYPE_FORK,
            skip,
            neighbor: {
              prefix: nibbles(neighbor.prefix),
              nibble: children.indexOf(neighbor),
              root: merkleRoot(neighbor.children),
            }
          }
      );
    } else {
      this.#steps.unshift({
        type: Proof.#TYPE_BRANCH,
        skip,
        neighbors: merkleProof(children, me),
      });
    }

    return this;
  }


  /** Compute the resulting root hash from this proof. This methods has two modes:
   *
   * - One that includes the value leaf in the proof and computes the
   * - One that computes the root without the element.
   *
   * The second mode is useful to prove insertion and removal of an element in
   * a trie. Consider a trie T0 that doesn't contain an element e, and a trie T1
   * that is T0 with e inserted. Then, one can provide a proof for e in T1.
   *
   * Computing the proof without e will yield T0's hash, whereas computing it
   * with e will yield T1.
   *
   * @param {bool} [includingItem=true]
   *   When set, computes the resulting root hash considering the underlying
   *   value is in the trie.
   * @return {Buffer|null}
   *   A resulting hash as a byte buffer, to be compared with a known root.
   *   Returns null when the resulting hash is an empty trie (e.g. when
   *   checking an empty proof in exclusion).
   */
  verify(includingItem = true, key) {
    assert(
      !(includingItem && this.#value === undefined),
      "attempted to verify an inclusion proof without value: use 'proof.setValue(..)', or build a new proof."
    );

    if (this.#steps.length === 0) {
      if (includingItem) {
        return Leaf.computeHash(this.#path, digest(this.#value));
      } else {
        return null;
      }
    }

    const loop = (cursor, ix) => {
      const step = this.#steps[ix];

      // Terminal case (or first case, depending how we look at it).
      if (step === undefined) {
        if (!includingItem) {
          return undefined;
        }

        const suffix = this.#path.slice(cursor);

        assert(
          this.#value !== undefined,
          `no value at path ${this.#path.slice(0, cursor)}`
        );

        return Leaf.computeHash(suffix, digest(this.#value))
      }

      const isLastStep = this.#steps[ix + 1] === undefined;

      const nextCursor = cursor + 1 + step.skip;

      const me = loop(nextCursor, ix + 1);

      const thisNibble = nibble(this.#path[nextCursor - 1]);

      // Merge nodes together into a new (sub-)root.
      const root = (nodes) => {
        const prefix = this.#path.slice(cursor, nextCursor - 1);
        const merkle = merkleRoot(sparseVector(nodes));
        return Branch.computeHash(prefix, merkle);
      };

      switch (step.type) {
        case Proof.#TYPE_BRANCH: {
          function h(left, right) {
            return digest(Buffer.concat([left ?? NULL_HASH, right ?? NULL_HASH]));
          }

          const [lvl1, lvl2, lvl3, lvl4] = step.neighbors;

          // NOTE: There are more elegant ways to do that but, it works, is
          // fairly easy to understand and fairly easy to maintain.
          const merkle = {
            0: h(h(h(h(me, lvl4), lvl3), lvl2), lvl1),
            1: h(h(h(h(lvl4, me), lvl3), lvl2), lvl1),
            2: h(h(h(lvl3, h(me, lvl4)), lvl2), lvl1),
            3: h(h(h(lvl3, h(lvl4, me)), lvl2), lvl1),
            4: h(h(lvl2, h(h(me, lvl4), lvl3)), lvl1),
            5: h(h(lvl2, h(h(lvl4, me), lvl3)), lvl1),
            6: h(h(lvl2, h(lvl3, h(me, lvl4))), lvl1),
            7: h(h(lvl2, h(lvl3, h(lvl4, me))), lvl1),
            8: h(lvl1, h(h(h(me, lvl4), lvl3), lvl2)),
            9: h(lvl1, h(h(h(lvl4, me), lvl3), lvl2)),
            10: h(lvl1, h(h(lvl3, h(me, lvl4)), lvl2)),
            11: h(lvl1, h(h(lvl3, h(lvl4, me)), lvl2)),
            12: h(lvl1, h(lvl2, h(h(me, lvl4), lvl3))),
            13: h(lvl1, h(lvl2, h(h(lvl4, me), lvl3))),
            14: h(lvl1, h(lvl2, h(lvl3, h(me, lvl4)))),
            15: h(lvl1, h(lvl2, h(lvl3, h(lvl4, me)))),
          }[thisNibble];

          const prefix = this.#path.slice(cursor, nextCursor - 1);

          return Branch.computeHash(prefix, merkle);
        }

        case Proof.#TYPE_FORK: {
          if (!includingItem && isLastStep) {
            const neighborPrefix = [
              Buffer.from([step.neighbor.nibble]),
              step.neighbor.prefix,
            ];

            // For skip > 0, we need to reconstruct the original neighbor node
            // before the fork was created. The original node had the full prefix:
            // (common prefix) + (neighbor nibble) + (neighbor's current prefix)
            const prefix = step.skip === 0
              ? neighborPrefix
              : [
                  nibbles(this.#path.slice(cursor, cursor + step.skip)),
                  ...neighborPrefix,
                ];

            return digest(Buffer.concat([...prefix, step.neighbor.root]));
          }

          assert(step.neighbor.nibble !== thisNibble);

          return root({
            [thisNibble]: me,
            [step.neighbor.nibble]: digest(Buffer.concat([
              step.neighbor.prefix,
              step.neighbor.root,
            ]))
          });
        }

        case Proof.#TYPE_LEAF: {
          const neighborPath = step.neighbor.key.toString('hex');

          assert(neighborPath.slice(0, cursor) === this.#path.slice(0, cursor));

          const neighborNibble = nibble(neighborPath[nextCursor - 1]);

          assert(neighborNibble !== thisNibble);

          if (!includingItem && isLastStep) {
            const suffix = neighborPath.slice(cursor);
            return Leaf.computeHash(suffix, step.neighbor.value);
          }

          const suffix = neighborPath.slice(nextCursor);

          return root({
            [thisNibble]: me,
            [neighborNibble]: Leaf.computeHash(suffix, step.neighbor.value),
          });
        }

        /* c8 ignore next 2 */
        default:
          throw new Error(`unknown step type ${step.type}`);
      }
    };

    return loop(0, 0);
  }


  /** Deserialize a proof from JSON.
   *
   * @param {Buffer|string} path
   *   The original key being proven. Strings are treated as UTF-8 byte buffers.
   * @param {Buffer|string|undefined} value
   *   The original value being proven. Strings are treated as UTF-8 byte buffers.
   * @param {Array<Object>} steps
   *   The steps serialized to JSON.
   * @return {Proof}
   */
  static fromJSON(key, value, steps) {
    return new Proof(intoPath(key), value, steps.map(step => {
      switch (step.type) {
        case Proof.#TYPE_LEAF.description:
          return {
            type: Proof.#TYPE_LEAF,
            skip: step.skip,
            neighbor: {
              key: Buffer.from(step.neighbor.key, 'hex'),
              value: Buffer.from(step.neighbor.value, 'hex'),
            }
          };
        case Proof.#TYPE_BRANCH.description:
          const neighbors = [];
          for (let i = 0; i < step.neighbors.length; i += 2 * DIGEST_LENGTH) {
            const hash = step.neighbors.slice(i, i + 2 * DIGEST_LENGTH);
            neighbors.push(Buffer.from(hash, 'hex'));
          }
          return {
            type: Proof.#TYPE_BRANCH,
            skip: step.skip,
            neighbors,
          };
        case Proof.#TYPE_FORK.description:
          return {
            type: Proof.#TYPE_FORK,
            skip: step.skip,
            neighbor: {
              prefix: Buffer.from(step.neighbor.prefix, 'hex'),
              nibble: step.neighbor.nibble,
              root: Buffer.from(step.neighbor.root, 'hex'),
            },
          };
        /* c8 ignore next 2 */
        default:
          throw new Error(`unknown step type ${step.type}`);
      }
    }));
  }


  /** Serialise the proof as a portable JSON.
   *
   * @return {object}
   */
  toJSON() {
    const serialisers = {
      [Proof.#TYPE_BRANCH](step) {
        return {
          ...step,
          type: step.type.description,
          neighbors: step.neighbors.map(x => x?.toString('hex') ?? '').join(''),
        };
      },

      [Proof.#TYPE_FORK](step) {
        return {
          ...step,
          type: step.type.description,
          neighbor: {
            ...step.neighbor,
            prefix: step.neighbor.prefix.toString('hex'),
            root: step.neighbor.root.toString('hex'),
          }
        };
      },

      [Proof.#TYPE_LEAF](step) {
        return {
          ...step,
          type: step.type.description,
          neighbor: {
            key: step.neighbor.key.toString('hex'),
            value: step.neighbor.value.toString('hex'),
          }
        };
      },
    };

    return this.#steps.map(step => serialisers[step.type](step));
  }

  toUPLC() {
    const steps = this.toJSON().map(step => {
        switch (step.type) {
          case Proof.#TYPE_BRANCH.description: {
            const skip = `I ${step.skip}`;
            const neighbors = `B #${step.neighbors}`;
            return `Constr 0 [${skip}, ${neighbors}]`;
          }
          case Proof.#TYPE_FORK.description: {
            const skip = `I ${step.skip}`;
            const nibble = `I ${step.neighbor.nibble}`;
            const prefix = `B #${step.neighbor.prefix}`;
            const root = `B #${step.neighbor.root}`;
            const neighbors = `Constr 0 [${nibble}, ${prefix}, ${root}]`;
            return `Constr 1 [${skip}, ${neighbors}]`;
          }
          case Proof.#TYPE_LEAF.description: {
            const skip = `I ${step.skip}`;
            const key = `B #${step.neighbor.key}`;
            const value = `B #${step.neighbor.value}`;
            return `Constr 2 [${skip}, ${key}, ${value}]`;
          }
        }
    });

    return `(con data (List [${steps.join(", ")}]))`;
  }


  /** Serialise the proof as a portable CBOR, ready to be decoded on-chain.
   *
   * @return {Buffer}
   */
  toCBOR() {
    return cbor.sequence(
      cbor.beginList(),
      ...this.toJSON().map(step => {
        switch (step.type) {
          case Proof.#TYPE_BRANCH.description: {
            return cbor.tag(121, cbor.sequence(
              cbor.beginList(),
              cbor.int(step.skip),
              cbor.sequence(
                cbor.beginBytes(),
                cbor.bytes(Buffer.from(step.neighbors.slice(0, 128), 'hex')),
                cbor.bytes(Buffer.from(step.neighbors.slice(128), 'hex')),
                cbor.end(),
              ),
              cbor.end(),
            ));
          }
          case Proof.#TYPE_FORK.description: {
            return cbor.tag(122, cbor.sequence(
              cbor.beginList(),
              cbor.int(step.skip),
              cbor.tag(121, cbor.sequence(
                cbor.beginList(),
                cbor.int(step.neighbor.nibble),
                cbor.bytes(Buffer.from(step.neighbor.prefix, 'hex')),
                cbor.bytes(Buffer.from(step.neighbor.root, 'hex')),
                cbor.end(),
              )),
              cbor.end(),
            ));
          }
          case Proof.#TYPE_LEAF.description: {
            return cbor.tag(123, cbor.sequence(
              cbor.beginList(),
              cbor.int(step.skip),
              cbor.bytes(Buffer.from(step.neighbor.key, 'hex')),
              cbor.bytes(Buffer.from(step.neighbor.value, 'hex')),
              cbor.end(),
            ));
          }
          /* c8 ignore next 2 */
          default:
            throw new Error(`unknown step type ${step.type}`);
        }
      }),
      cbor.end(),
    );
  }


  /** Serialise the proof as Aiken code. Mainly for debugging / testing.
   *
   * @return {string}
   */
  toAiken() {
    const steps = this.toJSON().map(step => {
      switch (step.type) {
        case Proof.#TYPE_BRANCH.description: {
          return `  Branch { skip: ${step.skip}, neighbors: #"${step.neighbors}" },\n`
        }
        case Proof.#TYPE_FORK.description: {
          const neighbor = `Neighbor { nibble: ${step.neighbor.nibble}, prefix: #"${step.neighbor.prefix}", root: #"${step.neighbor.root}" }`;
          return `  Fork { skip: ${step.skip}, neighbor: ${neighbor} },\n`
        }
        case Proof.#TYPE_LEAF.description: {
          return `  Leaf { skip: ${step.skip}, key: #"${step.neighbor.key}", value: #"${step.neighbor.value}" },\n`
        }
        /* c8 ignore next 2 */
        default:
          throw new Error(`unknown step type ${step.type}`);
      }
    });

    return `[\n${steps.join('')}]`;
  }
}

/**
 * Like 'insert', but as a raw sequence of operations (outside of any
 * database batch).
 *
 * This is useful to compose it with either another insert, or a delete
 * operation as part of the same database batch.
 *
 * For instance, to obtain a Trie containing a given element and build proofs
 * from it, while not actually modifying the database even in the event of a
 * crash / fault.
 *
 * @param {Buffer|string} key
 *   The key to insert. Strings are treated as UTF-8 byte buffers.
 *
 * @param {Buffer|string} value
 *   The value to insert. Strings are treated as UTF-8 byte buffers.
 *
 * @returns {Promise<Trie>}
 *   The modified trie, eventually.
 *
 * @throws {AssertionError} when a value already exists at the given key.
 */
async function tryInsert(self, key, value) {
  const loop = async (node, path, parents) => {
    const prefix = node.prefix.length > 0
      ? commonPrefix([node.prefix, path])
      : '';

    path = path.slice(prefix.length);

    const thisNibble = nibble(path[0]);

    await node.fetchChildren();

    if (prefix.length < node.prefix.length) {
      const newPrefix = node.prefix.slice(prefix.length);
      const newNibble = nibble(newPrefix[0]);

      assert(thisNibble !== newNibble);

      await node.into(Branch, prefix, {
        [thisNibble]: await Leaf.from(
          path.slice(1),
          key,
          value,
          self.store,
        ),
        [newNibble]: await Branch.from(
          node.prefix.slice(prefix.length + 1),
          node.children,
          self.store,
        ),
      });

      return parents;
    }

    parents.unshift(node);

    const child = node.children[thisNibble];

    if (child === undefined) {
      node.children[thisNibble] = await Leaf.from(
        path.slice(1),
        key,
        value,
        self.store
      );
      return parents;
    }

    if (child instanceof Leaf) {
      await child.insert(key, value);
      return parents;
    } else {
      return loop(child, path.slice(1), parents);
    }
  };

  const parents = await loop(self, intoPath(key), []);

  await parents.reduce(async (task, node) => {
    await task;
    node.size += 1;
    return node.save(node.hash);
  }, Promise.resolve());

  return self;
}

/**
 * Like delete, but as a raw sequence of operations (outside of any database
 * batch).
 *
 * This is useful to compose it with either another delete, or a delete
 * operation as part of the same database batch.
 *
 * For instance, to obtain a Trie containing a given element and build proofs
 * from it, while not actually modifying the database even in the event of a
 * crash / fault.
 *
 * @param {Buffer|string} key
 *   The key to insert. Strings are treated as UTF-8 byte buffers.
 *
 * @returns {Promise<Trie>}
 *   The modified trie, eventually.
 *
 * @throws {AssertionError} when a value doesn't exists at the given key.
 */
async function tryDelete(self, key) {
  key = typeof key === 'string' ? Buffer.from(key) : key;

  function nonEmptyChildren(node) {
    return node.children.flatMap((n, ix) => n === undefined ? [] : [[n, ix]]);
  }

  const loop = async (node, path) => {
    if (node instanceof Leaf) {
      await node.delete(key);
      return undefined;
    }

    await self.store.del(node.hash);

    const cursor = node.prefix.length;

    const thisNibble = nibble(path[cursor]);

    await node.fetchChildren();

    const child = await node.children[thisNibble];

    // NOTE: 'loop' returns 'undefined' when the child is a leaf, which means
    // we've reached the end of the trie. So that node gets effectively deleted.
    //
    // Then, because we call _loop_ before doing any further modification, we can
    // continue knowing that children have already been updated.
    node.children[thisNibble] = await loop(child, path.slice(cursor + 1));

    node.size -= 1;

    const neighbors = nonEmptyChildren(node);

    // NOTE: We do not allow branches with only one child. So if after modification,
    // there's only one child left (a.k.a the neighbor), we merge our only child up
    // with ourself while preserving its stucture for the child may be a Leaf or
    // another Branch node.
    if (neighbors.length === 1) {
      const [neighbor, neighborNibble] = neighbors[0];

      const prefix = [
        node.prefix,
        neighborNibble.toString(16),
        neighbor.prefix,
      ].join('');

      await self.store.del(neighbor.hash);

      if (neighbor instanceof Leaf) {
        return node.into(Leaf, prefix, neighbor.key, neighbor.value);
      }

      node.children = neighbor.children;
      node.prefix = prefix;
      node.size = neighbor.size;
    }

    return node.save();
  };

  return loop(self, intoPath(key));
}
