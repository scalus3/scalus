
let imports = {};
imports['__wbindgen_placeholder__'] = module.exports;

function addToExternrefTable0(obj) {
    const idx = wasm.__externref_table_alloc();
    wasm.__wbindgen_externrefs.set(idx, obj);
    return idx;
}

function _assertClass(instance, klass) {
    if (!(instance instanceof klass)) {
        throw new Error(`expected instance of ${klass.name}`);
    }
}

const CLOSURE_DTORS = (typeof FinalizationRegistry === 'undefined')
    ? { register: () => {}, unregister: () => {} }
    : new FinalizationRegistry(state => state.dtor(state.a, state.b));

function debugString(val) {
    // primitive types
    const type = typeof val;
    if (type == 'number' || type == 'boolean' || val == null) {
        return  `${val}`;
    }
    if (type == 'string') {
        return `"${val}"`;
    }
    if (type == 'symbol') {
        const description = val.description;
        if (description == null) {
            return 'Symbol';
        } else {
            return `Symbol(${description})`;
        }
    }
    if (type == 'function') {
        const name = val.name;
        if (typeof name == 'string' && name.length > 0) {
            return `Function(${name})`;
        } else {
            return 'Function';
        }
    }
    // objects
    if (Array.isArray(val)) {
        const length = val.length;
        let debug = '[';
        if (length > 0) {
            debug += debugString(val[0]);
        }
        for(let i = 1; i < length; i++) {
            debug += ', ' + debugString(val[i]);
        }
        debug += ']';
        return debug;
    }
    // Test for built-in
    const builtInMatches = /\[object ([^\]]+)\]/.exec(toString.call(val));
    let className;
    if (builtInMatches && builtInMatches.length > 1) {
        className = builtInMatches[1];
    } else {
        // Failed to match the standard '[object ClassName]'
        return toString.call(val);
    }
    if (className == 'Object') {
        // we're a user defined class or Object
        // JSON.stringify avoids problems with cycles, and is generally much
        // easier than looping through ownProperties of `val`.
        try {
            return 'Object(' + JSON.stringify(val) + ')';
        } catch (_) {
            return 'Object';
        }
    }
    // errors
    if (val instanceof Error) {
        return `${val.name}: ${val.message}\n${val.stack}`;
    }
    // TODO we could test for more things here, like `Set`s and `Map`s.
    return className;
}

function getArrayJsValueFromWasm0(ptr, len) {
    ptr = ptr >>> 0;
    const mem = getDataViewMemory0();
    const result = [];
    for (let i = ptr; i < ptr + 4 * len; i += 4) {
        result.push(wasm.__wbindgen_externrefs.get(mem.getUint32(i, true)));
    }
    wasm.__externref_drop_slice(ptr, len);
    return result;
}

function getArrayU8FromWasm0(ptr, len) {
    ptr = ptr >>> 0;
    return getUint8ArrayMemory0().subarray(ptr / 1, ptr / 1 + len);
}

let cachedDataViewMemory0 = null;
function getDataViewMemory0() {
    if (cachedDataViewMemory0 === null || cachedDataViewMemory0.buffer.detached === true || (cachedDataViewMemory0.buffer.detached === undefined && cachedDataViewMemory0.buffer !== wasm.memory.buffer)) {
        cachedDataViewMemory0 = new DataView(wasm.memory.buffer);
    }
    return cachedDataViewMemory0;
}

function getStringFromWasm0(ptr, len) {
    ptr = ptr >>> 0;
    return decodeText(ptr, len);
}

let cachedUint8ArrayMemory0 = null;
function getUint8ArrayMemory0() {
    if (cachedUint8ArrayMemory0 === null || cachedUint8ArrayMemory0.byteLength === 0) {
        cachedUint8ArrayMemory0 = new Uint8Array(wasm.memory.buffer);
    }
    return cachedUint8ArrayMemory0;
}

function handleError(f, args) {
    try {
        return f.apply(this, args);
    } catch (e) {
        const idx = addToExternrefTable0(e);
        wasm.__wbindgen_exn_store(idx);
    }
}

function isLikeNone(x) {
    return x === undefined || x === null;
}

function makeMutClosure(arg0, arg1, dtor, f) {
    const state = { a: arg0, b: arg1, cnt: 1, dtor };
    const real = (...args) => {

        // First up with a closure we increment the internal reference
        // count. This ensures that the Rust closure environment won't
        // be deallocated while we're invoking it.
        state.cnt++;
        const a = state.a;
        state.a = 0;
        try {
            return f(a, state.b, ...args);
        } finally {
            state.a = a;
            real._wbg_cb_unref();
        }
    };
    real._wbg_cb_unref = () => {
        if (--state.cnt === 0) {
            state.dtor(state.a, state.b);
            state.a = 0;
            CLOSURE_DTORS.unregister(state);
        }
    };
    CLOSURE_DTORS.register(real, state, state);
    return real;
}

function passArrayJsValueToWasm0(array, malloc) {
    const ptr = malloc(array.length * 4, 4) >>> 0;
    for (let i = 0; i < array.length; i++) {
        const add = addToExternrefTable0(array[i]);
        getDataViewMemory0().setUint32(ptr + 4 * i, add, true);
    }
    WASM_VECTOR_LEN = array.length;
    return ptr;
}

function passStringToWasm0(arg, malloc, realloc) {
    if (realloc === undefined) {
        const buf = cachedTextEncoder.encode(arg);
        const ptr = malloc(buf.length, 1) >>> 0;
        getUint8ArrayMemory0().subarray(ptr, ptr + buf.length).set(buf);
        WASM_VECTOR_LEN = buf.length;
        return ptr;
    }

    let len = arg.length;
    let ptr = malloc(len, 1) >>> 0;

    const mem = getUint8ArrayMemory0();

    let offset = 0;

    for (; offset < len; offset++) {
        const code = arg.charCodeAt(offset);
        if (code > 0x7F) break;
        mem[ptr + offset] = code;
    }
    if (offset !== len) {
        if (offset !== 0) {
            arg = arg.slice(offset);
        }
        ptr = realloc(ptr, len, len = offset + arg.length * 3, 1) >>> 0;
        const view = getUint8ArrayMemory0().subarray(ptr + offset, ptr + len);
        const ret = cachedTextEncoder.encodeInto(arg, view);

        offset += ret.written;
        ptr = realloc(ptr, len, offset, 1) >>> 0;
    }

    WASM_VECTOR_LEN = offset;
    return ptr;
}

let cachedTextDecoder = new TextDecoder('utf-8', { ignoreBOM: true, fatal: true });
cachedTextDecoder.decode();
function decodeText(ptr, len) {
    return cachedTextDecoder.decode(getUint8ArrayMemory0().subarray(ptr, ptr + len));
}

const cachedTextEncoder = new TextEncoder();

if (!('encodeInto' in cachedTextEncoder)) {
    cachedTextEncoder.encodeInto = function (arg, view) {
        const buf = cachedTextEncoder.encode(arg);
        view.set(buf);
        return {
            read: arg.length,
            written: buf.length
        };
    }
}

let WASM_VECTOR_LEN = 0;

function wasm_bindgen__convert__closures_____invoke__hc0a74f7bb86030d0(arg0, arg1, arg2) {
    wasm.wasm_bindgen__convert__closures_____invoke__hc0a74f7bb86030d0(arg0, arg1, arg2);
}

function wasm_bindgen__convert__closures_____invoke__h6b7e05d46d107c93(arg0, arg1) {
    wasm.wasm_bindgen__convert__closures_____invoke__h6b7e05d46d107c93(arg0, arg1);
}

function wasm_bindgen__convert__closures_____invoke__h2da143d4463a5f08(arg0, arg1, arg2, arg3) {
    wasm.wasm_bindgen__convert__closures_____invoke__h2da143d4463a5f08(arg0, arg1, arg2, arg3);
}

const __wbindgen_enum_ReadableStreamType = ["bytes"];

const __wbindgen_enum_RequestCache = ["default", "no-store", "reload", "no-cache", "force-cache", "only-if-cached"];

const __wbindgen_enum_RequestCredentials = ["omit", "same-origin", "include"];

const __wbindgen_enum_RequestMode = ["same-origin", "no-cors", "cors", "navigate"];

const BlockNumberFinalization = (typeof FinalizationRegistry === 'undefined')
    ? { register: () => {}, unregister: () => {} }
    : new FinalizationRegistry(ptr => wasm.__wbg_blocknumber_free(ptr >>> 0, 1));

const CardanoTransactionsProofsFinalization = (typeof FinalizationRegistry === 'undefined')
    ? { register: () => {}, unregister: () => {} }
    : new FinalizationRegistry(ptr => wasm.__wbg_cardanotransactionsproofs_free(ptr >>> 0, 1));

const CardanoTransactionsSetProofMessagePartFinalization = (typeof FinalizationRegistry === 'undefined')
    ? { register: () => {}, unregister: () => {} }
    : new FinalizationRegistry(ptr => wasm.__wbg_cardanotransactionssetproofmessagepart_free(ptr >>> 0, 1));

const IntoUnderlyingByteSourceFinalization = (typeof FinalizationRegistry === 'undefined')
    ? { register: () => {}, unregister: () => {} }
    : new FinalizationRegistry(ptr => wasm.__wbg_intounderlyingbytesource_free(ptr >>> 0, 1));

const IntoUnderlyingSinkFinalization = (typeof FinalizationRegistry === 'undefined')
    ? { register: () => {}, unregister: () => {} }
    : new FinalizationRegistry(ptr => wasm.__wbg_intounderlyingsink_free(ptr >>> 0, 1));

const IntoUnderlyingSourceFinalization = (typeof FinalizationRegistry === 'undefined')
    ? { register: () => {}, unregister: () => {} }
    : new FinalizationRegistry(ptr => wasm.__wbg_intounderlyingsource_free(ptr >>> 0, 1));

const JSBroadcastChannelFeedbackReceiverFinalization = (typeof FinalizationRegistry === 'undefined')
    ? { register: () => {}, unregister: () => {} }
    : new FinalizationRegistry(ptr => wasm.__wbg_jsbroadcastchannelfeedbackreceiver_free(ptr >>> 0, 1));

const MithrilClientFinalization = (typeof FinalizationRegistry === 'undefined')
    ? { register: () => {}, unregister: () => {} }
    : new FinalizationRegistry(ptr => wasm.__wbg_mithrilclient_free(ptr >>> 0, 1));

/**
 * BlockNumber is the block number of a Cardano transaction.
 */
class BlockNumber {
    static __wrap(ptr) {
        ptr = ptr >>> 0;
        const obj = Object.create(BlockNumber.prototype);
        obj.__wbg_ptr = ptr;
        BlockNumberFinalization.register(obj, obj.__wbg_ptr, obj);
        return obj;
    }
    __destroy_into_raw() {
        const ptr = this.__wbg_ptr;
        this.__wbg_ptr = 0;
        BlockNumberFinalization.unregister(this);
        return ptr;
    }
    free() {
        const ptr = this.__destroy_into_raw();
        wasm.__wbg_blocknumber_free(ptr, 0);
    }
    /**
     * @returns {bigint}
     */
    get 0() {
        const ret = wasm.__wbg_get_blocknumber_0(this.__wbg_ptr);
        return BigInt.asUintN(64, ret);
    }
    /**
     * @param {bigint} arg0
     */
    set 0(arg0) {
        wasm.__wbg_set_blocknumber_0(this.__wbg_ptr, arg0);
    }
}
if (Symbol.dispose) BlockNumber.prototype[Symbol.dispose] = BlockNumber.prototype.free;
exports.BlockNumber = BlockNumber;

/**
 * A cryptographic proof for a set of Cardano transactions
 */
class CardanoTransactionsProofs {
    static __wrap(ptr) {
        ptr = ptr >>> 0;
        const obj = Object.create(CardanoTransactionsProofs.prototype);
        obj.__wbg_ptr = ptr;
        CardanoTransactionsProofsFinalization.register(obj, obj.__wbg_ptr, obj);
        return obj;
    }
    __destroy_into_raw() {
        const ptr = this.__wbg_ptr;
        this.__wbg_ptr = 0;
        CardanoTransactionsProofsFinalization.unregister(this);
        return ptr;
    }
    free() {
        const ptr = this.__destroy_into_raw();
        wasm.__wbg_cardanotransactionsproofs_free(ptr, 0);
    }
    /**
     * Hash of the certificate that validate this proof merkle root
     * @returns {string}
     */
    get certificate_hash() {
        let deferred1_0;
        let deferred1_1;
        try {
            const ret = wasm.__wbg_get_cardanotransactionsproofs_certificate_hash(this.__wbg_ptr);
            deferred1_0 = ret[0];
            deferred1_1 = ret[1];
            return getStringFromWasm0(ret[0], ret[1]);
        } finally {
            wasm.__wbindgen_free(deferred1_0, deferred1_1, 1);
        }
    }
    /**
     * Hash of the certificate that validate this proof merkle root
     * @param {string} arg0
     */
    set certificate_hash(arg0) {
        const ptr0 = passStringToWasm0(arg0, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
        const len0 = WASM_VECTOR_LEN;
        wasm.__wbg_set_cardanotransactionsproofs_certificate_hash(this.__wbg_ptr, ptr0, len0);
    }
    /**
     * Transactions that have been certified
     * @returns {CardanoTransactionsSetProofMessagePart[]}
     */
    get certified_transactions() {
        const ret = wasm.__wbg_get_cardanotransactionsproofs_certified_transactions(this.__wbg_ptr);
        var v1 = getArrayJsValueFromWasm0(ret[0], ret[1]).slice();
        wasm.__wbindgen_free(ret[0], ret[1] * 4, 4);
        return v1;
    }
    /**
     * Transactions that have been certified
     * @param {CardanoTransactionsSetProofMessagePart[]} arg0
     */
    set certified_transactions(arg0) {
        const ptr0 = passArrayJsValueToWasm0(arg0, wasm.__wbindgen_malloc);
        const len0 = WASM_VECTOR_LEN;
        wasm.__wbg_set_cardanotransactionsproofs_certified_transactions(this.__wbg_ptr, ptr0, len0);
    }
    /**
     * Transactions that could not be certified
     * @returns {string[]}
     */
    get non_certified_transactions() {
        const ret = wasm.__wbg_get_cardanotransactionsproofs_non_certified_transactions(this.__wbg_ptr);
        var v1 = getArrayJsValueFromWasm0(ret[0], ret[1]).slice();
        wasm.__wbindgen_free(ret[0], ret[1] * 4, 4);
        return v1;
    }
    /**
     * Transactions that could not be certified
     * @param {string[]} arg0
     */
    set non_certified_transactions(arg0) {
        const ptr0 = passArrayJsValueToWasm0(arg0, wasm.__wbindgen_malloc);
        const len0 = WASM_VECTOR_LEN;
        wasm.__wbg_set_cardanotransactionsproofs_non_certified_transactions(this.__wbg_ptr, ptr0, len0);
    }
    /**
     * Latest block number that has been certified
     * @returns {BlockNumber}
     */
    get latest_block_number() {
        const ret = wasm.__wbg_get_cardanotransactionsproofs_latest_block_number(this.__wbg_ptr);
        return BlockNumber.__wrap(ret);
    }
    /**
     * Latest block number that has been certified
     * @param {BlockNumber} arg0
     */
    set latest_block_number(arg0) {
        _assertClass(arg0, BlockNumber);
        var ptr0 = arg0.__destroy_into_raw();
        wasm.__wbg_set_cardanotransactionsproofs_latest_block_number(this.__wbg_ptr, ptr0);
    }
    /**
     * Transactions that have been certified
     * @returns {string[]}
     */
    get transactions_hashes() {
        const ret = wasm.cardanotransactionsproofs_transactions_hashes(this.__wbg_ptr);
        var v1 = getArrayJsValueFromWasm0(ret[0], ret[1]).slice();
        wasm.__wbindgen_free(ret[0], ret[1] * 4, 4);
        return v1;
    }
}
if (Symbol.dispose) CardanoTransactionsProofs.prototype[Symbol.dispose] = CardanoTransactionsProofs.prototype.free;
exports.CardanoTransactionsProofs = CardanoTransactionsProofs;

/**
 * A cryptographic proof of a set of Cardano transactions is included in the global Cardano transactions set
 */
class CardanoTransactionsSetProofMessagePart {
    static __wrap(ptr) {
        ptr = ptr >>> 0;
        const obj = Object.create(CardanoTransactionsSetProofMessagePart.prototype);
        obj.__wbg_ptr = ptr;
        CardanoTransactionsSetProofMessagePartFinalization.register(obj, obj.__wbg_ptr, obj);
        return obj;
    }
    static __unwrap(jsValue) {
        if (!(jsValue instanceof CardanoTransactionsSetProofMessagePart)) {
            return 0;
        }
        return jsValue.__destroy_into_raw();
    }
    __destroy_into_raw() {
        const ptr = this.__wbg_ptr;
        this.__wbg_ptr = 0;
        CardanoTransactionsSetProofMessagePartFinalization.unregister(this);
        return ptr;
    }
    free() {
        const ptr = this.__destroy_into_raw();
        wasm.__wbg_cardanotransactionssetproofmessagepart_free(ptr, 0);
    }
    /**
     * Hashes of the certified transactions
     * @returns {string[]}
     */
    get transactions_hashes() {
        const ret = wasm.__wbg_get_cardanotransactionssetproofmessagepart_transactions_hashes(this.__wbg_ptr);
        var v1 = getArrayJsValueFromWasm0(ret[0], ret[1]).slice();
        wasm.__wbindgen_free(ret[0], ret[1] * 4, 4);
        return v1;
    }
    /**
     * Hashes of the certified transactions
     * @param {string[]} arg0
     */
    set transactions_hashes(arg0) {
        const ptr0 = passArrayJsValueToWasm0(arg0, wasm.__wbindgen_malloc);
        const len0 = WASM_VECTOR_LEN;
        wasm.__wbg_set_cardanotransactionssetproofmessagepart_transactions_hashes(this.__wbg_ptr, ptr0, len0);
    }
    /**
     * Proof of the transactions
     * @returns {string}
     */
    get proof() {
        let deferred1_0;
        let deferred1_1;
        try {
            const ret = wasm.__wbg_get_cardanotransactionssetproofmessagepart_proof(this.__wbg_ptr);
            deferred1_0 = ret[0];
            deferred1_1 = ret[1];
            return getStringFromWasm0(ret[0], ret[1]);
        } finally {
            wasm.__wbindgen_free(deferred1_0, deferred1_1, 1);
        }
    }
    /**
     * Proof of the transactions
     * @param {string} arg0
     */
    set proof(arg0) {
        const ptr0 = passStringToWasm0(arg0, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
        const len0 = WASM_VECTOR_LEN;
        wasm.__wbg_set_cardanotransactionssetproofmessagepart_proof(this.__wbg_ptr, ptr0, len0);
    }
}
if (Symbol.dispose) CardanoTransactionsSetProofMessagePart.prototype[Symbol.dispose] = CardanoTransactionsSetProofMessagePart.prototype.free;
exports.CardanoTransactionsSetProofMessagePart = CardanoTransactionsSetProofMessagePart;

class IntoUnderlyingByteSource {
    __destroy_into_raw() {
        const ptr = this.__wbg_ptr;
        this.__wbg_ptr = 0;
        IntoUnderlyingByteSourceFinalization.unregister(this);
        return ptr;
    }
    free() {
        const ptr = this.__destroy_into_raw();
        wasm.__wbg_intounderlyingbytesource_free(ptr, 0);
    }
    /**
     * @returns {number}
     */
    get autoAllocateChunkSize() {
        const ret = wasm.intounderlyingbytesource_autoAllocateChunkSize(this.__wbg_ptr);
        return ret >>> 0;
    }
    /**
     * @param {ReadableByteStreamController} controller
     * @returns {Promise<any>}
     */
    pull(controller) {
        const ret = wasm.intounderlyingbytesource_pull(this.__wbg_ptr, controller);
        return ret;
    }
    /**
     * @param {ReadableByteStreamController} controller
     */
    start(controller) {
        wasm.intounderlyingbytesource_start(this.__wbg_ptr, controller);
    }
    /**
     * @returns {ReadableStreamType}
     */
    get type() {
        const ret = wasm.intounderlyingbytesource_type(this.__wbg_ptr);
        return __wbindgen_enum_ReadableStreamType[ret];
    }
    cancel() {
        const ptr = this.__destroy_into_raw();
        wasm.intounderlyingbytesource_cancel(ptr);
    }
}
if (Symbol.dispose) IntoUnderlyingByteSource.prototype[Symbol.dispose] = IntoUnderlyingByteSource.prototype.free;
exports.IntoUnderlyingByteSource = IntoUnderlyingByteSource;

class IntoUnderlyingSink {
    __destroy_into_raw() {
        const ptr = this.__wbg_ptr;
        this.__wbg_ptr = 0;
        IntoUnderlyingSinkFinalization.unregister(this);
        return ptr;
    }
    free() {
        const ptr = this.__destroy_into_raw();
        wasm.__wbg_intounderlyingsink_free(ptr, 0);
    }
    /**
     * @param {any} reason
     * @returns {Promise<any>}
     */
    abort(reason) {
        const ptr = this.__destroy_into_raw();
        const ret = wasm.intounderlyingsink_abort(ptr, reason);
        return ret;
    }
    /**
     * @returns {Promise<any>}
     */
    close() {
        const ptr = this.__destroy_into_raw();
        const ret = wasm.intounderlyingsink_close(ptr);
        return ret;
    }
    /**
     * @param {any} chunk
     * @returns {Promise<any>}
     */
    write(chunk) {
        const ret = wasm.intounderlyingsink_write(this.__wbg_ptr, chunk);
        return ret;
    }
}
if (Symbol.dispose) IntoUnderlyingSink.prototype[Symbol.dispose] = IntoUnderlyingSink.prototype.free;
exports.IntoUnderlyingSink = IntoUnderlyingSink;

class IntoUnderlyingSource {
    __destroy_into_raw() {
        const ptr = this.__wbg_ptr;
        this.__wbg_ptr = 0;
        IntoUnderlyingSourceFinalization.unregister(this);
        return ptr;
    }
    free() {
        const ptr = this.__destroy_into_raw();
        wasm.__wbg_intounderlyingsource_free(ptr, 0);
    }
    /**
     * @param {ReadableStreamDefaultController} controller
     * @returns {Promise<any>}
     */
    pull(controller) {
        const ret = wasm.intounderlyingsource_pull(this.__wbg_ptr, controller);
        return ret;
    }
    cancel() {
        const ptr = this.__destroy_into_raw();
        wasm.intounderlyingsource_cancel(ptr);
    }
}
if (Symbol.dispose) IntoUnderlyingSource.prototype[Symbol.dispose] = IntoUnderlyingSource.prototype.free;
exports.IntoUnderlyingSource = IntoUnderlyingSource;

class JSBroadcastChannelFeedbackReceiver {
    __destroy_into_raw() {
        const ptr = this.__wbg_ptr;
        this.__wbg_ptr = 0;
        JSBroadcastChannelFeedbackReceiverFinalization.unregister(this);
        return ptr;
    }
    free() {
        const ptr = this.__destroy_into_raw();
        wasm.__wbg_jsbroadcastchannelfeedbackreceiver_free(ptr, 0);
    }
}
if (Symbol.dispose) JSBroadcastChannelFeedbackReceiver.prototype[Symbol.dispose] = JSBroadcastChannelFeedbackReceiver.prototype.free;
exports.JSBroadcastChannelFeedbackReceiver = JSBroadcastChannelFeedbackReceiver;

/**
 * Structure that wraps a [Client] and enables its functions to be used in WASM
 */
class MithrilClient {
    __destroy_into_raw() {
        const ptr = this.__wbg_ptr;
        this.__wbg_ptr = 0;
        MithrilClientFinalization.unregister(this);
        return ptr;
    }
    free() {
        const ptr = this.__destroy_into_raw();
        wasm.__wbg_mithrilclient_free(ptr, 0);
    }
    /**
     * Call the client to get a cardano database snapshot from a hash
     * @param {string} hash
     * @returns {Promise<any>}
     */
    get_cardano_database_v2(hash) {
        const ptr0 = passStringToWasm0(hash, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
        const len0 = WASM_VECTOR_LEN;
        const ret = wasm.mithrilclient_get_cardano_database_v2(this.__wbg_ptr, ptr0, len0);
        return ret;
    }
    /**
     * Call the client to get a mithril certificate from a certificate hash
     * @param {string} hash
     * @returns {Promise<any>}
     */
    get_mithril_certificate(hash) {
        const ptr0 = passStringToWasm0(hash, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
        const len0 = WASM_VECTOR_LEN;
        const ret = wasm.mithrilclient_get_mithril_certificate(this.__wbg_ptr, ptr0, len0);
        return ret;
    }
    /**
     * Call the client for the list of available cardano database snapshots
     * @returns {Promise<any>}
     */
    list_cardano_database_v2() {
        const ret = wasm.mithrilclient_list_cardano_database_v2(this.__wbg_ptr);
        return ret;
    }
    /**
     * Call the client to verify the certificate chain from a certificate hash
     * @param {string} hash
     * @returns {Promise<any>}
     */
    verify_certificate_chain(hash) {
        const ptr0 = passStringToWasm0(hash, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
        const len0 = WASM_VECTOR_LEN;
        const ret = wasm.mithrilclient_verify_certificate_chain(this.__wbg_ptr, ptr0, len0);
        return ret;
    }
    /**
     * Call the client to fetch the current Mithril era
     * @returns {Promise<any>}
     */
    fetch_current_mithril_era() {
        const ret = wasm.mithrilclient_fetch_current_mithril_era(this.__wbg_ptr);
        return ret;
    }
    /**
     * Call the client for the list of available mithril certificates
     * @returns {Promise<any>}
     */
    list_mithril_certificates() {
        const ret = wasm.mithrilclient_list_mithril_certificates(this.__wbg_ptr);
        return ret;
    }
    /**
     * Call the client to get a snapshot from a digest
     *
     * @deprecated superseded by `get_cardano_database_v2_snapshot`
     * @param {string} digest
     * @returns {Promise<any>}
     */
    get_cardano_database_snapshot(digest) {
        const ptr0 = passStringToWasm0(digest, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
        const len0 = WASM_VECTOR_LEN;
        const ret = wasm.mithrilclient_get_cardano_database_snapshot(this.__wbg_ptr, ptr0, len0);
        return ret;
    }
    /**
     * Call the client to get a cardano stake distribution from a hash
     * @param {string} hash
     * @returns {Promise<any>}
     */
    get_cardano_stake_distribution(hash) {
        const ptr0 = passStringToWasm0(hash, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
        const len0 = WASM_VECTOR_LEN;
        const ret = wasm.mithrilclient_get_cardano_stake_distribution(this.__wbg_ptr, ptr0, len0);
        return ret;
    }
    /**
     * Call the client to get a Cardano transactions proofs
     * @param {any[]} ctx_hashes
     * @returns {Promise<CardanoTransactionsProofs>}
     */
    get_cardano_transaction_proofs(ctx_hashes) {
        const ptr0 = passArrayJsValueToWasm0(ctx_hashes, wasm.__wbindgen_malloc);
        const len0 = WASM_VECTOR_LEN;
        const ret = wasm.mithrilclient_get_cardano_transaction_proofs(this.__wbg_ptr, ptr0, len0);
        return ret;
    }
    /**
     * Call the client to get a mithril stake distribution from a hash
     * @param {string} hash
     * @returns {Promise<any>}
     */
    get_mithril_stake_distribution(hash) {
        const ptr0 = passStringToWasm0(hash, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
        const len0 = WASM_VECTOR_LEN;
        const ret = wasm.mithrilclient_get_mithril_stake_distribution(this.__wbg_ptr, ptr0, len0);
        return ret;
    }
    /**
     * Call the client to get the list of available snapshots
     *
     * @deprecated superseded by `list_cardano_database_v2`
     * @returns {Promise<any>}
     */
    list_cardano_database_snapshots() {
        const ret = wasm.mithrilclient_list_cardano_database_snapshots(this.__wbg_ptr);
        return ret;
    }
    /**
     * Call the client for the list of available cardano stake distributions
     * @returns {Promise<any>}
     */
    list_cardano_stake_distributions() {
        const ret = wasm.mithrilclient_list_cardano_stake_distributions(this.__wbg_ptr);
        return ret;
    }
    /**
     * Call the client for the list of available mithril stake distributions
     * @returns {Promise<any>}
     */
    list_mithril_stake_distributions() {
        const ret = wasm.mithrilclient_list_mithril_stake_distributions(this.__wbg_ptr);
        return ret;
    }
    /**
     * `unstable` Reset the certificate verifier cache if enabled
     * @returns {Promise<void>}
     */
    reset_certificate_verifier_cache() {
        const ret = wasm.mithrilclient_reset_certificate_verifier_cache(this.__wbg_ptr);
        return ret;
    }
    /**
     * Call the client to verify a mithril stake distribution message
     * @param {any} message
     * @param {any} certificate
     * @returns {Promise<any>}
     */
    verify_message_match_certificate(message, certificate) {
        const ret = wasm.mithrilclient_verify_message_match_certificate(this.__wbg_ptr, message, certificate);
        return ret;
    }
    /**
     * Call the client to get a Cardano transactions snapshot from a hash
     * @param {string} hash
     * @returns {Promise<any>}
     */
    get_cardano_transactions_snapshot(hash) {
        const ptr0 = passStringToWasm0(hash, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
        const len0 = WASM_VECTOR_LEN;
        const ret = wasm.mithrilclient_get_cardano_transactions_snapshot(this.__wbg_ptr, ptr0, len0);
        return ret;
    }
    /**
     * Call the client for the list of available cardano database snapshots for a given epoch
     * @param {bigint} epoch
     * @returns {Promise<any>}
     */
    list_cardano_database_v2_per_epoch(epoch) {
        const ret = wasm.mithrilclient_list_cardano_database_v2_per_epoch(this.__wbg_ptr, epoch);
        return ret;
    }
    /**
     * Call the client for the list of available Cardano transactions snapshots
     * @returns {Promise<any>}
     */
    list_cardano_transactions_snapshots() {
        const ret = wasm.mithrilclient_list_cardano_transactions_snapshots(this.__wbg_ptr);
        return ret;
    }
    /**
     * `unstable` Check if the certificate verifier cache is enabled
     * @returns {Promise<boolean>}
     */
    is_certificate_verifier_cache_enabled() {
        const ret = wasm.mithrilclient_is_certificate_verifier_cache_enabled(this.__wbg_ptr);
        return ret;
    }
    /**
     * Call the client to get a cardano stake distribution from an epoch
     *
     * The epoch represents the epoch at the end of which the Cardano stake distribution is computed by the Cardano node
     * @param {bigint} epoch
     * @returns {Promise<any>}
     */
    get_cardano_stake_distribution_by_epoch(epoch) {
        const ret = wasm.mithrilclient_get_cardano_stake_distribution_by_epoch(this.__wbg_ptr, epoch);
        return ret;
    }
    /**
     * Constructor for wasm client
     * @param {string} aggregator_endpoint
     * @param {string} genesis_verification_key
     * @param {any} options
     */
    constructor(aggregator_endpoint, genesis_verification_key, options) {
        const ptr0 = passStringToWasm0(aggregator_endpoint, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
        const len0 = WASM_VECTOR_LEN;
        const ptr1 = passStringToWasm0(genesis_verification_key, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
        const len1 = WASM_VECTOR_LEN;
        const ret = wasm.mithrilclient_new(ptr0, len0, ptr1, len1, options);
        this.__wbg_ptr = ret >>> 0;
        MithrilClientFinalization.register(this, this.__wbg_ptr, this);
        return this;
    }
    /**
     * Call the client for the list of available cardano database snapshots for the latest epoch
     *
     * An optional offset can be provided
     * @param {any} param
     * @returns {Promise<any>}
     */
    list_cardano_database_v2_for_latest_epoch(param) {
        const ret = wasm.mithrilclient_list_cardano_database_v2_for_latest_epoch(this.__wbg_ptr, param);
        return ret;
    }
    /**
     * Call the client to compute a cardano stake distribution message
     * @param {any} certificate
     * @param {any} cardano_stake_distribution
     * @returns {Promise<any>}
     */
    compute_cardano_stake_distribution_message(certificate, cardano_stake_distribution) {
        const ret = wasm.mithrilclient_compute_cardano_stake_distribution_message(this.__wbg_ptr, certificate, cardano_stake_distribution);
        return ret;
    }
    /**
     * Call the client to compute a mithril stake distribution message
     * @param {any} stake_distribution
     * @param {any} certificate
     * @returns {Promise<any>}
     */
    compute_mithril_stake_distribution_message(stake_distribution, certificate) {
        const ret = wasm.mithrilclient_compute_mithril_stake_distribution_message(this.__wbg_ptr, stake_distribution, certificate);
        return ret;
    }
    /**
     * Call the client to get a cardano stake distribution from an epoch
     * @param {any} param
     * @returns {Promise<any>}
     */
    get_cardano_stake_distribution_for_latest_epoch(param) {
        const ret = wasm.mithrilclient_get_cardano_stake_distribution_for_latest_epoch(this.__wbg_ptr, param);
        return ret;
    }
    /**
     * Call the client to verify a cardano transaction proof and compute a message
     * @param {CardanoTransactionsProofs} cardano_transaction_proof
     * @param {any} certificate
     * @returns {Promise<any>}
     */
    verify_cardano_transaction_proof_then_compute_message(cardano_transaction_proof, certificate) {
        _assertClass(cardano_transaction_proof, CardanoTransactionsProofs);
        const ret = wasm.mithrilclient_verify_cardano_transaction_proof_then_compute_message(this.__wbg_ptr, cardano_transaction_proof.__wbg_ptr, certificate);
        return ret;
    }
}
if (Symbol.dispose) MithrilClient.prototype[Symbol.dispose] = MithrilClient.prototype.free;
exports.MithrilClient = MithrilClient;

exports.__wbg_Error_52673b7de5a0ca89 = function(arg0, arg1) {
    const ret = Error(getStringFromWasm0(arg0, arg1));
    return ret;
};

exports.__wbg_Number_2d1dcfcf4ec51736 = function(arg0) {
    const ret = Number(arg0);
    return ret;
};

exports.__wbg___wbindgen_bigint_get_as_i64_6e32f5e6aff02e1d = function(arg0, arg1) {
    const v = arg1;
    const ret = typeof(v) === 'bigint' ? v : undefined;
    getDataViewMemory0().setBigInt64(arg0 + 8 * 1, isLikeNone(ret) ? BigInt(0) : ret, true);
    getDataViewMemory0().setInt32(arg0 + 4 * 0, !isLikeNone(ret), true);
};

exports.__wbg___wbindgen_boolean_get_dea25b33882b895b = function(arg0) {
    const v = arg0;
    const ret = typeof(v) === 'boolean' ? v : undefined;
    return isLikeNone(ret) ? 0xFFFFFF : ret ? 1 : 0;
};

exports.__wbg___wbindgen_debug_string_adfb662ae34724b6 = function(arg0, arg1) {
    const ret = debugString(arg1);
    const ptr1 = passStringToWasm0(ret, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
    const len1 = WASM_VECTOR_LEN;
    getDataViewMemory0().setInt32(arg0 + 4 * 1, len1, true);
    getDataViewMemory0().setInt32(arg0 + 4 * 0, ptr1, true);
};

exports.__wbg___wbindgen_in_0d3e1e8f0c669317 = function(arg0, arg1) {
    const ret = arg0 in arg1;
    return ret;
};

exports.__wbg___wbindgen_is_bigint_0e1a2e3f55cfae27 = function(arg0) {
    const ret = typeof(arg0) === 'bigint';
    return ret;
};

exports.__wbg___wbindgen_is_function_8d400b8b1af978cd = function(arg0) {
    const ret = typeof(arg0) === 'function';
    return ret;
};

exports.__wbg___wbindgen_is_object_ce774f3490692386 = function(arg0) {
    const val = arg0;
    const ret = typeof(val) === 'object' && val !== null;
    return ret;
};

exports.__wbg___wbindgen_is_string_704ef9c8fc131030 = function(arg0) {
    const ret = typeof(arg0) === 'string';
    return ret;
};

exports.__wbg___wbindgen_is_undefined_f6b95eab589e0269 = function(arg0) {
    const ret = arg0 === undefined;
    return ret;
};

exports.__wbg___wbindgen_jsval_eq_b6101cc9cef1fe36 = function(arg0, arg1) {
    const ret = arg0 === arg1;
    return ret;
};

exports.__wbg___wbindgen_jsval_loose_eq_766057600fdd1b0d = function(arg0, arg1) {
    const ret = arg0 == arg1;
    return ret;
};

exports.__wbg___wbindgen_number_get_9619185a74197f95 = function(arg0, arg1) {
    const obj = arg1;
    const ret = typeof(obj) === 'number' ? obj : undefined;
    getDataViewMemory0().setFloat64(arg0 + 8 * 1, isLikeNone(ret) ? 0 : ret, true);
    getDataViewMemory0().setInt32(arg0 + 4 * 0, !isLikeNone(ret), true);
};

exports.__wbg___wbindgen_string_get_a2a31e16edf96e42 = function(arg0, arg1) {
    const obj = arg1;
    const ret = typeof(obj) === 'string' ? obj : undefined;
    var ptr1 = isLikeNone(ret) ? 0 : passStringToWasm0(ret, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
    var len1 = WASM_VECTOR_LEN;
    getDataViewMemory0().setInt32(arg0 + 4 * 1, len1, true);
    getDataViewMemory0().setInt32(arg0 + 4 * 0, ptr1, true);
};

exports.__wbg___wbindgen_throw_dd24417ed36fc46e = function(arg0, arg1) {
    throw new Error(getStringFromWasm0(arg0, arg1));
};

exports.__wbg__wbg_cb_unref_87dfb5aaa0cbcea7 = function(arg0) {
    arg0._wbg_cb_unref();
};

exports.__wbg_abort_07646c894ebbf2bd = function(arg0) {
    arg0.abort();
};

exports.__wbg_abort_399ecbcfd6ef3c8e = function(arg0, arg1) {
    arg0.abort(arg1);
};

exports.__wbg_append_c5cbdf46455cc776 = function() { return handleError(function (arg0, arg1, arg2, arg3, arg4) {
    arg0.append(getStringFromWasm0(arg1, arg2), getStringFromWasm0(arg3, arg4));
}, arguments) };

exports.__wbg_arrayBuffer_c04af4fce566092d = function() { return handleError(function (arg0) {
    const ret = arg0.arrayBuffer();
    return ret;
}, arguments) };

exports.__wbg_buffer_6cb2fecb1f253d71 = function(arg0) {
    const ret = arg0.buffer;
    return ret;
};

exports.__wbg_byobRequest_f8e3517f5f8ad284 = function(arg0) {
    const ret = arg0.byobRequest;
    return isLikeNone(ret) ? 0 : addToExternrefTable0(ret);
};

exports.__wbg_byteLength_faa9938885bdeee6 = function(arg0) {
    const ret = arg0.byteLength;
    return ret;
};

exports.__wbg_byteOffset_3868b6a19ba01dea = function(arg0) {
    const ret = arg0.byteOffset;
    return ret;
};

exports.__wbg_call_3020136f7a2d6e44 = function() { return handleError(function (arg0, arg1, arg2) {
    const ret = arg0.call(arg1, arg2);
    return ret;
}, arguments) };

exports.__wbg_call_abb4ff46ce38be40 = function() { return handleError(function (arg0, arg1) {
    const ret = arg0.call(arg1);
    return ret;
}, arguments) };

exports.__wbg_cardanotransactionsproofs_new = function(arg0) {
    const ret = CardanoTransactionsProofs.__wrap(arg0);
    return ret;
};

exports.__wbg_cardanotransactionssetproofmessagepart_new = function(arg0) {
    const ret = CardanoTransactionsSetProofMessagePart.__wrap(arg0);
    return ret;
};

exports.__wbg_cardanotransactionssetproofmessagepart_unwrap = function(arg0) {
    const ret = CardanoTransactionsSetProofMessagePart.__unwrap(arg0);
    return ret;
};

exports.__wbg_clearTimeout_7a42b49784aea641 = function(arg0) {
    const ret = clearTimeout(arg0);
    return ret;
};

exports.__wbg_close_0af5661bf3d335f2 = function() { return handleError(function (arg0) {
    arg0.close();
}, arguments) };

exports.__wbg_close_3ec111e7b23d94d8 = function() { return handleError(function (arg0) {
    arg0.close();
}, arguments) };

exports.__wbg_close_c956ddbf0426a990 = function(arg0) {
    arg0.close();
};

exports.__wbg_done_62ea16af4ce34b24 = function(arg0) {
    const ret = arg0.done;
    return ret;
};

exports.__wbg_enqueue_a7e6b1ee87963aad = function() { return handleError(function (arg0, arg1) {
    arg0.enqueue(arg1);
}, arguments) };

exports.__wbg_entries_83c79938054e065f = function(arg0) {
    const ret = Object.entries(arg0);
    return ret;
};

exports.__wbg_fetch_74a3e84ebd2c9a0e = function(arg0) {
    const ret = fetch(arg0);
    return ret;
};

exports.__wbg_fetch_90447c28cc0b095e = function(arg0, arg1) {
    const ret = arg0.fetch(arg1);
    return ret;
};

exports.__wbg_getItem_1340bfc9a10d5991 = function() { return handleError(function (arg0, arg1, arg2, arg3) {
    const ret = arg1.getItem(getStringFromWasm0(arg2, arg3));
    var ptr1 = isLikeNone(ret) ? 0 : passStringToWasm0(ret, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
    var len1 = WASM_VECTOR_LEN;
    getDataViewMemory0().setInt32(arg0 + 4 * 1, len1, true);
    getDataViewMemory0().setInt32(arg0 + 4 * 0, ptr1, true);
}, arguments) };

exports.__wbg_getRandomValues_9b655bdd369112f2 = function() { return handleError(function (arg0, arg1) {
    globalThis.crypto.getRandomValues(getArrayU8FromWasm0(arg0, arg1));
}, arguments) };

exports.__wbg_getTime_ad1e9878a735af08 = function(arg0) {
    const ret = arg0.getTime();
    return ret;
};

exports.__wbg_get_6b7bd52aca3f9671 = function(arg0, arg1) {
    const ret = arg0[arg1 >>> 0];
    return ret;
};

exports.__wbg_get_af9dab7e9603ea93 = function() { return handleError(function (arg0, arg1) {
    const ret = Reflect.get(arg0, arg1);
    return ret;
}, arguments) };

exports.__wbg_get_with_ref_key_1dc361bd10053bfe = function(arg0, arg1) {
    const ret = arg0[arg1];
    return ret;
};

exports.__wbg_has_0e670569d65d3a45 = function() { return handleError(function (arg0, arg1) {
    const ret = Reflect.has(arg0, arg1);
    return ret;
}, arguments) };

exports.__wbg_headers_654c30e1bcccc552 = function(arg0) {
    const ret = arg0.headers;
    return ret;
};

exports.__wbg_instanceof_ArrayBuffer_f3320d2419cd0355 = function(arg0) {
    let result;
    try {
        result = arg0 instanceof ArrayBuffer;
    } catch (_) {
        result = false;
    }
    const ret = result;
    return ret;
};

exports.__wbg_instanceof_Response_cd74d1c2ac92cb0b = function(arg0) {
    let result;
    try {
        result = arg0 instanceof Response;
    } catch (_) {
        result = false;
    }
    const ret = result;
    return ret;
};

exports.__wbg_instanceof_Uint8Array_da54ccc9d3e09434 = function(arg0) {
    let result;
    try {
        result = arg0 instanceof Uint8Array;
    } catch (_) {
        result = false;
    }
    const ret = result;
    return ret;
};

exports.__wbg_instanceof_Window_b5cf7783caa68180 = function(arg0) {
    let result;
    try {
        result = arg0 instanceof Window;
    } catch (_) {
        result = false;
    }
    const ret = result;
    return ret;
};

exports.__wbg_isArray_51fd9e6422c0a395 = function(arg0) {
    const ret = Array.isArray(arg0);
    return ret;
};

exports.__wbg_isSafeInteger_ae7d3f054d55fa16 = function(arg0) {
    const ret = Number.isSafeInteger(arg0);
    return ret;
};

exports.__wbg_iterator_27b7c8b35ab3e86b = function() {
    const ret = Symbol.iterator;
    return ret;
};

exports.__wbg_key_45e131ab9f56e328 = function() { return handleError(function (arg0, arg1, arg2) {
    const ret = arg1.key(arg2 >>> 0);
    var ptr1 = isLikeNone(ret) ? 0 : passStringToWasm0(ret, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
    var len1 = WASM_VECTOR_LEN;
    getDataViewMemory0().setInt32(arg0 + 4 * 1, len1, true);
    getDataViewMemory0().setInt32(arg0 + 4 * 0, ptr1, true);
}, arguments) };

exports.__wbg_length_22ac23eaec9d8053 = function(arg0) {
    const ret = arg0.length;
    return ret;
};

exports.__wbg_length_642e4ab7950e94e4 = function() { return handleError(function (arg0) {
    const ret = arg0.length;
    return ret;
}, arguments) };

exports.__wbg_length_d45040a40c570362 = function(arg0) {
    const ret = arg0.length;
    return ret;
};

exports.__wbg_localStorage_e7a9e9fee8fc608d = function() { return handleError(function (arg0) {
    const ret = arg0.localStorage;
    return isLikeNone(ret) ? 0 : addToExternrefTable0(ret);
}, arguments) };

exports.__wbg_new_0_23cedd11d9b40c9d = function() {
    const ret = new Date();
    return ret;
};

exports.__wbg_new_1ba21ce319a06297 = function() {
    const ret = new Object();
    return ret;
};

exports.__wbg_new_25f239778d6112b9 = function() {
    const ret = new Array();
    return ret;
};

exports.__wbg_new_3c79b3bb1b32b7d3 = function() { return handleError(function () {
    const ret = new Headers();
    return ret;
}, arguments) };

exports.__wbg_new_6421f6084cc5bc5a = function(arg0) {
    const ret = new Uint8Array(arg0);
    return ret;
};

exports.__wbg_new_881a222c65f168fc = function() { return handleError(function () {
    const ret = new AbortController();
    return ret;
}, arguments) };

exports.__wbg_new_b3dd747604c3c93e = function() { return handleError(function (arg0, arg1) {
    const ret = new BroadcastChannel(getStringFromWasm0(arg0, arg1));
    return ret;
}, arguments) };

exports.__wbg_new_b546ae120718850e = function() {
    const ret = new Map();
    return ret;
};

exports.__wbg_new_df1173567d5ff028 = function(arg0, arg1) {
    const ret = new Error(getStringFromWasm0(arg0, arg1));
    return ret;
};

exports.__wbg_new_ff12d2b041fb48f1 = function(arg0, arg1) {
    try {
        var state0 = {a: arg0, b: arg1};
        var cb0 = (arg0, arg1) => {
            const a = state0.a;
            state0.a = 0;
            try {
                return wasm_bindgen__convert__closures_____invoke__h2da143d4463a5f08(a, state0.b, arg0, arg1);
            } finally {
                state0.a = a;
            }
        };
        const ret = new Promise(cb0);
        return ret;
    } finally {
        state0.a = state0.b = 0;
    }
};

exports.__wbg_new_from_slice_f9c22b9153b26992 = function(arg0, arg1) {
    const ret = new Uint8Array(getArrayU8FromWasm0(arg0, arg1));
    return ret;
};

exports.__wbg_new_no_args_cb138f77cf6151ee = function(arg0, arg1) {
    const ret = new Function(getStringFromWasm0(arg0, arg1));
    return ret;
};

exports.__wbg_new_with_byte_offset_and_length_d85c3da1fd8df149 = function(arg0, arg1, arg2) {
    const ret = new Uint8Array(arg0, arg1 >>> 0, arg2 >>> 0);
    return ret;
};

exports.__wbg_new_with_str_and_init_c5748f76f5108934 = function() { return handleError(function (arg0, arg1, arg2) {
    const ret = new Request(getStringFromWasm0(arg0, arg1), arg2);
    return ret;
}, arguments) };

exports.__wbg_next_138a17bbf04e926c = function(arg0) {
    const ret = arg0.next;
    return ret;
};

exports.__wbg_next_3cfe5c0fe2a4cc53 = function() { return handleError(function (arg0) {
    const ret = arg0.next();
    return ret;
}, arguments) };

exports.__wbg_postMessage_ee7b4e76cd1ed685 = function() { return handleError(function (arg0, arg1) {
    arg0.postMessage(arg1);
}, arguments) };

exports.__wbg_prototypesetcall_dfe9b766cdc1f1fd = function(arg0, arg1, arg2) {
    Uint8Array.prototype.set.call(getArrayU8FromWasm0(arg0, arg1), arg2);
};

exports.__wbg_queueMicrotask_9b549dfce8865860 = function(arg0) {
    const ret = arg0.queueMicrotask;
    return ret;
};

exports.__wbg_queueMicrotask_fca69f5bfad613a5 = function(arg0) {
    queueMicrotask(arg0);
};

exports.__wbg_removeItem_33ed1aeb2dc68e96 = function() { return handleError(function (arg0, arg1, arg2) {
    arg0.removeItem(getStringFromWasm0(arg1, arg2));
}, arguments) };

exports.__wbg_resolve_fd5bfbaa4ce36e1e = function(arg0) {
    const ret = Promise.resolve(arg0);
    return ret;
};

exports.__wbg_respond_9f7fc54636c4a3af = function() { return handleError(function (arg0, arg1) {
    arg0.respond(arg1 >>> 0);
}, arguments) };

exports.__wbg_setItem_1167ad38996d6426 = function() { return handleError(function (arg0, arg1, arg2, arg3, arg4) {
    arg0.setItem(getStringFromWasm0(arg1, arg2), getStringFromWasm0(arg3, arg4));
}, arguments) };

exports.__wbg_setTimeout_7bb3429662ab1e70 = function(arg0, arg1) {
    const ret = setTimeout(arg0, arg1);
    return ret;
};

exports.__wbg_set_169e13b608078b7b = function(arg0, arg1, arg2) {
    arg0.set(getArrayU8FromWasm0(arg1, arg2));
};

exports.__wbg_set_3f1d0b984ed272ed = function(arg0, arg1, arg2) {
    arg0[arg1] = arg2;
};

exports.__wbg_set_7df433eea03a5c14 = function(arg0, arg1, arg2) {
    arg0[arg1 >>> 0] = arg2;
};

exports.__wbg_set_body_8e743242d6076a4f = function(arg0, arg1) {
    arg0.body = arg1;
};

exports.__wbg_set_cache_0e437c7c8e838b9b = function(arg0, arg1) {
    arg0.cache = __wbindgen_enum_RequestCache[arg1];
};

exports.__wbg_set_credentials_55ae7c3c106fd5be = function(arg0, arg1) {
    arg0.credentials = __wbindgen_enum_RequestCredentials[arg1];
};

exports.__wbg_set_efaaf145b9377369 = function(arg0, arg1, arg2) {
    const ret = arg0.set(arg1, arg2);
    return ret;
};

exports.__wbg_set_headers_5671cf088e114d2b = function(arg0, arg1) {
    arg0.headers = arg1;
};

exports.__wbg_set_method_76c69e41b3570627 = function(arg0, arg1, arg2) {
    arg0.method = getStringFromWasm0(arg1, arg2);
};

exports.__wbg_set_mode_611016a6818fc690 = function(arg0, arg1) {
    arg0.mode = __wbindgen_enum_RequestMode[arg1];
};

exports.__wbg_set_signal_e89be862d0091009 = function(arg0, arg1) {
    arg0.signal = arg1;
};

exports.__wbg_signal_3c14fbdc89694b39 = function(arg0) {
    const ret = arg0.signal;
    return ret;
};

exports.__wbg_static_accessor_GLOBAL_769e6b65d6557335 = function() {
    const ret = typeof global === 'undefined' ? null : global;
    return isLikeNone(ret) ? 0 : addToExternrefTable0(ret);
};

exports.__wbg_static_accessor_GLOBAL_THIS_60cf02db4de8e1c1 = function() {
    const ret = typeof globalThis === 'undefined' ? null : globalThis;
    return isLikeNone(ret) ? 0 : addToExternrefTable0(ret);
};

exports.__wbg_static_accessor_SELF_08f5a74c69739274 = function() {
    const ret = typeof self === 'undefined' ? null : self;
    return isLikeNone(ret) ? 0 : addToExternrefTable0(ret);
};

exports.__wbg_static_accessor_WINDOW_a8924b26aa92d024 = function() {
    const ret = typeof window === 'undefined' ? null : window;
    return isLikeNone(ret) ? 0 : addToExternrefTable0(ret);
};

exports.__wbg_status_9bfc680efca4bdfd = function(arg0) {
    const ret = arg0.status;
    return ret;
};

exports.__wbg_stringify_655a6390e1f5eb6b = function() { return handleError(function (arg0) {
    const ret = JSON.stringify(arg0);
    return ret;
}, arguments) };

exports.__wbg_text_51046bb33d257f63 = function() { return handleError(function (arg0) {
    const ret = arg0.text();
    return ret;
}, arguments) };

exports.__wbg_then_429f7caf1026411d = function(arg0, arg1, arg2) {
    const ret = arg0.then(arg1, arg2);
    return ret;
};

exports.__wbg_then_4f95312d68691235 = function(arg0, arg1) {
    const ret = arg0.then(arg1);
    return ret;
};

exports.__wbg_url_b6d11838a4f95198 = function(arg0, arg1) {
    const ret = arg1.url;
    const ptr1 = passStringToWasm0(ret, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
    const len1 = WASM_VECTOR_LEN;
    getDataViewMemory0().setInt32(arg0 + 4 * 1, len1, true);
    getDataViewMemory0().setInt32(arg0 + 4 * 0, ptr1, true);
};

exports.__wbg_value_57b7b035e117f7ee = function(arg0) {
    const ret = arg0.value;
    return ret;
};

exports.__wbg_view_788aaf149deefd2f = function(arg0) {
    const ret = arg0.view;
    return isLikeNone(ret) ? 0 : addToExternrefTable0(ret);
};

exports.__wbg_warn_6e567d0d926ff881 = function(arg0) {
    console.warn(arg0);
};

exports.__wbindgen_cast_17a320bf0cb03ca7 = function(arg0, arg1) {
    // Cast intrinsic for `Closure(Closure { dtor_idx: 1796, function: Function { arguments: [Externref], shim_idx: 1797, ret: Unit, inner_ret: Some(Unit) }, mutable: true }) -> Externref`.
    const ret = makeMutClosure(arg0, arg1, wasm.wasm_bindgen__closure__destroy__h99811cac73495ece, wasm_bindgen__convert__closures_____invoke__hc0a74f7bb86030d0);
    return ret;
};

exports.__wbindgen_cast_2241b6af4c4b2941 = function(arg0, arg1) {
    // Cast intrinsic for `Ref(String) -> Externref`.
    const ret = getStringFromWasm0(arg0, arg1);
    return ret;
};

exports.__wbindgen_cast_4625c577ab2ec9ee = function(arg0) {
    // Cast intrinsic for `U64 -> Externref`.
    const ret = BigInt.asUintN(64, arg0);
    return ret;
};

exports.__wbindgen_cast_7fcb4b52657c40f7 = function(arg0, arg1) {
    // Cast intrinsic for `Closure(Closure { dtor_idx: 1746, function: Function { arguments: [], shim_idx: 1747, ret: Unit, inner_ret: Some(Unit) }, mutable: true }) -> Externref`.
    const ret = makeMutClosure(arg0, arg1, wasm.wasm_bindgen__closure__destroy__hea47394e049eff9b, wasm_bindgen__convert__closures_____invoke__h6b7e05d46d107c93);
    return ret;
};

exports.__wbindgen_cast_d6cd19b81560fd6e = function(arg0) {
    // Cast intrinsic for `F64 -> Externref`.
    const ret = arg0;
    return ret;
};

exports.__wbindgen_init_externref_table = function() {
    const table = wasm.__wbindgen_externrefs;
    const offset = table.grow(4);
    table.set(0, undefined);
    table.set(offset + 0, undefined);
    table.set(offset + 1, null);
    table.set(offset + 2, true);
    table.set(offset + 3, false);
};

const wasmPath = `${__dirname}/mithril_client_wasm_bg.wasm`;
const wasmBytes = require('fs').readFileSync(wasmPath);
const wasmModule = new WebAssembly.Module(wasmBytes);
const wasm = exports.__wasm = new WebAssembly.Instance(wasmModule, imports).exports;

wasm.__wbindgen_start();
