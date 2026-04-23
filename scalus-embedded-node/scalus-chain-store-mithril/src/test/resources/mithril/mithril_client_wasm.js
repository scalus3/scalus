/* @ts-self-types="./mithril_client_wasm.d.ts" */

/**
 * Cardano block message representation
 */
class CardanoBlock {
    static __wrap(ptr) {
        ptr = ptr >>> 0;
        const obj = Object.create(CardanoBlock.prototype);
        obj.__wbg_ptr = ptr;
        CardanoBlockFinalization.register(obj, obj.__wbg_ptr, obj);
        return obj;
    }
    __destroy_into_raw() {
        const ptr = this.__wbg_ptr;
        this.__wbg_ptr = 0;
        CardanoBlockFinalization.unregister(this);
        return ptr;
    }
    free() {
        const ptr = this.__destroy_into_raw();
        wasm.__wbg_cardanoblock_free(ptr, 0);
    }
    /**
     * Block number
     * @returns {bigint}
     */
    get block_number() {
        const ret = wasm.cardanoblock_block_number(this.__wbg_ptr);
        return BigInt.asUintN(64, ret);
    }
    /**
     * Block number
     * @returns {bigint}
     */
    get slot_number() {
        const ret = wasm.cardanoblock_slot_number(this.__wbg_ptr);
        return BigInt.asUintN(64, ret);
    }
    /**
     * Block hash
     * @returns {string}
     */
    get block_hash() {
        let deferred1_0;
        let deferred1_1;
        try {
            const ret = wasm.__wbg_get_cardanoblock_block_hash(this.__wbg_ptr);
            deferred1_0 = ret[0];
            deferred1_1 = ret[1];
            return getStringFromWasm0(ret[0], ret[1]);
        } finally {
            wasm.__wbindgen_free(deferred1_0, deferred1_1, 1);
        }
    }
    /**
     * Block hash
     * @param {string} arg0
     */
    set block_hash(arg0) {
        const ptr0 = passStringToWasm0(arg0, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
        const len0 = WASM_VECTOR_LEN;
        wasm.__wbg_set_cardanoblock_block_hash(this.__wbg_ptr, ptr0, len0);
    }
}
if (Symbol.dispose) CardanoBlock.prototype[Symbol.dispose] = CardanoBlock.prototype.free;
exports.CardanoBlock = CardanoBlock;

/**
 * A cryptographic proof for a set of Cardano blocks
 */
class CardanoBlocksProofs {
    static __wrap(ptr) {
        ptr = ptr >>> 0;
        const obj = Object.create(CardanoBlocksProofs.prototype);
        obj.__wbg_ptr = ptr;
        CardanoBlocksProofsFinalization.register(obj, obj.__wbg_ptr, obj);
        return obj;
    }
    __destroy_into_raw() {
        const ptr = this.__wbg_ptr;
        this.__wbg_ptr = 0;
        CardanoBlocksProofsFinalization.unregister(this);
        return ptr;
    }
    free() {
        const ptr = this.__destroy_into_raw();
        wasm.__wbg_cardanoblocksproofs_free(ptr, 0);
    }
    /**
     * Hashes of the Cardano blocks that have been certified
     * @returns {string[]}
     */
    get blocks_hashes() {
        const ret = wasm.cardanoblocksproofs_blocks_hashes(this.__wbg_ptr);
        var v1 = getArrayJsValueFromWasm0(ret[0], ret[1]).slice();
        wasm.__wbindgen_free(ret[0], ret[1] * 4, 4);
        return v1;
    }
    /**
     * Cardano blocks that have been certified
     * @returns {CardanoBlock[]}
     */
    get certified_blocks() {
        const ret = wasm.cardanoblocksproofs_certified_blocks(this.__wbg_ptr);
        var v1 = getArrayJsValueFromWasm0(ret[0], ret[1]).slice();
        wasm.__wbindgen_free(ret[0], ret[1] * 4, 4);
        return v1;
    }
    /**
     * Latest block number that has been certified by the associated Mithril certificate
     * @returns {bigint}
     */
    get latest_block_number() {
        const ret = wasm.cardanoblocksproofs_latest_block_number(this.__wbg_ptr);
        return BigInt.asUintN(64, ret);
    }
    /**
     * Security parameter that has been certified by the associated Mithril certificate
     * @returns {bigint}
     */
    get security_parameter() {
        const ret = wasm.cardanoblocksproofs_security_parameter(this.__wbg_ptr);
        return BigInt.asUintN(64, ret);
    }
    /**
     * Hash of the certificate that validates this proof Merkle root
     * @returns {string}
     */
    get certificate_hash() {
        let deferred1_0;
        let deferred1_1;
        try {
            const ret = wasm.__wbg_get_cardanoblocksproofs_certificate_hash(this.__wbg_ptr);
            deferred1_0 = ret[0];
            deferred1_1 = ret[1];
            return getStringFromWasm0(ret[0], ret[1]);
        } finally {
            wasm.__wbindgen_free(deferred1_0, deferred1_1, 1);
        }
    }
    /**
     * Hashes of the blocks that could not be certified
     * @returns {string[]}
     */
    get non_certified_blocks() {
        const ret = wasm.__wbg_get_cardanoblocksproofs_non_certified_blocks(this.__wbg_ptr);
        var v1 = getArrayJsValueFromWasm0(ret[0], ret[1]).slice();
        wasm.__wbindgen_free(ret[0], ret[1] * 4, 4);
        return v1;
    }
    /**
     * Hash of the certificate that validates this proof Merkle root
     * @param {string} arg0
     */
    set certificate_hash(arg0) {
        const ptr0 = passStringToWasm0(arg0, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
        const len0 = WASM_VECTOR_LEN;
        wasm.__wbg_set_cardanoblocksproofs_certificate_hash(this.__wbg_ptr, ptr0, len0);
    }
    /**
     * Hashes of the blocks that could not be certified
     * @param {string[]} arg0
     */
    set non_certified_blocks(arg0) {
        const ptr0 = passArrayJsValueToWasm0(arg0, wasm.__wbindgen_malloc);
        const len0 = WASM_VECTOR_LEN;
        wasm.__wbg_set_cardanoblocksproofs_non_certified_blocks(this.__wbg_ptr, ptr0, len0);
    }
}
if (Symbol.dispose) CardanoBlocksProofs.prototype[Symbol.dispose] = CardanoBlocksProofs.prototype.free;
exports.CardanoBlocksProofs = CardanoBlocksProofs;

/**
 * Cardano transaction message representation
 */
class CardanoTransaction {
    static __wrap(ptr) {
        ptr = ptr >>> 0;
        const obj = Object.create(CardanoTransaction.prototype);
        obj.__wbg_ptr = ptr;
        CardanoTransactionFinalization.register(obj, obj.__wbg_ptr, obj);
        return obj;
    }
    __destroy_into_raw() {
        const ptr = this.__wbg_ptr;
        this.__wbg_ptr = 0;
        CardanoTransactionFinalization.unregister(this);
        return ptr;
    }
    free() {
        const ptr = this.__destroy_into_raw();
        wasm.__wbg_cardanotransaction_free(ptr, 0);
    }
    /**
     * Block number
     * @returns {bigint}
     */
    get block_number() {
        const ret = wasm.cardanotransaction_block_number(this.__wbg_ptr);
        return BigInt.asUintN(64, ret);
    }
    /**
     * Block number
     * @returns {bigint}
     */
    get slot_number() {
        const ret = wasm.cardanotransaction_slot_number(this.__wbg_ptr);
        return BigInt.asUintN(64, ret);
    }
    /**
     * Block hash of the transaction
     * @returns {string}
     */
    get block_hash() {
        let deferred1_0;
        let deferred1_1;
        try {
            const ret = wasm.__wbg_get_cardanotransaction_block_hash(this.__wbg_ptr);
            deferred1_0 = ret[0];
            deferred1_1 = ret[1];
            return getStringFromWasm0(ret[0], ret[1]);
        } finally {
            wasm.__wbindgen_free(deferred1_0, deferred1_1, 1);
        }
    }
    /**
     * Unique hash of the transaction
     * @returns {string}
     */
    get transaction_hash() {
        let deferred1_0;
        let deferred1_1;
        try {
            const ret = wasm.__wbg_get_cardanotransaction_transaction_hash(this.__wbg_ptr);
            deferred1_0 = ret[0];
            deferred1_1 = ret[1];
            return getStringFromWasm0(ret[0], ret[1]);
        } finally {
            wasm.__wbindgen_free(deferred1_0, deferred1_1, 1);
        }
    }
    /**
     * Block hash of the transaction
     * @param {string} arg0
     */
    set block_hash(arg0) {
        const ptr0 = passStringToWasm0(arg0, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
        const len0 = WASM_VECTOR_LEN;
        wasm.__wbg_set_cardanotransaction_block_hash(this.__wbg_ptr, ptr0, len0);
    }
    /**
     * Unique hash of the transaction
     * @param {string} arg0
     */
    set transaction_hash(arg0) {
        const ptr0 = passStringToWasm0(arg0, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
        const len0 = WASM_VECTOR_LEN;
        wasm.__wbg_set_cardanotransaction_transaction_hash(this.__wbg_ptr, ptr0, len0);
    }
}
if (Symbol.dispose) CardanoTransaction.prototype[Symbol.dispose] = CardanoTransaction.prototype.free;
exports.CardanoTransaction = CardanoTransaction;

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
     * Latest block number that has been certified
     * @returns {bigint}
     */
    get latest_block_number() {
        const ret = wasm.cardanotransactionsproofs_latest_block_number(this.__wbg_ptr);
        return BigInt.asUintN(64, ret);
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
     * @param {CardanoTransactionsSetProofMessagePart[]} arg0
     */
    set certified_transactions(arg0) {
        const ptr0 = passArrayJsValueToWasm0(arg0, wasm.__wbindgen_malloc);
        const len0 = WASM_VECTOR_LEN;
        wasm.__wbg_set_cardanotransactionsproofs_certified_transactions(this.__wbg_ptr, ptr0, len0);
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
}
if (Symbol.dispose) CardanoTransactionsProofs.prototype[Symbol.dispose] = CardanoTransactionsProofs.prototype.free;
exports.CardanoTransactionsProofs = CardanoTransactionsProofs;

/**
 * A cryptographic proof for a set of Cardano transactions
 */
class CardanoTransactionsProofsV2 {
    static __wrap(ptr) {
        ptr = ptr >>> 0;
        const obj = Object.create(CardanoTransactionsProofsV2.prototype);
        obj.__wbg_ptr = ptr;
        CardanoTransactionsProofsV2Finalization.register(obj, obj.__wbg_ptr, obj);
        return obj;
    }
    __destroy_into_raw() {
        const ptr = this.__wbg_ptr;
        this.__wbg_ptr = 0;
        CardanoTransactionsProofsV2Finalization.unregister(this);
        return ptr;
    }
    free() {
        const ptr = this.__destroy_into_raw();
        wasm.__wbg_cardanotransactionsproofsv2_free(ptr, 0);
    }
    /**
     * Cardano transactions that have been certified
     * @returns {CardanoTransaction[]}
     */
    get certified_transactions() {
        const ret = wasm.cardanotransactionsproofsv2_certified_transactions(this.__wbg_ptr);
        var v1 = getArrayJsValueFromWasm0(ret[0], ret[1]).slice();
        wasm.__wbindgen_free(ret[0], ret[1] * 4, 4);
        return v1;
    }
    /**
     * Latest block number that has been certified by the associated Mithril certificate
     * @returns {bigint}
     */
    get latest_block_number() {
        const ret = wasm.cardanotransactionsproofsv2_latest_block_number(this.__wbg_ptr);
        return BigInt.asUintN(64, ret);
    }
    /**
     * Security parameter that has been certified by the associated Mithril certificate
     * @returns {bigint}
     */
    get security_parameter() {
        const ret = wasm.cardanotransactionsproofsv2_security_parameter(this.__wbg_ptr);
        return BigInt.asUintN(64, ret);
    }
    /**
     * Hashes of the Cardano transactions that have been certified
     * @returns {string[]}
     */
    get transactions_hashes() {
        const ret = wasm.cardanotransactionsproofsv2_transactions_hashes(this.__wbg_ptr);
        var v1 = getArrayJsValueFromWasm0(ret[0], ret[1]).slice();
        wasm.__wbindgen_free(ret[0], ret[1] * 4, 4);
        return v1;
    }
    /**
     * Hash of the certificate that validates this proof Merkle root
     * @returns {string}
     */
    get certificate_hash() {
        let deferred1_0;
        let deferred1_1;
        try {
            const ret = wasm.__wbg_get_cardanotransactionsproofsv2_certificate_hash(this.__wbg_ptr);
            deferred1_0 = ret[0];
            deferred1_1 = ret[1];
            return getStringFromWasm0(ret[0], ret[1]);
        } finally {
            wasm.__wbindgen_free(deferred1_0, deferred1_1, 1);
        }
    }
    /**
     * Hashes of the transactions that could not be certified
     * @returns {string[]}
     */
    get non_certified_transactions() {
        const ret = wasm.__wbg_get_cardanotransactionsproofsv2_non_certified_transactions(this.__wbg_ptr);
        var v1 = getArrayJsValueFromWasm0(ret[0], ret[1]).slice();
        wasm.__wbindgen_free(ret[0], ret[1] * 4, 4);
        return v1;
    }
    /**
     * Hash of the certificate that validates this proof Merkle root
     * @param {string} arg0
     */
    set certificate_hash(arg0) {
        const ptr0 = passStringToWasm0(arg0, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
        const len0 = WASM_VECTOR_LEN;
        wasm.__wbg_set_cardanotransactionsproofsv2_certificate_hash(this.__wbg_ptr, ptr0, len0);
    }
    /**
     * Hashes of the transactions that could not be certified
     * @param {string[]} arg0
     */
    set non_certified_transactions(arg0) {
        const ptr0 = passArrayJsValueToWasm0(arg0, wasm.__wbindgen_malloc);
        const len0 = WASM_VECTOR_LEN;
        wasm.__wbg_set_cardanotransactionsproofsv2_non_certified_transactions(this.__wbg_ptr, ptr0, len0);
    }
}
if (Symbol.dispose) CardanoTransactionsProofsV2.prototype[Symbol.dispose] = CardanoTransactionsProofsV2.prototype.free;
exports.CardanoTransactionsProofsV2 = CardanoTransactionsProofsV2;

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
     * Proof of the transactions
     * @param {string} arg0
     */
    set proof(arg0) {
        const ptr0 = passStringToWasm0(arg0, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
        const len0 = WASM_VECTOR_LEN;
        wasm.__wbg_set_cardanotransactionssetproofmessagepart_proof(this.__wbg_ptr, ptr0, len0);
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
    cancel() {
        const ptr = this.__destroy_into_raw();
        wasm.intounderlyingbytesource_cancel(ptr);
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
    cancel() {
        const ptr = this.__destroy_into_raw();
        wasm.intounderlyingsource_cancel(ptr);
    }
    /**
     * @param {ReadableStreamDefaultController} controller
     * @returns {Promise<any>}
     */
    pull(controller) {
        const ret = wasm.intounderlyingsource_pull(this.__wbg_ptr, controller);
        return ret;
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
     * Call the client to fetch the current Mithril era
     * @returns {Promise<any>}
     */
    fetch_current_mithril_era() {
        const ret = wasm.mithrilclient_fetch_current_mithril_era(this.__wbg_ptr);
        return ret;
    }
    /**
     * `unstable` Call the client to get a Cardano block proof
     * @param {any[]} ctx_hashes
     * @returns {Promise<CardanoBlocksProofs>}
     */
    get_cardano_block_proof(ctx_hashes) {
        const ptr0 = passArrayJsValueToWasm0(ctx_hashes, wasm.__wbindgen_malloc);
        const len0 = WASM_VECTOR_LEN;
        const ret = wasm.mithrilclient_get_cardano_block_proof(this.__wbg_ptr, ptr0, len0);
        return ret;
    }
    /**
     * `unstable` Call the client to get a Cardano blocks snapshot from a hash
     * @param {string} hash
     * @returns {Promise<any>}
     */
    get_cardano_blocks_snapshot(hash) {
        const ptr0 = passStringToWasm0(hash, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
        const len0 = WASM_VECTOR_LEN;
        const ret = wasm.mithrilclient_get_cardano_blocks_snapshot(this.__wbg_ptr, ptr0, len0);
        return ret;
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
     * Call the client to get a cardano stake distribution from an epoch
     * @param {any} param
     * @returns {Promise<any>}
     */
    get_cardano_stake_distribution_for_latest_epoch(param) {
        const ret = wasm.mithrilclient_get_cardano_stake_distribution_for_latest_epoch(this.__wbg_ptr, param);
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
     * `unstable` Call the client to get a Cardano transactions V2 proof
     * @param {any[]} ctx_hashes
     * @returns {Promise<CardanoTransactionsProofsV2>}
     */
    get_cardano_transaction_v2_proof(ctx_hashes) {
        const ptr0 = passArrayJsValueToWasm0(ctx_hashes, wasm.__wbindgen_malloc);
        const len0 = WASM_VECTOR_LEN;
        const ret = wasm.mithrilclient_get_cardano_transaction_v2_proof(this.__wbg_ptr, ptr0, len0);
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
     * `unstable` Call the client to get a Cardano transactions V2 snapshot from a hash
     * @param {string} hash
     * @returns {Promise<any>}
     */
    get_cardano_transactions_v2_snapshot(hash) {
        const ptr0 = passStringToWasm0(hash, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
        const len0 = WASM_VECTOR_LEN;
        const ret = wasm.mithrilclient_get_cardano_transactions_v2_snapshot(this.__wbg_ptr, ptr0, len0);
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
     * `unstable` Check if the certificate verifier cache is enabled
     * @returns {Promise<boolean>}
     */
    is_certificate_verifier_cache_enabled() {
        const ret = wasm.mithrilclient_is_certificate_verifier_cache_enabled(this.__wbg_ptr);
        return ret;
    }
    /**
     * `unstable` Call the client for the list of available Cardano blocks snapshots
     * @returns {Promise<any>}
     */
    list_cardano_blocks_snapshots() {
        const ret = wasm.mithrilclient_list_cardano_blocks_snapshots(this.__wbg_ptr);
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
     * Call the client for the list of available cardano database snapshots for a given epoch
     * @param {bigint} epoch
     * @returns {Promise<any>}
     */
    list_cardano_database_v2_per_epoch(epoch) {
        const ret = wasm.mithrilclient_list_cardano_database_v2_per_epoch(this.__wbg_ptr, epoch);
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
     * Call the client for the list of available Cardano transactions snapshots
     * @returns {Promise<any>}
     */
    list_cardano_transactions_snapshots() {
        const ret = wasm.mithrilclient_list_cardano_transactions_snapshots(this.__wbg_ptr);
        return ret;
    }
    /**
     * `unstable` Call the client for the list of available Cardano transactions V2 snapshots
     * @returns {Promise<any>}
     */
    list_cardano_transactions_v2_snapshots() {
        const ret = wasm.mithrilclient_list_cardano_transactions_v2_snapshots(this.__wbg_ptr);
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
     * Call the client for the list of available mithril stake distributions
     * @returns {Promise<any>}
     */
    list_mithril_stake_distributions() {
        const ret = wasm.mithrilclient_list_mithril_stake_distributions(this.__wbg_ptr);
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
     * `unstable` Reset the certificate verifier cache if enabled
     * @returns {Promise<void>}
     */
    reset_certificate_verifier_cache() {
        const ret = wasm.mithrilclient_reset_certificate_verifier_cache(this.__wbg_ptr);
        return ret;
    }
    /**
     * `unstable` Call the client to verify a cardano block proof and compute a message
     * @param {CardanoBlocksProofs} cardano_block_proof
     * @param {any} certificate
     * @returns {Promise<any>}
     */
    verify_cardano_block_proof_then_compute_message(cardano_block_proof, certificate) {
        _assertClass(cardano_block_proof, CardanoBlocksProofs);
        const ret = wasm.mithrilclient_verify_cardano_block_proof_then_compute_message(this.__wbg_ptr, cardano_block_proof.__wbg_ptr, certificate);
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
    /**
     * `unstable` Call the client to verify a cardano transaction V2 proof and compute a message
     * @param {CardanoTransactionsProofsV2} cardano_transaction_proof
     * @param {any} certificate
     * @returns {Promise<any>}
     */
    verify_cardano_transaction_v2_proof_then_compute_message(cardano_transaction_proof, certificate) {
        _assertClass(cardano_transaction_proof, CardanoTransactionsProofsV2);
        const ret = wasm.mithrilclient_verify_cardano_transaction_v2_proof_then_compute_message(this.__wbg_ptr, cardano_transaction_proof.__wbg_ptr, certificate);
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
     * Call the client to verify a mithril stake distribution message
     * @param {any} message
     * @param {any} certificate
     * @returns {Promise<any>}
     */
    verify_message_match_certificate(message, certificate) {
        const ret = wasm.mithrilclient_verify_message_match_certificate(this.__wbg_ptr, message, certificate);
        return ret;
    }
}
if (Symbol.dispose) MithrilClient.prototype[Symbol.dispose] = MithrilClient.prototype.free;
exports.MithrilClient = MithrilClient;
function __wbg_get_imports() {
    const import0 = {
        __proto__: null,
        __wbg_Error_960c155d3d49e4c2: function(arg0, arg1) {
            const ret = Error(getStringFromWasm0(arg0, arg1));
            return ret;
        },
        __wbg_Number_32bf70a599af1d4b: function(arg0) {
            const ret = Number(arg0);
            return ret;
        },
        __wbg___wbindgen_bigint_get_as_i64_3d3aba5d616c6a51: function(arg0, arg1) {
            const v = arg1;
            const ret = typeof(v) === 'bigint' ? v : undefined;
            getDataViewMemory0().setBigInt64(arg0 + 8 * 1, isLikeNone(ret) ? BigInt(0) : ret, true);
            getDataViewMemory0().setInt32(arg0 + 4 * 0, !isLikeNone(ret), true);
        },
        __wbg___wbindgen_boolean_get_6ea149f0a8dcc5ff: function(arg0) {
            const v = arg0;
            const ret = typeof(v) === 'boolean' ? v : undefined;
            return isLikeNone(ret) ? 0xFFFFFF : ret ? 1 : 0;
        },
        __wbg___wbindgen_debug_string_ab4b34d23d6778bd: function(arg0, arg1) {
            const ret = debugString(arg1);
            const ptr1 = passStringToWasm0(ret, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
            const len1 = WASM_VECTOR_LEN;
            getDataViewMemory0().setInt32(arg0 + 4 * 1, len1, true);
            getDataViewMemory0().setInt32(arg0 + 4 * 0, ptr1, true);
        },
        __wbg___wbindgen_in_a5d8b22e52b24dd1: function(arg0, arg1) {
            const ret = arg0 in arg1;
            return ret;
        },
        __wbg___wbindgen_is_bigint_ec25c7f91b4d9e93: function(arg0) {
            const ret = typeof(arg0) === 'bigint';
            return ret;
        },
        __wbg___wbindgen_is_function_3baa9db1a987f47d: function(arg0) {
            const ret = typeof(arg0) === 'function';
            return ret;
        },
        __wbg___wbindgen_is_object_63322ec0cd6ea4ef: function(arg0) {
            const val = arg0;
            const ret = typeof(val) === 'object' && val !== null;
            return ret;
        },
        __wbg___wbindgen_is_string_6df3bf7ef1164ed3: function(arg0) {
            const ret = typeof(arg0) === 'string';
            return ret;
        },
        __wbg___wbindgen_is_undefined_29a43b4d42920abd: function(arg0) {
            const ret = arg0 === undefined;
            return ret;
        },
        __wbg___wbindgen_jsval_eq_d3465d8a07697228: function(arg0, arg1) {
            const ret = arg0 === arg1;
            return ret;
        },
        __wbg___wbindgen_jsval_loose_eq_cac3565e89b4134c: function(arg0, arg1) {
            const ret = arg0 == arg1;
            return ret;
        },
        __wbg___wbindgen_number_get_c7f42aed0525c451: function(arg0, arg1) {
            const obj = arg1;
            const ret = typeof(obj) === 'number' ? obj : undefined;
            getDataViewMemory0().setFloat64(arg0 + 8 * 1, isLikeNone(ret) ? 0 : ret, true);
            getDataViewMemory0().setInt32(arg0 + 4 * 0, !isLikeNone(ret), true);
        },
        __wbg___wbindgen_string_get_7ed5322991caaec5: function(arg0, arg1) {
            const obj = arg1;
            const ret = typeof(obj) === 'string' ? obj : undefined;
            var ptr1 = isLikeNone(ret) ? 0 : passStringToWasm0(ret, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
            var len1 = WASM_VECTOR_LEN;
            getDataViewMemory0().setInt32(arg0 + 4 * 1, len1, true);
            getDataViewMemory0().setInt32(arg0 + 4 * 0, ptr1, true);
        },
        __wbg___wbindgen_throw_6b64449b9b9ed33c: function(arg0, arg1) {
            throw new Error(getStringFromWasm0(arg0, arg1));
        },
        __wbg__wbg_cb_unref_b46c9b5a9f08ec37: function(arg0) {
            arg0._wbg_cb_unref();
        },
        __wbg_abort_4ce5b484434ef6fd: function(arg0) {
            arg0.abort();
        },
        __wbg_abort_d53712380a54cc81: function(arg0, arg1) {
            arg0.abort(arg1);
        },
        __wbg_append_e8fc56ce7c00e874: function() { return handleError(function (arg0, arg1, arg2, arg3, arg4) {
            arg0.append(getStringFromWasm0(arg1, arg2), getStringFromWasm0(arg3, arg4));
        }, arguments); },
        __wbg_arrayBuffer_848c392b70c67d3d: function() { return handleError(function (arg0) {
            const ret = arg0.arrayBuffer();
            return ret;
        }, arguments); },
        __wbg_buffer_d0f5ea0926a691fd: function(arg0) {
            const ret = arg0.buffer;
            return ret;
        },
        __wbg_byobRequest_dc6aed9db01b12c6: function(arg0) {
            const ret = arg0.byobRequest;
            return isLikeNone(ret) ? 0 : addToExternrefTable0(ret);
        },
        __wbg_byteLength_3e660e5661f3327e: function(arg0) {
            const ret = arg0.byteLength;
            return ret;
        },
        __wbg_byteOffset_ecd62abe44dd28d4: function(arg0) {
            const ret = arg0.byteOffset;
            return ret;
        },
        __wbg_call_14b169f759b26747: function() { return handleError(function (arg0, arg1) {
            const ret = arg0.call(arg1);
            return ret;
        }, arguments); },
        __wbg_call_a24592a6f349a97e: function() { return handleError(function (arg0, arg1, arg2) {
            const ret = arg0.call(arg1, arg2);
            return ret;
        }, arguments); },
        __wbg_cardanoblock_new: function(arg0) {
            const ret = CardanoBlock.__wrap(arg0);
            return ret;
        },
        __wbg_cardanoblocksproofs_new: function(arg0) {
            const ret = CardanoBlocksProofs.__wrap(arg0);
            return ret;
        },
        __wbg_cardanotransaction_new: function(arg0) {
            const ret = CardanoTransaction.__wrap(arg0);
            return ret;
        },
        __wbg_cardanotransactionsproofs_new: function(arg0) {
            const ret = CardanoTransactionsProofs.__wrap(arg0);
            return ret;
        },
        __wbg_cardanotransactionsproofsv2_new: function(arg0) {
            const ret = CardanoTransactionsProofsV2.__wrap(arg0);
            return ret;
        },
        __wbg_cardanotransactionssetproofmessagepart_new: function(arg0) {
            const ret = CardanoTransactionsSetProofMessagePart.__wrap(arg0);
            return ret;
        },
        __wbg_cardanotransactionssetproofmessagepart_unwrap: function(arg0) {
            const ret = CardanoTransactionsSetProofMessagePart.__unwrap(arg0);
            return ret;
        },
        __wbg_clearTimeout_2256f1e7b94ef517: function(arg0) {
            const ret = clearTimeout(arg0);
            return ret;
        },
        __wbg_close_0aa6756f298a2c2d: function(arg0) {
            arg0.close();
        },
        __wbg_close_e6c8977a002e9e13: function() { return handleError(function (arg0) {
            arg0.close();
        }, arguments); },
        __wbg_close_fb954dfaf67b5732: function() { return handleError(function (arg0) {
            arg0.close();
        }, arguments); },
        __wbg_done_9158f7cc8751ba32: function(arg0) {
            const ret = arg0.done;
            return ret;
        },
        __wbg_enqueue_4767ce322820c94d: function() { return handleError(function (arg0, arg1) {
            arg0.enqueue(arg1);
        }, arguments); },
        __wbg_entries_bf727fcd7bf35a41: function(arg0) {
            const ret = arg0.entries();
            return ret;
        },
        __wbg_entries_e0b73aa8571ddb56: function(arg0) {
            const ret = Object.entries(arg0);
            return ret;
        },
        __wbg_fetch_0d322c0aed196b8b: function(arg0, arg1) {
            const ret = arg0.fetch(arg1);
            return ret;
        },
        __wbg_fetch_43b2f110608a59ff: function(arg0) {
            const ret = fetch(arg0);
            return ret;
        },
        __wbg_getItem_7fe1351b9ea3b2f3: function() { return handleError(function (arg0, arg1, arg2, arg3) {
            const ret = arg1.getItem(getStringFromWasm0(arg2, arg3));
            var ptr1 = isLikeNone(ret) ? 0 : passStringToWasm0(ret, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
            var len1 = WASM_VECTOR_LEN;
            getDataViewMemory0().setInt32(arg0 + 4 * 1, len1, true);
            getDataViewMemory0().setInt32(arg0 + 4 * 0, ptr1, true);
        }, arguments); },
        __wbg_getRandomValues_ef12552bf5acd2fe: function() { return handleError(function (arg0, arg1) {
            globalThis.crypto.getRandomValues(getArrayU8FromWasm0(arg0, arg1));
        }, arguments); },
        __wbg_getTime_da7c55f52b71e8c6: function(arg0) {
            const ret = arg0.getTime();
            return ret;
        },
        __wbg_get_1affdbdd5573b16a: function() { return handleError(function (arg0, arg1) {
            const ret = Reflect.get(arg0, arg1);
            return ret;
        }, arguments); },
        __wbg_get_8360291721e2339f: function(arg0, arg1) {
            const ret = arg0[arg1 >>> 0];
            return ret;
        },
        __wbg_get_unchecked_17f53dad852b9588: function(arg0, arg1) {
            const ret = arg0[arg1 >>> 0];
            return ret;
        },
        __wbg_get_with_ref_key_6412cf3094599694: function(arg0, arg1) {
            const ret = arg0[arg1];
            return ret;
        },
        __wbg_has_880f1d472f7cecba: function() { return handleError(function (arg0, arg1) {
            const ret = Reflect.has(arg0, arg1);
            return ret;
        }, arguments); },
        __wbg_headers_6022deb4e576fb8e: function(arg0) {
            const ret = arg0.headers;
            return ret;
        },
        __wbg_instanceof_ArrayBuffer_7c8433c6ed14ffe3: function(arg0) {
            let result;
            try {
                result = arg0 instanceof ArrayBuffer;
            } catch (_) {
                result = false;
            }
            const ret = result;
            return ret;
        },
        __wbg_instanceof_Response_9b2d111407865ff2: function(arg0) {
            let result;
            try {
                result = arg0 instanceof Response;
            } catch (_) {
                result = false;
            }
            const ret = result;
            return ret;
        },
        __wbg_instanceof_Uint8Array_152ba1f289edcf3f: function(arg0) {
            let result;
            try {
                result = arg0 instanceof Uint8Array;
            } catch (_) {
                result = false;
            }
            const ret = result;
            return ret;
        },
        __wbg_instanceof_Window_cc64c86c8ef9e02b: function(arg0) {
            let result;
            try {
                result = arg0 instanceof Window;
            } catch (_) {
                result = false;
            }
            const ret = result;
            return ret;
        },
        __wbg_isArray_c3109d14ffc06469: function(arg0) {
            const ret = Array.isArray(arg0);
            return ret;
        },
        __wbg_isSafeInteger_4fc213d1989d6d2a: function(arg0) {
            const ret = Number.isSafeInteger(arg0);
            return ret;
        },
        __wbg_iterator_013bc09ec998c2a7: function() {
            const ret = Symbol.iterator;
            return ret;
        },
        __wbg_key_8413ee53931c540f: function() { return handleError(function (arg0, arg1, arg2) {
            const ret = arg1.key(arg2 >>> 0);
            var ptr1 = isLikeNone(ret) ? 0 : passStringToWasm0(ret, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
            var len1 = WASM_VECTOR_LEN;
            getDataViewMemory0().setInt32(arg0 + 4 * 1, len1, true);
            getDataViewMemory0().setInt32(arg0 + 4 * 0, ptr1, true);
        }, arguments); },
        __wbg_length_1d7129addb9154f9: function() { return handleError(function (arg0) {
            const ret = arg0.length;
            return ret;
        }, arguments); },
        __wbg_length_3d4ecd04bd8d22f1: function(arg0) {
            const ret = arg0.length;
            return ret;
        },
        __wbg_length_9f1775224cf1d815: function(arg0) {
            const ret = arg0.length;
            return ret;
        },
        __wbg_localStorage_f5f66b1ffd2486bc: function() { return handleError(function (arg0) {
            const ret = arg0.localStorage;
            return isLikeNone(ret) ? 0 : addToExternrefTable0(ret);
        }, arguments); },
        __wbg_new_0_4d657201ced14de3: function() {
            const ret = new Date();
            return ret;
        },
        __wbg_new_0c7403db6e782f19: function(arg0) {
            const ret = new Uint8Array(arg0);
            return ret;
        },
        __wbg_new_15a4889b4b90734d: function() { return handleError(function () {
            const ret = new Headers();
            return ret;
        }, arguments); },
        __wbg_new_34d45cc8e36aaead: function() {
            const ret = new Map();
            return ret;
        },
        __wbg_new_5e360d2ff7b9e1c3: function(arg0, arg1) {
            const ret = new Error(getStringFromWasm0(arg0, arg1));
            return ret;
        },
        __wbg_new_682678e2f47e32bc: function() {
            const ret = new Array();
            return ret;
        },
        __wbg_new_98c22165a42231aa: function() { return handleError(function () {
            const ret = new AbortController();
            return ret;
        }, arguments); },
        __wbg_new_aa8d0fa9762c29bd: function() {
            const ret = new Object();
            return ret;
        },
        __wbg_new_aadb2b3f13e701cf: function() { return handleError(function (arg0, arg1) {
            const ret = new BroadcastChannel(getStringFromWasm0(arg0, arg1));
            return ret;
        }, arguments); },
        __wbg_new_from_slice_b5ea43e23f6008c0: function(arg0, arg1) {
            const ret = new Uint8Array(getArrayU8FromWasm0(arg0, arg1));
            return ret;
        },
        __wbg_new_typed_323f37fd55ab048d: function(arg0, arg1) {
            try {
                var state0 = {a: arg0, b: arg1};
                var cb0 = (arg0, arg1) => {
                    const a = state0.a;
                    state0.a = 0;
                    try {
                        return wasm_bindgen__convert__closures_____invoke__he20fbab6b1673584(a, state0.b, arg0, arg1);
                    } finally {
                        state0.a = a;
                    }
                };
                const ret = new Promise(cb0);
                return ret;
            } finally {
                state0.a = 0;
            }
        },
        __wbg_new_with_byte_offset_and_length_01848e8d6a3d49ad: function(arg0, arg1, arg2) {
            const ret = new Uint8Array(arg0, arg1 >>> 0, arg2 >>> 0);
            return ret;
        },
        __wbg_new_with_str_and_init_897be1708e42f39d: function() { return handleError(function (arg0, arg1, arg2) {
            const ret = new Request(getStringFromWasm0(arg0, arg1), arg2);
            return ret;
        }, arguments); },
        __wbg_next_0340c4ae324393c3: function() { return handleError(function (arg0) {
            const ret = arg0.next();
            return ret;
        }, arguments); },
        __wbg_next_7646edaa39458ef7: function(arg0) {
            const ret = arg0.next;
            return ret;
        },
        __wbg_postMessage_f9ee88e3c733baf9: function() { return handleError(function (arg0, arg1) {
            arg0.postMessage(arg1);
        }, arguments); },
        __wbg_prototypesetcall_a6b02eb00b0f4ce2: function(arg0, arg1, arg2) {
            Uint8Array.prototype.set.call(getArrayU8FromWasm0(arg0, arg1), arg2);
        },
        __wbg_queueMicrotask_5d15a957e6aa920e: function(arg0) {
            queueMicrotask(arg0);
        },
        __wbg_queueMicrotask_f8819e5ffc402f36: function(arg0) {
            const ret = arg0.queueMicrotask;
            return ret;
        },
        __wbg_removeItem_487c385a3066a8ed: function() { return handleError(function (arg0, arg1, arg2) {
            arg0.removeItem(getStringFromWasm0(arg1, arg2));
        }, arguments); },
        __wbg_resolve_e6c466bc1052f16c: function(arg0) {
            const ret = Promise.resolve(arg0);
            return ret;
        },
        __wbg_respond_008ca9525ae22847: function() { return handleError(function (arg0, arg1) {
            arg0.respond(arg1 >>> 0);
        }, arguments); },
        __wbg_setItem_e6399d3faae141dc: function() { return handleError(function (arg0, arg1, arg2, arg3, arg4) {
            arg0.setItem(getStringFromWasm0(arg1, arg2), getStringFromWasm0(arg3, arg4));
        }, arguments); },
        __wbg_setTimeout_b188b3bcc8977c7d: function(arg0, arg1) {
            const ret = setTimeout(arg0, arg1);
            return ret;
        },
        __wbg_set_3bf1de9fab0cd644: function(arg0, arg1, arg2) {
            arg0[arg1 >>> 0] = arg2;
        },
        __wbg_set_3d484eb794afec82: function(arg0, arg1, arg2) {
            arg0.set(getArrayU8FromWasm0(arg1, arg2));
        },
        __wbg_set_6be42768c690e380: function(arg0, arg1, arg2) {
            arg0[arg1] = arg2;
        },
        __wbg_set_body_be11680f34217f75: function(arg0, arg1) {
            arg0.body = arg1;
        },
        __wbg_set_cache_968edea422613d1b: function(arg0, arg1) {
            arg0.cache = __wbindgen_enum_RequestCache[arg1];
        },
        __wbg_set_credentials_6577be90e0e85eb6: function(arg0, arg1) {
            arg0.credentials = __wbindgen_enum_RequestCredentials[arg1];
        },
        __wbg_set_fde2cec06c23692b: function(arg0, arg1, arg2) {
            const ret = arg0.set(arg1, arg2);
            return ret;
        },
        __wbg_set_headers_50fc01786240a440: function(arg0, arg1) {
            arg0.headers = arg1;
        },
        __wbg_set_method_c9f1f985f6b6c427: function(arg0, arg1, arg2) {
            arg0.method = getStringFromWasm0(arg1, arg2);
        },
        __wbg_set_mode_5e08d503428c06b9: function(arg0, arg1) {
            arg0.mode = __wbindgen_enum_RequestMode[arg1];
        },
        __wbg_set_signal_1d4e73c2305a0e7c: function(arg0, arg1) {
            arg0.signal = arg1;
        },
        __wbg_signal_fdc54643b47bf85b: function(arg0) {
            const ret = arg0.signal;
            return ret;
        },
        __wbg_static_accessor_GLOBAL_8cfadc87a297ca02: function() {
            const ret = typeof global === 'undefined' ? null : global;
            return isLikeNone(ret) ? 0 : addToExternrefTable0(ret);
        },
        __wbg_static_accessor_GLOBAL_THIS_602256ae5c8f42cf: function() {
            const ret = typeof globalThis === 'undefined' ? null : globalThis;
            return isLikeNone(ret) ? 0 : addToExternrefTable0(ret);
        },
        __wbg_static_accessor_SELF_e445c1c7484aecc3: function() {
            const ret = typeof self === 'undefined' ? null : self;
            return isLikeNone(ret) ? 0 : addToExternrefTable0(ret);
        },
        __wbg_static_accessor_WINDOW_f20e8576ef1e0f17: function() {
            const ret = typeof window === 'undefined' ? null : window;
            return isLikeNone(ret) ? 0 : addToExternrefTable0(ret);
        },
        __wbg_status_43e0d2f15b22d69f: function(arg0) {
            const ret = arg0.status;
            return ret;
        },
        __wbg_text_595ef75535aa25c1: function() { return handleError(function (arg0) {
            const ret = arg0.text();
            return ret;
        }, arguments); },
        __wbg_then_792e0c862b060889: function(arg0, arg1, arg2) {
            const ret = arg0.then(arg1, arg2);
            return ret;
        },
        __wbg_then_8e16ee11f05e4827: function(arg0, arg1) {
            const ret = arg0.then(arg1);
            return ret;
        },
        __wbg_url_2bf741820e6563a0: function(arg0, arg1) {
            const ret = arg1.url;
            const ptr1 = passStringToWasm0(ret, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
            const len1 = WASM_VECTOR_LEN;
            getDataViewMemory0().setInt32(arg0 + 4 * 1, len1, true);
            getDataViewMemory0().setInt32(arg0 + 4 * 0, ptr1, true);
        },
        __wbg_value_ee3a06f4579184fa: function(arg0) {
            const ret = arg0.value;
            return ret;
        },
        __wbg_view_701664ffb3b1ce67: function(arg0) {
            const ret = arg0.view;
            return isLikeNone(ret) ? 0 : addToExternrefTable0(ret);
        },
        __wbg_warn_3cc416af27dbdc02: function(arg0) {
            console.warn(arg0);
        },
        __wbindgen_cast_0000000000000001: function(arg0, arg1) {
            // Cast intrinsic for `Closure(Closure { owned: true, function: Function { arguments: [Externref], shim_idx: 1925, ret: Result(Unit), inner_ret: Some(Result(Unit)) }, mutable: true }) -> Externref`.
            const ret = makeMutClosure(arg0, arg1, wasm_bindgen__convert__closures_____invoke__h023fd7017d50e9e0);
            return ret;
        },
        __wbindgen_cast_0000000000000002: function(arg0, arg1) {
            // Cast intrinsic for `Closure(Closure { owned: true, function: Function { arguments: [], shim_idx: 1754, ret: Unit, inner_ret: Some(Unit) }, mutable: true }) -> Externref`.
            const ret = makeMutClosure(arg0, arg1, wasm_bindgen__convert__closures_____invoke__hc4b57cdb0d692e9e);
            return ret;
        },
        __wbindgen_cast_0000000000000003: function(arg0) {
            // Cast intrinsic for `F64 -> Externref`.
            const ret = arg0;
            return ret;
        },
        __wbindgen_cast_0000000000000004: function(arg0, arg1) {
            // Cast intrinsic for `Ref(String) -> Externref`.
            const ret = getStringFromWasm0(arg0, arg1);
            return ret;
        },
        __wbindgen_cast_0000000000000005: function(arg0) {
            // Cast intrinsic for `U64 -> Externref`.
            const ret = BigInt.asUintN(64, arg0);
            return ret;
        },
        __wbindgen_init_externref_table: function() {
            const table = wasm.__wbindgen_externrefs;
            const offset = table.grow(4);
            table.set(0, undefined);
            table.set(offset + 0, undefined);
            table.set(offset + 1, null);
            table.set(offset + 2, true);
            table.set(offset + 3, false);
        },
    };
    return {
        __proto__: null,
        "./mithril_client_wasm_bg.js": import0,
    };
}

function wasm_bindgen__convert__closures_____invoke__hc4b57cdb0d692e9e(arg0, arg1) {
    wasm.wasm_bindgen__convert__closures_____invoke__hc4b57cdb0d692e9e(arg0, arg1);
}

function wasm_bindgen__convert__closures_____invoke__h023fd7017d50e9e0(arg0, arg1, arg2) {
    const ret = wasm.wasm_bindgen__convert__closures_____invoke__h023fd7017d50e9e0(arg0, arg1, arg2);
    if (ret[1]) {
        throw takeFromExternrefTable0(ret[0]);
    }
}

function wasm_bindgen__convert__closures_____invoke__he20fbab6b1673584(arg0, arg1, arg2, arg3) {
    wasm.wasm_bindgen__convert__closures_____invoke__he20fbab6b1673584(arg0, arg1, arg2, arg3);
}


const __wbindgen_enum_ReadableStreamType = ["bytes"];


const __wbindgen_enum_RequestCache = ["default", "no-store", "reload", "no-cache", "force-cache", "only-if-cached"];


const __wbindgen_enum_RequestCredentials = ["omit", "same-origin", "include"];


const __wbindgen_enum_RequestMode = ["same-origin", "no-cors", "cors", "navigate"];
const CardanoBlockFinalization = (typeof FinalizationRegistry === 'undefined')
    ? { register: () => {}, unregister: () => {} }
    : new FinalizationRegistry(ptr => wasm.__wbg_cardanoblock_free(ptr >>> 0, 1));
const CardanoBlocksProofsFinalization = (typeof FinalizationRegistry === 'undefined')
    ? { register: () => {}, unregister: () => {} }
    : new FinalizationRegistry(ptr => wasm.__wbg_cardanoblocksproofs_free(ptr >>> 0, 1));
const CardanoTransactionFinalization = (typeof FinalizationRegistry === 'undefined')
    ? { register: () => {}, unregister: () => {} }
    : new FinalizationRegistry(ptr => wasm.__wbg_cardanotransaction_free(ptr >>> 0, 1));
const CardanoTransactionsProofsFinalization = (typeof FinalizationRegistry === 'undefined')
    ? { register: () => {}, unregister: () => {} }
    : new FinalizationRegistry(ptr => wasm.__wbg_cardanotransactionsproofs_free(ptr >>> 0, 1));
const CardanoTransactionsProofsV2Finalization = (typeof FinalizationRegistry === 'undefined')
    ? { register: () => {}, unregister: () => {} }
    : new FinalizationRegistry(ptr => wasm.__wbg_cardanotransactionsproofsv2_free(ptr >>> 0, 1));
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
    : new FinalizationRegistry(state => wasm.__wbindgen_destroy_closure(state.a, state.b));

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

function makeMutClosure(arg0, arg1, f) {
    const state = { a: arg0, b: arg1, cnt: 1 };
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
            wasm.__wbindgen_destroy_closure(state.a, state.b);
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

function takeFromExternrefTable0(idx) {
    const value = wasm.__wbindgen_externrefs.get(idx);
    wasm.__externref_table_dealloc(idx);
    return value;
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
    };
}

let WASM_VECTOR_LEN = 0;

const wasmPath = `${__dirname}/mithril_client_wasm_bg.wasm`;
const wasmBytes = require('fs').readFileSync(wasmPath);
const wasmModule = new WebAssembly.Module(wasmBytes);
let wasm = new WebAssembly.Instance(wasmModule, __wbg_get_imports()).exports;
wasm.__wbindgen_start();
