/*
 * libsecp256k1 bindings. The call order matches cardano-crypto-class, as Plutus uses it:
 * parse the key, parse the compact signature, verify. No other checks. Verification needs no
 * precomputed tables since v0.2.0, so every call uses the built-in secp256k1_context_static.
 */
#include <stdlib.h>
#include <secp256k1.h>
#include <secp256k1_extrakeys.h>
#include <secp256k1_schnorrsig.h>
#include "jni_util.h"

#define CTX secp256k1_context_static

/* ec_pubkey_parse, ecdsa_signature_parse_compact, ecdsa_verify. No S normalisation. A key or
 * signature that does not parse throws IllegalArgumentException, where cardano-crypto-class raises
 * an error; a parsed signature that does not verify returns false. */
JNIEXPORT jboolean JNICALL Java_scalus_crypto_jni_Secp256k1_ecdsaVerify0(
    JNIEnv *env, jclass cls, jbyteArray msg32, jbyteArray sig64, jbyteArray pk33) {
    unsigned char msg[32], sig[64], pk[33];
    if (!read_exact(env, msg32, msg, 32, "message hash must be 32 bytes")) return JNI_FALSE;
    if (!read_exact(env, sig64, sig, 64, "signature must be 64 bytes")) return JNI_FALSE;
    if (!read_exact(env, pk33, pk, 33, "public key must be 33 bytes")) return JNI_FALSE;
    secp256k1_pubkey pubkey;
    secp256k1_ecdsa_signature signature;
    if (secp256k1_ec_pubkey_parse(CTX, &pubkey, pk, 33) != 1) {
        throw_iae(env, "invalid public key");
        return JNI_FALSE;
    }
    if (secp256k1_ecdsa_signature_parse_compact(CTX, &signature, sig) != 1) {
        throw_iae(env, "invalid signature: r or s out of range");
        return JNI_FALSE;
    }
    return secp256k1_ecdsa_verify(CTX, &signature, msg, &pubkey) == 1 ? JNI_TRUE : JNI_FALSE;
}

/* xonly_pubkey_parse, schnorrsig_verify (BIP-340, any message length). A key that does not
 * parse throws IllegalArgumentException; a signature that does not verify returns false. */
JNIEXPORT jboolean JNICALL Java_scalus_crypto_jni_Secp256k1_schnorrVerify0(
    JNIEnv *env, jclass cls, jbyteArray sig64, jbyteArray msg, jbyteArray pk32) {
    unsigned char sig[64], pk[32];
    if (!read_exact(env, sig64, sig, 64, "signature must be 64 bytes")) return JNI_FALSE;
    if (!read_exact(env, pk32, pk, 32, "x-only public key must be 32 bytes")) return JNI_FALSE;
    secp256k1_xonly_pubkey pubkey;
    if (secp256k1_xonly_pubkey_parse(CTX, &pubkey, pk) != 1) {
        throw_iae(env, "invalid public key");
        return JNI_FALSE;
    }
    jsize len;
    unsigned char *m = read_bytes(env, msg, &len);
    if (m == NULL) return JNI_FALSE;
    int ok = secp256k1_schnorrsig_verify(CTX, sig, m, (size_t) len, &pubkey);
    free(m);
    return ok == 1 ? JNI_TRUE : JNI_FALSE;
}
