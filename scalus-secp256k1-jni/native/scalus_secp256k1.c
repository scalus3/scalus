/**
 * JNI bindings for libsecp256k1
 *
 * Provides ECDSA and Schnorr signature verification for Cardano CIP-49 compatibility.
 * Uses secp256k1 v0.6.0 API.
 *
 * Copyright 2024 Scalus
 * Licensed under the Apache License, Version 2.0
 */

#include <jni.h>
#include <secp256k1.h>
#include <secp256k1_extrakeys.h>
#include <secp256k1_schnorrsig.h>
#include <string.h>
#include <stdint.h>
#include <pthread.h>

/* Context for secp256k1 operations - lazily initialized */
static secp256k1_context* ctx = NULL;
static pthread_once_t ctx_init_once = PTHREAD_ONCE_INIT;

/**
 * Initialize the secp256k1 context (called once via pthread_once)
 * Uses SECP256K1_CONTEXT_VERIFY from secp256k1.h for verification operations.
 */
static void init_context(void) {
    ctx = secp256k1_context_create(SECP256K1_CONTEXT_VERIFY);
}

/**
 * Get the secp256k1 context, initializing if necessary
 */
static secp256k1_context* get_context(void) {
    pthread_once(&ctx_init_once, init_context);
    return ctx;
}

/**
 * JNI method: scalus.crypto.Secp256k1Context.secp256k1_init_context
 *
 * Initializes the secp256k1 context and returns a pointer to it.
 *
 * @return the context pointer as a long
 */
JNIEXPORT jlong JNICALL Java_scalus_crypto_Secp256k1Context_secp256k1_1init_1context(
    JNIEnv *env,
    jclass cls
) {
    secp256k1_context* context = get_context();
    return (jlong)(uintptr_t)context;
}

/**
 * JNI method: scalus.crypto.NativeSecp256k1.isValidPubKey
 *
 * Validates a secp256k1 public key by attempting to parse it.
 *
 * @param pubKey The public key bytes (33 bytes compressed or 65 bytes uncompressed)
 * @return true if valid, false otherwise
 */
JNIEXPORT jboolean JNICALL Java_scalus_crypto_NativeSecp256k1_isValidPubKey(
    JNIEnv *env,
    jclass cls,
    jbyteArray pubKey
) {
    secp256k1_context* context = get_context();
    if (context == NULL) {
        return JNI_FALSE;
    }

    jsize pubkey_len = (*env)->GetArrayLength(env, pubKey);
    if (pubkey_len != 33 && pubkey_len != 65) {
        return JNI_FALSE;
    }

    jbyte* pubkey_bytes = (*env)->GetByteArrayElements(env, pubKey, NULL);
    if (pubkey_bytes == NULL) {
        return JNI_FALSE;
    }

    secp256k1_pubkey parsed_pubkey;
    int result = secp256k1_ec_pubkey_parse(
        context,
        &parsed_pubkey,
        (const unsigned char*)pubkey_bytes,
        (size_t)pubkey_len
    );

    (*env)->ReleaseByteArrayElements(env, pubKey, pubkey_bytes, JNI_ABORT);

    return result == 1 ? JNI_TRUE : JNI_FALSE;
}

/**
 * JNI method: scalus.crypto.NativeSecp256k1.ecdsaVerify
 *
 * Verifies an ECDSA signature on secp256k1.
 *
 * @param msg32 The 32-byte message hash
 * @param sig64 The 64-byte compact signature (r || s)
 * @param pubKey33 The 33-byte compressed public key
 * @return true if signature is valid, false otherwise
 */
JNIEXPORT jboolean JNICALL Java_scalus_crypto_NativeSecp256k1_ecdsaVerify(
    JNIEnv *env,
    jclass cls,
    jbyteArray msg32,
    jbyteArray sig64,
    jbyteArray pubKey33
) {
    secp256k1_context* context = get_context();
    if (context == NULL) {
        return JNI_FALSE;
    }

    /* Validate input lengths */
    if ((*env)->GetArrayLength(env, msg32) != 32 ||
        (*env)->GetArrayLength(env, sig64) != 64 ||
        (*env)->GetArrayLength(env, pubKey33) != 33) {
        return JNI_FALSE;
    }

    /* Get byte arrays */
    jbyte* msg_bytes = (*env)->GetByteArrayElements(env, msg32, NULL);
    jbyte* sig_bytes = (*env)->GetByteArrayElements(env, sig64, NULL);
    jbyte* pubkey_bytes = (*env)->GetByteArrayElements(env, pubKey33, NULL);

    if (msg_bytes == NULL || sig_bytes == NULL || pubkey_bytes == NULL) {
        if (msg_bytes) (*env)->ReleaseByteArrayElements(env, msg32, msg_bytes, JNI_ABORT);
        if (sig_bytes) (*env)->ReleaseByteArrayElements(env, sig64, sig_bytes, JNI_ABORT);
        if (pubkey_bytes) (*env)->ReleaseByteArrayElements(env, pubKey33, pubkey_bytes, JNI_ABORT);
        return JNI_FALSE;
    }

    int result = 0;

    /* Parse the public key */
    secp256k1_pubkey pubkey;
    if (secp256k1_ec_pubkey_parse(context, &pubkey,
            (const unsigned char*)pubkey_bytes, 33) != 1) {
        goto cleanup;
    }

    /* Parse the compact signature */
    secp256k1_ecdsa_signature sig;
    if (secp256k1_ecdsa_signature_parse_compact(context, &sig,
            (const unsigned char*)sig_bytes) != 1) {
        goto cleanup;
    }

    /* Verify the signature */
    result = secp256k1_ecdsa_verify(context, &sig,
        (const unsigned char*)msg_bytes, &pubkey);

cleanup:
    (*env)->ReleaseByteArrayElements(env, msg32, msg_bytes, JNI_ABORT);
    (*env)->ReleaseByteArrayElements(env, sig64, sig_bytes, JNI_ABORT);
    (*env)->ReleaseByteArrayElements(env, pubKey33, pubkey_bytes, JNI_ABORT);

    return result == 1 ? JNI_TRUE : JNI_FALSE;
}

/**
 * JNI method: scalus.crypto.NativeSecp256k1.schnorrVerify
 *
 * Verifies a Schnorr signature on secp256k1 per BIP-340.
 * Supports arbitrary message lengths as required by the BIP-340 specification.
 *
 * @param sig64 The 64-byte Schnorr signature
 * @param msg The message to verify (arbitrary length)
 * @param pubKey32 The 32-byte x-only public key
 * @return true if signature is valid, false otherwise
 */
JNIEXPORT jboolean JNICALL Java_scalus_crypto_NativeSecp256k1_schnorrVerify(
    JNIEnv *env,
    jclass cls,
    jbyteArray sig64,
    jbyteArray msg,
    jbyteArray pubKey32
) {
    secp256k1_context* context = get_context();
    if (context == NULL) {
        return JNI_FALSE;
    }

    /* Validate signature and public key lengths */
    if ((*env)->GetArrayLength(env, sig64) != 64 ||
        (*env)->GetArrayLength(env, pubKey32) != 32) {
        return JNI_FALSE;
    }

    jsize msg_len = (*env)->GetArrayLength(env, msg);

    /* Get byte arrays */
    jbyte* sig_bytes = (*env)->GetByteArrayElements(env, sig64, NULL);
    jbyte* msg_bytes = (*env)->GetByteArrayElements(env, msg, NULL);
    jbyte* pubkey_bytes = (*env)->GetByteArrayElements(env, pubKey32, NULL);

    if (sig_bytes == NULL || msg_bytes == NULL || pubkey_bytes == NULL) {
        if (sig_bytes) (*env)->ReleaseByteArrayElements(env, sig64, sig_bytes, JNI_ABORT);
        if (msg_bytes) (*env)->ReleaseByteArrayElements(env, msg, msg_bytes, JNI_ABORT);
        if (pubkey_bytes) (*env)->ReleaseByteArrayElements(env, pubKey32, pubkey_bytes, JNI_ABORT);
        return JNI_FALSE;
    }

    int result = 0;

    /* Parse the x-only public key directly */
    secp256k1_xonly_pubkey xonly_pubkey;
    if (secp256k1_xonly_pubkey_parse(context, &xonly_pubkey,
            (const unsigned char*)pubkey_bytes) != 1) {
        goto cleanup;
    }

    /* Verify the Schnorr signature */
    result = secp256k1_schnorrsig_verify(
        context,
        (const unsigned char*)sig_bytes,
        (const unsigned char*)msg_bytes,
        (size_t)msg_len,
        &xonly_pubkey
    );

cleanup:
    (*env)->ReleaseByteArrayElements(env, sig64, sig_bytes, JNI_ABORT);
    (*env)->ReleaseByteArrayElements(env, msg, msg_bytes, JNI_ABORT);
    (*env)->ReleaseByteArrayElements(env, pubKey32, pubkey_bytes, JNI_ABORT);

    return result == 1 ? JNI_TRUE : JNI_FALSE;
}
