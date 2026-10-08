#ifndef SCALUS_CRYPTO_JNI_UTIL_H
#define SCALUS_CRYPTO_JNI_UTIL_H

#include <jni.h>
#include <stddef.h>
#include <stdlib.h>

/* Throws IllegalArgumentException with the message. */
static inline void throw_iae(JNIEnv *env, const char *msg) {
    jclass cls = (*env)->FindClass(env, "java/lang/IllegalArgumentException");
    if (cls != NULL) (*env)->ThrowNew(env, cls, msg);
}

/* Throws OutOfMemoryError: a native allocation failed. */
static inline void throw_oom(JNIEnv *env) {
    jclass cls = (*env)->FindClass(env, "java/lang/OutOfMemoryError");
    if (cls != NULL) (*env)->ThrowNew(env, cls, "scalus_crypto: native allocation failed");
}

/* Copies a Java array of exactly len bytes into buf; throws and returns 0 otherwise. */
static inline int read_exact(JNIEnv *env, jbyteArray a, void *buf, jsize len, const char *what) {
    if (a == NULL || (*env)->GetArrayLength(env, a) != len) {
        throw_iae(env, what);
        return 0;
    }
    (*env)->GetByteArrayRegion(env, a, 0, len, (jbyte *) buf);
    return 1;
}

/*
 * Copies a whole Java array (not null) into a new malloc'ed buffer; *len receives its length.
 * The buffer is never NULL on success, even for an empty array, so it can go straight to a C API.
 * Throws OutOfMemoryError and returns NULL if the allocation fails. The caller frees the buffer.
 */
static inline unsigned char *read_bytes(JNIEnv *env, jbyteArray a, jsize *len) {
    *len = (*env)->GetArrayLength(env, a);
    unsigned char *buf = malloc(*len > 0 ? (size_t) *len : 1);
    if (buf == NULL) {
        throw_oom(env);
        return NULL;
    }
    if (*len > 0) (*env)->GetByteArrayRegion(env, a, 0, *len, (jbyte *) buf);
    return buf;
}

/* Returns a new Java byte array holding len bytes from buf. */
static inline jbyteArray new_array(JNIEnv *env, const void *buf, jsize len) {
    jbyteArray out = (*env)->NewByteArray(env, len);
    if (out != NULL) (*env)->SetByteArrayRegion(env, out, 0, len, (const jbyte *) buf);
    return out;
}

#endif
