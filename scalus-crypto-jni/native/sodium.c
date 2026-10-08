#include <stdlib.h>
#include <sodium.h>
#include "jni_util.h"

/* crypto_sign_ed25519_verify_detached, called as cardano-crypto-class Ed25519DSIGN does. */
JNIEXPORT jboolean JNICALL Java_scalus_crypto_jni_Sodium_ed25519VerifyDetached0(
    JNIEnv *env, jclass cls, jbyteArray sig, jbyteArray msg, jbyteArray pk) {
    unsigned char sig_buf[crypto_sign_ed25519_BYTES];
    unsigned char pk_buf[crypto_sign_ed25519_PUBLICKEYBYTES];
    if (!read_exact(env, sig, sig_buf, sizeof sig_buf, "signature must be 64 bytes")) return JNI_FALSE;
    if (!read_exact(env, pk, pk_buf, sizeof pk_buf, "public key must be 32 bytes")) return JNI_FALSE;
    jsize len;
    unsigned char *m = read_bytes(env, msg, &len);
    if (m == NULL) return JNI_FALSE;
    int rc = crypto_sign_ed25519_verify_detached(sig_buf, m, (unsigned long long) len, pk_buf);
    free(m);
    return rc == 0 ? JNI_TRUE : JNI_FALSE;
}
