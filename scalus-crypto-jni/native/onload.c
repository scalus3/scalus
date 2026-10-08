/*
 * scalus_crypto: JNI bindings for the C crypto libraries that cardano-node links
 * (libsodium IOG fork, libsecp256k1, blst). See README.md for the pinned commits.
 */
#include <jni.h>
#include <sodium.h>

/*
 * Runs once when the JVM loads the library. Initialises libsodium, as cardano-node does at startup.
 * A failure fails the load, so CryptoJni.isEnabled() is false. libsecp256k1 needs no setup: the
 * verification calls use secp256k1_context_static.
 */
JNIEXPORT jint JNICALL JNI_OnLoad(JavaVM *vm, void *reserved) {
    if (sodium_init() < 0) return JNI_ERR;
    return JNI_VERSION_1_8;
}
