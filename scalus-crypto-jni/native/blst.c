/*
 * blst bindings. Each function mirrors the cardano-crypto-class BLS12_381 function it replaces
 * (cardano-base 060819b5, Internal.hs). Points and ML results are raw blst structs.
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <blst.h>
#include "jni_util.h"

static void throw_blst(JNIEnv *env, BLST_ERROR err) {
    char msg[32];
    snprintf(msg, sizeof msg, "BLST_ERROR %d", (int) err);
    throw_iae(env, msg);
}

#define BLST_GROUP(X, G, POINT, AFFINE, COMPRESSED)                                              \
JNIEXPORT jbyteArray JNICALL Java_scalus_crypto_jni_Blst_##X##Uncompress0(                      \
    JNIEnv *env, jclass c, jbyteArray in) {                                                      \
    byte buf[COMPRESSED]; AFFINE a; POINT p;                                                     \
    if (!read_exact(env, in, buf, COMPRESSED, "wrong compressed point length")) return NULL;     \
    BLST_ERROR err = blst_##X##_uncompress(&a, buf);                                             \
    if (err != BLST_SUCCESS) { throw_blst(env, err); return NULL; }                              \
    blst_##X##_from_affine(&p, &a);                                                              \
    if (!blst_##X##_in_##G(&p)) { throw_iae(env, "point not in group"); return NULL; }          \
    return new_array(env, &p, sizeof p);                                                         \
}                                                                                                \
JNIEXPORT jbyteArray JNICALL Java_scalus_crypto_jni_Blst_##X##Compress0(                        \
    JNIEnv *env, jclass c, jbyteArray in) {                                                      \
    POINT p; byte out[COMPRESSED];                                                               \
    if (!read_exact(env, in, &p, sizeof p, "wrong point length")) return NULL;                  \
    blst_##X##_compress(out, &p);                                                                \
    return new_array(env, out, COMPRESSED);                                                      \
}                                                                                                \
JNIEXPORT jbyteArray JNICALL Java_scalus_crypto_jni_Blst_##X##AddOrDouble0(                     \
    JNIEnv *env, jclass c, jbyteArray a, jbyteArray b) {                                         \
    POINT pa, pb, r;                                                                             \
    if (!read_exact(env, a, &pa, sizeof pa, "wrong point length")) return NULL;                 \
    if (!read_exact(env, b, &pb, sizeof pb, "wrong point length")) return NULL;                 \
    blst_##X##_add_or_double(&r, &pa, &pb);                                                      \
    return new_array(env, &r, sizeof r);                                                         \
}                                                                                                \
JNIEXPORT jbyteArray JNICALL Java_scalus_crypto_jni_Blst_##X##Neg0(                             \
    JNIEnv *env, jclass c, jbyteArray in) {                                                      \
    POINT p;                                                                                     \
    if (!read_exact(env, in, &p, sizeof p, "wrong point length")) return NULL;                  \
    blst_##X##_cneg(&p, 1);                                                                      \
    return new_array(env, &p, sizeof p);                                                         \
}                                                                                                \
JNIEXPORT jbyteArray JNICALL Java_scalus_crypto_jni_Blst_##X##Mult0(                            \
    JNIEnv *env, jclass c, jbyteArray in, jbyteArray scalar_be) {                                \
    POINT p, r; byte be[32]; blst_scalar s;                                                      \
    if (!read_exact(env, in, &p, sizeof p, "wrong point length")) return NULL;                  \
    if (!read_exact(env, scalar_be, be, 32, "scalar must be 32 bytes")) return NULL;            \
    blst_scalar_from_bendian(&s, be);                                                            \
    blst_##X##_mult(&r, &p, s.b, 256);                                                           \
    return new_array(env, &r, sizeof r);                                                         \
}                                                                                                \
JNIEXPORT jboolean JNICALL Java_scalus_crypto_jni_Blst_##X##IsEqual0(                           \
    JNIEnv *env, jclass c, jbyteArray a, jbyteArray b) {                                         \
    POINT pa, pb;                                                                                \
    if (!read_exact(env, a, &pa, sizeof pa, "wrong point length")) return JNI_FALSE;            \
    if (!read_exact(env, b, &pb, sizeof pb, "wrong point length")) return JNI_FALSE;            \
    return blst_##X##_is_equal(&pa, &pb) ? JNI_TRUE : JNI_FALSE;                                 \
}                                                                                                \
JNIEXPORT jbyteArray JNICALL Java_scalus_crypto_jni_Blst_##X##HashTo0(                          \
    JNIEnv *env, jclass c, jbyteArray msg, jbyteArray dst) {                                     \
    jsize ml, dl; POINT r;                                                                       \
    unsigned char *m = read_bytes(env, msg, &ml);                                                \
    if (m == NULL) return NULL;                                                                  \
    unsigned char *d = read_bytes(env, dst, &dl);                                                \
    if (d == NULL) { free(m); return NULL; }                                                     \
    blst_hash_to_##G(&r, m, (size_t) ml, d, (size_t) dl, NULL, 0);                               \
    free(m); free(d);                                                                            \
    return new_array(env, &r, sizeof r);                                                         \
}                                                                                                \
/* cardano-crypto-class blsMSM: skip infinity points and zero scalars; 0 pairs -> zero,         \
   1 pair -> mult with 256 bits, otherwise Pippenger with nbits = 255. */                        \
JNIEXPORT jbyteArray JNICALL Java_scalus_crypto_jni_Blst_##X##Msm0(                             \
    JNIEnv *env, jclass c, jbyteArray points, jbyteArray scalars_be) {                           \
    jbyteArray out = NULL; POINT r;                                                              \
    POINT *ps = NULL; blst_scalar *ss = NULL; unsigned char *sb = NULL;                          \
    const POINT **pp = NULL; const byte **sp = NULL; AFFINE *aff = NULL; limb_t *scratch = NULL; \
    jsize pl = (*env)->GetArrayLength(env, points), sl;                                          \
    size_t n = (size_t) pl / sizeof(POINT);                                                      \
    if ((size_t) pl != n * sizeof(POINT) || (size_t) (*env)->GetArrayLength(env, scalars_be) != n * 32) { \
        throw_iae(env, "points and scalars do not match"); return NULL; }                        \
    ps = malloc((n ? n : 1) * sizeof(POINT));                                                    \
    ss = malloc((n ? n : 1) * sizeof(blst_scalar));                                              \
    if (ps == NULL || ss == NULL) { throw_oom(env); goto cleanup; }                              \
    if ((sb = read_bytes(env, scalars_be, &sl)) == NULL) goto cleanup;                           \
    if (pl > 0) (*env)->GetByteArrayRegion(env, points, 0, pl, (jbyte *) ps);                    \
    size_t k = 0;                                                                                \
    static const blst_scalar zero_scalar;                                                        \
    for (size_t i = 0; i < n; i++) {                                                             \
        blst_scalar_from_bendian(&ss[k], sb + i * 32);                                           \
        if (blst_##X##_is_inf(&ps[i])) continue;                                                 \
        if (memcmp(ss[k].b, zero_scalar.b, sizeof ss[k].b) == 0) continue;                       \
        if (k != i) ps[k] = ps[i];                                                               \
        k++;                                                                                     \
    }                                                                                            \
    if (k == 0) {                                                                                \
        byte inf[COMPRESSED] = { 0xc0 }; AFFINE a;                                               \
        blst_##X##_uncompress(&a, inf);                                                          \
        blst_##X##_from_affine(&r, &a);                                                          \
    } else if (k == 1) {                                                                         \
        blst_##X##_mult(&r, &ps[0], ss[0].b, 256);                                               \
    } else {                                                                                     \
        pp = malloc(k * sizeof(POINT *));                                                        \
        sp = malloc(k * sizeof(byte *));                                                         \
        aff = malloc(k * sizeof(AFFINE));                                                        \
        scratch = malloc(blst_##X##s_mult_pippenger_scratch_sizeof(k));                          \
        if (pp == NULL || sp == NULL || aff == NULL || scratch == NULL) { throw_oom(env); goto cleanup; } \
        for (size_t i = 0; i < k; i++) { pp[i] = &ps[i]; sp[i] = ss[i].b; }                      \
        blst_##X##s_to_affine(aff, (const POINT *const *) pp, k);                                \
        const AFFINE *ap[2] = { aff, NULL };                                                     \
        blst_##X##s_mult_pippenger(&r, (const AFFINE *const *) ap, k, (const byte *const *) sp, 255, scratch); \
    }                                                                                            \
    out = new_array(env, &r, sizeof r);                                                          \
cleanup:                                                                                         \
    free(ps); free(ss); free(sb); free(pp); free(sp); free(aff); free(scratch);                  \
    return out;                                                                                  \
}

BLST_GROUP(p1, g1, blst_p1, blst_p1_affine, 48)
BLST_GROUP(p2, g2, blst_p2, blst_p2_affine, 96)

JNIEXPORT jbyteArray JNICALL Java_scalus_crypto_jni_Blst_millerLoop0(
    JNIEnv *env, jclass c, jbyteArray p1, jbyteArray p2) {
    blst_p1 a; blst_p2 b; blst_p1_affine aa; blst_p2_affine ba; blst_fp12 r;
    if (!read_exact(env, p1, &a, sizeof a, "wrong G1 point length")) return NULL;
    if (!read_exact(env, p2, &b, sizeof b, "wrong G2 point length")) return NULL;
    blst_p1_to_affine(&aa, &a);
    blst_p2_to_affine(&ba, &b);
    blst_miller_loop(&r, &ba, &aa);
    return new_array(env, &r, sizeof r);
}

JNIEXPORT jbyteArray JNICALL Java_scalus_crypto_jni_Blst_fp12Mul0(
    JNIEnv *env, jclass c, jbyteArray x, jbyteArray y) {
    blst_fp12 a, b, r;
    if (!read_exact(env, x, &a, sizeof a, "wrong ML result length")) return NULL;
    if (!read_exact(env, y, &b, sizeof b, "wrong ML result length")) return NULL;
    blst_fp12_mul(&r, &a, &b);
    return new_array(env, &r, sizeof r);
}

JNIEXPORT jboolean JNICALL Java_scalus_crypto_jni_Blst_fp12IsEqual0(
    JNIEnv *env, jclass c, jbyteArray x, jbyteArray y) {
    blst_fp12 a, b;
    if (!read_exact(env, x, &a, sizeof a, "wrong ML result length")) return JNI_FALSE;
    if (!read_exact(env, y, &b, sizeof b, "wrong ML result length")) return JNI_FALSE;
    return blst_fp12_is_equal(&a, &b) ? JNI_TRUE : JNI_FALSE;
}

JNIEXPORT jboolean JNICALL Java_scalus_crypto_jni_Blst_finalVerify0(
    JNIEnv *env, jclass c, jbyteArray x, jbyteArray y) {
    blst_fp12 a, b;
    if (!read_exact(env, x, &a, sizeof a, "wrong ML result length")) return JNI_FALSE;
    if (!read_exact(env, y, &b, sizeof b, "wrong ML result length")) return JNI_FALSE;
    return blst_fp12_finalverify(&a, &b) ? JNI_TRUE : JNI_FALSE;
}
