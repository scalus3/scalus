/*
 * Copyright 2026 Scalus
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package scalus.crypto.jni;

import static java.util.Objects.requireNonNull;

/**
 * BLS12-381 with blst at the commit cardano-node links (v0.3.15). Each method mirrors the
 * cardano-crypto-class function behind the matching Plutus builtin.
 *
 * <p>Points and ML results are raw blst structs: {@code blst_p1} (144 bytes), {@code blst_p2}
 * (288 bytes), {@code blst_fp12} (576 bytes). Pass only arrays this class returned; methods check
 * lengths and nothing else. Errors throw {@link IllegalArgumentException}.</p>
 */
public final class Blst {
    private Blst() {}

    /** Uncompresses a 48-byte G1 point: error if the encoding is bad or the point is not in G1. */
    public static byte[] p1Uncompress(byte[] compressed) {
        CryptoJni.requireEnabled();
        requireNonNull(compressed, "compressed");
        return p1Uncompress0(compressed);
    }

    /** Compresses a G1 point to 48 bytes. */
    public static byte[] p1Compress(byte[] p) {
        CryptoJni.requireEnabled();
        requireNonNull(p, "p");
        return p1Compress0(p);
    }

    /** {@code blst_p1_add_or_double}: a + b, also when a == b. */
    public static byte[] p1AddOrDouble(byte[] a, byte[] b) {
        CryptoJni.requireEnabled();
        requireNonNull(a, "a");
        requireNonNull(b, "b");
        return p1AddOrDouble0(a, b);
    }

    /** -p. */
    public static byte[] p1Neg(byte[] p) {
        CryptoJni.requireEnabled();
        requireNonNull(p, "p");
        return p1Neg0(p);
    }

    /** [s]p, with s given as 32 big-endian bytes already reduced mod r. */
    public static byte[] p1Mult(byte[] p, byte[] scalarBE32) {
        CryptoJni.requireEnabled();
        requireNonNull(p, "p");
        requireNonNull(scalarBE32, "scalarBE32");
        return p1Mult0(p, scalarBE32);
    }

    /** {@code blst_p1_is_equal}. */
    public static boolean p1IsEqual(byte[] a, byte[] b) {
        CryptoJni.requireEnabled();
        requireNonNull(a, "a");
        requireNonNull(b, "b");
        return p1IsEqual0(a, b);
    }

    /** {@code blst_hash_to_g1} with the DST as raw bytes and no augmentation. */
    public static byte[] p1HashTo(byte[] msg, byte[] dst) {
        CryptoJni.requireEnabled();
        requireNonNull(msg, "msg");
        requireNonNull(dst, "dst");
        return p1HashTo0(msg, dst);
    }

    /**
     * Multi-scalar multiplication, as cardano-crypto-class blsMSM: points concatenated (144 bytes
     * each), scalars concatenated (32 big-endian bytes each, reduced mod r).
     */
    public static byte[] p1Msm(byte[] points, byte[] scalarsBE) {
        CryptoJni.requireEnabled();
        requireNonNull(points, "points");
        requireNonNull(scalarsBE, "scalarsBE");
        return p1Msm0(points, scalarsBE);
    }

    private static native byte[] p1Uncompress0(byte[] compressed);
    private static native byte[] p1Compress0(byte[] p);
    private static native byte[] p1AddOrDouble0(byte[] a, byte[] b);
    private static native byte[] p1Neg0(byte[] p);
    private static native byte[] p1Mult0(byte[] p, byte[] scalarBE32);
    private static native boolean p1IsEqual0(byte[] a, byte[] b);
    private static native byte[] p1HashTo0(byte[] msg, byte[] dst);
    private static native byte[] p1Msm0(byte[] points, byte[] scalarsBE);

    /** Uncompresses a 96-byte G2 point: error if the encoding is bad or the point is not in G2. */
    public static byte[] p2Uncompress(byte[] compressed) {
        CryptoJni.requireEnabled();
        requireNonNull(compressed, "compressed");
        return p2Uncompress0(compressed);
    }

    /** Compresses a G2 point to 96 bytes. */
    public static byte[] p2Compress(byte[] p) {
        CryptoJni.requireEnabled();
        requireNonNull(p, "p");
        return p2Compress0(p);
    }

    /** {@code blst_p2_add_or_double}: a + b, also when a == b. */
    public static byte[] p2AddOrDouble(byte[] a, byte[] b) {
        CryptoJni.requireEnabled();
        requireNonNull(a, "a");
        requireNonNull(b, "b");
        return p2AddOrDouble0(a, b);
    }

    /** -p. */
    public static byte[] p2Neg(byte[] p) {
        CryptoJni.requireEnabled();
        requireNonNull(p, "p");
        return p2Neg0(p);
    }

    /** [s]p, with s given as 32 big-endian bytes already reduced mod r. */
    public static byte[] p2Mult(byte[] p, byte[] scalarBE32) {
        CryptoJni.requireEnabled();
        requireNonNull(p, "p");
        requireNonNull(scalarBE32, "scalarBE32");
        return p2Mult0(p, scalarBE32);
    }

    /** {@code blst_p2_is_equal}. */
    public static boolean p2IsEqual(byte[] a, byte[] b) {
        CryptoJni.requireEnabled();
        requireNonNull(a, "a");
        requireNonNull(b, "b");
        return p2IsEqual0(a, b);
    }

    /** {@code blst_hash_to_g2} with the DST as raw bytes and no augmentation. */
    public static byte[] p2HashTo(byte[] msg, byte[] dst) {
        CryptoJni.requireEnabled();
        requireNonNull(msg, "msg");
        requireNonNull(dst, "dst");
        return p2HashTo0(msg, dst);
    }

    /**
     * Multi-scalar multiplication, as cardano-crypto-class blsMSM: points concatenated (288 bytes
     * each), scalars concatenated (32 big-endian bytes each, reduced mod r).
     */
    public static byte[] p2Msm(byte[] points, byte[] scalarsBE) {
        CryptoJni.requireEnabled();
        requireNonNull(points, "points");
        requireNonNull(scalarsBE, "scalarsBE");
        return p2Msm0(points, scalarsBE);
    }

    private static native byte[] p2Uncompress0(byte[] compressed);
    private static native byte[] p2Compress0(byte[] p);
    private static native byte[] p2AddOrDouble0(byte[] a, byte[] b);
    private static native byte[] p2Neg0(byte[] p);
    private static native byte[] p2Mult0(byte[] p, byte[] scalarBE32);
    private static native boolean p2IsEqual0(byte[] a, byte[] b);
    private static native byte[] p2HashTo0(byte[] msg, byte[] dst);
    private static native byte[] p2Msm0(byte[] points, byte[] scalarsBE);

    /** {@code blst_miller_loop} on the affine forms of a G1 and a G2 point. */
    public static byte[] millerLoop(byte[] p1, byte[] p2) {
        CryptoJni.requireEnabled();
        requireNonNull(p1, "p1");
        requireNonNull(p2, "p2");
        return millerLoop0(p1, p2);
    }

    /** {@code blst_fp12_mul}. */
    public static byte[] fp12Mul(byte[] a, byte[] b) {
        CryptoJni.requireEnabled();
        requireNonNull(a, "a");
        requireNonNull(b, "b");
        return fp12Mul0(a, b);
    }

    /** {@code blst_fp12_is_equal}. */
    public static boolean fp12IsEqual(byte[] a, byte[] b) {
        CryptoJni.requireEnabled();
        requireNonNull(a, "a");
        requireNonNull(b, "b");
        return fp12IsEqual0(a, b);
    }

    /** {@code blst_fp12_finalverify}. */
    public static boolean finalVerify(byte[] a, byte[] b) {
        CryptoJni.requireEnabled();
        requireNonNull(a, "a");
        requireNonNull(b, "b");
        return finalVerify0(a, b);
    }


    private static native byte[] millerLoop0(byte[] p1, byte[] p2);
    private static native byte[] fp12Mul0(byte[] a, byte[] b);
    private static native boolean fp12IsEqual0(byte[] a, byte[] b);
    private static native boolean finalVerify0(byte[] a, byte[] b);
}
