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
 * secp256k1 verification with libsecp256k1 at the commit cardano-node links (v0.3.2), called in
 * the order cardano-crypto-class uses for the Plutus builtins.
 */
public final class Secp256k1 {
    private Secp256k1() {}

    /**
     * {@code ec_pubkey_parse}, {@code ecdsa_signature_parse_compact}, {@code ecdsa_verify}. False if
     * the signature does not verify.
     *
     * @throws IllegalArgumentException if a length is wrong, the key does not parse, or r or s is
     *     not below the group order, the cases where cardano-crypto-class raises an error
     */
    public static boolean ecdsaVerify(byte[] msg32, byte[] sig64, byte[] pk33) {
        CryptoJni.requireEnabled();
        requireNonNull(msg32, "msg32");
        requireNonNull(sig64, "sig64");
        requireNonNull(pk33, "pk33");
        return ecdsaVerify0(msg32, sig64, pk33);
    }

    /**
     * {@code xonly_pubkey_parse}, {@code schnorrsig_verify} (BIP-340, any message length). False if
     * the signature does not verify.
     *
     * @throws IllegalArgumentException if a length is wrong or the key does not parse
     */
    public static boolean schnorrVerify(byte[] sig64, byte[] msg, byte[] pk32) {
        CryptoJni.requireEnabled();
        requireNonNull(sig64, "sig64");
        requireNonNull(msg, "msg");
        requireNonNull(pk32, "pk32");
        return schnorrVerify0(sig64, msg, pk32);
    }

    private static native boolean ecdsaVerify0(byte[] msg32, byte[] sig64, byte[] pk33);
    private static native boolean schnorrVerify0(byte[] sig64, byte[] msg, byte[] pk32);
}
