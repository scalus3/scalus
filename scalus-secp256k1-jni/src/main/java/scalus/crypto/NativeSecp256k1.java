/*
 * Copyright 2024 Scalus
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
package scalus.crypto;

/**
 * JNI bindings for libsecp256k1.
 *
 * Provides ECDSA and Schnorr signature verification for Cardano CIP-49 compatibility.
 * Uses secp256k1 v0.6.0 API.
 */
public class NativeSecp256k1 {

    /**
     * Validates a secp256k1 public key.
     *
     * @param pubKey The public key bytes (33 bytes compressed or 65 bytes uncompressed)
     * @return true if the public key is valid, false otherwise
     */
    public static native boolean isValidPubKey(byte[] pubKey);

    /**
     * Verifies an ECDSA signature on secp256k1.
     *
     * @param msg32 The 32-byte message hash to verify
     * @param sig64 The 64-byte compact signature (r || s format)
     * @param pubKey33 The 33-byte compressed public key
     * @return true if the signature is valid, false otherwise
     */
    public static native boolean ecdsaVerify(byte[] msg32, byte[] sig64, byte[] pubKey33);

    /**
     * Verifies a Schnorr signature on secp256k1 (BIP-340).
     * Supports arbitrary message lengths as required by the BIP-340 specification.
     *
     * @param sig64 The 64-byte Schnorr signature
     * @param msg The message to verify (arbitrary length per BIP-340)
     * @param pubKey32 The 32-byte x-only public key
     * @return true if the signature is valid, false otherwise
     */
    public static native boolean schnorrVerify(byte[] sig64, byte[] msg, byte[] pubKey32);
}
