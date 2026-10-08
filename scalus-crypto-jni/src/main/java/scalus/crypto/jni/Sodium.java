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

/** Ed25519 verification with libsodium at the commit cardano-node links (IOG fork dbb48cce). */
public final class Sodium {
    private Sodium() {}

    /**
     * {@code crypto_sign_ed25519_verify_detached}. No checks beyond the lengths.
     *
     * @throws IllegalArgumentException unless sig is 64 bytes and pk is 32 bytes
     */
    public static boolean ed25519VerifyDetached(byte[] sig, byte[] msg, byte[] pk) {
        CryptoJni.requireEnabled();
        requireNonNull(sig, "sig");
        requireNonNull(msg, "msg");
        requireNonNull(pk, "pk");
        return ed25519VerifyDetached0(sig, msg, pk);
    }

    private static native boolean ed25519VerifyDetached0(byte[] sig, byte[] msg, byte[] pk);
}
