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

import java.io.IOException;
import org.scijava.nativelib.NativeLoader;

/**
 * Loads the scalus_crypto native library: libsodium, libsecp256k1 and blst at the commits
 * cardano-node links. Every public method of the other classes in this package calls
 * {@link #requireEnabled()} first, so the library is loaded before their first native call.
 */
public final class CryptoJni {
    private static final boolean enabled;

    static {
        boolean ok = true;
        try {
            NativeLoader.loadLibrary("scalus_crypto");
        } catch (IOException | UnsatisfiedLinkError e) {
            System.err.println("Failed to load scalus_crypto native library: " + e.getMessage());
            ok = false;
        }
        enabled = ok;
    }

    private CryptoJni() {}

    /** @return true if the native library loaded on this platform */
    public static boolean isEnabled() {
        return enabled;
    }

    /** @throws IllegalStateException if the native library did not load on this platform */
    public static void requireEnabled() {
        if (!enabled) {
            throw new IllegalStateException("scalus-crypto-jni native library not available on "
                + System.getProperty("os.name") + "/" + System.getProperty("os.arch"));
        }
    }
}
