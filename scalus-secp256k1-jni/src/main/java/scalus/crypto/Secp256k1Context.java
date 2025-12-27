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

import org.scijava.nativelib.NativeLoader;
import java.io.IOException;

/**
 * Manages the secp256k1 native library loading and context initialization.
 *
 * <p>This class automatically detects the operating system and architecture,
 * extracts the appropriate native library from JAR resources, and loads it.</p>
 *
 * <p>Supported platforms:</p>
 * <ul>
 *   <li>Linux x86_64 (linux_64)</li>
 *   <li>Linux ARM64 (linux_arm64)</li>
 *   <li>macOS x86_64 (osx_64)</li>
 *   <li>macOS ARM64/Apple Silicon (osx_arm64)</li>
 * </ul>
 */
public class Secp256k1Context {
    private static final boolean enabled;
    private static final long context;

    static {
        boolean isEnabled = true;
        long contextRef = -1;

        try {
            // Use NativeLoader to extract and load the library from JAR resources
            NativeLoader.loadLibrary("scalus_secp256k1");
            contextRef = secp256k1_init_context();
        } catch (IOException | UnsatisfiedLinkError e) {
            System.err.println("Failed to load secp256k1 native library: " + e.getMessage());
            isEnabled = false;
        }

        enabled = isEnabled;
        context = contextRef;
    }

    /**
     * Returns whether the secp256k1 native library was successfully loaded.
     *
     * @return true if the library is available, false otherwise
     */
    public static boolean isEnabled() {
        return enabled;
    }

    /**
     * Returns the native secp256k1 context pointer.
     *
     * @return the context pointer, or -1 if the library failed to load
     */
    public static long getContext() {
        return context;
    }

    /**
     * Initializes the native secp256k1 context.
     *
     * @return the context pointer
     */
    private static native long secp256k1_init_context();
}
