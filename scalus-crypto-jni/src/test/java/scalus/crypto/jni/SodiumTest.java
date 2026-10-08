package scalus.crypto.jni;

import static org.junit.Assert.*;

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.junit.Test;

public class SodiumTest {
    static final String FIXTURE = "../scalus-core/shared/src/test/resources/ed25519/libsodium-verdicts.tsv";

    static final Pattern HEADER = Pattern.compile("# rows=(\\d+) accepts=(\\d+)");

    static byte[] hex(String s) {
        byte[] out = new byte[s.length() / 2];
        for (int i = 0; i < out.length; i++) out[i] = (byte) Integer.parseInt(s.substring(2 * i, 2 * i + 2), 16);
        return out;
    }

    @Test
    public void libraryLoads() {
        assertTrue("scalus_crypto did not load", CryptoJni.isEnabled());
    }

    @Test
    public void givesLibsodiumVerdictOnEveryVector() throws Exception {
        List<String> wrong = new ArrayList<>();
        int rows = 0, accepted = 0, expectedRows = -1, expectedAccepts = -1;
        for (String line : Files.readAllLines(Paths.get(FIXTURE), StandardCharsets.UTF_8)) {
            Matcher header = HEADER.matcher(line);
            if (header.matches()) {
                expectedRows = Integer.parseInt(header.group(1));
                expectedAccepts = Integer.parseInt(header.group(2));
            }
            if (line.isEmpty() || line.startsWith("#")) continue;
            String[] f = line.split("\t");
            boolean expected = f[5].equals("accept");
            rows++;
            if (expected) accepted++;
            if (Sodium.ed25519VerifyDetached(hex(f[4]), hex(f[3]), hex(f[2])) != expected) wrong.add(f[0] + "#" + f[1]);
        }
        assertTrue("fixture has no '# rows=N accepts=M' header", expectedRows >= 0);
        assertEquals(expectedRows, rows);
        assertEquals(expectedAccepts, accepted);
        assertTrue(wrong.size() + " mismatches: " + wrong, wrong.isEmpty());
    }

    @Test(expected = IllegalArgumentException.class)
    public void rejectsShortSignature() {
        Sodium.ed25519VerifyDetached(new byte[63], new byte[0], new byte[32]);
    }

    @Test
    public void nullArgumentsThrowNullPointerException() {
        assertThrows(NullPointerException.class, () -> Sodium.ed25519VerifyDetached(new byte[64], null, new byte[32]));
        assertThrows(NullPointerException.class, () -> Sodium.ed25519VerifyDetached(null, new byte[0], new byte[32]));
        assertThrows(NullPointerException.class, () -> Sodium.ed25519VerifyDetached(new byte[64], new byte[0], null));
    }
}
