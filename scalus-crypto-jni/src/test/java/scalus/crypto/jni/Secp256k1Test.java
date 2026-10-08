package scalus.crypto.jni;

import static org.junit.Assert.*;
import static scalus.crypto.jni.SodiumTest.hex;

import org.junit.Test;

public class Secp256k1Test {
    static final byte[] ECDSA_PK = hex("03427d3132a06e31bf66791dda478b5ebec79bd045247126396fccdf11e42a3627");
    static final byte[] MSG = hex("2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824");
    static final String S = "5ffac010a1bd9b9a275ad685ea4052f4bc72c0dc27094422ba9379e7bf44b29b";
    static final String R = "040f5b6a2bb4e024d47eab02d4073da655af77c0cf0efdb19c6771378da175c4";

    @Test public void ecdsaValid() { assertTrue(Secp256k1.ecdsaVerify(MSG, hex(R + S), ECDSA_PK)); }
    @Test public void ecdsaZeroRIsFalse() {
        assertFalse(Secp256k1.ecdsaVerify(MSG, hex("00".repeat(32) + S), ECDSA_PK));
    }
    // A key or signature that does not parse is an error, as in cardano-crypto-class; a parsed
    // signature that does not verify is false.
    @Test public void ecdsaBadKeyThrows() {
        assertThrows(IllegalArgumentException.class, () -> Secp256k1.ecdsaVerify(MSG, hex(R + S),
            hex("FFFF7d3132a06e31bf66791dda478b5ebec79bd045247126396fccdf11e42a3627")));
    }
    @Test public void schnorrValidCip49() {
        assertTrue(Secp256k1.schnorrVerify(
            hex("4fd97a0c4ad719f89cba68a522e0dee13bcf656ae9c0a395404cda858a7992d8dea979dbc4c83659d695b7d380fe8a75264ba51a63a53fc2a8bd225e50f223f4"),
            MSG,
            hex("427d3132a06e31bf66791dda478b5ebec79bd045247126396fccdf11e42a3627")));
    }
    // BIP-340 test vector 15: an empty message.
    @Test public void schnorrValidEmptyMessage() {
        assertTrue(Secp256k1.schnorrVerify(
            hex("71535db165ecd9fbbc046e5ffaea61186bb6ad436732fccc25291a55895464cf6069ce26bf03466228f19a3a62db8a649f2d560fac652827d1af0574e427ab63"),
            new byte[0],
            hex("778caa53b4393ac467774d09497a87224bf9fab6f6e68b23086497324d6fd117")));
    }

    static final String N = "fffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364141";

    // Cardano raises an error exactly when secp256k1_ecdsa_signature_parse_compact fails: r or s >= n.
    @Test public void ecdsaWithROfNThrows() {
        assertThrows(IllegalArgumentException.class, () -> Secp256k1.ecdsaVerify(MSG, hex(N + S), ECDSA_PK));
    }
    @Test public void ecdsaWithSOfNThrows() {
        assertThrows(IllegalArgumentException.class, () -> Secp256k1.ecdsaVerify(MSG, hex(R + N), ECDSA_PK));
    }
    @Test public void ecdsaOfWrongLengthThrows() {
        assertThrows(IllegalArgumentException.class, () -> Secp256k1.ecdsaVerify(MSG, new byte[63], ECDSA_PK));
    }
    // x = 2^256 - 1 is not below the field prime, so the x-only key does not parse.
    @Test public void schnorrBadKeyThrows() {
        assertThrows(IllegalArgumentException.class, () -> Secp256k1.schnorrVerify(new byte[64], MSG, hex("ff".repeat(32))));
    }
    @Test public void schnorrKeyOfWrongLengthThrows() {
        assertThrows(IllegalArgumentException.class, () -> Secp256k1.schnorrVerify(new byte[64], MSG, new byte[31]));
    }

    @Test public void nullArgumentsThrowNullPointerException() {
        byte[] sig = hex(R + S);
        assertThrows(NullPointerException.class, () -> Secp256k1.ecdsaVerify(null, sig, ECDSA_PK));
        assertThrows(NullPointerException.class, () -> Secp256k1.schnorrVerify(sig, null, new byte[32]));
    }
}
