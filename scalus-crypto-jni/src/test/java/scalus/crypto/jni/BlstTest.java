package scalus.crypto.jni;

import static org.junit.Assert.*;
import static scalus.crypto.jni.SodiumTest.hex;

import org.junit.Test;

public class BlstTest {
    // plutus-conformance bls12_381_G1_hashToGroup/hash-dst-len-255: a 255-byte DST with bytes >= 0x80.
    // blst-java turned this DST into a String and got a different point (supranational/blst#232).
    static final String DST255 = "1234567890".repeat(51); // 255 bytes
    static final String EXPECTED_G1 =
        "931bd1f65dd2d34a55c93d82c20dcacd3a91afa5932fdd7fed06119f8574520c9609d337d680060b4bd2c59f0b60bb54";

    static String toHex(byte[] b) {
        StringBuilder sb = new StringBuilder();
        for (byte x : b) sb.append(String.format("%02x", x));
        return sb.toString();
    }

    @Test public void hashToG1WithBinaryDst() {
        assertEquals(EXPECTED_G1, toHex(Blst.p1Compress(Blst.p1HashTo(hex("3f"), hex(DST255)))));
    }

    @Test public void addingAPointToItselfDoubles() {
        byte[] g = Blst.p1HashTo("g".getBytes(), "DST".getBytes());
        byte[] two = new byte[32];
        two[31] = 2;
        assertTrue(Blst.p1IsEqual(Blst.p1AddOrDouble(g, g), Blst.p1Mult(g, two)));
    }

    @Test(expected = IllegalArgumentException.class)
    public void uncompressRejectsBadEncoding() {
        Blst.p1Uncompress(new byte[48]); // compression bit not set
    }

    @Test public void msmOfOnePairEqualsMult() {
        byte[] g = Blst.p1HashTo("g".getBytes(), "DST".getBytes());
        byte[] three = new byte[32];
        three[31] = 3;
        assertTrue(Blst.p1IsEqual(Blst.p1Msm(g, three), Blst.p1Mult(g, three)));
    }

    @Test public void pairingBilinearity() {
        byte[] p = Blst.p1HashTo("p".getBytes(), "DST".getBytes());
        byte[] q = Blst.p2HashTo("q".getBytes(), "DST".getBytes());
        byte[] five = new byte[32];
        five[31] = 5;
        assertTrue(Blst.finalVerify(Blst.millerLoop(Blst.p1Mult(p, five), q), Blst.millerLoop(p, Blst.p2Mult(q, five))));
    }
    static byte[] scalar(int v) {
        byte[] s = new byte[32];
        s[31] = (byte) v;
        return s;
    }

    static byte[] concat(byte[]... parts) {
        int n = 0;
        for (byte[] p : parts) n += p.length;
        byte[] out = new byte[n];
        int off = 0;
        for (byte[] p : parts) { System.arraycopy(p, 0, out, off, p.length); off += p.length; }
        return out;
    }

    // Two or more pairs take the Pippenger path.
    @Test public void msmOfThreePairsEqualsSumOfMults() {
        byte[] a = Blst.p2HashTo("a".getBytes(), "DST".getBytes());
        byte[] b = Blst.p2HashTo("b".getBytes(), "DST".getBytes());
        byte[] c = Blst.p2HashTo("c".getBytes(), "DST".getBytes());
        byte[] expected = Blst.p2AddOrDouble(
            Blst.p2AddOrDouble(Blst.p2Mult(a, scalar(7)), Blst.p2Mult(b, scalar(11))), Blst.p2Mult(c, scalar(13)));
        byte[] msm = Blst.p2Msm(concat(a, b, c), concat(scalar(7), scalar(11), scalar(13)));
        assertTrue(Blst.p2IsEqual(expected, msm));
    }

    // Zero scalars and the point at infinity are dropped; nothing left gives the zero point.
    @Test public void msmWithOnlyZeroScalarsIsZero() {
        byte[] g = Blst.p1HashTo("g".getBytes(), "DST".getBytes());
        byte[] zero = new byte[48];
        zero[0] = (byte) 0xc0;
        assertTrue(Blst.p1IsEqual(Blst.p1Uncompress(zero), Blst.p1Msm(concat(g, g), concat(scalar(0), scalar(0)))));
    }

    // blst_fp12_is_equal compares raw bytes, so MLResult can hash its raw bytes.
    @Test public void equalMlResultsHaveEqualBytes() {
        byte[] p = Blst.p1HashTo("p".getBytes(), "DST".getBytes());
        byte[] q = Blst.p2HashTo("q".getBytes(), "DST".getBytes());
        byte[] a = Blst.millerLoop(Blst.p1AddOrDouble(p, p), q);
        byte[] b = Blst.millerLoop(Blst.p1Mult(p, scalar(2)), q);
        assertTrue(Blst.fp12IsEqual(a, b));
        assertArrayEquals(a, b);
    }

    // plutus-conformance bls12_381_G1_uncompress/out-of-group: on the curve, not in G1.
    @Test public void uncompressRejectsPointOutsideG1() {
        byte[] offGroup = hex("a0" + "00".repeat(46) + "05");
        IllegalArgumentException e = assertThrows(IllegalArgumentException.class, () -> Blst.p1Uncompress(offGroup));
        assertEquals("BLST_ERROR: point is not in group", e.getMessage());
    }

    static byte[] g1Zero() {
        byte[] zero = new byte[48];
        zero[0] = (byte) 0xc0;
        return Blst.p1Uncompress(zero);
    }

    @Test public void msmDropsThePointAtInfinity() {
        byte[] g = Blst.p1HashTo("g".getBytes(), "DST".getBytes());
        assertTrue(Blst.p1IsEqual(Blst.p1Mult(g, scalar(3)), Blst.p1Msm(concat(g, g1Zero()), concat(scalar(3), scalar(5)))));
    }

    @Test public void msmOfNoPairsIsZero() {
        assertTrue(Blst.p1IsEqual(g1Zero(), Blst.p1Msm(new byte[0], new byte[0])));
    }

    // plutus-conformance bls12_381_G1_hashToGroup/hash-empty-dst.
    @Test public void hashToG1WithEmptyDst() {
        assertEquals(
            "9019067bf1fa5b2a7a40fb31a70c66f25a3de7e3ef42f8365c9b7963dc01e15a2e086df6d1a181b1d12811a520440909",
            toHex(Blst.p1Compress(Blst.p1HashTo(hex("8e"), new byte[0]))));
    }

    @Test public void nullArgumentsThrowNullPointerException() {
        byte[] g = Blst.p1HashTo("g".getBytes(), "DST".getBytes());
        assertThrows(NullPointerException.class, () -> Blst.p1HashTo(null, new byte[0]));
        assertThrows(NullPointerException.class, () -> Blst.p1HashTo(new byte[0], null));
        assertThrows(NullPointerException.class, () -> Blst.p1Msm(null, new byte[0]));
        assertThrows(NullPointerException.class, () -> Blst.p1Msm(new byte[0], null));
        assertThrows(NullPointerException.class, () -> Blst.p2HashTo(null, new byte[0]));
        assertThrows(NullPointerException.class, () -> Blst.p2Msm(null, new byte[0]));
        assertThrows(NullPointerException.class, () -> Blst.p1Uncompress(null));
        assertThrows(NullPointerException.class, () -> Blst.p1AddOrDouble(g, null));
        assertThrows(NullPointerException.class, () -> Blst.millerLoop(g, null));
        assertThrows(NullPointerException.class, () -> Blst.fp12Mul(null, null));
    }
}
