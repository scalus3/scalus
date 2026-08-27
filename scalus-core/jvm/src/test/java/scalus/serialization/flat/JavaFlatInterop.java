package scalus.serialization.flat;

/** Exercises the flat API from real Java. If this compiles, the API is Java-usable. */
public final class JavaFlatInterop {
    private JavaFlatInterop() {}

    // Uses the @static facade — no MODULE$, no summon.
    public static long roundTripLongViaFacade(long v) {
        byte[] bytes = FlatCodec.encodeLong(v);
        return FlatCodec.decodeLong(bytes);
    }

    // Uses the top-level EncoderState/DecoderState/Flat types directly from Java.
    public static byte[] roundTripBytesViaState(byte[] input) {
        Flat<byte[]> fl = Flats.byteArrayFlat();
        EncoderState enc = new EncoderState(fl.bitSize(input) / 8 + 1);
        fl.encode(input, enc);
        byte[] wire = enc.result();
        return fl.decode(new DecoderState(wire));
    }

    // Low-level helpers as plain statics (mirror-class forwarders of object FlatCodec).
    public static long zigZagRoundTrip(long v) {
        return FlatCodec.zagZig(FlatCodec.zigZag(v));
    }

    public static byte[] word7(long v) {
        return FlatCodec.word7Bytes(v);
    }

    public static String bitString(byte b) {
        return FlatCodec.byteAsBitString(b);
    }

    // Generic encode/decode with an explicit instance from Flats — no summon needed.
    public static String genericStringRoundTrip(String s) {
        byte[] wire = FlatCodec.encode(s, Flats.stringFlat());
        return FlatCodec.decode(wire, Flats.stringFlat());
    }

    // Implements a custom Flat[A] in Java (proves the trait is Java-implementable).
    public static int customFlatBitSize() {
        Flat<String> constFlat = new Flat<String>() {
            public int bitSize(String a) { return 0; }
            public void encode(String a, EncoderState e) {}
            public String decode(DecoderState d) { return ""; }
        };
        return constFlat.bitSize("ignored");
    }
}
