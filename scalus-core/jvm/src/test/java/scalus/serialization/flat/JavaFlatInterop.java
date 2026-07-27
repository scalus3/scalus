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
