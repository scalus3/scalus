package scalus.serialization.flat

import org.scalatest.funsuite.AnyFunSuite

class JavaInteropTest extends AnyFunSuite {
    test("Java can use the flat facade and top-level types") {
        assert(JavaFlatInterop.roundTripLongViaFacade(123456789012345L) == 123456789012345L)
        val bytes = Array[Byte](11, 22, 33)
        assert(JavaFlatInterop.roundTripBytesViaState(bytes).sameElements(bytes))
        assert(JavaFlatInterop.customFlatBitSize() == 0)
    }

    test("facade encodeLong/decodeLong round-trips large and negative values") {
        for v <- List(0L, -1L, 1L << 40, Long.MinValue, Long.MaxValue) do
            assert(FlatCodec.decodeLong(FlatCodec.encodeLong(v)) == v, s"for $v")
    }
}
