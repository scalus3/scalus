package scalus.serialization.flat

import org.scalatest.funsuite.AnyFunSuite
import scalus.compiler.sir.SIRType

class FlatInstancesCodecTest extends AnyFunSuite {

    test("corrupt SIR.Case pattern tag fails with a descriptive exception (audit S4)") {
        // 0xc0 = binary 11000000: the first 2 bits decode to pattern tag 3, which is unused
        val dec = HashConsedDecoderState(DecoderState(Array(0xc0.toByte)), HashConsed.State.empty)
        val e = intercept[IllegalStateException] {
            FlatInstances.SIRCaseHashConsedFlat.decodeHC(dec)
        }
        assert(e.getMessage.contains("pattern tag"))
    }

    test("Data type bitSizeHC matches its tag-only encoding (audit S4)") {
        import FlatInstances.SIRTypeHashConsedFlat
        val size = SIRTypeHashConsedFlat.bitSizeHC(SIRType.Data.tp, HashConsed.State.empty)
        assert(size == SIRTypeHashConsedFlat.tagWidth)
    }

    test("TypeVar optId round-trips, including Some(0) and negative ids (audit S2)") {
        import FlatInstances.given
        val fl = summon[HashConsedFlat[SIRType.TypeVar]]
        for optId <- List(None, Some(0L), Some(1L), Some(-65L), Some(Long.MinValue)) do
            val tv = SIRType.TypeVar("A", optId, SIRType.TypeVarKind.Fixed)
            val state = HashConsed.State.empty
            val enc = HashConsedEncoderState(
              EncoderState(fl.bitSizeHC(tv, state) / 8 + 1),
              state
            )
            fl.encodeHC(tv, enc)
            val dec =
                HashConsedDecoderState(DecoderState(enc.encode.result), HashConsed.State.empty)
            assert(fl.decodeHC(dec) == tv, s"for optId=$optId")
    }
}
